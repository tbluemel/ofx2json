//
// ofx2json converts OFX files to JSON
// Copyright (C) 2019  Thomas Bluemel <thomas@reactsoft.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
#include <config.h>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>
#include <string>
#include <utility>
#include <map>
#include <list>
#include <memory>
#include <functional>
#include <algorithm>
#include <ctime>
#include <cassert>
#include <argp.h>
#define RAPIDJSON_HAS_STDSTRING 1
#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>

static char* g_input = nullptr;
static char *g_output = nullptr;
static bool g_quiet = false;

#ifdef DEBUG
#define _logLocationStmt \
	<< __FILE__ << ':' << __LINE__ << ": "
#else
#define _logLocationStmt
#endif
#define logErr(stmt) \
	if (!g_quiet) { \
		std::cerr _logLocationStmt << "Error: " << stmt << std::endl; \
	}
#define logDbg(stmt) \
	if (!g_quiet) { \
		std::ostream& __os = g_output ? std::cerr : std::cout; \
		__os _logLocationStmt << stmt << std::endl; \
	}

static inline std::string str_lower(const std::string& str)
{
	std::string ret(str);
	std::transform(ret.begin(), ret.end(), ret.begin(), ::tolower);
	return ret;
}

static inline bool skip_ws(const std::string& str, size_t& pos)
{
	size_t start = pos;
	while (pos < str.size() && isspace(str[pos]))
		pos++;
	return pos > start;
}

template <typename IntType>
static size_t parse_digits(const std::string& text, size_t pos, size_t len, IntType& val)
{
	val = 0;
	size_t slen = text.size();
	if (len != std::string::npos)
	{
		if (pos + len > slen)
			return 0;
		slen = pos + len;
	}
	size_t i = 0;
	while (pos + i < slen)
	{
		char ch = text[pos + i];
		if (!isdigit(ch))
		{
			if (len != std::string::npos || i == 0)
				return 0;
			break;
		}
		IntType pval = val;
		val = val * 10;
		val += (ch - '0');
		if (val < pval)
			return 0;
		i++;
	}
	return i;
}

static bool parse_datetime(const std::string& text, struct tm& tm, unsigned int& msecs, int& tzoff_min)
{
	size_t len = text.size();
	if (len < 8)
		return false;
	tzoff_min = 0;
	if (!parse_digits(text, 0, 4, tm.tm_year) || tm.tm_year > 9999)
		return false;
	tm.tm_year -= 1900;
	if (!parse_digits(text, 4, 2, tm.tm_mon) || tm.tm_mon == 0 || tm.tm_mon > 12)
		return false;
	tm.tm_mon--;
	if (!parse_digits(text, 6, 2, tm.tm_mday) || tm.tm_mday == 0 || tm.tm_mday > 31)
		return false;
	if (len >= 14)
	{
		if (!parse_digits(text, 8, 2, tm.tm_hour) || tm.tm_hour > 23)
			return false;
		if (!parse_digits(text, 10, 2, tm.tm_min) || tm.tm_min > 59)
			return false;
		if (!parse_digits(text, 12, 2, tm.tm_sec) || tm.tm_sec > 60)
			return false;
		if (len >= 18)
		{
			size_t i = 14;
			if (text[i] == '.')
			{
				i++;
				if (!parse_digits(text, i, 3, msecs))
					return false;
				i += 3;
			}
			
			while (i < len && isspace(text[i]))
				i++;
			
			if (i < len)
			{
				if (text[i] != '[')
					return false;
				i++;
				while (i < len && isspace(text[i]))
					i++;
				if (i >= len)
					return false;
				bool neg = false;
				if (text[i] == '-' || text[i] == '+')
				{
					neg = (text[i] == '-');
					if (++i >= len)
						return false;
				}
				size_t digs = parse_digits(text, i, std::string::npos, tzoff_min);
				if (digs == 0 || tzoff_min > 12)
					return false;
				tzoff_min *= 60;
				i += digs;
				if (i >= len)
					return false;
				if (text[i] == '.')
				{
					if (++i >= len)
						return false;
					
					int tzoff_frac = 0;
					digs = parse_digits(text, i, std::string::npos, tzoff_frac);
					if (digs == 0 || tzoff_frac)
						return false;
					i += digs;
					// TODO: adjust tzoff_min by minutes in tzoff_frac, or is tzoff_frac a fraction of a whole hour?
				}
				if (neg)
					tzoff_min = -tzoff_min;
				while (i < len && isspace(text[i]))
					i++;
				if (i >= len)
					return false;
				if (text[i] == ':')
				{
					i++;
					while (i < len && text[i] != ']')
						i++;
					if (i >= len)
						return false;
				}
				
				if (text[i] != ']')
					return false;
				i++;
				while (i < len && isspace(text[i]))
					i++;
				if (i < len)
					return false;
			}
		}
		else if (len > 14)
			return false;
	}
	else if (len > 8)
	{
		return false;
	}
	else
	{
		tm.tm_hour = 0;
		tm.tm_min = 0;
		tm.tm_sec = 0;
	}

	tm.tm_isdst = -1; // todo?
	
	return true;
}

static bool parse_number(const std::string& text, double& val)
{
	double r = 0.0;
	double f = 1.0;
	size_t pos = 0;
	skip_ws(text, pos);
	if (pos >= text.length())
		return false;
	char ch = text[pos];
	if (ch == '-' || ch == '+')
	{
		if (ch == '-')
			f = -1.0;
		skip_ws(text, pos);
		if (++pos >= text.length())
			return false;
	}
	
	bool isf = false;
	while (pos < text.length())
	{
		ch = text[pos];
		if (ch == '.')
		{
			if (isf)
				return false;
			isf = true;
		}
		else if (ch >= '0' && ch <= '9')
		{
			if (isf)
				f /= 10.0;
			double pr = r;
			r = r * 10.0 + (ch - '0');
			if (r < pr)
				return false;
		}
		else
			break;
		pos++;
	}
	
	skip_ws(text, pos);
	if (pos < text.length())
		return false;
	val = r * f;
	return true;
}

static bool parse_bool(const std::string& text, bool& val)
{
	size_t pos = 0;
	skip_ws(text, pos);
	if (pos >= text.length())
		return false;
	char ch = text[pos];
	if (ch == 'Y' || ch == 'y')
		val = true;
	else if (ch == 'N' || ch == 'n')
		val = false;
	else
		return false;
	skip_ws(text, ++pos);
	return pos >= text.length();
}

static bool read_text(const std::string& str, size_t& pos, std::string& txt)
{
	skip_ws(str, pos);
	while (pos < str.size())
	{
		char ch = str[pos];
		if (ch == '<' || ch == '>')
		{
			if (!txt.empty())
			{
				size_t i = txt.size();
				while (i > 0 && isspace(txt[i - 1]))
					i--;
				txt.erase(i);
			}
			return true;
		}
		txt.push_back(ch);
		pos++;
	}
	
	return false;
}

static bool read_name(const std::string& str, size_t& pos, std::string& txt)
{
	while (pos < str.size())
	{
		char ch = str[pos];
		if (isspace(ch) || ch == '<' || ch == '>' || ch == '/' || ch == '=' || ch == '\"')
			return !txt.empty();
		txt.push_back(ch);
		pos++;
	}
	
	if (!txt.empty())
		return true;
	if (pos >= str.size())
		return false;
	return false;
}

static bool read_attrval(const std::string& str, size_t& pos, std::string& txt, bool quoted)
{
	while (pos < str.size())
	{
		char ch = str[pos];
		if (!quoted && (isspace(ch) || ch == '<' || ch == '>' || ch == '/' || ch == '=' || ch == '\"'))
			return !txt.empty();
		else if (quoted && ch == '\"')
			return !txt.empty();
		
		txt.push_back(ch);
		pos++;
	}
	
	if (!quoted && !txt.empty())
		return true;
	if (pos >= str.size())
		return false;
	return false;
}

static std::string try_xml_decode(const std::string& txt)
{
	std::string ret;
	ret.reserve(txt.size());
	size_t pos = 0;
	while (pos < txt.size())
	{
		char ch = txt[pos];
		if (ch == '&')
		{
			std::string entity;
			bool found_end = false;
			for (size_t i = 0; i < 5 && pos + 1 + i < txt.size(); i++)
			{
				char ch_e = txt[pos + 1 + i];
				if (ch_e == ';')
				{
					found_end = true;
					break;
				}
				entity.push_back(ch_e);
			}
			
			if (found_end && !entity.empty())
			{
				static const std::map<std::string, char> xml_entities = {
					{ "quot", '\"' },
					{ "amp", '&' },
					{ "apos", '\'' },
					{ "lt", '<' },
					{ "gt", '>' },
				};
				
				auto const& it = xml_entities.find(entity);
				if (it != xml_entities.end())
				{
					ret.append(1, it->second);
					pos += entity.size() + 2;
					continue;
				}
			}
		}
		
		ret.push_back(ch);
		pos++;
	}
	
	return ret;
}

template <typename HandleElement>
static bool iterate_elements(const std::string& str, size_t& pos, HandleElement handle_element)
{
	while (pos < str.size())
	{
		skip_ws(str, pos);
		if (pos >= str.size())
			break;
		if (str[pos] != '<')
			return false;
		pos++;
		skip_ws(str, pos);
		if (pos >= str.size())
			return false;
		std::string el_name;
		bool simple_tag = false;
		if (str[pos] == '/')
		{
			el_name.push_back('/');
			pos++;
			skip_ws(str, pos);
			if (pos >= str.size())
				return false;
		}
		std::string el_text;
		std::map<std::string, std::string> el_attrs;
		if (!read_name(str, pos, el_name))
			return false;
		assert(!el_name.empty());
		if (el_name[0] != '/')
		{
			if (skip_ws(str, pos))
			{
				do
				{
					if (str[pos] == '>' || str[pos] == '/')
						break;
					
					std::string at_name, at_val;
					if (!read_name(str, pos, at_name))
						return false;
					skip_ws(str, pos);
					if (pos >= str.size())
						return false;
					if (str[pos] == '=')
					{
						pos++;
						skip_ws(str, pos);
						if (pos >= str.size())
							return false;
						bool quoted = (str[pos] == '\"');
						if (quoted)
							pos++;
						if (!read_attrval(str, pos, at_val, quoted))
							return false;
						if (quoted)
						{
							if (str[pos] != '\"')
								return false;
							pos++;
						}
					}
					skip_ws(str, pos);
					el_attrs.insert(std::make_pair(at_name, try_xml_decode(at_val)));
				} while (pos < str.size());
			}
			
			if (pos >= str.size())
				return false;
			
			
			if (str.size() > 1 && str[pos] == '/')
			{
				simple_tag = true;
				pos++;
			}
			
			skip_ws(str, pos);
			if (pos >= str.size() || str[pos] != '>')
				return false;
			pos++;
			
			if (!simple_tag && !read_text(str, pos, el_text))
				return false;
		}
		else
		{
			skip_ws(str, pos);
			if (pos >= str.size() || str[pos] != '>')
				return false;
			pos++;
		}
		
		if (el_name == "/OFX")
			break;
		
		if (!handle_element(el_name, el_attrs, try_xml_decode(el_text)))
			return false;
		if (simple_tag)
		{
			assert(el_name[0] != '/');
			if (!handle_element('/' + el_name, el_attrs, std::string()))
				return false;
		}
	}
	
	return true;
}

struct ofx_container;

struct process_ctx
{
	std::shared_ptr<rapidjson::Document> doc_;
	std::list<std::unique_ptr<ofx_container>> ostack_;
	
	template <typename Doc>
	process_ctx(Doc doc):
		doc_(doc)
	{
	}
	
	void push_container(ofx_container *container)
	{
		ostack_.push_front(std::unique_ptr<ofx_container>(container));
	}
	
	void pop_container()
	{
		ostack_.pop_front();
	}
};

struct ofx_cont
{
	enum serialize_as
	{
		nothing = 0,
		object,
		object_in_array,
		object_with_name_in_array,
		array
	};
	
	enum tag_fmt
	{
		string = 0,
		number,
		boolean,
		datetime
	};
	
	serialize_as serialize;
	std::map<std::string, const ofx_cont*> sub;
	std::map<std::string const, tag_fmt> tags;
};

struct ofx_container
{
	const std::string name_;
	const ofx_cont * const cont_;
	process_ctx& pctx_;
	std::shared_ptr<rapidjson::Value> val_;
	std::list<std::pair<std::string, std::string>> tags_;
	
	ofx_container(const std::string& name, const ofx_cont *cont, process_ctx& pctx):
		name_(name),
		cont_(cont),
		pctx_(pctx)
	{
		switch (cont_->serialize)
		{
			case ofx_cont::object:
			case ofx_cont::object_in_array:
			case ofx_cont::object_with_name_in_array:
				val_ = std::make_shared<rapidjson::Value>(rapidjson::kObjectType);
				break;
			case ofx_cont::array:
				val_ = std::make_shared<rapidjson::Value>(rapidjson::kArrayType);
				break;
			default:
				val_ = !pctx_.ostack_.empty() ? pctx_.ostack_.front()->val_ : pctx_.doc_;
				break;
		}
	}
	
	void done()
	{
		assert(!pctx_.ostack_.empty());
		auto it = pctx_.ostack_.begin();
		assert(it->get() == this);
		if (++it != pctx_.ostack_.end())
		{
			auto pcontainer = it->get();
			switch (cont_->serialize)
			{
				case ofx_cont::object:
				case ofx_cont::array:
					pcontainer->val_->AddMember(rapidjson::Value(str_lower(name_).c_str(), pctx_.doc_->GetAllocator()), *val_, pctx_.doc_->GetAllocator());
					break;
				case ofx_cont::object_in_array:
					pcontainer->val_->PushBack(*val_, pctx_.doc_->GetAllocator());
					break;
				case ofx_cont::object_with_name_in_array:
				{
					rapidjson::Value obj(rapidjson::kObjectType);
					obj.AddMember(rapidjson::Value(str_lower(name_).c_str(), pctx_.doc_->GetAllocator()), *val_, pctx_.doc_->GetAllocator());
					pcontainer->val_->PushBack(obj, pctx_.doc_->GetAllocator());
					break;
				}
				default:
					break;
			}
		}
	};
	
	void add_datetime(const std::string& element, const std::string& text)
	{
		struct tm tm;
		unsigned int msecs;
		int tzoff_min;
		if (parse_datetime(text, tm, msecs, tzoff_min))
		{
			char buf[26];
			size_t len = strftime(buf, sizeof buf, "%FT%T", &tm);
			if (tzoff_min == 0)
			{
				buf[len++] = 'Z';
				buf[len] = '\0';
			}
			else
			{
				int off_min = abs(tzoff_min) % 60;
				if (off_min != 0)
					sprintf(&buf[len], "%+03d:%02d", tzoff_min / 60, off_min);
				else
					sprintf(&buf[len], "%+03d", tzoff_min / 60);
			}
			add_string(element, buf);
		}
		else
			add_string(element, text);
	}
	
	void add_number(const std::string& element, const std::string& text)
	{
		double val;
		if (parse_number(text, val))
			val_->AddMember(rapidjson::Value(str_lower(element).c_str(), pctx_.doc_->GetAllocator()), rapidjson::Value(val), pctx_.doc_->GetAllocator());
		else
		{
			std::cerr << '<' << element << "> failed to parse '" << text << "' as a number" << std::endl;
			abort();
			add_string(element, text);
		}
	}
	
	void add_bool(const std::string& element, const std::string& text)
	{
		bool val;
		if (parse_bool(text, val))
			val_->AddMember(rapidjson::Value(str_lower(element).c_str(), pctx_.doc_->GetAllocator()), rapidjson::Value(val), pctx_.doc_->GetAllocator());
		else
		{
			std::cerr << '<' << element << "> failed to parse '" << text << "' as a boolean" << std::endl;
			abort();
			add_string(element, text);
		}
	}
	
	void add_string(const std::string& element, const std::string& text)
	{
		val_->AddMember(rapidjson::Value(str_lower(element).c_str(), pctx_.doc_->GetAllocator()), rapidjson::Value(text.c_str(), pctx_.doc_->GetAllocator()), pctx_.doc_->GetAllocator());
	}
	
	bool handle_tag(const std::string& element, const std::map<std::string, std::string>& /*attrs*/, const std::string& text)
	{
		auto its = cont_->sub.find(element);
		if (its != cont_->sub.end())
		{
			pctx_.push_container(new ofx_container(its->first, its->second, pctx_));
		}
		else
		{
			auto itt = cont_->tags.find(element);
			if (itt != cont_->tags.end())
			{
				switch (itt->second)
				{
					case ofx_cont::string:
						add_string(element, text);
						break;
					case ofx_cont::number:
						add_number(element, text);
						break;
					case ofx_cont::boolean:
						add_bool(element, text);
						break;
					case ofx_cont::datetime:
						add_datetime(element, text);
						break;
				}
			}
			else
				logErr('<' << name_ << "> unhandled element: '" << element << "' text: '" << text << "'");
			tags_.push_back(std::make_pair(element, text));
		}
		return true;
	}
	
	bool handle_close(const std::string& close_tag, bool& container_done)
	{
		bool found = false;
		while (!tags_.empty() && !found)
		{
			auto const& t = tags_.back();
			if (t.first == close_tag)
				found = true;
			tags_.pop_back();
		}
		if (tags_.empty() && close_tag == name_)
		{
			container_done = true;
			return true;
		}
		
		return found;
	}
};

static const ofx_cont ofx_status = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "CODE", ofx_cont::string },
		{ "SEVERITY", ofx_cont::string },
		{ "MESSAGE", ofx_cont::string },
	}
};

static const ofx_cont ofx_signon_sonrs_fi = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "ORG", ofx_cont::string },
		{ "FID", ofx_cont::string },
	}
};

static const ofx_cont ofx_signon_sonrs = {
	serialize: ofx_cont::object,
	sub: {
		{ "STATUS", &ofx_status },
		{ "FI", &ofx_signon_sonrs_fi },
	},
	tags: {
		{ "DTSERVER", ofx_cont::datetime },
		{ "DTPROFUP", ofx_cont::datetime },
		{ "LANGUAGE", ofx_cont::string },
		{ "SESSCOOKIE", ofx_cont::string },
	}
};

static const ofx_cont ofx_signonmsgsrsv1 = {
	serialize: ofx_cont::object,
	sub: {
		{ "SONRS", &ofx_signon_sonrs },
	},
	tags: {}
};

static const ofx_cont ofx_signupmsgsrsv1 = {
	serialize: ofx_cont::object,
	sub: {}, // TODO
	tags: {} // TODO
};

static const ofx_cont ofx_investment_entry_status = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "CODE", ofx_cont::string },
		{ "SEVERITY", ofx_cont::string },
	}
};

static const ofx_cont ofx_invacctfrom = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "BROKERID", ofx_cont::string },
		{ "ACCTID", ofx_cont::string },
	}
};

static const ofx_cont ofx_currency = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "CURRATE", ofx_cont::string },
		{ "CURSYM", ofx_cont::string },
	}
};

static const ofx_cont ofx_escrwamt = {
	serialize: ofx_cont::object,
	sub: {
	},
	tags: {
		{ "ESCRWTOTAL", ofx_cont::number },
		{ "ESCRWTAX", ofx_cont::number },
		{ "ESCRWINS", ofx_cont::number },
		{ "ESCRWPMI", ofx_cont::number },
		{ "ESCRWFEES", ofx_cont::number },
		{ "ESCRWOTHER", ofx_cont::number },
	}
};

static const ofx_cont ofx_payee = {
	serialize: ofx_cont::object,
	sub: {
	},
	tags: {
		{ "NAME", ofx_cont::string },
		{ "ADDR1", ofx_cont::string },
		{ "ADDR2", ofx_cont::string },
		{ "ADDR3", ofx_cont::string },
		{ "CITY", ofx_cont::string },
		{ "STATE", ofx_cont::string },
		{ "POSTALCODE", ofx_cont::string },
		{ "COUNTRY", ofx_cont::string },
		{ "PHONE", ofx_cont::string },
	}
};

static const ofx_cont ofx_bankacctto = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "BANKID", ofx_cont::string },
		{ "BRANCHID", ofx_cont::string },
		{ "ACCTID", ofx_cont::string },
		{ "ACCTTYPE", ofx_cont::string },
		{ "ACCTKEY", ofx_cont::string },
	}
};


static const ofx_cont ofx_ccacct_fromorto = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "ACCTID", ofx_cont::string },
		{ "ACCTKEY", ofx_cont::string },
	}
};

static const ofx_cont ofx_loanpmtinfo = {
	serialize: ofx_cont::object,
	sub: {
		{ "ESCRWAMT", &ofx_escrwamt },
	},
	tags: {
		{ "PRINAMT", ofx_cont::number },
		{ "INTAMT", ofx_cont::number },
		{ "INSURANCE", ofx_cont::number },
		{ "LATEFEEAMT", ofx_cont::number },
		{ "OTHERAMT", ofx_cont::number },
	}
};

static const ofx_cont ofx_imagedata = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "IMAGETYPE", ofx_cont::string },
		{ "IMAGEREF", ofx_cont::string },
		{ "IMAGEREFTYPE", ofx_cont::string },
		{ "IMAGEDELAY", ofx_cont::string },
		{ "DTIMAGEAVAIL", ofx_cont::string },
		{ "IMAGETTL", ofx_cont::string },
		{ "CHECKSUP", ofx_cont::string },
	}
};

static const ofx_cont ofx_stmttrn = {
	serialize: ofx_cont::object,
	sub: {
		{ "LOANPMTINFO", &ofx_loanpmtinfo },
		{ "PAYEE", &ofx_payee },
		{ "BANKACCTTO", &ofx_bankacctto },
		{ "CCACCTTO", &ofx_ccacct_fromorto },
		{ "IMAGEDATA", &ofx_imagedata },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "TRNTYPE", ofx_cont::string },
		{ "DTPOSTED", ofx_cont::datetime },
		{ "DTUSER", ofx_cont::datetime },
		{ "DTAVAIL", ofx_cont::datetime },
		{ "TRNAMT", ofx_cont::number },
		{ "FITID", ofx_cont::string },
		{ "CORRECTFITID", ofx_cont::string },
		{ "CORRECTACTION", ofx_cont::string },
		{ "SRVRTID", ofx_cont::string },
		{ "CHECKNUM", ofx_cont::string },
		{ "REFNUM", ofx_cont::string },
		{ "SIC", ofx_cont::string },
		{ "PAYEEID", ofx_cont::string },
		{ "NAME", ofx_cont::string },
		{ "EXTDNAME", ofx_cont::string },
		{ "MEMO", ofx_cont::string },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_secid = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "UNIQUEID", ofx_cont::string },
		{ "UNIQUEIDTYPE", ofx_cont::string },
	}
};

static const ofx_cont ofx_invtran = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "FITID", ofx_cont::string },
		{ "SRVRTID", ofx_cont::string },
		{ "DTTRADE", ofx_cont::datetime },
		{ "DTSETTLE", ofx_cont::datetime },
		{ "REVERSALFITID", ofx_cont::string },
		{ "MEMO", ofx_cont::string },
	}
};

static const ofx_cont ofx_invbuy = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
	},
	tags: {
		{ "UNITS", ofx_cont::number },
		{ "UNITPRICE", ofx_cont::number },
		{ "TOTAL", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "SUBACCTFUND", ofx_cont::string },
	}
};

static const ofx_cont ofx_invsell = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "UNITS", ofx_cont::number },
		{ "UNITPRICE", ofx_cont::number },
		{ "MARKDOWN", ofx_cont::number },
		{ "COMMISSION", ofx_cont::number },
		{ "TAXES", ofx_cont::number },
		{ "FEES", ofx_cont::number },
		{ "LOAD", ofx_cont::number },
		{ "WITHHOLDING", ofx_cont::number },
		{ "TAXEXEMPT", ofx_cont::boolean },
		{ "TOTAL", ofx_cont::number },
		{ "GAIN", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "SUBACCTFUND", ofx_cont::string },
		{ "LOANID", ofx_cont::string },
		{ "STATEWITHHOLDING", ofx_cont::number },
		{ "PENALTY", ofx_cont::number },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_invstmttrnrs_invstmtrs_invtranlist_invbanktran = {
	serialize: ofx_cont::object,
	sub: {
		{ "STMTTRN", &ofx_stmttrn },
	},
	tags: {
		{ "SUBACCTFUND", ofx_cont::string },
	}
};

static const ofx_cont ofx_selldebt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVSELL", &ofx_invsell },
	},
	tags: {
		{ "SELLREASON", ofx_cont::string },
		{ "ACCRDINT", ofx_cont::number },
	}
};

static const ofx_cont ofx_sellmf = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVSELL", &ofx_invsell },
	},
	tags: {
		{ "SELLTYPE", ofx_cont::string },
		{ "AVGCOSTBASIS", ofx_cont::number },
		{ "RELFITID", ofx_cont::string },
	}
};

static const ofx_cont ofx_sellopt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVSELL", &ofx_invsell },
	},
	tags: {
		{ "OPTSELLTYPE", ofx_cont::string },
		{ "SHPERCTRCT", ofx_cont::number },
		{ "RELFITID", ofx_cont::string },
		{ "RELTYPE", ofx_cont::string },
		{ "SECURED", ofx_cont::string },
	}
};

static const ofx_cont ofx_sellother = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVSELL", &ofx_invsell },
	},
	tags: {}
};

static const ofx_cont ofx_sellstock = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVSELL", &ofx_invsell },
	},
	tags: {
		{ "SELLTYPE", ofx_cont::string },
	}
};

static const ofx_cont ofx_buydebt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBUY", &ofx_invbuy },
	},
	tags: {
		{ "ACCRDINT", ofx_cont::string },
	}
};

static const ofx_cont ofx_buymf = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBUY", &ofx_invbuy },
	},
	tags: {
		{ "BUYTYPE", ofx_cont::string },
		{ "RELFITID", ofx_cont::string },
	}
};

static const ofx_cont ofx_buyopt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBUY", &ofx_invbuy },
	},
	tags: {
		{ "OPTBUYTYPE", ofx_cont::string },
		{ "SHPERCTRCT", ofx_cont::number },
	}
};

static const ofx_cont ofx_buyother = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBUY", &ofx_invbuy },
	},
	tags: {}
};

static const ofx_cont ofx_buystock = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBUY", &ofx_invbuy },
	},
	tags: {
		{ "BUYTYPE", ofx_cont::string },
	}
};

static const ofx_cont ofx_closureopt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
	},
	tags: {
		{ "OPTACTION", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
		{ "SHPERCTRCT", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "RELFITID", ofx_cont::string },
		{ "GAIN", ofx_cont::number },
	}
};

static const ofx_cont ofx_income = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "INCOMETYPE", ofx_cont::string },
		{ "TOTAL", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "SUBACCTFUND", ofx_cont::string },
		{ "TAXEXEMPT", ofx_cont::boolean },
		{ "WITHHOLDING", ofx_cont::number },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_invexpense = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "TOTAL", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "SUBACCTFUND", ofx_cont::string },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_jrnlfund = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
	},
	tags: {
		{ "SUBACCTTO", ofx_cont::string },
		{ "SUBACCTFROM", ofx_cont::string },
		{ "TOTAL", ofx_cont::number },
	}
};

static const ofx_cont ofx_jrnlsec = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
	},
	tags: {
		{ "SUBACCTTO", ofx_cont::string },
		{ "SUBACCTFROM", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
	}
};

static const ofx_cont ofx_margininterest = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "TOTAL", ofx_cont::number },
		{ "SUBACCTFUND", ofx_cont::string },
	}
};

static const ofx_cont ofx_reinvest = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "INCOMETYPE", ofx_cont::string },
		{ "TOTAL", ofx_cont::number },
		{ "SUBACCTSEC", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
		{ "UNITPRICE", ofx_cont::number },
		{ "COMMISSION", ofx_cont::number },
		{ "TAXES", ofx_cont::number },
		{ "FEES", ofx_cont::number },
		{ "LOAD", ofx_cont::number },
		{ "TAXEXEMPT", ofx_cont::boolean },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_retofcap = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "SUBACCTSEC", ofx_cont::string },
		{ "SUBACCTFUND", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_split = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
		{ "ORIGCURRENCY", &ofx_currency },
	},
	tags: {
		{ "SUBACCTSEC", ofx_cont::string },
		{ "OLDUNITS", ofx_cont::number },
		{ "NEWUNITS", ofx_cont::number },
		{ "NUMERATOR", ofx_cont::number },
		{ "DENOMINATOR", ofx_cont::number },
		{ "FRACCASH", ofx_cont::number },
		{ "SUBACCTFUND", ofx_cont::string },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_transfer = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVTRAN", &ofx_invtran },
		{ "SECID", &ofx_secid },
		{ "INVACCTFROM", &ofx_invacctfrom },
		
	},
	tags: {
		{ "SUBACCTSEC", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
		{ "TFERACTION", ofx_cont::string },
		{ "POSTYPE", ofx_cont::string },
		{ "AVGCOSTBASIS", ofx_cont::number },
		{ "UNITPRICE", ofx_cont::number },
		{ "DTPURCHASE", ofx_cont::datetime },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_invstmttrnrs_invstmtrs_invtranlist = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVBANKTRAN", &ofx_invstmttrnrs_invstmtrs_invtranlist_invbanktran },
		{ "BUYDEBT", &ofx_buydebt },
		{ "BUYMF", &ofx_buymf },
		{ "BUYOPT", &ofx_buyopt },
		{ "BUYOTHER", &ofx_buyother },
		{ "BUYSTOCK", &ofx_buystock },
		{ "CLOSUREOPT", &ofx_closureopt },
		{ "INCOME", &ofx_income },
		{ "INVEXPENSE", &ofx_invexpense },
		{ "JRNLFUND", &ofx_jrnlfund },
		{ "JRNLSEC", &ofx_jrnlsec },
		{ "MARGININTEREST", &ofx_margininterest },
		{ "REINVEST", &ofx_reinvest },
		{ "RETOFCAP", &ofx_retofcap },
		{ "SELLDEBT", &ofx_selldebt },
		{ "SELLMF", &ofx_sellmf },
		{ "SELLOPT", &ofx_sellopt },
		{ "SELLOTHER", &ofx_sellother },
		{ "SELLSTOCK", &ofx_sellstock },
		{ "SPLIT", &ofx_split },
		{ "TRANSFER", &ofx_transfer },
	},
	tags: {
		{ "DTSTART", ofx_cont::datetime },
		{ "DTEND", ofx_cont::datetime },
	}
};

static const ofx_cont ofx_invpos = {
	serialize: ofx_cont::object,
	sub: {
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
	},
	tags: {
		{ "HELDINACCT", ofx_cont::string },
		{ "POSTYPE", ofx_cont::string },
		{ "UNITS", ofx_cont::number },
		{ "UNITPRICE", ofx_cont::number },
		{ "MKTVAL", ofx_cont::number },
		{ "AVGCOSTBASIS", ofx_cont::number },
		{ "DTPRICEASOF", ofx_cont::datetime },
		{ "MEMO", ofx_cont::string },
		{ "INV401KSOURCE", ofx_cont::string },
	}
};

static const ofx_cont ofx_posdebt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVPOS", &ofx_invpos },
	},
	tags: {}
};

static const ofx_cont ofx_posmf = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVPOS", &ofx_invpos },
	},
	tags: {
		{ "UNITSSTREET", ofx_cont::number },
		{ "UNITSUSER", ofx_cont::number },
		{ "REINVDIV", ofx_cont::boolean },
		{ "REINVCG", ofx_cont::boolean },
	}
};

static const ofx_cont ofx_posopt = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVPOS", &ofx_invpos },
	},
	tags: {
		{ "SECURED", ofx_cont::string },
	}
};

static const ofx_cont ofx_posother = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVPOS", &ofx_invpos },
	},
	tags: {}
};

static const ofx_cont ofx_posstock = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVPOS", &ofx_invpos },
	},
	tags: {
		{ "UNITSSTREET", ofx_cont::number },
		{ "UNITSUSER", ofx_cont::number },
		{ "REINVDIV", ofx_cont::boolean },
	}
};

static const ofx_cont ofx_invstmttrnrs_invstmtrs_invposlist = {
	serialize: ofx_cont::object,
	sub: {
		{ "POSMF", &ofx_posmf },
		{ "POSSTOCK", &ofx_posstock },
		{ "POSDEBT", &ofx_posdebt },
		{ "POSOPT", &ofx_posopt },
		{ "POSOTHER", &ofx_posother },
	},
	tags: {}
};

static const ofx_cont ofx_invstmttrnrs_invstmtrs_invbal = {
	serialize: ofx_cont::object,
	sub: {
		// todo
	},
	tags: {
		{ "AVAILCASH", ofx_cont::number },
		{ "MARGINBALANCE", ofx_cont::number },
		{ "SHORTBALANCE", ofx_cont::number },
		// todo
	}
};

static const ofx_cont ofx_invstmttrnrs_invstmtrs = {
	serialize: ofx_cont::object,
	sub: {
		{ "INVACCTFROM", &ofx_invacctfrom },
		{ "INVTRANLIST", &ofx_invstmttrnrs_invstmtrs_invtranlist },
		{ "INVPOSLIST", &ofx_invstmttrnrs_invstmtrs_invposlist },
		{ "INVBAL", &ofx_invstmttrnrs_invstmtrs_invbal},
	//	{ "INVOOLIST", },
	//	{ "INV401K", },
	//	{ "INV401KBAL", }
	},
	tags: {
		{ "DTASOF", ofx_cont::datetime },
		{ "CURDEF", ofx_cont::string },
		{ "MKTGINFO", ofx_cont::string },
	}
};

static const ofx_cont ofx_invstmtmsgsrsv1_invstmttrnrs = {
	serialize: ofx_cont::object_with_name_in_array,
	sub: {
		{ "STATUS", &ofx_status },
		{ "INVSTMTRS", &ofx_invstmttrnrs_invstmtrs },
	},
	tags: {
		{ "TRNUID", ofx_cont::string },
		{ "CLTCOOKIE", ofx_cont::string },
	}
};

static const ofx_cont ofx_invstmtmsgsrsv1 = {
	serialize: ofx_cont::array,
	sub: {
		{ "INVSTMTTRNRS", &ofx_invstmtmsgsrsv1_invstmttrnrs },
	//	{ "INVMAILTRNRS", },
	//	{ "INVMAILSYNCRS", },
	//	{ "INVSTMTENDTRNRS", },
	},
	tags: {} // TODO
};

static const ofx_cont ofx_secinfo = {
	serialize: ofx_cont::object,
	sub: {
		{ "SECID", &ofx_secid },
		{ "CURRENCY", &ofx_currency },
	},
	tags: {
		{ "SECNAME", ofx_cont::string },
		{ "TICKER", ofx_cont::string },
		{ "FIID", ofx_cont::string },
		{ "RATING", ofx_cont::string },
		{ "UNITPRICE", ofx_cont::number },
		{ "DTASOF", ofx_cont::datetime },
		{ "MEMO", ofx_cont::string },
	}
};

static const ofx_cont ofx_seclistmsgsrsv1_seclist_debtinfo = {
	serialize: ofx_cont::object,
	sub: {
		{ "SECINFO", &ofx_secinfo },
	},
	tags: {
		{ "PARVALUE", ofx_cont::number },
		{ "DEBTTYPE", ofx_cont::string },
		{ "DEBTCLASS", ofx_cont::string },
		{ "COUPONRT", ofx_cont::number },
		{ "DTCOUPON", ofx_cont::datetime },
		{ "COUPONFREQ", ofx_cont::datetime },
		{ "CALLPRICE", ofx_cont::number },
		{ "YIELDTOCALL", ofx_cont::number },
		{ "DTCALL", ofx_cont::datetime },
		{ "CALLTYPE", ofx_cont::string },
		{ "YIELDTOMAT", ofx_cont::string },
		{ "DTMAT", ofx_cont::datetime },
		{ "ASSETCLASS", ofx_cont::string },
		{ "FIASSETCLASS", ofx_cont::string },
	}
};

static const ofx_cont ofx_mfassetclass_portion = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "ASSETCLASS", ofx_cont::string },
		{ "PERCENT", ofx_cont::number },
	}
};

static const ofx_cont ofx_mfassetclass = {
	serialize: ofx_cont::object,
	sub: {
		{ "PORTION", &ofx_mfassetclass_portion },
	},
	tags: {}
};

static const ofx_cont ofx_fimfassetclass_portion = {
	serialize: ofx_cont::object,
	sub: {},
	tags: {
		{ "FIASSETCLASS", ofx_cont::string },
		{ "PERCENT", ofx_cont::number },
	}
};

static const ofx_cont ofx_fimfassetclass = {
	serialize: ofx_cont::object,
	sub: {
		{ "FIPORTION", &ofx_fimfassetclass_portion },
	},
	tags: {}
};

static const ofx_cont ofx_seclistmsgsrsv1_seclist_mfinfo = {
	serialize: ofx_cont::object,
	sub: {
		{ "SECINFO", &ofx_secinfo },
		{ "MFASSETCLASS", &ofx_mfassetclass },
		{ "FIMFASSETCLASS", &ofx_fimfassetclass },
	},
	tags: {
		{ "MFTYPE", ofx_cont::string },
		{ "YIELD", ofx_cont::number },
		{ "DTYIELDASOF", ofx_cont::datetime },
	}
};

static const ofx_cont ofx_seclistmsgsrsv1_seclist = {
	serialize: ofx_cont::object_with_name_in_array,
	sub: {
		{ "DEBTINFO", &ofx_seclistmsgsrsv1_seclist_debtinfo },
		{ "MFINFO", &ofx_seclistmsgsrsv1_seclist_mfinfo },
	//	{ "OPTINFO", &ofx_seclistmsgsrsv1_seclist_optinfo },
	//	{ "OTHERINFO", &ofx_seclistmsgsrsv1_seclist_otherinfo },
	//	{ "STOCKINFO", &ofx_seclistmsgsrsv1_seclist_stockinfo },
	},
	tags: {}
};

static const ofx_cont ofx_seclistmsgsrsv1 = {
	serialize: ofx_cont::array,
	sub: {
		{ "SECLIST", &ofx_seclistmsgsrsv1_seclist },
	},
	tags: {}
};

static const ofx_cont ofx_main = {
	serialize: ofx_cont::nothing,
	sub: {
		{ "SIGNONMSGSRSV1", &ofx_signonmsgsrsv1 },
		{ "SIGNUPMSGSRSV1", &ofx_signupmsgsrsv1 },
		{ "INVSTMTMSGSRSV1", &ofx_invstmtmsgsrsv1 },
		{ "SECLISTMSGSRSV1", &ofx_seclistmsgsrsv1 },
	},
	tags: {}
};

static bool process_ofx(const std::shared_ptr<rapidjson::Document>& doc, const std::string& in, size_t& pos)
{
	process_ctx pctx(doc);
	
	pctx.push_container(new ofx_container("OFX", &ofx_main, pctx));
	
	if (!iterate_elements(in, pos,
		[&](const std::string& element, const std::map<std::string, std::string>& attrs, const std::string& text) -> bool
		{
			if (element[0] != '/')
			{
				assert(!pctx.ostack_.empty());
				auto& os_top = *pctx.ostack_.front();
				return os_top.handle_tag(element, attrs, text);
			}
			else
			{
				if (!pctx.ostack_.empty())
				{
					auto& os_top = *pctx.ostack_.front();
					bool container_done = false;
					if (!os_top.handle_close(element.substr(1), container_done))
					{
						logErr("mismatch for " << element << ", expecting /" << os_top.name_);
						return false;
					}
					
					if (container_done)
					{
						os_top.done();
						pctx.ostack_.pop_front();
					}
				}
				else
				{
					logErr("unexpected tag found: <" << element << '>');
					return false;
				}
			}
			
			
			return true;
		}))
	{
		logErr("Processing failed.");
		return true;
	}
	
	if (pctx.ostack_.size() == 1)
	{
		auto& os_top = *pctx.ostack_.front();
		bool container_done = false;
		os_top.handle_close(os_top.name_, container_done);
		pctx.ostack_.pop_front();
		
		if (!container_done)
			return false;
	}
	
	if (!pctx.ostack_.empty())
	{
		logErr("Stack not empty!");
		return false;
	}
	
	logDbg("Processing succeeded.");
	return true;
}

int main(int argc, char *argv[])
{
	int ret = 0;
	static const argp_option opts[] =
	{
		{ nullptr, 0, nullptr, 0, "The following options are available:", -1 },
		{ "output", 'o', "OUTPUT", 0, "Write json output to file OUTPUT", -1 },
		{ "quiet", 'q', nullptr, 0, "Do not output errors", -1 },
		{ nullptr, 0, nullptr, 0, nullptr, 0 }
	};
	static const argp popts =
	{
		opts,
		[](int key, char* arg, struct argp_state* state) -> error_t
		{
			switch (key)
			{
				case ARGP_KEY_ARG:
				{
					if (state->arg_num == 0)
					{
						if (strcmp(arg, "-"))
							g_input = strdup(arg);
					}
					else
						argp_usage(state); /* too many arguments */
					break;
				}
				case 'o':
					if (arg[0])
						g_output = strdup(arg);
					break;
				case 'q':
					g_quiet = true;
					break;
				case ARGP_KEY_END:
					break;
				case ARGP_KEY_NO_ARGS:
					argp_usage(state);
					break;
				default:
					return ARGP_ERR_UNKNOWN;
			}
			return 0;
		},
		"[OFXFILE]",
		nullptr, nullptr, nullptr, nullptr
	};
	argp_parse(&popts, argc, argv, ARGP_IN_ORDER, nullptr, nullptr);
	try
	{
		std::string in;
		auto eit = std::istreambuf_iterator<char>();
		if (g_input)
		{
			std::ifstream fi(g_input);
			fi.exceptions(std::ifstream::failbit);
			in.assign(std::istreambuf_iterator<char>(fi), eit);
		}
		else
			in.assign(std::istreambuf_iterator<char>(std::cin), eit);
		
		size_t pos = in.find("<OFX>");
		if (pos == std::string::npos)
			throw std::runtime_error("Not an OFX file");
		pos += 5;
		
		auto doc = std::make_shared<rapidjson::Document>(rapidjson::kObjectType);
		if (process_ofx(doc, in, pos))
		{
			rapidjson::StringBuffer sbuf;
			rapidjson::Writer<rapidjson::StringBuffer> writer(sbuf);
			doc->Accept(writer);
			
			std::ofstream fo;
			if (g_output)
			{
				fo.exceptions(std::ifstream::failbit);
				fo.open(g_output);
			}
			std::ostream& out = g_output ? fo : std::cout;
			out << sbuf.GetString() << std::endl;
		}
	}
	catch (std::ifstream::failure& e)
	{
		logErr("File operation failed");
		ret = 1;
	}
	catch (const std::runtime_error& e)
	{
		logErr(e.what());
		ret = 1;
	}
	free(g_input);
	free(g_output);
	return ret;
}
