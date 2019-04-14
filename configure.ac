AC_INIT([ofx2json], [0.1], [thomas@reactsoft.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CXX
AC_LANG_PUSH([C++])
m4_ifdef([AX_CXX_COMPILE_STDCXX_11], [
    AX_CXX_COMPILE_STDCXX_11([noext],[mandatory])
], [
    AC_MSG_ERROR([You need to install the "autoconf-archive" package.])
])
AX_CHECK_COMPILE_FLAG([-Wall], [AX_APPEND_FLAG([-Wall])], [], [])
AX_CHECK_COMPILE_FLAG([-Wextra], [AX_APPEND_FLAG([-Wextra])], [], [])
PKG_CHECK_MODULES([RapidJSON], RapidJSON, [], [AC_MSG_ERROR([rapidjson was not found])])
AC_CHECK_HEADERS([argp.h],,[AC_MSG_ERROR([argp.h header was not found])])
AC_CONFIG_HEADERS([config.h])
AC_LANG_POP([C++])
AC_CONFIG_FILES([
    Makefile
    src/Makefile
])
AC_OUTPUT
