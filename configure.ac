#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(guile-avahi, 0.0, ludovic.courtes@laas.fr)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE(guile-avahi, 0.1)


AC_CONFIG_SRCDIR([src/common.c])
AC_CONFIG_HEADER([src/config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
GUILE_SITE_DIR

# Avahi.
AC_MSG_CHECKING([whether Avahi is available])
PKG_CHECK_MODULES([AVAHI], [avahi-client], [],
   [AC_MSG_ERROR([Avahi could not be found, please install it first.])])

# Checks for header files.
AC_CHECK_HEADERS([net/if.h arpa/inet.h gmp.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([if_nametoindex if_indextoname])
AC_CHECK_LIB([gmp], [__gmpz_init], [LIBS="$LIBS -lgmp"],
  [AC_MSG_WARN([GNU MP not found, see README])])


AC_CONFIG_FILES([Makefile
	         modules/Makefile
		 doc/Makefile
	         src/Makefile
		 tests/Makefile
		 pre-inst-guile])
AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x pre-inst-guile])

AC_OUTPUT

