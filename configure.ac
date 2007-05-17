#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(guile-avahi, 0.0, ludovic.courtes@laas.fr)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE(guile-avahi, 0.0)


AC_CONFIG_SRCDIR([src/common.c])
AC_CONFIG_HEADER([src/config.h])

# Checks for programs.
AC_PROG_CC
gl_EARLY
AC_PROG_LIBTOOL

GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
GUILE_FLAGS
GUILE_SITE_DIR

# Avahi.
AC_MSG_CHECKING([whether Avahi is available])
PKG_CHECK_MODULES([AVAHI], [avahi-client], [],
   [AC_MSG_ERROR([Avahi could not be found, please install it first.])])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
	         modules/Makefile
	         src/Makefile
		 pre-inst-guile])
AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x pre-inst-guile])

AC_OUTPUT

