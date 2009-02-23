#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(guile-avahi, 0.3, guile-avahi-bugs@nongnu.org)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu check-news -Wall -Wno-portability])


AC_CONFIG_SRCDIR([src/common.c])
AC_CONFIG_HEADER([src/config.h])


AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

# Checks for programs.
AC_PROG_CC
AC_LANG_C
gl_EARLY
AC_PROG_LIBTOOL

# Gnulib
gl_INIT

if test "x$GCC" = "xyes"; then
  # Enable useful GCC compilation flags.
  GCC_CFLAGS="-Wall -Werror"
  GCC_CFLAGS="-Wcast-align -Wpointer-arith $GCC_CFLAGS"
  GCC_CFLAGS="-Wdeclaration-after-statement $GCC_CFLAGS"
else
  GCC_CFLAGS=""
fi
AC_SUBST([GCC_CFLAGS])


GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
GUILE_SITE_DIR

AC_CACHE_SAVE


# Avahi.
AC_MSG_CHECKING([whether Avahi is available])
PKG_CHECK_MODULES([AVAHI], [avahi-client], [],
   [AC_MSG_ERROR([Avahi could not be found, please install it first.])])
AVAHI_CPPFLAGS="`$PKG_CONFIG avahi-client --cflags-only-I`"
AC_SUBST([AVAHI_CPPFLAGS])

# Checks for header files.
AC_CHECK_HEADERS([net/if.h gmp.h])
AC_CHECK_HEADER([avahi-common/thread-watch.h], [:],
   [AC_MSG_ERROR([Avahi >= 0.6.4 (with `AvahiThreadedPoll') is needed.])])

# `AVAHI_ERR_NO_CHANGE' was added in Avahi 0.6.13 (svn rev. 1266).
old_CFLAGS="$CFLAGS"
CFLAGS="$AVAHI_CFLAGS $CFLAGS"
GA_CHECK_ENUM_VALUE([#include <avahi-common/error.h>], [AVAHI_ERR_NO_CHANGE],
  [have_err_no_change=yes], [have_err_no_change=no])
AM_CONDITIONAL([HAVE_AVAHI_ERR_NO_CHANGE], test "x$have_err_no_change" = "xyes")
CFLAGS="$old_CFLAGS"

# `avahi_client_set_host_name ()' was added in Avahi 0.6.13.
old_LIBS="$LIBS"
LIBS="$AVAHI_LIBS $LIBS"
AC_CHECK_FUNCS([avahi_client_set_host_name])
LIBS="$old_LIBS"

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([if_nametoindex if_indextoname])
AC_CHECK_LIB([gmp], [__gmpz_init], [LIBS="$LIBS -lgmp"],
  [AC_MSG_WARN([GNU MP not found, see README])])


pkgdatadir="$datadir/$PACKAGE_NAME"
if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$pkgdatadir"
fi
AC_SUBST([guilemoduledir])

if test "$guilemoduledir" != "$GUILE_SITE"; then
   # Guile won't be able to locate the module "out of the box", so
   # warn the user.
   AC_MSG_WARN([`guilemoduledir' ($guilemoduledir) is different from `GUILE_SITE' ($GUILE_SITE).])
   AC_MSG_WARN([Make sure to adjust the `GUILE_LOAD_PATH' environment variable accordingly,])
   AC_MSG_WARN([or re-run `configure' with `--with-guilemoduledir=$GUILE_SITE'.])
fi

AC_CONFIG_FILES([Makefile
	         modules/Makefile
		 doc/Makefile
		 lib/Makefile
	         src/Makefile
		 tests/Makefile
		 pre-inst-guile])
AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x pre-inst-guile])

AC_OUTPUT

