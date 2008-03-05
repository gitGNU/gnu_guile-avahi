# Guile-Avahi --- Guile bindings for Avahi.
# Copyright (C) 2008  Ludovic Courtès <ludo@gnu.org>
#
# This file is part of Guile-Avahi.
#
# Guile-Avahi is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Guile-Avahi is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Autoconf snippets


# GA_CHECK_ENUM_VALUE(INCLUDES VALUE ACTION-IF-DEFINED ACTION-OTHERWISE)
#
# Check whether VALUE is already defined in some `enum'.  If it's
# already defined, ACTION-IF-DEFINED is executed, otherwise
# ACTION-OTHERWISE.
AC_DEFUN([GA_CHECK_ENUM_VALUE],
  [AC_MSG_CHECKING([whether the `$2' enum is defined])
   AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM(
       [[$1
         /* Try to redefine the enum value.  It will fail
	    if it's already defined.  */
	 enum ga_check_enum_test { $2 };]],
       [[return $2;]])],
     [AC_MSG_RESULT([no])
      $4],
     [AC_MSG_RESULT([yes])
      $3])])

# Local Variables:
# mode: autoconf
# coding: latin-1
# End:
