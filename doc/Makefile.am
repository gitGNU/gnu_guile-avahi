# Guile-Avahi --- Guile bindings for Avahi.
# Copyright (C) 2007  Ludovic Courtès <ludo@gnu.org>
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

GUILE_FOR_BUILD = $(GUILE) -L $(top_srcdir)/modules

SNARF_CPPFLAGS = -I$(top_srcdir)/src -I$(top_builddir)/src


BUILT_SOURCES = common.c.texi watch.c.texi client.c.texi	\
		publish.c.texi lookup.c.texi
CLEANFILES    = $(BUILT_SOURCES)
info_TEXINFOS = guile-avahi.texi
EXTRA_DIST    = extract-c-doc.scm

%.c.texi: $(top_srcdir)/src/%.c
	$(GUILE_FOR_BUILD) -l "$(srcdir)/extract-c-doc.scm"	\
	   -e '(apply main (cdr (command-line)))'		\
	   -- "$^" "$(CPP)" "$(SNARF_CPPFLAGS)"			\
	   > "$@"
