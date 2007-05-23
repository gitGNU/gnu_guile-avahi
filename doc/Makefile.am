#  Guile-Avahi -- Guile bindings for Avahi.
#  Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
#
#  Guile-Avahi is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  Guile-Avahi is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with Guile-Avahi; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

GUILE_FOR_BUILD = $(GUILE) -L $(top_srcdir)/modules

SNARF_CPPFLAGS = -I$(top_srcdir)/src -I$(top_builddir)/src


BUILT_SOURCES = common.c.texi watch.c.texi client.c.texi	\
		publish.c.texi
CLEANFILES    = $(BUILT_SOURCES)
info_TEXINFOS = guile-avahi.texi
EXTRA_DIST    = extract-c-doc.scm

%.c.texi: $(top_srcdir)/src/%.c
	$(GUILE_FOR_BUILD) -l "$(srcdir)/extract-c-doc.scm"	\
	   -e '(apply main (cdr (command-line)))'		\
	   -- "$^" "$(CPP)" "$(SNARF_CPPFLAGS)"			\
	   > "$@"
