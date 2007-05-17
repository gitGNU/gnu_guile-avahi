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

noinst_HEADERS = watch.h

EXTRA_DIST =					\
  make-enum-map.scm make-smob-types.scm		\
  make-enum-header.scm make-smob-header.scm

BUILT_SOURCES = common-smobs.h common-enums.h		\
		client-smobs.h client-enums.h		\
		common-enums.i.c common-smobs.i.c	\
		client-enums.i.c client-smobs.i.c	\
		common.c.x watch.c.x client.c.x
CLEANFILES = $(BUILT_SOURCES)

lib_LTLIBRARIES = libguile-avahi-v-0.la

libguile_avahi_v_0_la_SOURCES = watch.c common.c client.c

AM_CPPFLAGS = -I$(builddir)


%-enums.h: $(srcdir)/make-enum-header.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-smobs.h: $(srcdir)/make-smob-header.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-enums.i.c: $(srcdir)/make-enum-map.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-smobs.i.c: $(srcdir)/make-smob-types.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@


# C file snarfing.

snarfcppopts = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
	       $(CFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
%.c.x: %.c
	$(guile_snarf) -o $@ $< $(snarfcppopts)
