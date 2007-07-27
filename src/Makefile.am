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

noinst_HEADERS = watch.h utils.h errors.h

EXTRA_DIST =							\
  make-enum-map.scm make-smob-types.scm				\
  make-enum-header.scm make-smob-header.scm			\
  make-callback-header.scm make-callback-trampolines.scm

BUILT_SOURCES = common-smobs.h common-enums.h		\
		client-smobs.h client-enums.h		\
		client-callbacks.h			\
		publish-smobs.h publish-enums.h		\
		publish-callbacks.h			\
		lookup-smobs.h lookup-enums.h		\
		lookup-callbacks.h			\
		common-enums.i.c common-smobs.i.c	\
		client-enums.i.c client-smobs.i.c	\
		client-callbacks.i.c			\
		publish-enums.i.c publish-smobs.i.c	\
		publish-callbacks.i.c			\
		lookup-enums.i.c lookup-smobs.i.c	\
		lookup-callbacks.i.c			\
		common.c.x watch.c.x client.c.x		\
		errors.c.x publish.c.x lookup.c.x
CLEANFILES = $(BUILT_SOURCES)

lib_LTLIBRARIES = libguile-avahi-v-0.la

libguile_avahi_v_0_la_SOURCES =			\
  utils.c errors.c watch.c common.c client.c	\
  publish.c lookup.c
libguile_avahi_v_0_la_CFLAGS = $(AVAHI_CFLAGS)
libguile_avahi_v_0_la_LIBADD =			\
  $(top_builddir)/lib/libgnu.la $(AVAHI_LIBS)

AM_CPPFLAGS = -I$(builddir) -I$(srcdir)			\
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib


%-enums.h: $(srcdir)/make-enum-header.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-smobs.h: $(srcdir)/make-smob-header.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-enums.i.c: $(srcdir)/make-enum-map.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-smobs.i.c: $(srcdir)/make-smob-types.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-callbacks.h: $(srcdir)/make-callback-header.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@

%-callbacks.i.c: $(srcdir)/make-callback-trampolines.scm
	$(GUILE_FOR_BUILD) $^ `echo $@ | cut -f 1 -d -` > $@


# C file snarfing.

snarfcppopts = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
	       $(CFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
%.c.x: %.c
	$(guile_snarf) -o $@ $< $(snarfcppopts)
