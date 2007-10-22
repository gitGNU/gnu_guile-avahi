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

nobase_dist_guilemodule_DATA =			\
  avahi.scm avahi/client.scm			\
  avahi/client/lookup.scm			\
  avahi/client/publish.scm

documentation_modules = system/documentation/README		\
		        system/documentation/c-snarf.scm	\
                        system/documentation/output.scm

EXTRA_DIST = avahi/build/enums.scm avahi/build/smobs.scm	\
	     avahi/build/callbacks.scm avahi/build/utils.scm	\
	     avahi/test.scm					\
             $(documentation_modules)
