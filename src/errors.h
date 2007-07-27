/* Guile-Avahi --- Guile bindings for Avahi.
   Copyright (C) 2007  Ludovic Courtès <ludo@gnu.org>

   This file is part of Guile-Avahi.

   Guile-Avahi is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   Guile-Avahi is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
   General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef GUILE_AVAHI_ERRORS_H
#define GUILE_AVAHI_ERRORS_H

#include <libguile.h>

#include "utils.h"

SCM_API void scm_avahi_error (int, const char *) NO_RETURN;
SCM_API void scm_avahi_init_error (void);

#endif

/* arch-tag: e7a92e44-b399-4c85-99d4-2dd3564600f7
 */
