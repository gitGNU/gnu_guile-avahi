/* Guile-Avahi --- Guile bindings for Avahi.
   Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>

   Guile-Avahi is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   Guile-Avahi is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile-Avahi; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA  */

#ifndef GUILE_AVAHI_UTILS_H
#define GUILE_AVAHI_UTILS_H

#include <libguile.h>

#include <avahi-common/watch.h>
#include <avahi-client/client.h>

SCM_API SCM scm_from_avahi_watch_events (AvahiWatchEvent events);
SCM_API AvahiWatchEvent scm_to_avahi_watch_events (SCM events, int pos,
						   const char *func_name);
SCM_API AvahiClientFlags scm_to_avahi_client_flags (SCM flags, int pos,
						    const char *func_name);

#endif

/* arch-tag: 2cd14488-a545-43e4-8991-7c25b048fd72
 */
