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

#ifndef GUILE_AVAHI_WATCH_H
#define GUILE_AVAHI_WATCH_H

/* Guile-friendly implementation of the `AvahiPoll' and `AvahiWatch'
   interfaces.  */

#include <avahi-common/watch.h>

#include <libguile.h>

typedef struct AvahiGuilePoll AvahiGuilePoll;

struct AvahiGuilePoll
{
  AvahiPoll api;

  /* Closures.  */
  SCM new_watch;
  SCM free_watch;
  SCM update_watch_x;
  SCM new_timeout;
  SCM free_timeout;
  SCM update_timeout_x;

  /* The corresponding SMOB, if any.  */
  SCM poll_smob;
};

extern AvahiGuilePoll *avahi_guile_poll_new (SCM new_watch,
					     SCM update_watch_x,
					     SCM free_watch,
					     SCM new_timeout,
					     SCM update_timeout_x,
					     SCM free_timeout);

extern const AvahiPoll *avahi_guile_poll_get (AvahiGuilePoll *guile_poll);

extern void avahi_guile_poll_free (AvahiGuilePoll *guile_poll);

extern void scm_avahi_init_watch (void);

#endif
