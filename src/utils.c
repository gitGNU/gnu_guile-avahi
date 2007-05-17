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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "utils.h"

#include "common-enums.h"
#include "client-enums.h"


SCM
scm_from_avahi_watch_events (AvahiWatchEvent c_events)
{
  SCM events = SCM_EOL;

#define MATCH_EVENT(_value)					\
  if (c_events & (_value))					\
    {								\
      events = scm_cons (scm_from_avahi_watch_event (_value),	\
			events);				\
      c_events &= ~(_value);					\
    }

  MATCH_EVENT (AVAHI_WATCH_IN);
  MATCH_EVENT (AVAHI_WATCH_OUT);
  MATCH_EVENT (AVAHI_WATCH_ERR);
  MATCH_EVENT (AVAHI_WATCH_HUP);

  if (c_events != 0)
    /* XXX: We failed to interpret one of the events flags.  */
    abort ();

#undef MATCH_EVENTS

  return events;
}

AvahiWatchEvent
scm_to_avahi_watch_events (SCM events, int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiWatchEvent c_events;

  SCM_VALIDATE_LIST (1, events);

  for (c_events = 0;
       !scm_is_null (events);
       events = SCM_CDR (events))
    {
      c_events |= scm_to_avahi_watch_event (SCM_CAR (events), 1,
					    FUNC_NAME);
    }

  return (c_events);
}
#undef FUNC_NAME

AvahiClientFlags
scm_to_avahi_client_flags (SCM flags, int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiClientFlags c_flags;

  SCM_VALIDATE_LIST (1, flags);

  for (c_flags = 0;
       !scm_is_null (flags);
       flags = SCM_CDR (flags))
    {
      c_flags |= scm_to_avahi_client_flag (SCM_CAR (flags), 1,
					   FUNC_NAME);
    }

  return (c_flags);
}
#undef FUNC_NAME

/* arch-tag: 6e832fbe-4f3a-48c2-9464-2051ca964619
 */
