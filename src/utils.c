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

#ifdef HAVE_NET_IF_H
# include <net/if.h>
#endif

#include "utils.h"
#include "errors.h"

#include "common-enums.h"
#include "client-enums.h"
#include "publish-enums.h"
#include "lookup-enums.h"



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
    scm_avahi_error (AVAHI_ERR_FAILURE, __FUNCTION__);

#undef MATCH_EVENT

  return events;
}

SCM
scm_from_avahi_lookup_result_flags (AvahiLookupResultFlags c_flags)
{
  SCM flags = SCM_EOL;

#define MATCH_FLAG(_value)						\
  if (c_flags & (_value))						\
    {									\
      flags = scm_cons (scm_from_avahi_lookup_result_flag (_value),	\
			flags);						\
      c_flags &= ~(_value);						\
    }

  MATCH_FLAG (AVAHI_LOOKUP_RESULT_CACHED);
  MATCH_FLAG (AVAHI_LOOKUP_RESULT_WIDE_AREA);
  MATCH_FLAG (AVAHI_LOOKUP_RESULT_MULTICAST);
  MATCH_FLAG (AVAHI_LOOKUP_RESULT_LOCAL);
  MATCH_FLAG (AVAHI_LOOKUP_RESULT_OUR_OWN);
  MATCH_FLAG (AVAHI_LOOKUP_RESULT_STATIC);

  if (c_flags != 0)
    /* XXX: We failed to interpret one of the flags flags.  */
    scm_avahi_error (AVAHI_ERR_FAILURE, __FUNCTION__);

#undef MATCH_FLAG

  return flags;
}

SCM
scm_from_avahi_interface_index (AvahiIfIndex c_interface)
{
#if (defined HAVE_IF_INDEXTONAME) && (defined IFNAMSIZ)
  char c_name[IFNAMSIZ];

  if (c_interface < 0)
    scm_avahi_error (AVAHI_ERR_FAILURE, __FUNCTION__);

  if (if_indextoname ((unsigned int) c_interface, c_name) == NULL)
    scm_avahi_error (AVAHI_ERR_FAILURE, __FUNCTION__);

  return (scm_from_locale_string (c_name));
#else
  return (scm_from_int ((int) c_interface));
#endif /* HAVE_IF_INDEXTONAME */
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

AvahiPublishFlags
scm_to_avahi_publish_flags (SCM flags, int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiPublishFlags c_flags;

  SCM_VALIDATE_LIST (1, flags);

  for (c_flags = 0;
       !scm_is_null (flags);
       flags = SCM_CDR (flags))
    {
      c_flags |= scm_to_avahi_publish_flag (SCM_CAR (flags), 1,
					    FUNC_NAME);
    }

  return (c_flags);
}
#undef FUNC_NAME

AvahiLookupFlags
scm_to_avahi_lookup_flags (SCM flags, int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiLookupFlags c_flags;

  SCM_VALIDATE_LIST (1, flags);

  for (c_flags = 0;
       !scm_is_null (flags);
       flags = SCM_CDR (flags))
    {
      c_flags |= scm_to_avahi_lookup_flag (SCM_CAR (flags), 1,
					   FUNC_NAME);
    }

  return (c_flags);
}
#undef FUNC_NAME



/* Interfaces.  */

AvahiIfIndex
scm_to_avahi_interface_index (SCM interface, int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiIfIndex c_result;

  if (scm_is_integer (interface))
    c_result = (AvahiIfIndex) scm_to_int (interface);
#ifdef HAVE_IF_NAMETOINDEX
  else if (scm_is_string (interface))
    {
      char *c_interface;

      SCM_AVAHI_TO_C_STRING (interface, c_interface);
      c_result = (AvahiIfIndex) if_nametoindex (c_interface);
    }
#endif
  else
    c_result = scm_to_avahi_interface (interface, pos, func_name);

  return (c_result);
}
#undef FUNC_NAME

/* arch-tag: 6e832fbe-4f3a-48c2-9464-2051ca964619
 */
