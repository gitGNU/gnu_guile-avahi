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

#ifdef HAVE_GMP_H
# include <gmp.h>
#endif

#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
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

SCM
scm_from_avahi_address (const AvahiAddress *c_address)
{
  SCM result;

  switch (c_address->proto)
    {
#ifdef HAVE_ARPA_INET_H
    case AVAHI_PROTO_INET:
      result = scm_from_uint32 (ntohl (c_address->data.ipv4.address));
      break;
#endif

#ifdef HAVE_GMP_H
    case AVAHI_PROTO_INET6:
      {
	mpz_t mpz;

	/* FIXME: This is broken.  We need to check the host endianness and
	   determine the ENDIAN argument as a function of it.  */
	mpz_init (mpz);
	mpz_import (mpz, 16 /* count = 128 bits */,
		    1 /* order = most significant word first */,
		    8 /* word size */, 1 /* endian */, 0 /* nails */,
		    &c_address->data.ipv6.address);

	result = scm_from_mpz (mpz);
	mpz_clear (mpz);
	break;
      }
#endif

    default:
      scm_avahi_error (AVAHI_ERR_NOT_SUPPORTED, __FUNCTION__);
    }

  return result;
}

SCM
scm_from_avahi_string_list (const AvahiStringList *c_lst)
{
  SCM lst;

  for (lst = SCM_EOL;
       c_lst != NULL;
       c_lst = avahi_string_list_get_next ((AvahiStringList *) c_lst))
    {
      uint8_t *c_str;

      c_str = avahi_string_list_get_text ((AvahiStringList *) c_lst);
      lst = scm_cons (scm_from_locale_string ((const char *) c_str),
		      lst);
    }

  return (scm_reverse_x (lst, SCM_EOL));
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


void
scm_to_avahi_address (SCM address_protocol, SCM address,
		      AvahiAddress *c_address,
		      int pos, const char *func_name)
#define FUNC_NAME func_name
{
  AvahiProtocol c_addrproto;

  c_addrproto = scm_to_avahi_protocol (address_protocol, pos - 1,
				       FUNC_NAME);

  c_address->proto = c_addrproto;

  switch (c_addrproto)
    {
#ifdef HAVE_ARPA_INET_H
    case AVAHI_PROTO_INET:
      c_address->data.ipv4.address = htonl (scm_to_uint32 (address));
      break;
#endif

#ifdef HAVE_GMP_H
    case AVAHI_PROTO_INET6:
      {
	mpz_t mpz;
	size_t count;

	mpz_init (mpz);
	scm_to_mpz (address, mpz);
	if (EXPECT_FALSE (mpz_sizeinbase (mpz, 2) > 128))
	  {
	    mpz_clear (mpz);
	    scm_wrong_type_arg (FUNC_NAME, pos, address);
	  }
	else
	  /* FIXME: This is broken.  We need to check the host endianness and
	     determine the ENDIAN argument as a function of it.  */
	  mpz_export (&c_address->data.ipv6.address, &count,
		      1 /* order = most significant word first */,
		      8 /* word size */, 1 /* endian */,
		      0 /* nails */, mpz);

	mpz_clear (mpz);
	break;
      }
#endif

    default:
      scm_avahi_error (AVAHI_ERR_NOT_SUPPORTED, FUNC_NAME);
    }
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
