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

#include <avahi-common/watch.h>
#include <avahi-common/simple-watch.h>
#include <libguile.h>

#include "common-smobs.h"
#include "common-enums.h"

#include "client-smobs.h"
#include "client-enums.h"

#include "utils.h"
#include "errors.h"


/* SMOB and enums type definitions.  */

static inline void
scm_avahi_client_free (AvahiClient *client)
{
  /* Since client SMOBs are temporarily created from a NULL pointer, we must
     make sure we don't mess things up.  */
  if (client != NULL)
    avahi_client_free (client);
}

#include "client-enums.i.c"
#include "client-smobs.i.c"


/* Procedures.  */

#define SCM_AVAHI_SET_CLIENT_CALLBACK(client, callback)	\
  SCM_SET_SMOB_OBJECT_2 (client, callback)
#define SCM_AVAHI_CLIENT_CALLBACK(client)	\
  SCM_SMOB_OBJECT_2 (client)
#define SCM_AVAHI_SET_CLIENT_POLL(client, poll)	\
  SCM_SET_SMOB_OBJECT_3 (client, poll)
#define SCM_AVAHI_CLIENT_POLL(client)		\
  SCM_SMOB_OBJECT_3 (client)

/* Mark the poll and closure associated with CLIENT.  */
SCM_SMOB_MARK (scm_tc16_avahi_client, mark_avahi_client, client)
{
  scm_gc_mark (SCM_AVAHI_CLIENT_POLL (client));

  return (SCM_AVAHI_CLIENT_CALLBACK (client));
}


/* The client callback.  Note: it may be called at client creation time with
   C_CLIENT == NULL.  */
static void
client_trampoline (AvahiClient *c_client,
		   AvahiClientState c_state,
		   void *data)
#define FUNC_NAME "client_trampoline"
{
  SCM client, callback;
  AvahiClient *c_client2;

  client = (SCM) SCM_PACK ((scm_t_bits) data);
  c_client2 = scm_to_avahi_client (client, 0, FUNC_NAME);

  /* Sanity check.  */
  if ((c_client != NULL) && (c_client2 != NULL))
    if (c_client != c_client2)
      abort ();

  callback = SCM_AVAHI_CLIENT_CALLBACK (client);

  scm_call_2 (callback, client,
	      scm_from_avahi_client_state (c_state));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_make_client, "make-client",
	    3, 0, 0,
	    (SCM poll, SCM flags, SCM callback),
	    "Return a new Avahi client.  The client will use @var{poll} "
	    "(a @code{poll} object as returned by, e.g., "
	    "@code{(simple-poll (make-simple-poll))}) for I/O management.  "
	    "In addition, when the client state changes, @code{callback} "
	    "(a two-argument procedure) will be invoked and passed the "
	    "client object and a client-state value.  @var{flags} must "
	    "be a list of client flags (i.e., @code{client-flag/} values).")
#define FUNC_NAME s_scm_avahi_make_client
{
  int err;
  SCM client;
  AvahiClient *c_client;
  AvahiPoll *c_poll;
  AvahiClientFlags c_flags;

  c_poll = scm_to_avahi_poll (poll, 1, FUNC_NAME);
  c_flags = scm_to_avahi_client_flags (flags, 2, FUNC_NAME);
  SCM_VALIDATE_PROC (3, callback);

  /* We have to create the SMOB first so that we can pass it as "user data"
     to `avahi_client_new ()'.  Thus, we need to set it afterwards.  */
  client = scm_from_avahi_client (NULL);
  SCM_AVAHI_SET_CLIENT_CALLBACK (client, callback);
  SCM_AVAHI_SET_CLIENT_POLL (client, poll);

  c_client = avahi_client_new (c_poll, c_flags, client_trampoline,
			       (void *) client, &err);
  if (c_client == NULL)
    scm_avahi_error (err, FUNC_NAME);

  SCM_SET_SMOB_DATA (client, (scm_t_bits) c_client);

  return (client);
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_client_server_version, "client-server-version",
	    1, 0, 0,
	    (SCM client),
	    "Return the version (a string) of the server the client "
	    "is connected to.")
#define FUNC_NAME s_scm_avahi_client_server_version
{
  const char *c_version;
  AvahiClient *c_client;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_version = avahi_client_get_version_string (c_client);

  return (scm_from_locale_string (c_version));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_client_host_name, "client-host-name",
	    1, 0, 0,
	    (SCM client),
	    "Return the host name of the server @var{client} is "
	    "connected to.")
#define FUNC_NAME s_scm_avahi_client_host_name
{
  const char *c_hostname;
  AvahiClient *c_client;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_hostname = avahi_client_get_host_name (c_client);

  return (scm_from_locale_string (c_hostname));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_set_client_host_name_x, "set-client-host-name!",
	    2, 0, 0,
	    (SCM client, SCM name),
	    "Change @var{client}'s host name to @var{name}.")
#define FUNC_NAME s_scm_avahi_set_client_host_name_x
{
  char *c_hostname;
  size_t c_hostname_len;
  AvahiClient *c_client;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  SCM_VALIDATE_STRING (2, name);

  c_hostname_len = scm_c_string_length (name);
  c_hostname = (char *) alloca (c_hostname_len + 1);
  (void) scm_to_locale_stringbuf (name, c_hostname, c_hostname_len);
  c_hostname[c_hostname_len] = '\0';

  /* XXX: What's the meaning of the return value? */
  (void) avahi_client_set_host_name (c_client, c_hostname);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_client_host_fqdn, "client-host-fqdn",
	    1, 0, 0,
	    (SCM client),
	    "Return the fully qualified domain name (FQDN) of the "
	    "server @var{client} is connected to.")
#define FUNC_NAME s_scm_avahi_client_host_fqdn
{
  const char *c_fqdn;
  AvahiClient *c_client;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_fqdn = avahi_client_get_host_name_fqdn (c_client);

  return (scm_from_locale_string (c_fqdn));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_client_state, "client-state",
	    1, 0, 0,
	    (SCM client),
	    "Return the state (a @code{client-state/} value) of "
	    "@var{client}.")
#define FUNC_NAME s_scm_avahi_client_state
{
  AvahiClientState c_state;
  AvahiClient *c_client;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_state = avahi_client_get_state (c_client);

  return (scm_from_avahi_client_state (c_state));
}
#undef FUNC_NAME


/* Initialization.  */
void
scm_avahi_client_init (void)
{
#include "client.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: bfed1cab-478e-4272-9cd3-884f47ce3506
 */
