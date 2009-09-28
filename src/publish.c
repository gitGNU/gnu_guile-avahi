/* Guile-Avahi --- Guile bindings for Avahi.
   Copyright (C) 2007, 2008, 2009  Ludovic Courtès <ludo@gnu.org>

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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <alloca.h>

#include <avahi-client/publish.h>
#include <avahi-common/alternative.h>
#include <avahi-common/malloc.h>
#include <libguile.h>

#include "utils.h"
#include "errors.h"

#include "common-enums.h"
#include "common-smobs.h"
#include "client-smobs.h"
#include "client-enums.h"

#include "publish-smobs.h"
#include "publish-enums.h"


/* SMOB and enums type definitions.  */

static inline void
scm_avahi_entry_group_free (AvahiEntryGroup *c_group)
{
  if (c_group != NULL)
    (void) avahi_entry_group_free (c_group);
}

#include "publish-smobs.i.c"
#include "publish-enums.i.c"


/* Callback forward declarations.  */

#include "publish-callbacks.h"



/* Procedures.  */

#define SCM_AVAHI_SET_ENTRY_GROUP_CALLBACK(group, callback)	\
  SCM_SET_SMOB_OBJECT_2 (group, callback)
#define SCM_AVAHI_ENTRY_GROUP_CALLBACK(group)	\
  SCM_SMOB_OBJECT_2 (group)
#define SCM_AVAHI_SET_ENTRY_GROUP_CLIENT(group, client)	\
  SCM_SET_SMOB_OBJECT_3 (group, client)
#define SCM_AVAHI_ENTRY_GROUP_CLIENT(group)	\
  SCM_SMOB_OBJECT_3 (group)


#if SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION <= 8

/* Mark the client and closure associated with GROUP.  */
SCM_SMOB_MARK (scm_tc16_avahi_entry_group, mark_entry_group, group)
{
  scm_gc_mark (SCM_AVAHI_ENTRY_GROUP_CLIENT (group));

  return (SCM_AVAHI_ENTRY_GROUP_CALLBACK (group));
}

#endif /* SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION <= 8 */

static void
entry_group_callback (AvahiEntryGroup *c_group,
		      AvahiEntryGroupState c_state,
		      void *data)
{
  SCM group, callback;

  group = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_ENTRY_GROUP_CALLBACK (group);

  (void) scm_call_2 (callback, group,
		     scm_from_avahi_entry_group_state (c_state));
}

SCM_DEFINE (scm_avahi_make_entry_group, "make-entry-group",
	    2, 0, 0,
	    (SCM client, SCM callback),
	    "Return a new entry group using @var{client} and "
	    "@var{callback} as the state-change notification procedure.  "
	    "@var{callback} should be a two-argument procedure.  It will "
	    "be passed the group object and the group entry's state "
	    "(i.e., a @code{group-entry-state/} value).")
#define FUNC_NAME s_scm_avahi_make_entry_group
{
  SCM group;
  AvahiClient *c_client;
  AvahiEntryGroup *c_group;

  c_client = scm_to_avahi_client (client, 1, FUNC_NAME);
  SCM_VALIDATE_PROC (2, callback);

  /* We have to create the SMOB first so that we can pass it as "user data"
     to `avahi_entry_group_new ()'.  Thus, we need to set it afterwards.  */
  group = scm_from_avahi_entry_group (NULL);
  SCM_AVAHI_SET_ENTRY_GROUP_CALLBACK (group, callback);
  SCM_AVAHI_SET_ENTRY_GROUP_CLIENT (group, client);

  c_group = avahi_entry_group_new (c_client, entry_group_callback_trampoline,
				   (void *) group);
  if (c_group == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (group, (scm_t_bits) c_group);

  return (scm_gc_protect_object (group));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_commit_entry_group, "commit-entry-group",
	    1, 0, 0,
	    (SCM group),
	    "Commit entry group @var{group}, i.e., register its entries "
	    "on the network.  It is an error to commit an empty group.")
#define FUNC_NAME s_scm_avahi_commit_entry_group
{
  int err;
  AvahiEntryGroup *c_group;

  c_group = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  SCM_AVAHI_C_ASSERT_VALID (c_group);

  err = avahi_entry_group_commit (c_group);
  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_reset_entry_group_x, "reset-entry-group!",
	    1, 0, 0,
	    (SCM group),
	    "Reset @var{group}.")
#define FUNC_NAME s_scm_avahi_reset_entry_group_x
{
  int err;
  AvahiEntryGroup *c_group;

  c_group = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  SCM_AVAHI_C_ASSERT_VALID (c_group);

  err = avahi_entry_group_reset (c_group);
  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_entry_group_state, "entry-group-state",
	    1, 0, 0,
	    (SCM group),
	    "Return the state of @var{group}, i.e., an "
	    "@code{entry-group-state/} value.")
#define FUNC_NAME s_scm_avahi_entry_group_state
{
  int c_state;
  AvahiEntryGroup *c_group;

  c_group = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  SCM_AVAHI_C_ASSERT_VALID (c_group);

  c_state = avahi_entry_group_get_state (c_group);
  if (c_state < 0)
    scm_avahi_error (c_state, FUNC_NAME);

  return (scm_from_avahi_entry_group_state ((AvahiEntryGroupState) c_state));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_entry_group_empty_p, "entry-group-empty?",
	    1, 0, 0,
	    (SCM group),
	    "Return @code{#t} if @var{group} is empty, @code{#f} otherwise.")
#define FUNC_NAME s_scm_avahi_entry_group_empty_p
{
  AvahiEntryGroup *c_group;

  c_group = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  SCM_AVAHI_C_ASSERT_VALID (c_group);

  return (scm_from_bool (avahi_entry_group_is_empty (c_group)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_entry_group_client, "entry-group-client",
	    1, 0, 0,
	    (SCM group),
	    "Return the client used by @var{group}.")
#define FUNC_NAME s_scm_avahi_entry_group_client
{
  /* Type-check.  */
  (void) scm_to_avahi_entry_group (group, 1, FUNC_NAME);

  return (SCM_AVAHI_ENTRY_GROUP_CLIENT (group));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_add_entry_group_service_x,
	    "add-entry-group-service!",
	    9, 0, 1,
	    (SCM group, SCM interface, SCM protocol, SCM publish_flags,
	     SCM service_name, SCM service_type, SCM domain, SCM host,
	     SCM port, SCM txt),
	    "Add a service of type @var{service-type} (e.g., "
	    "@code{\"_http._tcp\"}) named @var{service-name} to "
	    "@var{group}.  @var{port} should be an integer telling "
	    "which port this service is listening on; @var{host} can "
	    "be a string indicating which host it is running on, or "
	    "@code{#f} to let the daemon decide by itself (recommended).  "
	    "Likewise, @var{domain} can be @code{#f} (recommended) or "
	    "a string indicating the domain where this service is to be "
	    "registered.  Additionaly @var{txt} arguments should be string "
	    "denoting additional @code{txt} properties (e.g., "
	    "@code{\"color-printer=yes\"}).  Finally, @var{interface} and "
	    "@var{protocol} denote, respectively, the network interface "
	    "and protocol used to publish the service.  @var{interface} may "
	    "be @code{interface/unspecified}, in which case the daemon "
	    "will choose the most appropriate interface, or it can be a "
	    "string (e.g., @code{\"eth0\"}), or an integer OS-provided "
	    "integer index; similarly, @var{protocol} may be "
	    "@code{protocol/unspecified}, in which case the daemon will "
	    "choose a protocol, or it can be any other @code{protocol/} "
	    "value.")
#define FUNC_NAME s_scm_avahi_add_entry_group_service_x
{
  int err;
  AvahiEntryGroup *c_group;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiPublishFlags c_flags;
  AvahiStringList *c_txt;
  char *c_name, *c_type, *c_domain, *c_host;
  unsigned short c_port;
  int c_pos;

  c_group     = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
  c_flags     = scm_to_avahi_publish_flags (publish_flags, 4, FUNC_NAME);
                scm_avahi_to_c_string (service_name, c_name, 5, FUNC_NAME);
                scm_avahi_to_c_string (service_type, c_type, 6, FUNC_NAME);
  if (!scm_is_eq (domain, SCM_BOOL_F))
    scm_avahi_to_c_string (domain, c_domain, 7, FUNC_NAME);
  else
    c_domain  = NULL;
  if (!scm_is_eq (host, SCM_BOOL_F))
    scm_avahi_to_c_string (host, c_host, 8, FUNC_NAME);
  else
    c_host    = NULL;
  c_port      = scm_to_ushort (port);

  for (c_pos = 9, c_txt = NULL;
       scm_is_pair (txt);
       txt = SCM_CDR (txt), c_pos++)
    {
      if (EXPECT_TRUE (scm_is_string (SCM_CAR (txt))))
	{
	  char *c_txt_item;
	  AvahiStringList *c_head;

	  /* XXX: This is both stack- and heap-intensive.  */
	  SCM_AVAHI_TO_C_STRING (SCM_CAR (txt), c_txt_item);

	  if (c_txt == NULL)
	    c_head = avahi_string_list_new (c_txt_item, NULL);
	  else
	    c_head = avahi_string_list_add (c_txt, c_txt_item);

	  if (EXPECT_TRUE (c_head == NULL))
	    avahi_string_list_free (c_txt);
	  else
	    c_txt = c_head;
	}
      else
	{
	  avahi_string_list_free (c_txt);
	  scm_wrong_type_arg (FUNC_NAME, c_pos, SCM_CAR (txt));
	}
    }

  err = avahi_entry_group_add_service_strlst (c_group, c_interface,
					      c_protocol, c_flags,
					      c_name, c_type,
					      c_domain, c_host,
					      c_port, c_txt);
  avahi_string_list_free (c_txt);

  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_add_entry_group_service_subtype_x,
	    "add-entry-group-service-subtype!",
	    8, 0, 0,
	    (SCM group, SCM interface, SCM protocol, SCM publish_flags,
	     SCM service_name, SCM service_type, SCM domain,
	     SCM subtype),
	    "Add @var{subtype} as a sub-type of a service already "
	    "present in @var{group}.  You may add as many subtypes "
	    "for a service as you wish.")
#define FUNC_NAME s_scm_avahi_add_entry_group_service_subtype_x
{
  int err;
  AvahiEntryGroup *c_group;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiPublishFlags c_flags;
  char *c_name, *c_type, *c_domain, *c_subtype;

  c_group     = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
  c_flags     = scm_to_avahi_publish_flags (publish_flags, 4, FUNC_NAME);
                scm_avahi_to_c_string (service_name, c_name, 5, FUNC_NAME);
                scm_avahi_to_c_string (service_type, c_type, 6, FUNC_NAME);
  if (!scm_is_eq (domain, SCM_BOOL_F))
    scm_avahi_to_c_string (domain, c_domain, 7, FUNC_NAME);
  else
    c_domain  = NULL;

  scm_avahi_to_c_string (subtype, c_subtype, 8, FUNC_NAME);

  err = avahi_entry_group_add_service_subtype (c_group, c_interface,
					       c_protocol, c_flags,
					       c_name, c_type,
					       c_domain, c_subtype);
  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_update_entry_group_service_x,
	    "update-entry-group-service!",
	    7, 0, 1,
	    (SCM group, SCM interface, SCM protocol, SCM publish_flags,
	     SCM service_name, SCM service_type, SCM domain,
	     SCM txt),
	    "Update the service named @var{service-name} in "
	    "@var{group}.")
#define FUNC_NAME s_scm_avahi_update_entry_group_service_x
{
  int err;
  AvahiEntryGroup *c_group;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiPublishFlags c_flags;
  AvahiStringList *c_txt;
  char *c_name, *c_type, *c_domain;
  int c_pos;

  c_group     = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
  c_flags     = scm_to_avahi_publish_flags (publish_flags, 4, FUNC_NAME);
                scm_avahi_to_c_string (service_name, c_name, 5, FUNC_NAME);
                scm_avahi_to_c_string (service_type, c_type, 6, FUNC_NAME);
  if (!scm_is_eq (domain, SCM_BOOL_F))
    scm_avahi_to_c_string (domain, c_domain, 7, FUNC_NAME);
  else
    c_domain  = NULL;

  for (c_pos = 8, c_txt = NULL;
       scm_is_pair (txt);
       txt = SCM_CDR (txt), c_pos++)
    {
      if (EXPECT_TRUE (scm_is_string (SCM_CAR (txt))))
	{
	  char *c_txt_item;
	  AvahiStringList *c_head;

	  /* XXX: This is both stack- and heap-intensive.  */
	  SCM_AVAHI_TO_C_STRING (SCM_CAR (txt), c_txt_item);

	  if (c_txt == NULL)
	    c_head = avahi_string_list_new (c_txt_item, NULL);
	  else
	    c_head = avahi_string_list_add (c_txt, c_txt_item);

	  if (EXPECT_TRUE (c_head == NULL))
	    avahi_string_list_free (c_txt);
	  else
	    c_txt = c_head;
	}
      else
	{
	  avahi_string_list_free (c_txt);
	  scm_wrong_type_arg (FUNC_NAME, c_pos, SCM_CAR (txt));
	}
    }

  err = avahi_entry_group_update_service_txt_strlst (c_group, c_interface,
						     c_protocol, c_flags,
						     c_name, c_type,
						     c_domain, c_txt);
  avahi_string_list_free (c_txt);

  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_add_entry_group_address_x, "add-entry-group-address!",
	    7, 0, 0,
	    (SCM group, SCM interface, SCM protocol, SCM publish_flags,
	     SCM fqdn, SCM address_protocol, SCM address),
	    "Add to @var{group} a mapping from fully-qualified domain name "
	    "@var{fqdn} to address @var{address}.  Depending on "
	    "@var{address-protocol} (a @code{protocol/} value), @var{address} "
	    "should be a 32-bit or 128-bit integer (for IPv4 and IPv6, "
	    "respectively) in host byte order (@pxref{Network Address "
	    "Conversion,,, guile, The GNU Guile Reference Manual}).")
#define FUNC_NAME s_scm_avahi_add_entry_group_address_x
{
  int err;
  AvahiEntryGroup *c_group;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiPublishFlags c_flags;
  AvahiAddress c_address;
  char *c_fqdn;

  c_group     = scm_to_avahi_entry_group (group, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
  c_flags     = scm_to_avahi_publish_flags (publish_flags, 4, FUNC_NAME);
                scm_avahi_to_c_string (fqdn, c_fqdn, 5, FUNC_NAME);
		scm_to_avahi_address (address_protocol, address,
				      &c_address, 7, FUNC_NAME);

  err = avahi_entry_group_add_address (c_group, c_interface,
				       c_protocol, c_flags,
				       c_fqdn, &c_address);
  if (EXPECT_FALSE (err))
    scm_avahi_error (err, FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_alternative_host_name, "alternative-host-name",
	    1, 0, 0,
	    (SCM hostname),
	    "Find an alternative name to @var{hostname}.  If called "
	    "with an original host name, @code{\"2\"} is appended.  "
	    "Afterwards the number is incremented on each call (i.e., "
	    "@code{\"foo\"} becomes @code{\"foo2\"}, which becomes "
	    "@code{\"foo3\"}, and so on).")
#define FUNC_NAME s_scm_avahi_alternative_host_name
{
  SCM result;
  char *c_hostname, *c_new_name;

  scm_avahi_to_c_string (hostname, c_hostname, 1, FUNC_NAME);

  c_new_name = avahi_alternative_host_name (c_hostname);
  if (EXPECT_FALSE (c_new_name == NULL))
    scm_avahi_error (AVAHI_ERR_NO_MEMORY, FUNC_NAME);

  result = scm_from_locale_string (c_new_name);
  avahi_free (c_new_name);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_alternative_service_name, "alternative-service-name",
	    1, 0, 0,
	    (SCM service_name),
	    "Find an alternative name to @var{service-name}.  If called "
	    "with an original service name, @code{\" #2\"} is appended.  "
	    "Afterwards the number is incremented on each call (i.e., "
	    "@code{\"foo\"} becomes @code{\"foo #2\"}, which becomes "
	    "@code{\"foo #3\"}, and so on).")
#define FUNC_NAME s_scm_avahi_alternative_service_name
{
  SCM result;
  char *c_service_name, *c_new_name;

  scm_avahi_to_c_string (service_name, c_service_name, 1, FUNC_NAME);

  c_new_name = avahi_alternative_service_name (c_service_name);
  if (EXPECT_FALSE (c_new_name == NULL))
    scm_avahi_error (AVAHI_ERR_NO_MEMORY, FUNC_NAME);

  result = scm_from_locale_string (c_new_name);
  avahi_free (c_new_name);

  return result;
}
#undef FUNC_NAME


/* Callback trampolines.  */

#include "publish-callbacks.i.c"



/* Initialization.  */

void
scm_avahi_publish_init (void)
{
#include "publish.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: ffe4f98f-ac60-49e3-8848-e85b6db2d0c7
 */
