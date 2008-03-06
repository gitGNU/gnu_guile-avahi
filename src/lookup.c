/* Guile-Avahi --- Guile bindings for Avahi.
   Copyright (C) 2007, 2008  Ludovic Courtès <ludo@gnu.org>

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

#include <avahi-client/lookup.h>
#include <libguile.h>

#include "utils.h"
#include "errors.h"

#include "common-enums.h"
#include "common-smobs.h"
#include "client-smobs.h"
#include "client-enums.h"

#include "lookup-smobs.h"
#include "lookup-enums.h"


/* SMOB and enums type definitions.  */

#include "lookup-smobs.i.c"
#include "lookup-enums.i.c"


/* SMOB helpers.  */

/* Note: Here we refer to `lookup' (as a noun) as either a browser or a
   resolver object.  */

#define SCM_AVAHI_SET_LOOKUP_CALLBACK(group, callback)	\
  SCM_SET_SMOB_OBJECT_2 (group, callback)
#define SCM_AVAHI_LOOKUP_CALLBACK(group)	\
  SCM_SMOB_OBJECT_2 (group)
#define SCM_AVAHI_SET_LOOKUP_CLIENT(group, client)	\
  SCM_SET_SMOB_OBJECT_3 (group, client)
#define SCM_AVAHI_LOOKUP_CLIENT(group)	\
  SCM_SMOB_OBJECT_3 (group)


/* Mark the client and closure associated with the given lookup SMOB.  */

#define SMOB_MARKER(_underscores)			\
  SCM_SMOB_MARK (scm_tc16_avahi_ ## _underscores,	\
		 mark_ ## _underscores,			\
		 lookup)				\
  {							\
    scm_gc_mark (SCM_AVAHI_LOOKUP_CLIENT (lookup));	\
							\
    return (SCM_AVAHI_LOOKUP_CALLBACK (lookup));	\
  }

SMOB_MARKER (domain_browser)
SMOB_MARKER (service_type_browser)
SMOB_MARKER (service_browser)
SMOB_MARKER (service_resolver)
SMOB_MARKER (host_name_resolver)
SMOB_MARKER (address_resolver)

#undef SMOB_MARKER


/* Produce a Scheme procedure that returns the client associated with the
   given lookup SMOB.  */
#define CLIENT_ACCESSOR(_underscores, _dashes)				\
  SCM_DEFINE (scm_avahi_ ## _underscores ## _client, _dashes "-client",	\
	      1, 0, 0,							\
	      (SCM _underscores),					\
	      "Return the client associated with @var{" _dashes "}.")	\
  {									\
    void *c_obj;							\
    c_obj = scm_to_avahi_ ## _underscores (_underscores, 1,		\
					   s_scm_avahi_ ## _underscores	\
					   ## _client);			\
    if (EXPECT_FALSE (c_obj == NULL))					\
      scm_avahi_error (AVAHI_ERR_INVALID_OBJECT, _dashes "-client");	\
									\
    return (SCM_AVAHI_LOOKUP_CLIENT (_underscores));			\
  }

CLIENT_ACCESSOR (domain_browser, "domain-browser")
CLIENT_ACCESSOR (service_type_browser, "service-type-browser")
CLIENT_ACCESSOR (service_browser, "service-browser")
CLIENT_ACCESSOR (service_resolver, "service-resolver")
CLIENT_ACCESSOR (host_name_resolver, "host-name-resolver")
CLIENT_ACCESSOR (address_resolver, "address-resolver")

#undef CLIENT_ACCESSOR

/* Callbacks may be passed NULL pointers for domain, service type and name,
   e.g., in the case of a `AVAHI_BROWSER_CACHE_EXHAUSTED' event.  The
   following macros address this.  */
#define scm_from_avahi_domain(_domain)					\
  (((_domain) == NULL) ? SCM_BOOL_F : scm_from_locale_string (_domain))
#define scm_from_avahi_service_type scm_from_avahi_domain
#define scm_from_avahi_service_name scm_from_avahi_domain
#define scm_from_avahi_host_name    scm_from_avahi_domain

/* Likewise, `domain' arguments can be NULL in C.  */
#define scm_to_avahi_domain(_domain, _c_domain, _pos, _func)		\
do									\
{									\
  if (scm_is_false (_domain))						\
    (_c_domain) = NULL;							\
  else									\
    scm_avahi_to_c_string ((_domain), (_c_domain), (_pos), (_func));	\
}									\
while (0)



/* Callback forward declarations.  */

#include "lookup-callbacks.h"



/* Browsers.  */

static void
domain_browser_callback (AvahiDomainBrowser *c_browser,
			 AvahiIfIndex c_interface,
			 AvahiProtocol c_protocol,
			 AvahiBrowserEvent c_event,
			 const char *c_domain,
			 AvahiLookupResultFlags c_flags,
			 void *data)
{
  SCM browser, callback;
  SCM interface, protocol;

  browser = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (browser);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);

  (void) scm_apply (callback,
		    scm_list_n (browser, interface, protocol,
				scm_from_avahi_browser_event (c_event),
				scm_from_avahi_domain (c_domain),
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}


SCM_DEFINE (scm_avahi_make_domain_browser, "make-domain-browser",
	    7, 0, 0,
	    (SCM client, SCM interface, SCM protocol, SCM domain,
	     SCM domain_browser_type, SCM lookup_flags,
	     SCM callback),
	    "Return a new domain browser of type @var{domain-browser-type} "
	    "(a @code{domain-browser-type/} value) for @var{domain} that "
	    "uses @var{client}.  Upon browsing events (discovery, removal, "
	    "etc.) @var{callback} will be called and passed:\n\n"
	    "@itemize\n"
	    "@item the domain browser object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a browser event type (i.e., one of the "
	    "@code{browser-event/} values);\n"
	    "@item the domain;\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n")
#define FUNC_NAME s_scm_avahi_make_domain_browser
{
  SCM browser;
  AvahiClient *c_client;
  AvahiDomainBrowser *c_browser;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiDomainBrowserType c_type;
  AvahiLookupFlags c_flags;
  char *c_domain;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
                scm_to_avahi_domain (domain, c_domain, 4, FUNC_NAME);
  c_type      = scm_to_avahi_domain_browser_type (domain_browser_type,
						  5, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 6, FUNC_NAME);
  SCM_VALIDATE_PROC (7, callback);


  /* We have to create the SMOB first so that we can pass it as "user data"
     to `avahi_domain_browser_new ()'.  Thus, we need to set it afterwards.  */
  browser = scm_from_avahi_domain_browser (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (browser, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (browser, client);

  c_browser = avahi_domain_browser_new (c_client, c_interface, c_protocol,
					c_domain, c_type, c_flags,
					domain_browser_callback_trampoline,
					(void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (scm_gc_protect_object (browser));
}
#undef FUNC_NAME


static void
service_type_browser_callback (AvahiServiceTypeBrowser *c_browser,
			       AvahiIfIndex c_interface,
			       AvahiProtocol c_protocol,
			       AvahiBrowserEvent c_event,
			       const char *c_type,
			       const char *c_domain,
			       AvahiLookupResultFlags c_flags,
			       void *data)
{
  SCM browser, callback;
  SCM interface, protocol;

  browser = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (browser);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);

  (void) scm_apply (callback,
		    scm_list_n (browser, interface, protocol,
				scm_from_avahi_browser_event (c_event),
				scm_from_avahi_service_type (c_type),
				scm_from_avahi_domain (c_domain),
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}

SCM_DEFINE (scm_avahi_make_service_type_browser, "make-service-type-browser",
	    6, 0, 0,
	    (SCM client, SCM interface, SCM protocol, SCM domain,
	     SCM lookup_flags, SCM callback),
	    "Return a new service type browser using the specified "
	    "@var{client}, @var{interface}, etc.  Upon browsing events "
	    "(discovery, removal, etc.) @var{callback} will be called "
	    "and passed:\n\n"
	    "@itemize\n"
	    "@item the service type browser object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a browser event type (i.e., one of the "
	    "@code{browser-event/} values);\n"
	    "@item a service type (e.g., @code{\"_http._tcp\"});\n"
	    "@item the domain;\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n")
#define FUNC_NAME s_scm_avahi_make_service_type_browser
{
  SCM browser;
  AvahiClient *c_client;
  AvahiServiceTypeBrowser *c_browser;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiLookupFlags c_flags;
  char *c_domain;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
                scm_to_avahi_domain (domain, c_domain, 4, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 5, FUNC_NAME);
  SCM_VALIDATE_PROC (6, callback);


  browser = scm_from_avahi_service_type_browser (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (browser, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (browser, client);

  c_browser =
    avahi_service_type_browser_new (c_client, c_interface,
				    c_protocol, c_domain, c_flags,
				    service_type_browser_callback_trampoline,
				    (void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (scm_gc_protect_object (browser));
}
#undef FUNC_NAME


static void
service_browser_callback (AvahiServiceBrowser *c_browser,
			  AvahiIfIndex c_interface,
			  AvahiProtocol c_protocol,
			  AvahiBrowserEvent c_event,
			  const char *c_name,
			  const char *c_type,
			  const char *c_domain,
			  AvahiLookupResultFlags c_flags,
			  void *data)
{
  SCM browser, callback;
  SCM interface, protocol;

  browser = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (browser);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);

  (void) scm_apply (callback,
		    scm_list_n (browser, interface, protocol,
				scm_from_avahi_browser_event (c_event),
				scm_from_avahi_service_name (c_name),
				scm_from_avahi_service_type (c_type),
				scm_from_avahi_domain (c_domain),
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}


SCM_DEFINE (scm_avahi_make_service_browser, "make-service-browser",
	    7, 0, 0,
	    (SCM client, SCM interface, SCM protocol, SCM type,
	     SCM domain, SCM lookup_flags, SCM callback),
	    "Return a new service browser using the specified "
	    "@var{client}, @var{interface}, etc.  Upon browsing events "
	    "(discovery, removal, etc.) @var{callback} will be called "
	    "and passed:\n\n"
	    "@itemize\n"
	    "@item the service browser object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a browser event type (i.e., one of the "
	    "@code{browser-event/} values);\n"
	    "@item the service name;\n"
	    "@item the service type (e.g., @code{\"_http._tcp\"});\n"
	    "@item the domain;\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n")
#define FUNC_NAME s_scm_avahi_make_service_browser
{
  SCM browser;
  AvahiClient *c_client;
  AvahiServiceBrowser *c_browser;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiLookupFlags c_flags;
  char *c_domain, *c_type;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
		scm_avahi_to_c_string (type, c_type, 4, FUNC_NAME);
                scm_to_avahi_domain (domain, c_domain, 5, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 6, FUNC_NAME);
  SCM_VALIDATE_PROC (7, callback);


  browser = scm_from_avahi_service_browser (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (browser, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (browser, client);

  c_browser = avahi_service_browser_new (c_client, c_interface, c_protocol,
					 c_type, c_domain, c_flags,
					 service_browser_callback_trampoline,
					 (void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (scm_gc_protect_object (browser));
}
#undef FUNC_NAME



/* Resolvers.  */

static void
service_resolver_callback (AvahiServiceResolver *c_resolver,
			   AvahiIfIndex c_interface,
			   AvahiProtocol c_protocol,
			   AvahiResolverEvent c_event,
			   const char *c_name,
			   const char *c_type,
			   const char *c_domain,
			   const char *c_host_name,
			   const AvahiAddress *c_address,
			   uint16_t c_port,
			   AvahiStringList *c_txt,
			   AvahiLookupResultFlags c_flags,
			   void *data)
{
  SCM resolver, callback;
  SCM interface, protocol, address, address_type;

  resolver = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (resolver);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);
  address = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_address (c_address);
  address_type = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_address->proto);

  (void) scm_apply (callback,
		    scm_list_n (resolver, interface, protocol,
				scm_from_avahi_resolver_event (c_event),
				scm_from_avahi_service_name (c_name),
				scm_from_avahi_service_type (c_type),
				scm_from_avahi_domain (c_domain),
				scm_from_avahi_host_name (c_host_name),
				address_type, address,
				scm_from_ushort (c_port),
				scm_from_avahi_string_list (c_txt),
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}

SCM_DEFINE (scm_avahi_make_service_resolver, "make-service-resolver",
	    9, 0, 0,
	    (SCM client, SCM interface, SCM protocol, SCM service_name,
	     SCM type, SCM domain, SCM a_protocol, SCM lookup_flags,
	     SCM callback),
	    "Return a new service resolver using the specified "
	    "@var{client}, @var{interface}, etc., that will resolve the "
	    "host name, IP address, port and @code{txt} properties of "
	    "the service of type @var{type} named @var{service-name}.  "
	    "Upon resolution, @var{callback} is invoked and passed:\n\n"
	    "@itemize\n"
	    "@item the service type resolver object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a resolver event type (i.e., one of the "
	    "@code{resolver-event/} values);\n"
	    "@item the service name;\n"
	    "@item the service type (e.g., @code{\"_http._tcp\"});\n"
	    "@item the domain;\n"
	    "@item the host name (name of the host the service is running "
	    "on);\n"
	    "@item the host IP address type (i.e., "
	    "@code{protocol/inet} for an IPv4 address and "
	    "@code{protocol/inet6} for an IPv6 address);\n"
	    "@item the host IP address in host byte order "
	    "(@pxref{Network Address Conversion,,, guile, The GNU Guile "
	    "Reference Manual});\n"
	    "@item a list of @code{txt} properties (strings);\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n"
	    "An exception may be raised on failure.")
#define FUNC_NAME s_scm_avahi_make_service_resolver
{
  SCM resolver;
  AvahiClient *c_client;
  AvahiServiceResolver *c_resolver;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol, c_a_proto;
  AvahiLookupFlags c_flags;
  char *c_domain, *c_type, *c_name;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
		scm_avahi_to_c_string (service_name, c_name, 4, FUNC_NAME);
		scm_avahi_to_c_string (type, c_type, 5, FUNC_NAME);
                scm_to_avahi_domain (domain, c_domain, 6, FUNC_NAME);
  c_a_proto   = scm_to_avahi_protocol (a_protocol, 7, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 8, FUNC_NAME);
  SCM_VALIDATE_PROC (9, callback);


  resolver = scm_from_avahi_service_resolver (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (resolver, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (resolver, client);

  c_resolver =
    avahi_service_resolver_new (c_client, c_interface, c_protocol,
				c_name, c_type, c_domain,
				c_a_proto, c_flags,
				service_resolver_callback_trampoline,
				(void *) resolver);
  if (c_resolver == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (resolver, (scm_t_bits) c_resolver);

  return (scm_gc_protect_object (resolver));
}
#undef FUNC_NAME


static void
host_name_resolver_callback (AvahiHostNameResolver *c_resolver,
			     AvahiIfIndex c_interface,
			     AvahiProtocol c_protocol,
			     AvahiResolverEvent c_event,
			     const char *c_host_name,
			     const AvahiAddress *c_address,
			     AvahiLookupResultFlags c_flags,
			     void *data)
{
  SCM resolver, callback;
  SCM interface, protocol, address, address_type;;

  resolver = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (resolver);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);
  address = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_address (c_address);
  address_type = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_address->proto);

  (void) scm_apply (callback,
		    scm_list_n (resolver, interface, protocol,
				scm_from_avahi_resolver_event (c_event),
				scm_from_avahi_host_name (c_host_name),
				address_type, address,
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}

SCM_DEFINE (scm_avahi_make_host_name_resolver, "make-host-name-resolver",
	    7, 0, 0,
	    (SCM client, SCM interface, SCM protocol, SCM host_name,
	     SCM a_protocol, SCM lookup_flags, SCM callback),
	    "Return a new host-name resolver using the specified "
	    "@var{client}, @var{interface}, etc., that will resolve "
	    "@var{host-name}, i.e., find the corresponding IP address.  "
	    "Upon resolution, @var{callback} is invoked and passed:\n\n"
	    "@itemize\n"
	    "@item the host-name resolver object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a resolver event type (i.e., one of the "
	    "@code{resolver-event/} values);\n"
	    "@item the host name;\n"
	    "@item the host IP address type (i.e., "
	    "@code{protocol/inet} for an IPv4 address and "
	    "@code{protocol/inet6} for an IPv6 address);\n"
	    "@item the host IP address in host byte order "
	    "(@pxref{Network Address Conversion,,, guile, The GNU Guile "
	    "Reference Manual});\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n"
	    "An exception may be raised on failure.")
#define FUNC_NAME s_scm_avahi_make_host_name_resolver
{
  SCM resolver;
  AvahiClient *c_client;
  AvahiHostNameResolver *c_resolver;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol, c_a_proto;
  AvahiLookupFlags c_flags;
  char *c_name;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
		scm_avahi_to_c_string (host_name, c_name, 4, FUNC_NAME);
  c_a_proto   = scm_to_avahi_protocol (a_protocol, 5, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 6, FUNC_NAME);
  SCM_VALIDATE_PROC (7, callback);


  resolver = scm_from_avahi_host_name_resolver (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (resolver, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (resolver, client);

  c_resolver =
    avahi_host_name_resolver_new (c_client, c_interface, c_protocol,
				  c_name, c_a_proto, c_flags,
				  host_name_resolver_callback_trampoline,
				  (void *) resolver);
  if (c_resolver == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (resolver, (scm_t_bits) c_resolver);

  return (scm_gc_protect_object (resolver));
}
#undef FUNC_NAME


static void
address_resolver_callback (AvahiAddressResolver *c_resolver,
			   AvahiIfIndex c_interface,
			   AvahiProtocol c_protocol,
			   AvahiResolverEvent c_event,
			   const AvahiAddress *c_address,
			   const char *c_host_name,
			   AvahiLookupResultFlags c_flags,
			   void *data)
{
  SCM resolver, callback;
  SCM interface, protocol, address, address_type;

  resolver = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_LOOKUP_CALLBACK (resolver);

  interface = (c_interface < 0)
    ? SCM_BOOL_F : scm_from_avahi_interface_index (c_interface);
  protocol  = (c_protocol  < 0)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_protocol);
  address = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_address (c_address);
  address_type = (c_address == NULL)
    ? SCM_BOOL_F : scm_from_avahi_protocol (c_address->proto);

  (void) scm_apply (callback,
		    scm_list_n (resolver, interface, protocol,
				scm_from_avahi_resolver_event (c_event),
				address_type, address,
				scm_from_avahi_host_name (c_host_name),
				scm_from_avahi_lookup_result_flags (c_flags),
				SCM_UNDEFINED),
		    SCM_EOL);
}

SCM_DEFINE (scm_avahi_make_address_resolver, "make-address-resolver",
	    7, 0, 0,
	    (SCM client, SCM interface, SCM protocol,
	     SCM address_type, SCM address,
	     SCM lookup_flags, SCM callback),
	    "Return a new address resolver using the specified "
	    "@var{client}, @var{interface}, etc., that will resolve the "
	    "host name corresponding to @var{address} of type "
	    "@var{address-type} (either @code{protocol/inet} for an "
	    "IPv4 address or @code{protocol/inet6} for an IPv6 "
	    "address).  As usual, @var{address} should be the raw IP "
	    "address in host byte order (@pxref{Network Address "
	    "Conversion,,, guile, The GNU Guile Reference Manual}).  "
	    "Upon resolution, @var{callback} is invoked and passed:\n\n"
	    "@itemize\n"
	    "@item the address resolver object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a resolver event type (i.e., one of the "
	    "@code{resolver-event/} values);\n"
	    "@item the host IP address type (i.e., "
	    "@var{address-type});\n"
	    "@item the host IP address (i.e., @var{address});\n"
	    "@item the corresponding host name;\n"
	    "@item lookup result flags (i.e., a list of "
	    "@code{lookup-result-flag/} values).\n"
	    "@end itemize\n"
	    "An exception may be raised on failure.")
#define FUNC_NAME s_scm_avahi_make_address_resolver
{
  SCM resolver;
  AvahiClient *c_client;
  AvahiAddressResolver *c_resolver;
  AvahiIfIndex c_interface;
  AvahiProtocol c_protocol;
  AvahiLookupFlags c_flags;
  AvahiAddress c_address;

  c_client    = scm_to_avahi_client (client, 1, FUNC_NAME);
  c_interface = scm_to_avahi_interface_index (interface, 2, FUNC_NAME);
  c_protocol  = scm_to_avahi_protocol (protocol, 3, FUNC_NAME);
                scm_to_avahi_address (address_type, address, &c_address,
				      5, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 6, FUNC_NAME);
  SCM_VALIDATE_PROC (7, callback);


  resolver = scm_from_avahi_address_resolver (NULL);
  SCM_AVAHI_SET_LOOKUP_CALLBACK (resolver, callback);
  SCM_AVAHI_SET_LOOKUP_CLIENT (resolver, client);

  c_resolver =
    avahi_address_resolver_new (c_client, c_interface, c_protocol,
				&c_address, c_flags,
				address_resolver_callback_trampoline,
				(void *) resolver);
  if (c_resolver == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (resolver, (scm_t_bits) c_resolver);

  return (scm_gc_protect_object (resolver));
}
#undef FUNC_NAME


/* Callback trampolines.  */

#include "lookup-callbacks.i.c"



/* Initialization.  */

void
scm_avahi_lookup_init (void)
{
#include  "lookup.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: f7484a05-8f2a-4ed5-95f6-e865098eb81f
 */
