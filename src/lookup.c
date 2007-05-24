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


/* Produce a "safe" destructor, i.e., one that can deal with NULL pointers.
   This is needed because of the two-phase browser SMOB creation.  */
#define SAFE_DESTRUCTOR(_underscores, _mixedcase)			\
  static inline void							\
  scm_avahi_ ## _underscores ## _free (Avahi ## _mixedcase *c_thing)	\
  {									\
    if (c_thing)							\
      avahi_ ## _underscores ## _free (c_thing);			\
  }

SAFE_DESTRUCTOR (domain_browser, DomainBrowser)
SAFE_DESTRUCTOR (service_type_browser, ServiceTypeBrowser)
SAFE_DESTRUCTOR (service_browser, ServiceBrowser)
SAFE_DESTRUCTOR (service_resolver, ServiceResolver)
SAFE_DESTRUCTOR (host_name_resolver, HostNameResolver)
SAFE_DESTRUCTOR (address_resolver, AddressResolver)

#undef SAFE_DESTRUCTOR

#include "lookup-smobs.i.c"
#include "lookup-enums.i.c"


/* SMOB helpers.  */

#define SCM_AVAHI_SET_BROWSER_CALLBACK(group, callback)	\
  SCM_SET_SMOB_OBJECT_2 (group, callback)
#define SCM_AVAHI_BROWSER_CALLBACK(group)	\
  SCM_SMOB_OBJECT_2 (group)
#define SCM_AVAHI_SET_BROWSER_CLIENT(group, client)	\
  SCM_SET_SMOB_OBJECT_3 (group, client)
#define SCM_AVAHI_BROWSER_CLIENT(group)	\
  SCM_SMOB_OBJECT_3 (group)


/* Mark the client and closure associated with the given browser SMOB.  */

#define BROWSER_SMOB_MARK(_underscores)			\
  SCM_SMOB_MARK (scm_tc16_avahi_ ## _underscores,	\
		 mark_ ## _underscores,			\
		 browser)				\
  {							\
    scm_gc_mark (SCM_AVAHI_BROWSER_CLIENT (browser));	\
							\
    return (SCM_AVAHI_BROWSER_CALLBACK (browser));	\
  }

BROWSER_SMOB_MARK (domain_browser)
BROWSER_SMOB_MARK (service_type_browser)
BROWSER_SMOB_MARK (service_browser)

#undef BROWSER_SMOB_MARK


/* Produce a Scheme procedure that returns the client associated with the
   given browser SMOB.  */
#define BROWSER_CLIENT_ACCESSOR(_underscores, _dashes)			\
  SCM_DEFINE (scm_avahi_ ## _underscores ## _client, _dashes "-client",	\
	      1, 0, 0,							\
	      (SCM _underscores),					\
	      "Return the client associated with @var{" _dashes "}.")	\
  {									\
    /* Type-check.  */							\
    (void) scm_to_avahi_ ## _underscores (_underscores, 1,		\
					  s_scm_avahi_ ## _underscores	\
					  ## _client);			\
									\
    return (SCM_AVAHI_BROWSER_CLIENT (_underscores));			\
  }

BROWSER_CLIENT_ACCESSOR (domain_browser, "domain-browser")
BROWSER_CLIENT_ACCESSOR (service_type_browser, "service-type-browser")
BROWSER_CLIENT_ACCESSOR (service_browser, "service-browser")

#undef BROWSER_CLIENT_ACCESSOR

/* Callbacks may be passed NULL pointers for domain, service type and name,
   e.g., in the case of a `AVAHI_BROWSER_CACHE_EXHAUSTED' event.  The
   following macros address this.  */
#define scm_from_avahi_domain(_domain)					\
  (((_domain) == NULL) ? SCM_BOOL_F : scm_from_locale_string (_domain))
#define scm_from_avahi_service_type scm_from_avahi_domain
#define scm_from_avahi_service_name scm_from_avahi_domain

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



/* Browsers.  */

static void
domain_browser_trampoline (AvahiDomainBrowser *c_browser,
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
  callback = SCM_AVAHI_BROWSER_CALLBACK (browser);

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
	    "@item lookup result flags (i.e., one of the "
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
  SCM_AVAHI_SET_BROWSER_CALLBACK (browser, callback);
  SCM_AVAHI_SET_BROWSER_CLIENT (browser, client);

  c_browser = avahi_domain_browser_new (c_client, c_interface, c_protocol,
					c_domain, c_type, c_flags,
					domain_browser_trampoline,
					(void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (browser);
}
#undef FUNC_NAME


static void
service_type_browser_trampoline (AvahiServiceTypeBrowser *c_browser,
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
  callback = SCM_AVAHI_BROWSER_CALLBACK (browser);

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
	    "@var{client}, @var{interface, etc.  Upon browsing events "
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
	    "@item lookup result flags (i.e., one of the "
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
  SCM_AVAHI_SET_BROWSER_CALLBACK (browser, callback);
  SCM_AVAHI_SET_BROWSER_CLIENT (browser, client);

  c_browser = avahi_service_type_browser_new (c_client, c_interface,
					      c_protocol, c_domain, c_flags,
					      service_type_browser_trampoline,
					      (void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (browser);
}
#undef FUNC_NAME


static void
service_browser_trampoline (AvahiServiceBrowser *c_browser,
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
  callback = SCM_AVAHI_BROWSER_CALLBACK (browser);

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
	    "@var{client}, @var{interface, etc.  Upon browsing events "
	    "(discovery, removal, etc.) @var{callback} will be called "
	    "and passed:\n\n"
	    "@itemize\n"
	    "@item the service type browser object;\n"
	    "@item an interface name or number (depending on the OS);\n"
	    "@item the protocol (i.e., one of the @code{protocol/} values);\n"
	    "@item a browser event type (i.e., one of the "
	    "@code{browser-event/} values);\n"
	    "@item the service name;\n"
	    "@item the service type (e.g., @code{\"_http._tcp\"});\n"
	    "@item the domain;\n"
	    "@item lookup result flags (i.e., one of the "
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
  SCM_AVAHI_SET_BROWSER_CALLBACK (browser, callback);
  SCM_AVAHI_SET_BROWSER_CLIENT (browser, client);

  c_browser = avahi_service_browser_new (c_client, c_interface, c_protocol,
					 c_type, c_domain, c_flags,
					 service_browser_trampoline,
					 (void *) browser);
  if (c_browser == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (browser, (scm_t_bits) c_browser);

  return (browser);
}
#undef FUNC_NAME



/* Initialization.  */

void
scm_avahi_lookup_init (void)
{
#include  "lookup.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: f7484a05-8f2a-4ed5-95f6-e865098eb81f
 */
