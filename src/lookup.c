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

#include "lookup-smobs.i.c"
#include "lookup-enums.i.c"


/* Procedures.  */

#define SCM_AVAHI_SET_DOMAIN_BROWSER_CALLBACK(group, callback)	\
  SCM_SET_SMOB_OBJECT_2 (group, callback)
#define SCM_AVAHI_DOMAIN_BROWSER_CALLBACK(group)	\
  SCM_SMOB_OBJECT_2 (group)
#define SCM_AVAHI_SET_DOMAIN_BROWSER_CLIENT(group, client)	\
  SCM_SET_SMOB_OBJECT_3 (group, client)
#define SCM_AVAHI_DOMAIN_BROWSER_CLIENT(group)	\
  SCM_SMOB_OBJECT_3 (group)


/* Mark the client and closure associated with GROUP.  */
SCM_SMOB_MARK (scm_tc16_avahi_domain_browser, mark_domain_browser, browser)
{
  scm_gc_mark (SCM_AVAHI_DOMAIN_BROWSER_CLIENT (browser));

  return (SCM_AVAHI_DOMAIN_BROWSER_CALLBACK (browser));
}

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

  browser = SCM_PACK ((scm_t_bits) data);
  callback = SCM_AVAHI_DOMAIN_BROWSER_CALLBACK (browser);

  (void) scm_apply (callback,
		    scm_list_n (browser,
				scm_from_avahi_interface_index (c_interface),
				scm_from_avahi_protocol (c_protocol),
				scm_from_avahi_browser_event (c_event),
				scm_from_locale_string (c_domain),
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
                scm_avahi_to_c_string (domain, c_domain, 4, FUNC_NAME);
  c_type      = scm_to_avahi_domain_browser_type (domain_browser_type,
						  5, FUNC_NAME);
  c_flags     = scm_to_avahi_lookup_flags (lookup_flags, 6, FUNC_NAME);
  SCM_VALIDATE_PROC (7, callback);


  /* We have to create the SMOB first so that we can pass it as "user data"
     to `avahi_domain_browser_new ()'.  Thus, we need to set it afterwards.  */
  browser = scm_from_avahi_domain_browser (NULL);
  SCM_AVAHI_SET_DOMAIN_BROWSER_CALLBACK (browser, callback);
  SCM_AVAHI_SET_DOMAIN_BROWSER_CLIENT (browser, client);

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


/* Initialization.  */

void
scm_avahi_lookup_init (void)
{
#include  "lookup.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: f7484a05-8f2a-4ed5-95f6-e865098eb81f
 */
