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

#include <avahi-client/publish.h>
#include <libguile.h>

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
    avahi_entry_group_free (c_group);
}

#include "publish-smobs.i.c"
#include "publish-enums.i.c"


/* Procedures.  */

#define SCM_AVAHI_SET_ENTRY_GROUP_CALLBACK(group, callback)	\
  SCM_SET_SMOB_OBJECT_2 (group, callback)
#define SCM_AVAHI_ENTRY_GROUP_CALLBACK(group)	\
  SCM_SMOB_OBJECT_2 (group)

/* Mark the closure associated with GROUP.  */
SCM_SMOB_MARK (scm_tc16_avahi_entry_group, mark_entry_group, group)
{
  return (SCM_AVAHI_ENTRY_GROUP_CALLBACK (group));
}

static void
entry_group_trampoline (AvahiEntryGroup *c_group,
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
	    "be passed the group object and the groupe entry's state "
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

  c_group = avahi_entry_group_new (c_client, entry_group_trampoline,
				   (void *) group);
  if (c_group == NULL)
    scm_avahi_error (avahi_client_errno (c_client), FUNC_NAME);

  SCM_SET_SMOB_DATA (group, (scm_t_bits) c_group);

  return (group);
}
#undef FUNC_NAME


/* Initialization.  */

void
scm_avahi_publish_init (void)
{
#include "publish.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: ffe4f98f-ac60-49e3-8848-e85b6db2d0c7
 */
