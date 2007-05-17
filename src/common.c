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

#include "watch.h"

#include "common-enums.h"
#include "common-smobs.h"



/* SMOB and enums type definitions.  */

static inline void
scm_avahi_nop (void *something)
{
  /* Do nothing.  */
}

#include "common-enums.i.c"
#include "common-smobs.i.c"


/* Procedures.  */

#define SCM_AVAHI_SET_POLL_PARENT_POLL(poll, parent_poll) \
  SCM_SET_SMOB_OBJECT_3 (poll, parent_poll)
#define SCM_AVAHI_POLL_PARENT_POLL(poll)	\
  SCM_SMOB_OBJECT_3 (poll)

SCM_SMOB_MARK (scm_tc16_avahi_poll, mark_avahi_poll, poll)
{
  /* This trick allows `AvahiPoll' SMOBs to be kept alive as long as their
     "parent" poll (e.g., an `AvahiGuilePoll' or `AvahiSimplePoll') is alive.
     This is needed because `AvahiPoll's are "owned" (and eventually freed)
     by their parent poll.  */
  return (SCM_AVAHI_POLL_PARENT_POLL (poll));
}

/* Mark the closures associated with GUILE_POLL.  */
SCM_SMOB_MARK (scm_tc16_avahi_guile_poll, mark_avahi_guile_poll, guile_poll)
{
  AvahiGuilePoll *c_guile_poll;

  c_guile_poll = scm_to_avahi_guile_poll (guile_poll, 1, __FUNCTION__);

  scm_gc_mark (c_guile_poll->new_watch);
  scm_gc_mark (c_guile_poll->free_watch);
  scm_gc_mark (c_guile_poll->update_watch_x);
  scm_gc_mark (c_guile_poll->new_timeout);
  scm_gc_mark (c_guile_poll->free_timeout);
  return      (c_guile_poll->update_timeout_x);
}

SCM_DEFINE (scm_avahi_make_guile_poll, "make-guile-poll",
	    6, 0, 0,
	    (SCM new_watch, SCM update_watch_x, SCM free_watch,
	     SCM new_timeout, SCM update_timeout_x, SCM free_timeout),
	    "Return a @code{guile-poll} object that can then be used to "
	    "handle I/O events for Avahi objects such as clients.  All "
	    "arguments should be procedures...")
#define FUNC_NAME s_scm_avahi_make_guile_poll
{
  AvahiGuilePoll *c_guile_poll;

  c_guile_poll = avahi_guile_poll_new (new_watch, update_watch_x, free_watch,
				       new_timeout, update_timeout_x,
				       free_timeout);
  if (!c_guile_poll)
    abort ();

  return (scm_from_avahi_guile_poll (c_guile_poll));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_guile_poll, "guile-poll",
	    1, 0, 0,
	    (SCM guile_poll),
	    "Return the @code{poll} object associated with @var{guile-poll}.")
#define FUNC_NAME s_scm_avahi_guile_poll
{
  SCM poll;
  AvahiGuilePoll *c_guile_poll;
  const AvahiPoll *c_poll;

  c_guile_poll = scm_to_avahi_guile_poll (guile_poll, 1, FUNC_NAME);

  c_poll = avahi_guile_poll_get (c_guile_poll);
  poll = scm_from_avahi_poll ((AvahiPoll *) c_poll);

  SCM_AVAHI_SET_POLL_PARENT_POLL (poll, guile_poll);

  return (poll);
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_make_simple_poll, "make-simple-poll",
	    0, 0, 0,
	    (void),
	    "Return a @code{simple-poll} object.  This is the easiest way "
	    "to handle I/O of Avahi @code{client} objects and similar.")
#define FUNC_NAME s_scm_avahi_make_simple_poll
{
  AvahiSimplePoll *c_simple_poll;

  c_simple_poll = avahi_simple_poll_new ();
  if (!c_simple_poll)
    abort ();

  return (scm_from_avahi_simple_poll (c_simple_poll));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_simple_poll, "simple-poll",
	    1, 0, 0,
	    (SCM simple_poll),
	    "Return the @code{poll} object associated with "
	    "@var{simple-poll}.")
#define FUNC_NAME s_scm_avahi_simple_poll
{
  SCM poll;
  AvahiSimplePoll *c_simple_poll;
  const AvahiPoll *c_poll;

  c_simple_poll = scm_to_avahi_simple_poll (simple_poll, 1, FUNC_NAME);

  c_poll = avahi_simple_poll_get (c_simple_poll);
  poll = scm_from_avahi_poll ((AvahiPoll *) c_poll);

  SCM_AVAHI_SET_POLL_PARENT_POLL (poll, simple_poll);

  return (poll);
}
#undef FUNC_NAME


/* Initialization.  */
void
scm_avahi_common_init (void)
{
#include "common.c.x"

  scm_avahi_define_enums ();
  scm_avahi_init_watch ();
}

/* arch-tag: d880c883-2fe1-49a0-b0c8-cd5a45880cec
 */
