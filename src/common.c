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
#include "errors.h"

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
	    (SCM new_watch, SCM update_watch, SCM free_watch,
	     SCM new_timeout, SCM update_timeout, SCM free_timeout),
	    "Return a @code{guile-poll} object that can then be used to "
	    "handle I/O events for Avahi objects such as clients.  All "
	    "arguments should be procedures:\n\n"
	    "@itemize\n"
	    "@item @var{new-watch} and @var{new-timeout} are invoked "
	    "when the poll-using code requires a new file descriptor "
	    "to be watched after, or a new timeout to be honored, "
	    "respectively.  @var{new-watch} is passed a @code{watch} "
	    "object and a list of @code{watch-event} values; "
	    "@var{new-timeout} is passed a @var{timeout} object and "
	    "a number of seconds and nanoseconds representing the absolute "
	    "date when the timeout expires, or @code{#f} if the newly "
	    "created timeout is disabled.\n"
	    "@item @var{update-watch} and @var{update-timeout} are called "
	    "to modify a previously created watch or timeout.  "
	    "@var{update-watch} is passed the @code{watch} object and a "
	    "new list of events; @var{update-timeout} is passed a new "
	    "expiration time or @code{#f}.\n"
	    "@item Finally, @var{free-watch} and @var{free-timeout} are "
	    "called when the poll is asked to to no longer look handle "
	    "them.  For instance, when @var{free-watch} is called, the "
	    "event loop code may remove the associated file descriptor "
	    "from the list of descriptors passed to @code{select}.\n"
	    "@end itemize\n\n"
	    "The Guile-Avahi distribution comes with a detailed example.")
#define FUNC_NAME s_scm_avahi_make_guile_poll
{
  AvahiGuilePoll *c_guile_poll;

  c_guile_poll = avahi_guile_poll_new (new_watch, update_watch, free_watch,
				       new_timeout, update_timeout,
				       free_timeout);
  if (!c_guile_poll)
    scm_avahi_error (AVAHI_ERR_NO_MEMORY, FUNC_NAME);

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
    scm_avahi_error (AVAHI_ERR_NO_MEMORY, FUNC_NAME);

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

struct iterate_args
{
  AvahiSimplePoll *c_simple_poll;
  int              c_sleep_time;
};

static void *
do_iterate (void *data)
{
  int err;
  struct iterate_args *args;

  args = (struct iterate_args *) data;
  err = avahi_simple_poll_iterate (args->c_simple_poll,
				   args->c_sleep_time);

  return ((void *) err);
}

static void *
do_loop (void *data)
{
  int err;

  err = avahi_simple_poll_loop ((AvahiSimplePoll *) data);

  return ((void *) err);
}

SCM_DEFINE (scm_avahi_iterate_simple_poll, "iterate-simple-poll",
	    1, 1, 0,
	    (SCM simple_poll, SCM sleep_time),
	    "Handle events registered by @var{simple-poll}.  If "
	    "@var{sleep-time} is not specified, the function blocks "
	    "until an I/O event occurs.  If @var{sleep-time} is specified, "
	    "it is the maximum number of milliseconds of blocking.  Return "
	    "@code{#f} is a quit request has been scheduled, @code{#t} "
	    "otherwise.")
#define FUNC_NAME s_scm_avahi_iterate_simple_poll
{
  int err, c_sleep_time;
  AvahiSimplePoll *c_simple_poll;
  struct iterate_args args;
  SCM result;

  c_simple_poll = scm_to_avahi_simple_poll (simple_poll, 1, FUNC_NAME);
  if (sleep_time != SCM_UNDEFINED)
    c_sleep_time = (int) scm_to_uint (sleep_time);
  else
    c_sleep_time = -1;

  args.c_simple_poll = c_simple_poll;
  args.c_sleep_time  = c_sleep_time;
  err = (int) scm_without_guile (do_iterate, &args);

  if (err == 0)
    result = SCM_BOOL_T;
  else if (err > 0)
    result = SCM_BOOL_F;
  else
    /* XXX: How to get a more meaningful error code? */
    scm_avahi_error (AVAHI_ERR_FAILURE, FUNC_NAME);

  return (result);
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_run_simple_poll, "run-simple-poll",
	    1, 0, 0,
	    (SCM simple_poll),
	    "Run the event loop of @var{simple-poll} until either an "
	    "error occurs or a quit request is scheduled.  In the former "
	    "case, an error is raised; in the latter, @code{#f} is "
	    "returned.")
#define FUNC_NAME s_scm_avahi_run_simple_poll
{
  int err;
  AvahiSimplePoll *c_simple_poll;
  SCM result;

  c_simple_poll = scm_to_avahi_simple_poll (simple_poll, 1, FUNC_NAME);

  err = (int) scm_without_guile (do_loop, c_simple_poll);
  if (err == 0)
    result = SCM_BOOL_T;
  else if (err > 0)
    result = SCM_BOOL_F;
  else
    scm_avahi_error (AVAHI_ERR_FAILURE, FUNC_NAME);

  return (result);
}
#undef FUNC_NAME


/* Initialization.  */
void
scm_avahi_common_init (void)
{
#include "common.c.x"

  scm_avahi_define_enums ();
  scm_avahi_init_watch ();
  scm_avahi_init_error ();
}

/* arch-tag: d880c883-2fe1-49a0-b0c8-cd5a45880cec
 */
