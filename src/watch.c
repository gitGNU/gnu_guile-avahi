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

#include <avahi-common/watch.h>

#include <libguile.h>

#include <assert.h>

#include "watch.h"
#include "common-smobs.h"
#include "common-enums.h"
#include "utils.h"
#include "errors.h"


struct AvahiWatch
{
  int                 fd;
  int                 dead;
  AvahiWatchEvent     events;
  AvahiWatchCallback  callback;
  void               *userdata;
  SCM                 watch_smob;
  SCM                 stuff;

  AvahiGuilePoll     *guile_poll;
};

struct AvahiTimeout
{
  int                   dead;
  int                   enabled;
  struct timeval        expiry;
  AvahiTimeoutCallback  callback;
  void                 *userdata;
  SCM                   timeout_smob;
  SCM                   stuff;

  AvahiGuilePoll       *guile_poll;
};



/* Watches.  */

static AvahiWatch *
watch_new (const AvahiPoll *api, int fd, AvahiWatchEvent events,
	   AvahiWatchCallback callback, void *userdata)
{
  AvahiWatch *watch;
  AvahiGuilePoll *guile_poll;

  guile_poll = (AvahiGuilePoll *) api->userdata;

  watch = scm_malloc (sizeof (*watch));
  watch->fd = fd;
  watch->dead = 0;
  watch->events = events;
  watch->callback = callback;
  watch->userdata = userdata;
  watch->guile_poll = guile_poll;
  watch->stuff = SCM_BOOL_F;

  watch->watch_smob = scm_from_avahi_watch (watch);
  watch->watch_smob = scm_gc_protect_object (watch->watch_smob);

  (void) scm_call_3 (guile_poll->new_watch, watch->watch_smob,
		     scm_from_int (fd),
		     scm_from_avahi_watch_events (events));

  return (watch);
}

static void
watch_update (AvahiWatch *watch, AvahiWatchEvent events)
{
  scm_call_4 (watch->guile_poll->update_watch_x,
	      watch->watch_smob, scm_from_int (watch->fd),
	      scm_from_avahi_watch_events (events),
	      watch->stuff);

  watch->events = events;
}

static AvahiWatchEvent
watch_get_events (AvahiWatch *watch)
{
  return (watch->events);
}

static void
watch_free (AvahiWatch *watch)
{
  scm_call_1 (watch->guile_poll->free_watch,
	      watch->watch_smob);

  /* We don't free the C object here.  Instead, it will get freed eventually,
     when its Scheme representative gets GC'd.  */
  scm_gc_unprotect_object (watch->watch_smob);

  watch->fd = -1;
  watch->dead = 1;
}


/* Timeouts.  */


static AvahiTimeout *
timeout_new (const AvahiPoll *api, const struct timeval *tv,
	     AvahiTimeoutCallback callback, void *userdata)
{
  SCM sec, nsec;
  AvahiTimeout *timeout;
  AvahiGuilePoll *guile_poll;

  guile_poll = (AvahiGuilePoll *) api->userdata;

  timeout = scm_malloc (sizeof (*timeout));
  timeout->dead = 0;
  timeout->enabled = (tv != NULL);
  timeout->callback = callback;
  timeout->userdata = userdata;
  timeout->guile_poll = guile_poll;
  timeout->stuff = SCM_BOOL_F;

  timeout->timeout_smob = scm_from_avahi_timeout (timeout);
  timeout->timeout_smob = scm_gc_protect_object (timeout->timeout_smob);

  if (tv != NULL)
    {
      timeout->expiry = *tv;
      sec = scm_from_long (tv->tv_sec);
      nsec = scm_from_long (tv->tv_usec * 1000L);
    }
  else
    sec = nsec = SCM_BOOL_F;

  (void) scm_call_3 (guile_poll->new_timeout,
		     timeout->timeout_smob, sec, nsec);

  return (timeout);
}

static void
timeout_update (AvahiTimeout *timeout, const struct timeval *tv)
{
  SCM sec, nsec;

  if (tv)
    {
      sec = scm_from_long (tv->tv_sec);
      nsec = scm_from_long (tv->tv_usec * 1000L);
    }
  else
    sec = nsec = SCM_BOOL_F;

  (void) scm_call_3 (timeout->guile_poll->update_timeout_x,
		     timeout->timeout_smob, sec, nsec);

  if (tv)
    timeout->enabled = 1, timeout->expiry = *tv;
  else
    timeout->enabled = 0;
}

static void
timeout_free (AvahiTimeout *timeout)
{
  (void) scm_call_1 (timeout->guile_poll->free_timeout,
		     timeout->timeout_smob);

  /* We don't free the C object here.  Instead, it will get freed eventually,
     when its Scheme representative gets GC'd.  */
  scm_gc_unprotect_object (timeout->timeout_smob);

  timeout->dead = 1;
}


/* Scheme procedures.  */

/* Mark the user data associated with WATCH.  */
SCM_SMOB_MARK (scm_tc16_avahi_watch, mark_avahi_watch, watch)
{
  AvahiWatch *c_watch;

  c_watch = scm_to_avahi_watch (watch, 1, __FUNCTION__);

  scm_gc_mark (c_watch->stuff);

  return (c_watch->guile_poll->poll_smob);
}

/* Mark the user data associated with TIMEOUT.  */
SCM_SMOB_MARK (scm_tc16_avahi_timeout, mark_avahi_timeout, timeout)
{
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, __FUNCTION__);

  scm_gc_mark (c_timeout->stuff);

  return (c_timeout->guile_poll->poll_smob);
}


SCM_DEFINE (scm_avahi_invoke_watch, "invoke-watch",
	    2, 0, 0,
	    (SCM watch, SCM events),
	    "Invoke the call-back associated with @var{watch}.  This "
	    "notifies the interested code that the events listed in "
	    "@var{events} (a list of @code{watch-event/} values) occurred "
	    "on the file descriptor associated with @var{watch}.  The "
	    "return value is unspecified.  An @code{error/invalid-object} "
	    "error is raised if @var{watch} is no longer valid.")
#define FUNC_NAME s_scm_avahi_invoke_watch
{
  int c_fd;
  AvahiWatch *c_watch;
  AvahiWatchEvent c_events;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);
  c_events = scm_to_avahi_watch_events (events, 2, FUNC_NAME);

  assert (c_watch);
  c_fd = c_watch->fd;

  if (c_watch->dead)
    scm_avahi_error (AVAHI_ERR_INVALID_OBJECT, FUNC_NAME);
  else
    c_watch->callback (c_watch, c_fd, c_events,
		       c_watch->userdata);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_invoke_timeout, "invoke-timeout",
	    1, 0, 0,
	    (SCM timeout),
	    "Invoke the call-back associated with @var{timeout}.  This "
	    "notifies the interested code that the timeout associated "
	    "with @var{timeout} has been reached.  The return value is "
	    "unspecified.  An @code{error/invalid-object} error is raised "
	    "if @var{timeout} is disabled or is no longer valid.")
#define FUNC_NAME s_scm_avahi_invoke_timeout
{
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, FUNC_NAME);
  assert (c_timeout);
  assert (c_timeout->callback);

  if ((c_timeout->dead) || (!c_timeout->enabled))
    scm_avahi_error (AVAHI_ERR_INVALID_OBJECT, FUNC_NAME);
  else
    c_timeout->callback (c_timeout, c_timeout->userdata);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_watch_fd, "watch-fd",
	    1, 0, 0,
	    (SCM watch),
	    "Return the file descriptor associated with @var{watch}.")
#define FUNC_NAME s_scm_avahi_watch_fd
{
  AvahiWatch *c_watch;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);

  return (scm_from_int (c_watch->fd));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_watch_events, "watch-events",
	    1, 0, 0,
	    (SCM watch),
	    "Return the events of interest (a list of @code{watch-event/} "
	    "values) for @var{watch}.")
#define FUNC_NAME s_scm_avahi_watch_events
{
  AvahiWatch *c_watch;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);

  return (scm_from_avahi_watch_events (c_watch->events));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_watch_user_data, "watch-user-data",
	    1, 0, 0,
	    (SCM watch),
	    "Return the user-specified data associated with @var{watch}.")
#define FUNC_NAME s_scm_avahi_watch_user_data
{
  AvahiWatch *c_watch;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);

  return (c_watch->stuff);
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_set_watch_user_data_x, "set-watch-user-data!",
	    2, 0, 0,
	    (SCM watch, SCM data),
	    "Associated @var{data} (an arbitrary Scheme object) with "
	    "@var{watch}.")
#define FUNC_NAME s_scm_avahi_set_watch_user_data_x
{
  AvahiWatch *c_watch;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);

  c_watch->stuff = data;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_timeout_value, "timeout-value",
	    1, 0, 0,
	    (SCM timeout),
	    "Return the expiration time for @var{timeout} as two values: "
	    "the number of seconds and nanoseconds.  If @var{timeout} "
	    "is disabled, both values are @code{#f}.")
#define FUNC_NAME s_scm_avahi_timeout_value
{
  SCM sec, nsec;
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, FUNC_NAME);

  if (c_timeout->enabled)
    {
      sec = scm_from_long (c_timeout->expiry.tv_sec);
      nsec = scm_from_long (c_timeout->expiry.tv_usec * 1000L);
    }
  else
    sec = nsec = SCM_BOOL_F;

  return (scm_values (scm_list_2 (sec, nsec)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_timeout_user_data, "timeout-user-data",
	    1, 0, 0,
	    (SCM timeout),
	    "Return the user-specified data associated with @var{timeout}.")
#define FUNC_NAME s_scm_avahi_timeout_user_data
{
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, FUNC_NAME);

  return (c_timeout->stuff);
}
#undef FUNC_NAME

SCM_DEFINE (scm_avahi_set_timeout_user_data_x, "set-timeout-user-data!",
	    2, 0, 0,
	    (SCM timeout, SCM data),
	    "Associated @var{data} (an arbitrary Scheme object) with "
	    "@var{timeout}.")
#define FUNC_NAME s_scm_avahi_set_timeout_user_data_x
{
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, FUNC_NAME);

  c_timeout->stuff = data;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Public C API.  */

AvahiGuilePoll *
avahi_guile_poll_new (SCM new_watch, SCM update_watch_x, SCM free_watch,
		      SCM new_timeout, SCM update_timeout_x, SCM free_timeout)
{
  AvahiGuilePoll *guile_poll;

  guile_poll = scm_malloc (sizeof (*guile_poll));

  guile_poll->api.watch_new = watch_new;
  guile_poll->api.watch_update = watch_update;
  guile_poll->api.watch_get_events = watch_get_events;
  guile_poll->api.watch_free = watch_free;

  guile_poll->api.timeout_new = timeout_new;
  guile_poll->api.timeout_update = timeout_update;
  guile_poll->api.timeout_free = timeout_free;

  guile_poll->api.userdata = guile_poll;

  guile_poll->new_watch = new_watch;
  guile_poll->free_watch = free_watch;
  guile_poll->update_watch_x = update_watch_x;
  guile_poll->new_timeout = new_timeout;
  guile_poll->free_timeout = free_timeout;
  guile_poll->update_timeout_x = update_timeout_x;

  guile_poll->poll_smob = SCM_BOOL_F;

  return (guile_poll);
}

const AvahiPoll *
avahi_guile_poll_get (AvahiGuilePoll *guile_poll)
{
  return (&guile_poll->api);
}

void
avahi_guile_poll_free (AvahiGuilePoll *guile_poll)
{
  free (guile_poll);
}


/* Initialization.  */

void
scm_avahi_init_watch (void)
{
#include "watch.c.x"
}
