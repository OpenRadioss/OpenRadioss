//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you: 
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <signal.h>

#ifndef _WIN64
#include <errno.h>
#include <execinfo.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#define _FCALL

void    ARRET(int * n);
void    arret_(int * n);
void    arret_c(int n);


#ifdef _WIN64
#define trace_cf TRACE_CF
#else
#define trace_cf trace_cf_
#endif


void    setignorecore (int *on);
void    ignoreCore (int s);
void    trace_cf(int *s,int *iw);

#ifdef _WIN64
void    ignoreCore (int s)
{
  int iw = 1, is;
  if(s == SIGFPE ) is=2;
  else if(s == SIGSEGV) is=3;
	
  trace_cf(&is,&iw);

}

#else

void    ignoreCore (int s)
{
  int iw = 1, is;
  if     (s == SIGBUS ) is=1;
  else if(s == SIGFPE ) is=2;
  else if(s == SIGSEGV) is=3;
	
  trace_cf(&is,&iw);
/*  setIgnoreCore (1);*/
}

#endif

#ifndef _WIN64
static volatile sig_atomic_t signal_handler_active = 0;
static int exhaustive_signal_diagnostics = 0;
static char diagnostic_rank[32] = "unknown";

static const char *signal_name(int sig)
{
  switch (sig) {
#ifdef SIGINT
    case SIGINT: return "SIGINT";
#endif
#ifdef SIGABRT
    case SIGABRT: return "SIGABRT";
#endif
#ifdef SIGTERM
    case SIGTERM: return "SIGTERM";
#endif
#ifdef SIGBUS
    case SIGBUS: return "SIGBUS";
#endif
#ifdef SIGFPE
    case SIGFPE: return "SIGFPE";
#endif
#ifdef SIGSEGV
    case SIGSEGV: return "SIGSEGV";
#endif
#ifdef SIGILL
    case SIGILL: return "SIGILL";
#endif
    default: return "UNKNOWN";
  }
}

/* This diagnostic path intentionally uses direct file-descriptor output so
 * the evidence survives stdio buffering and a following MPI_Abort.  The
 * backtrace helpers are diagnostic best-effort routines rather than strictly
 * async-signal-safe; they are enabled only for explicitly instrumented runs. */
static void write_signal_diagnostics(int sig, siginfo_t *information)
{
  char message[1024];
  void *frames[96];
  int frame_count = 0;
  int saved_errno = errno;
  int length;
  int file_descriptor;
  pid_t process_id = getpid();
  pid_t sender_pid = information != NULL ? information->si_pid : (pid_t)-1;
  uid_t sender_uid = information != NULL ? information->si_uid : (uid_t)-1;
  int signal_code = information != NULL ? information->si_code : 0;
  int signal_errno = information != NULL ? information->si_errno : 0;
  void *signal_address = information != NULL ? information->si_addr : NULL;

  length = snprintf(
      message, sizeof(message),
      "\nOpenRadioss signal diagnostic: signal=%d (%s), code=%d, "
      "signal_errno=%d, sender_pid=%ld, sender_uid=%ld, address=%p, "
      "process_pid=%ld, mpi_rank=%s, interrupted_errno=%d\n",
      sig, signal_name(sig), signal_code, signal_errno, (long)sender_pid,
      (long)sender_uid, signal_address, (long)process_id, diagnostic_rank,
      saved_errno);
  if (length > 0) {
    size_t bytes = (size_t)length < sizeof(message)
                       ? (size_t)length
                       : sizeof(message) - 1;
    (void)write(STDERR_FILENO, message, bytes);
  }

  frame_count = backtrace(frames, (int)(sizeof(frames) / sizeof(frames[0])));
  if (frame_count > 0) {
    static const char header[] = "OpenRadioss native signal backtrace:\n";
    (void)write(STDERR_FILENO, header, sizeof(header) - 1);
    backtrace_symbols_fd(frames, frame_count, STDERR_FILENO);
  }

  file_descriptor = open("openradioss_signal_diagnostics.log",
                         O_WRONLY | O_CREAT | O_APPEND, 0644);
  if (file_descriptor >= 0) {
    if (length > 0) {
      size_t bytes = (size_t)length < sizeof(message)
                         ? (size_t)length
                         : sizeof(message) - 1;
      (void)write(file_descriptor, message, bytes);
    }
    if (frame_count > 0) {
      static const char header[] = "OpenRadioss native signal backtrace:\n";
      (void)write(file_descriptor, header, sizeof(header) - 1);
      backtrace_symbols_fd(frames, frame_count, file_descriptor);
    }
    (void)close(file_descriptor);
  }
  errno = saved_errno;
}

static void user_abrt_detailed(int sig, siginfo_t *information, void *context)
{
  int val=6;         /* arret_c(6) initiates STOP with 3 as error code (system error) */
  int fatal_signal = 0;
  (void)context;
  if (signal_handler_active) _exit(128 + sig);
  signal_handler_active = 1;
  write_signal_diagnostics(sig, information);

  /* Preserve the original fatal signal and a core image in exhaustive mode
   * instead of converting every failure into the generic Radioss stop path. */
#ifdef SIGABRT
  fatal_signal = fatal_signal || sig == SIGABRT;
#endif
#ifdef SIGBUS
  fatal_signal = fatal_signal || sig == SIGBUS;
#endif
#ifdef SIGFPE
  fatal_signal = fatal_signal || sig == SIGFPE;
#endif
#ifdef SIGSEGV
  fatal_signal = fatal_signal || sig == SIGSEGV;
#endif
#ifdef SIGILL
  fatal_signal = fatal_signal || sig == SIGILL;
#endif
  if (fatal_signal && exhaustive_signal_diagnostics) {
    struct sigaction default_action;
    sigset_t unblocked;
    memset(&default_action, 0, sizeof(default_action));
    default_action.sa_handler = SIG_DFL;
    sigemptyset(&default_action.sa_mask);
    (void)sigaction(sig, &default_action, NULL);
    sigemptyset(&unblocked);
    sigaddset(&unblocked, sig);
    (void)sigprocmask(SIG_UNBLOCK, &unblocked, NULL);
    (void)kill(getpid(), sig);
    _exit(128 + sig);
  }

  printf("\n\nUser or system abort detected (%s) !\n\n", signal_name(sig));
  fflush(stdout);
  arret_c(val);
}
#endif

void user_abrt(int sig)
{
 int val=6;         /* arret_c(6) initiates STOP with 3 as error code (system error) */
#ifndef _WIN64
 write_signal_diagnostics(sig, NULL);
 printf("\n\nUser or system abort detected (%s) !\n\n", signal_name(sig));
#else
 printf("\n\nUser or system abort detected (signal %d) !\n\n", sig);
#endif
 fflush(stdout);
 arret_c(val);
}


/************************/
/* get signals for core */
void _FCALL SETIGNORECORE (int *on)
{setignorecore (on);}
void setignorecore__ (int *on)
{setignorecore (on);}
void setignorecore_ (int *on)
{setignorecore (on);}

void    setignorecore (int *on)
{
#ifndef _WIN64
  const char *diagnostics = getenv("OPENRADIOSS_SIGNAL_DIAGNOSTICS");
  const char *rank = getenv("OMPI_COMM_WORLD_RANK");
  struct sigaction user_action;

  exhaustive_signal_diagnostics =
      diagnostics != NULL && diagnostics[0] != '\0' &&
      strcmp(diagnostics, "0") != 0;
  if (rank != NULL && rank[0] != '\0') {
    strncpy(diagnostic_rank, rank, sizeof(diagnostic_rank) - 1);
    diagnostic_rank[sizeof(diagnostic_rank) - 1] = '\0';
  }
#endif
  if (*on) {			/*ignoring core */
#ifdef SIGBUS
    signal (SIGBUS, ignoreCore);  /* 7 bus error*/
#endif
#ifdef SIGFPE
    signal (SIGFPE, ignoreCore);  /* 8 i/0 */
#endif
#ifdef SIGSEGV
    signal (SIGSEGV, ignoreCore);  /* 11 segmentation violation*/
#endif
  }
  else {			/* acknowledging core */
#ifdef SIGBUS
    signal (SIGBUS, SIG_DFL);  /* 7 bus error*/
#endif
#ifdef SIGFPE
    signal (SIGFPE, SIG_DFL);  /* 8 i/0 */
#endif
#ifdef SIGSEGV
    signal (SIGSEGV, SIG_DFL);  /* 11 segmentation violation*/
#endif
  }

#ifndef _WIN64
  memset(&user_action, 0, sizeof(user_action));
  user_action.sa_sigaction = user_abrt_detailed;
  sigemptyset(&user_action.sa_mask);
  user_action.sa_flags = SA_SIGINFO | SA_RESTART;
#ifdef SIGINT
  sigaction(SIGINT, &user_action, NULL);  /* 2 Interrupt from keyboard*/
#endif
#ifdef SIGABRT
  sigaction(SIGABRT, &user_action, NULL); /* 6 Abnormal termination*/
#endif
  if (exhaustive_signal_diagnostics) {
#ifdef SIGBUS
    sigaction(SIGBUS, &user_action, NULL);
#endif
#ifdef SIGFPE
    sigaction(SIGFPE, &user_action, NULL);
#endif
#ifdef SIGSEGV
    sigaction(SIGSEGV, &user_action, NULL);
#endif
#ifdef SIGILL
    sigaction(SIGILL, &user_action, NULL);
#endif
  }
#else
#ifdef SIGINT
signal (SIGINT,user_abrt);  /* 2 Interrupt from keyboard*/
#endif
#ifdef SIGABRT
signal (SIGABRT,user_abrt);  /* 6 Abnormal termination*/
#endif
#endif

#ifdef SIGBREAK
signal (SIGBREAK,user_abrt);  /* Ctrl-Break signal*/
#endif

#ifdef SIGTERM
#ifndef _WIN64
sigaction(SIGTERM, &user_action, NULL); /* 15 Termination signal*/
#else
signal (SIGTERM,user_abrt);  /* 15 Termination signal*/
#endif
#endif
}
