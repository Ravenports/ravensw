/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <fcntl.h>
#include <sys/time.h>
#include <sys/stat.h>

int
set_file_times (int fd, long access_time, long mod_time)
{
  struct timeval ftimes[2] = {
    {
      .tv_sec  = access_time,
      .tv_usec = 0
    },
    {
      .tv_sec  = mod_time,
      .tv_usec = 0
    }
  };
#ifdef _WIN32
  return (-1);
#else
 #if defined (__sun__) && !defined (HAVE_UTIMENSAT)
  return (futimesat (fd, NULL, ftimes));
 #else
  return (utimensat (fd, NULL, ftimes, 0));
 #endif
#endif
}

int
set_file_times2 (const char *path, long access_time, long mod_time)
{
  struct timeval ftimes[2] = {
    {
      .tv_sec  = access_time,
      .tv_usec = 0
    },
    {
      .tv_sec  = mod_time,
      .tv_usec = 0
    }
  };
#ifdef _WIN32
  return (-1);
#else
 #if defined (__sun__) && !defined (HAVE_UTIMENSAT)
  return utimes (path, ftimes);
 #else
  return (utimensat (AT_FDCWD, path, ftimes, 0));
 #endif
#endif
}
