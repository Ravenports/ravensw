/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <fcntl.h>

int
try_open(const char *path,
	 int flag_rdonly,
	 int flag_wronly,
	 int flag_nonblock,
	 int flag_directory,
	 int flag_cloexec,
	 int flag_creat,
         int flag_trunc)
{
  int flags = 0;

  if (flag_rdonly && !flag_wronly)
    flags |= O_RDONLY;

  if (flag_wronly && !flag_rdonly)
    flags |= O_WRONLY;

  if (flag_rdonly && flag_wronly)
    flags |= O_RDWR;

#ifdef O_NONBLOCK
  if (flag_nonblock)
    flags |= O_NONBLOCK;
#endif

#ifdef O_CLOEXEC
  if (flag_cloexec)
    flags |= O_CLOEXEC;
#endif

#ifdef O_DIRECTORY
  if (flag_directory)
    flags |= O_DIRECTORY;
#endif

  if (flag_creat)
    flags |= O_CREAT;

  if (flag_trunc)
    flags |= O_TRUNC;

  if (flag_creat)
    return (open (path, flags, 0644));
  else
    return (open (path, flags));
}

int
try_openat
  (int dirfd,
  const char *path,
  int flag_rdonly,
  int flag_wronly,
  int flag_nonblock,
  int flag_directory,
  int flag_cloexec,
  int flag_creat,
  int flag_trunc)
{
  int flags = 0;

  if (flag_rdonly && !flag_wronly)
    flags |= O_RDONLY;

  if (flag_wronly && !flag_rdonly)
    flags |= O_WRONLY;

  if (flag_rdonly && flag_wronly)
    flags |= O_RDWR;

#ifdef O_NONBLOCK
  if (flag_nonblock)
    flags |= O_NONBLOCK;
#endif

#ifdef O_CLOEXEC
  if (flag_cloexec)
    flags |= O_CLOEXEC;
#endif

#ifdef O_DIRECTORY
  if (flag_directory)
    flags |= O_DIRECTORY;
#endif

  if (flag_creat)
    flags |= O_CREAT;

  if (flag_trunc)
    flags |= O_TRUNC;

#ifndef _WIN32
  if (flag_creat)
    return (openat (dirfd, path, flags, 0644));
  else
    return (openat (dirfd, path, flags));
#else
  return (0);
#endif
}

