/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 */

#include <fcntl.h>

int
try_open(const char *path, int flag_wronly, int flag_nonblock)
{
  int flags = 0;
  if (flag_wronly)
    flags |= O_WRONLY;
#ifdef O_NONBLOCK
  if (flag_nonblock)
    flags |= O_NONBLOCK;
#endif

  return (open (path, flags));
}
