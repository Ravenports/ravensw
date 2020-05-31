/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <errno.h>

int
get_errno(void)
{
   return(errno);
}

void
reset_errno(void)
{
    errno = 0;
}

int last_error_ACCES(void)
{
  return (errno == EACCES || errno == EROFS);
}

int last_error_NOENT(void)
{
  return (errno == ENOENT);
}

int last_error_CONNRESET(void)
{
   return (errno == ECONNRESET);
}

int last_error_INTR(void)
{
   return (errno == EINTR);
}

int last_error_AGAIN(void)
{
   return (errno == EAGAIN);
}

void
set_ECONNRESET(void)
{
  errno = ECONNRESET;
}

void
set_ETIMEDOUT(void)
{
  errno = ETIMEDOUT;
}
