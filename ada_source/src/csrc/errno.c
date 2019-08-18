/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
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
