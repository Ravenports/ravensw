/*
 * Copyright (c) 2019 John Marino <draco@marino.st>
 */

#ifdef HAVE_CONFIG_H
#include "pkg_config.h"
#endif

#include <stdlib.h>
#if defined(HAVE_PROGRAM_INVOCATION_SHORT_NAME)
#include <errno.h>
#endif
#include "pkg.h.in"

const char*
user_agent(void) {
    static const char *progname;

    if (progname == NULL) {
#ifdef HAVE_GETEXECNAME
       /*  Solaris  */
       const char *e = getexecname();
       if (e != NULL) {
           /* Have to make a copy since getexecname can return a readonly
              string, but basename expects to be able to modify its arg. */
           char *n = strdup(e);
           if (n != NULL) {
               progname = basename(n);
           }
       }
#elif defined(HAVE_PROGRAM_INVOCATION_SHORT_NAME)
       /*  Linux  */
       progname = program_invocation_short_name;
#elif defined(HAVE_GETPROGNAME)
       /*  BSD | APPLE  */
       progname = getprogname();
#else
       progname = PKG_EXEC_NAME;
#endif
    }
    return progname;
}