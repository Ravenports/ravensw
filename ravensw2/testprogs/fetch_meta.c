#include <stdio.h>
#include <fetch.h>

int
main () 
{
    const char my_url[]  = "http://www.ravenports.com/repository/dragonfly:5.8:x86:64/meta.tzst";
    const char my_opts[] = "iv";
    
    struct url		*u = NULL;
    struct url_stat	st;
    estream_t		remote = NULL;
    char		buf[8192];
    size_t		buflen = 0;
    off_t		r;
    
    u = fetchParseURL(my_url);
    if (u == NULL) {
        printf ("failed to parse %s\n", my_url);
        return -1;
    }
    
    remote = fetchXGet(u, &st, my_opts);
    if (remote == NULL) {
        printf ("failed to reach resource.\n");
        return -1;
    } else {
        printf ("succeeded to reach resource\n");
        printf ("size is %d\n", st.size);
    }
    
    buflen = sizeof(buf);
    while ((r = FXFREAD(buf, 1, buflen, remote)) > 0) {
       printf ("fetched %d chars\n", r);
    }
    fetchFreeURL (u);
    printf ("done\n");
    
    return 0;
}
