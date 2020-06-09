#ifndef _WIN32
#include <string.h>
#include <netinet/in.h>
#include <resolv.h>
#include <netdb.h>
#endif

int
set_nameserver(const char *nsname) {
#if defined (__GLIBC__) || defined (_WIN32)
	/* __res_setservers is not implemented in glibc */
	return (-2);
#else
	struct __res_state res;
	union res_sockaddr_union u[MAXNS];
	struct addrinfo *answer = NULL;
	struct addrinfo *cur = NULL;
	struct addrinfo hint;
	int nscount = 0;

	memset(u, 0, sizeof(u));
	memset(&hint, 0, sizeof(hint));
	memset(&res, 0, sizeof(res));
	hint.ai_socktype = SOCK_DGRAM;
	hint.ai_flags = AI_NUMERICHOST;

	if (__res_ninit(&res) == -1)
		return (-1);

	if (getaddrinfo(nsname, NULL, &hint, &answer) == 0) {
		for (cur = answer; cur != NULL; cur = cur->ai_next) {
			if (nscount == MAXNS)
				break;
			switch (cur->ai_addr->sa_family) {
			case AF_INET6:
				u[nscount].sin6 = *(struct sockaddr_in6*)(void *)cur->ai_addr;
				u[nscount++].sin6.sin6_port = htons(53);
				break;
			case AF_INET:
				u[nscount].sin = *(struct sockaddr_in*)(void *)cur->ai_addr;
				u[nscount++].sin.sin_port = htons(53);
				break;
			}
		}
		if (nscount != 0)
			__res_setservers(&res, u, nscount);
		freeaddrinfo(answer);
	}
	if (nscount == 0)
		return (-1);

	_res = res;

	return (0);
#endif
}
