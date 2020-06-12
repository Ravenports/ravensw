all: dns_srv

clean:
	rm dns_srv

dns_srv: dns_srv.adb
	env PATH=${PATH}:/raven/toolchain/gcc9/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv -aI../src

retrieve_int64: retrieve_int64.adb
	env PATH=${PATH}:/raven/toolchain/gcc9/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv -aI../src
