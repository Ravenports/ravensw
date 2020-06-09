all: dns_srv

clean:
	rm dns_srv

dns_srv:
	env PATH=${PATH}:/raven/toolchain/gcc9/bin \
	gnatmake -g -fstack-check dns_srv -aI../src/resolv -aI../src
