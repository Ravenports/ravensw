# build blake3 library

.PATH:	${.CURDIR}/../../vendor/blake3

LIB=		blake3
SRCS=		blake3.c blake3_dispatch.c blake3_portable.c
ASSYSRCS=	blake3_sse2_x86-64_unix.S \
		blake3_sse41_x86-64_unix.S \
		blake3_avx2_x86-64_unix.S \
		blake3_avx512_x86-64_unix.S

PREFIX?=	/raven
SHOBJ_CFLAGS=	-fpic
SHOBJ_LDFLAGS=	-shared
CFLAGS?=	-O2 -pipe
CFLAGS+=	${CPPFLAGS}
SHOBJS?=	${SRCS:.c=.pico}
ASSYOBJS?=	${ASSYSRCS:.S=.So}
CC?=		gcc

.SUFFIXES: .pico .So

.c.pico:
	${CC} -Wall -Wextra -o $@ -c $(CFLAGS) $(LOCAL_CFLAGS) $(SHOBJ_CFLAGS) $<

.S.So:
	${CC} -o $@ -c $(CFLAGS) $(LOCAL_SFLAGS) $(SHOBJ_CFLAGS) $<

all: lib${LIB}_pic.a

lib${LIB}_pic.a: ${SHOBJS} ${ASSYOBJS}
	ar cr ${.TARGET} ${SHOBJS} ${ASSYOBJS}
	ranlib ${.TARGET}

clean:
	rm -f lib${LIB}_pic.a *.pico *.So
