# Ravensw 2.0 build system (bmake)
# ================================
# Prior to running, extract sqlite to vendor/sqlite3
#

.if defined(RAVENADM)
THISPATH=	${PATH}
.else
THISPATH=	/raven/toolchain/gcc10/bin:/raven/toolchain/bin:${PATH}
.endif

all:
	${MAKE} -C ${.CURDIR}/extlib/custom_sqlite
	${MAKE} -C ${.CURDIR}/extlib/blake3
	#env PATH=${THISPATH} gprbuild -p -P ravensw

clean:
	${MAKE} -C ${.CURDIR}/extlib/custom_sqlite clean
	${MAKE} -C ${.CURDIR}/extlib/blake3 clean
