# Ravensw 2.0 build system (bmake)
# ================================
# Prior to running, extract sqlite to vendor/sqlite3
#

.if defined(RAVENADM)
THISPATH=	${PATH}
.else
THISPATH=	/raven/toolchain/ravensys-gcc/bin:/raven/toolchain/bin:${PATH}
.endif

all:
	${MAKE} -C ${.CURDIR}/extlib/custom_sqlite
	env PATH=${THISPATH} gprbuild -p -P ${.CURDIR}/ravensw

clean:
	${MAKE} -C ${.CURDIR}/extlib/custom_sqlite clean
