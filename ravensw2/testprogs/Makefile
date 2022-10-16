all: usage

usage:
	@echo "usage:"
	@echo "make dns_srv"
	@echo "make retrieve_int64"
	@echo "make check bind"
	@echo "make fetch_meta_c";
	@echo "make fetch_meta";
	@echo "make check_blake3";
#	@echo "make load_pubkey_c";

clean:
	rm -f dns_srv retrieve_int64 check_bind fetch_meta_c fetch_meta check_blake3
	rm -f load_pubkey_c
	rm -f *.ali *.o b~*

CFLAGS=	-I/raven/include
SSLCFLAGS= -I/raven/libressl/include

dns_srv: dns_srv.adb
	env PATH=${PATH}:/raven/toolchain/ravensys-gcc/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv -aI../src

unix_objs=	errno.o fileat.o socket.o mode.o file_times.o open.o dprint.o stat_ops.o

errno.o: ../src/unix/errno.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

fileat.o: ../src/unix/fileat.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

socket.o: ../src/unix/socket.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

mode.o: ../src/unix/mode.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

file_times.o: ../src/unix/file_times.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

open.o: ../src/unix/open.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

dprint.o: ../src/unix/dprint.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

stat_ops.o: ../src/unix/stat_ops.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

sql3int.o: ../src/sqlite/sqlite3_interface.c
	${CC} -c ${CFLAGS} -I../../external/sqlite ${.ALLSRC} -o ${.TARGET}
	
retrieve_int64: retrieve_int64.adb ${unix_objs} sql3int.o
	env PATH=${PATH}:/raven/toolchain/ravensys-gcc/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv \
		-aI../src -aI../src/sqlite -aI../src/regex -aI../src/unix \
		-largs ../../external/sqlite/libsqlite_pic.a \
		${unix_objs} sql3int.o

check_bind: check_bind.adb ${unix_objs} sql3int.o
	env PATH=${PATH}:/raven/toolchain/ravensys-gcc/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv \
		-aI../src -aI../src/sqlite -aI../src/regex -aI../src/unix \
		-largs ../../external/sqlite/libsqlite_pic.a \
		${unix_objs} sql3int.o

fetch_meta_c: fetch_meta.c
	${CC} ${CFLAGS} -o ${.TARGET} ${.ALLSRC} \
		/raven/lib/libfetch_pic.a \
		/raven/libressl/lib/libssl_pic.a \
		/raven/libressl/lib/libcrypto_pic.a

fetch_meta: fetch_meta.adb ${unix_objs}
	env PATH=${PATH}:/raven/toolchain/ravensys-gcc/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/fetch -aI../src \
		-aI../src/unix -largs ${unix_objs} \
		/raven/lib/libfetch_pic.a \
		/raven/libressl/lib/libssl_pic.a \
		/raven/libressl/lib/libcrypto_pic.a

check_blake3: check_blake3.adb
	env PATH=${PATH}:/raven/toolchain/ravensys-gcc/bin \
	gnatmake -g -fstack-check ${.TARGET} \
		-aI../src/blake3 -aI../src \
		-largs /raven/lib/libblake3_pic.a

# load_pubkey_c: load_pubkey.c
#	${CC} ${CFLAGS} ${SSLCFLAGS} -o ${.TARGET} ${.ALLSRC} \
#		/raven/libressl/lib/libssl_pic.a \
#		/raven/libressl/lib/libcrypto_pic.a
