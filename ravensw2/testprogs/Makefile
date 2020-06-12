all: usage

usage:
	@echo "usage:"
	@echo "make dns_srv"
	@echo "make retrieve_int64"

clean:
	rm -f dns_srv retrieve_int64
	rm -f *.ali *.o b~*

dns_srv: dns_srv.adb
	env PATH=${PATH}:/raven/toolchain/gcc9/bin \
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
	env PATH=${PATH}:/raven/toolchain/gcc9/bin \
	gnatmake -g -fstack-check ${.TARGET} -aI../src/resolv \
		-aI../src -aI../src/sqlite -aI../src/regex -aI../src/unix \
		-largs ../../external/sqlite/libsqlite_pic.a \
		${unix_objs} sql3int.o
