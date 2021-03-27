# build custom sqlite3 library
# Requires $PREFIX/include/linenoise.h

LIB=		custom_sqlite
SRCS=		sqlite3.c shell.c

PREFIX?=	/raven
SHOBJ_CFLAGS=	-fpic
SHOBJ_LDFLAGS=	-shared
LOCAL_CFLAGS=	-I$(PREFIX)/include \
		-D_HAVE_SQLITE_CONFIG_H \
		-DHAVE_LINENOISE \
		-Wno-unused-variable \
		-Wno-unused-function \
		-Wno-strict-aliasing \
		-Wno-unused-parameter \
		-Wno-cast-function-type \
		-Wno-sign-compare \
		-Wno-empty-body \
		-Wno-implicit-fallthrough \
		-DHAVE_USLEEP=1 \
		-DSQLITE_OMIT_AUTOVACUUM \
		-DSQLITE_OMIT_BLOB_LITERAL \
		-DSQLITE_OMIT_DECLTYPE \
		-DSQLITE_OMIT_EXPLAIN \
		-DSQLITE_OMIT_DEPRECATED \
		-DSQLITE_OMIT_LOAD_EXTENSION \
		-DSQLITE_OMIT_PROGRESS_CALLBACK \
		-DSQLITE_OMIT_TCL_VARIABLE \
		-DSQLITE_OMIT_UTF16 \
		-DSQLITE_OMIT_CAT \
		-DSQLITE_OMIT_CHECK \
		-DSQLITE_OMIT_AUTOINIT \
		-DSQLITE_OMIT_COMPILEOPTION_DIAGS \
		-DSQLITE_OMIT_INTEGRITY_CHECK \
		-DSQLITE_OMIT_BUILTIN_TEST \
		-DSQLITE_OMIT_SHARED_CACHE \
		-DSQLITE_ENABLE_UNLOCK_NOTIFY=1 \
		-DUSE_PREAD \
		-DUSE_PWRITE \
		-DSQLITE_THREADSAFE=0 \
		-DSQLITE_TEMP_STORE=3 \
		-DSQLITE_ENABLE_FTS4 \
		-DSQLITE_SHELL_DBNAME_PROC=pkgshell_open \
		-DNDEBUG
CFLAGS?=	-O2 -pipe
CFLAGS+=	${CPPFLAGS}
SHOBJS?=	${SRCS:.c=.pico}
CC?=		gcc

.SUFFIXES: .pico

.c.pico:
	${CC} -Wall -Wextra -o $@ -c $(CFLAGS) $(LOCAL_CFLAGS) $(SHOBJ_CFLAGS) $<

all: lib${LIB}_pic.a

sqlite3.pico: sqlite3.c sqlite3.h

sqlite3.c: ${.CURDIR}/../../vendor/sqlite3/sqlite3.c
	cp ${.ALLSRC} ${.TARGET}

sqlite3.h: ${.CURDIR}/../../vendor/sqlite3/sqlite3.h
	cp ${.ALLSRC} ${.TARGET}

shell.c: ${.CURDIR}/../../vendor/sqlite3/shell.c
	sed -e 's|SQLITE_CDECL main|SQLITE_CDECL sqlite3_shell|'\
		${.ALLSRC} > ${.TARGET}

lib${LIB}_pic.a: ${SHOBJS}
	ar cr ${.TARGET} ${SHOBJS}
	ranlib ${.TARGET}

clean:
	rm -f sqlite3.[ch] shell.c lib${LIB}_pic.a *.pico
