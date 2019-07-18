// strlcpy(3) is meant to be checked for returned value.
//
// Confidence: High
// Copyright: (C) The pkgng project, see COPYING.
// URL: https://github.com/freebsd/pkg/tree/master/tests/cocci/pkg/strlcpy.cocci

@@
expression E, E1, S;
@@

- port_strlcpy(E, E1, S);
+ if (port_strlcpy(E, E1, S) >= S)
+ 	pkg_emit_errno("port_strlcpy", TEXT(S));
