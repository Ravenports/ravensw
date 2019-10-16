--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;

procedure manifest
is

   manifest : constant String :=
     "name: foobar" & LF &
     "version: 0.3" & LF &
     "origin: foo/bar" & LF &
     "categories: [foo, bar]" & LF &
     "comment: A dummy manifest" & LF &
     "arch: amd64" & LF &
     "www: http://www.foobar.com" & LF &
     "maintainer: test@pkgng.lan" & LF &
     "flatsize: 10000" & LF &
     "deps:" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}" & LF &
     "  depbar: {origin: dep/bar, version: 3.4}" & LF &
     "hello: world" & LF &  -- unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "prefix: /opt/prefix" & LF &
     "desc: |" & LF &
     "  port description" & LF &
     "message: |" & LF &
     "  pkg message" & LF &
     "options:" & LF &
     "  foo: true" & LF &
     "  bar: false" & LF &
     "files:" & LF &
     "  /usr/local/bin/foo: 01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b" & LF;

   --  Name empty
   wrong_manifest1 : constant String :=
     "name:" & LF &
     "version: 0.3" & LF &
     "origin: foo/bar" & LF &
     "comment: A dummy manifest" & LF &
     "arch: amd64" & LF &
     "www: http://www.foobar.com" & LF &
     "maintainer: test@pkgng.lan" & LF &
     "flatsize: 10000" & LF &
     "deps:" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}" & LF &
     "  depbar: {origin: dep/bar, version: 3.4}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "options:" & LF &
     "  foo: true" & LF &
     "  bar: false" & LF &
     "files:" & LF &
     "  /usr/local/bin/foo: 01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b" & LF;

   --  bad dependency line
   wrong_manifest2 : constant String :=
     "name: foobar" & LF &
     "version: 0.3" & LF &
     "origin: foo/bar" & LF &
     "comment: A dummy manifest" & LF &
     "arch: amd64" & LF &
     "www: http://www.foobar.com" & LF &
     "maintainer: test@pkgng.lan" & LF &
     "flatsize: 10000" & LF &
     "deps:" & LF &
     "  depfoo: {origin: dep/foo}" & LF &
     "  depbar: {origin: dep/bar, version: 3.4}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "options:" & LF &
     "  foo: true" & LF &
     "  bar: false" & LF &
     "files:" & LF &
     "  /usr/local/bin/foo: 01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b" & LF;

   --  bad conflict line
   wrong_manifest3 : constant String :=
     "name: foobar" & LF &
     "version: 0.3" & LF &
     "origin: foo/bar" & LF &
     "comment: A dummy manifest" & LF &
     "arch: amd64" & LF &
     "www: http://www.foobar.com" & LF &
     "maintainer: test@pkgng.lan" & LF &
     "flatsize: 10000" & LF &
     "deps:" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}" & LF &
     "  depbar: {origin: dep/bar, version: 3.4}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: []" & LF &
     "options:" & LF &
     "  foo: true" & LF &
     "  bar: false" & LF &
     "files:" & LF &
     "  /usr/local/bin/foo: 01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b" & LF;

   --  bad option line
   wrong_manifest4 : constant String :=
     "name: foobar" & LF &
     "version: 0.3" & LF &
     "origin: foo/bar" & LF &
     "comment: A dummy manifest" & LF &
     "arch: amd64" & LF &
     "www: http://www.foobar.com" & LF &
     "maintainer: test@pkgng.lan" & LF &
     "flatsize: 10000" & LF &
     "deps:" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}" & LF &
     "  depbar: {origin: dep/bar, version: 3.4}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "options:" & LF &
     "  foo:" & LF &
     "  bar: false" & LF &
     "files:" & LF &
     "  /usr/local/bin/foo: 01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b" & LF;

begin

end manifest;
