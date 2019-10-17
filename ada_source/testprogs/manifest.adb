--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Text_IO;
with Core.Pkg;                 use Core.Pkg;
with Core.Printf;
with Core.Manifest;

procedure manifest
is

   package CM  renames Core.Manifest;
   package CP  renames Core.Printf;
   package TIO renames Ada.Text_IO;

   procedure test_require (A, B : String);

   manifest0 : constant String :=
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

   P : aliased T_pkg;

   procedure test_require (A, B : String) is
   begin
      if A /= B then
         TIO.Put_Line ("Equality test failure: " & A & " /= " & B);
      end if;
   end test_require;

begin

   if CM.pkg_parse_manifest (P'Unchecked_Access, manifest0) /= EPKG_OK then
      TIO.Put_Line ("Failed to parse manifest0");
      return;
   end if;

   test_require (CP.format_attribute (P, CP.PKG_NAME),    "foobar");
   test_require (CP.format_attribute (P, CP.PKG_VERSION), "0.3");
   test_require (CP.format_attribute (P, CP.PKG_ORIGIN),  "foo/bar");
   test_require (CP.format_attribute (P, CP.PKG_COMMENT), "A dummy manifest");
   test_require (CP.format_attribute (P, CP.PKG_ARCH),    "amd64");
   test_require (CP.format_attribute (P, CP.PKG_WWW),     "http://www.foobar.com");
   test_require (CP.format_attribute (P, CP.PKG_PREFIX),  "/opt/prefix");
   test_require (CP.format_attribute (P, CP.PKG_MAINTAINER),  "test@pkgng.lan");
   test_require (CP.format_attribute (P, CP.PKG_DESCRIPTION), "port description");
   test_require (CP.format_attribute (P, CP.PKG_MSG_ALWAYS),  "pkg message");



end manifest;
