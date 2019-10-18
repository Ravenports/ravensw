--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Text_IO;
with Core.Pkg;                 use Core.Pkg;
with Core.Strings;             use Core.Strings;
with Core.Printf;
with Core.Manifest;
with Core.Event;
with Core.Unix;

use Core;

procedure manifest
is

   package CM  renames Core.Manifest;
   package CP  renames Core.Printf;
   package TIO renames Ada.Text_IO;

   procedure test_require (A, B : String);
   procedure test_require (A, B : Integer);
   procedure test_require_fail (id : String; result : Pkg_Error_Type);
   function event_callback (eventx : Event.pkg_event; data : Text) return Boolean;
   procedure regevent;

   csum : constant String := "01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b";
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
     "deps: {" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}," & LF &
     "  depbar: {origin: dep/bar, version: 3.4}," & LF &
     "}" & LF &
     "hello: world" & LF &  -- unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "prefix: /opt/prefix" & LF &
     "desc: <<EOD" & LF &
     "port description" & LF &
     "EOD" & LF &
     "options: {" & LF &
     "  foo: true," & LF &
     "  bar: false," & LF &
     "}" & LF &
     "files: {" & LF &
     "  /usr/local/bin/foo: " & csum & LF &
     "}" & LF;

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
     "deps: {" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}," & LF &
     "  depbar: {origin: dep/bar, version: 3.4}," & LF &
     "}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "options: {" & LF &
     "  foo: true," & LF &
     "  bar: false," & LF &
     "}" & LF &
     "files: {" & LF &
     "  /usr/local/bin/foo: " & csum & LF &
     "}" & LF;

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
     "deps: {" & LF &
     "  depfoo: {origin: dep/foo}," & LF &
     "  depbar: {origin: dep/bar, version: 3.4}," & LF &
     "}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: [foo-*, bar-*]" & LF &
     "options: {" & LF &
     "  foo: true," & LF &
     "  bar: false," & LF &
     "}" & LF &
     "files: {" & LF &
     "  /usr/local/bin/foo: " & csum & LF &
     "}" & LF;

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
     "deps: {" & LF &
     "  depfoo: {origin: dep/foo, version: 1.2}," & LF &
     "  depbar: {origin: dep/bar, version: 3.4}," & LF &
     "}" & LF &
     "hello: world" & LF & --  unknown keyword should not be a problem
     "conflicts: []" & LF &
     "options: {" & LF &
     "  foo: true," & LF &
     "  bar: false," & LF &
     "}" & LF &
     "files: {" & LF &
     "  /usr/local/bin/foo: " & csum & LF &
     "}" & LF;

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
     "options: {" & LF &
     "  foo," & LF &
     "  bar: false," & LF &
     "}" & LF &
     "files: {" & LF &
     "  /usr/local/bin/foo: " & csum & LF &
     "}" & LF;

   P : aliased T_pkg;
   all_good : Boolean := True;

   procedure test_require (A, B : String) is
   begin
      if A /= B then
         TIO.Put_Line ("Equality test failure: " & A & " /= " & B);
         all_good := False;
      end if;
   end test_require;

   procedure test_require (A, B : Integer) is
   begin
      if A /= B then
         TIO.Put_Line ("Equality test failure: " & A'Img & " /= " & B'Img);
         all_good := False;
      end if;
   end test_require;

   procedure test_require_fail (id : String; result : Pkg_Error_Type) is
   begin
      if result = EPKG_OK then
         TIO.Put_Line ("Unexpected pass on test " & id);
         all_good := False;
      end if;
   end test_require_fail;

   function event_callback (eventx : Event.pkg_event; data : Text) return Boolean
   is
   begin
      case eventx.this_event is

         when Event.PKG_EVENT_ERRNO =>
            TIO.Put_Line
              (TIO.Standard_Error,
               (USS (eventx.err_function) & '(' & USS (eventx.err_argument) & "): " &
                  Unix.strerror (eventx.err_number)));

         when Event.PKG_EVENT_ERROR =>
            TIO.Put_Line (TIO.Standard_Error, (USS (eventx.message)));

         when Event.PKG_EVENT_NOTICE =>
            TIO.Put_Line (USS (eventx.message));

         when Event.PKG_EVENT_DEVELOPER_MODE =>
            TIO.Put_Line (TIO.Standard_Error, ("DEVELOPER_MODE: " & USS (eventx.message)));

         when others => null;
      end case;
      return True;
   end event_callback;

   procedure regevent is
   begin
      Event.pkg_event_register (callback      => event_callback'Unrestricted_Access,
                                callback_data => blank);
   end regevent;
begin

   regevent;

   if CM.pkg_parse_manifest (P'Unchecked_Access, manifest0) /= EPKG_OK then
      TIO.Put_Line ("Failed to parse manifest0");
      return;
   end if;

   test_require (CP.format_attribute (P, CP.PKG_NAME),    "foobar");
   test_require (CP.format_attribute (P, CP.PKG_VERSION), "0.300000");
   test_require (CP.format_attribute (P, CP.PKG_ORIGIN),  "foo/bar");
   test_require (CP.format_attribute (P, CP.PKG_COMMENT), "A dummy manifest");
   test_require (CP.format_attribute (P, CP.PKG_ARCH),    "amd64");
   test_require (CP.format_attribute (P, CP.PKG_WWW),     "http://www.foobar.com");
   test_require (CP.format_attribute (P, CP.PKG_PREFIX),  "/opt/prefix");
   test_require (CP.format_attribute (P, CP.PKG_MAINTAINER),  "test@pkgng.lan");
   test_require (CP.format_attribute (P, CP.PKG_DESCRIPTION), "port description");
   test_require (Integer (P.flatsize), 10000);

   test_require (CP.dependency_count (P), 2);
   test_require (CP.format_dep_attribute (P, "depfoo", CP.DEP_NAME),    "depfoo");
   test_require (CP.format_dep_attribute (P, "depfoo", CP.DEP_ORIGIN),  "dep/foo");
   test_require (CP.format_dep_attribute (P, "depfoo", CP.DEP_VERSION), "1.200000");
   test_require (CP.format_dep_attribute (P, "depbar", CP.DEP_NAME),    "depbar");
   test_require (CP.format_dep_attribute (P, "depbar", CP.DEP_ORIGIN),  "dep/bar");
   test_require (CP.format_dep_attribute (P, "depbar", CP.DEP_VERSION), "3.400000");

   test_require (CP.option_count (P), 2);
   test_require (CP.format_option (P, "foo", CP.OPT_NAME),  "foo");
   test_require (CP.format_option (P, "foo", CP.OPT_VALUE), "on");
   test_require (CP.format_option (P, "bar", CP.OPT_NAME),  "bar");
   test_require (CP.format_option (P, "bar", CP.OPT_VALUE), "off");

   test_require (CP.category_count (P), 2);
   test_require (CP.format_category (P, 1), "foo");
   test_require (CP.format_category (P, 2), "bar");

   test_require (CP.files_count (P), 1);
   test_require (CP.format_file_attribute (P, 1, CP.FILE_PATH), "/usr/local/bin/foo");
   test_require (CP.format_file_attribute (P, 1, CP.FILE_CHECKSUM), csum);

   declare
      PX : aliased T_pkg;
   begin
      test_require_fail ("M1", CM.pkg_parse_manifest (PX'Unchecked_Access, wrong_manifest1));
   end;

   declare
      PX : aliased T_pkg;
   begin
      test_require_fail ("M2", CM.pkg_parse_manifest (PX'Unchecked_Access, wrong_manifest2));
   end;

   declare
      PX : aliased T_pkg;
   begin
      test_require_fail ("M3", CM.pkg_parse_manifest (PX'Unchecked_Access, wrong_manifest3));
   end;

   declare
      PX : aliased T_pkg;
   begin
      test_require_fail ("M4", CM.pkg_parse_manifest (PX'Unchecked_Access, wrong_manifest4));
   end;

   if all_good then
      TIO.Put_Line ("Success!");
   else
      TIO.Put_Line ("one or more tests failed.");
   end if;

end manifest;
