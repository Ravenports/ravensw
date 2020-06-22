--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Directories;
with System;

with Core.Event;
with Ucl;

package body Core.Repo.Keys is

   package DIR renames Ada.Directories;

   --------------------------------------------------------------------
   --  parse_fingerprint
   --------------------------------------------------------------------
   function parse_fingerprint (obj : access libucl.ucl_object_t) return A_fingerprint
   is
      result : A_fingerprint;
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
   begin
      result.hash_type := HASH_UNKNOWN;
      loop
         item := Ucl.ucl_object_iterate (obj, iter'Access, True);
         exit when item = null;

         declare
            key : String := Ucl.ucl_object_key (item);
         begin
            if Ucl.type_is_string (item) then
               if key = "function" then
                  declare
                     hash_function : constant String := Ucl.ucl_object_tostring (item);
                  begin
                     if hash_function = "sha256" then
                        result.hash_type := HASH_SHA256;
                     else
                        Event.emit_error ("Unsupported hashing function: " & hash_function);
                     end if;
                  end;
               elsif key = "fingerprint" then
                  result.hash := SUS (Ucl.ucl_object_tostring (item));
               end if;
            end if;
         end;
      end loop;

      --  Failure means either hash_type := HASH_UNKNOWN or hash is blank
      return result;
   end parse_fingerprint;


   --------------------------------------------------------------------
   --  load_fingerprint
   --------------------------------------------------------------------
   function load_fingerprint (folder : String; filename : String) return A_fingerprint
   is
      full_path  : constant String := folder & "/" & filename;
      open_flags : Unix.T_Open_Flags;
      fd         : Unix.File_Descriptor;
      result     : A_fingerprint;
      parser     : Ucl.T_parser;
      onward     : Boolean := True;
   begin
      open_flags.RDONLY := True;
      fd := Unix.open_file (full_path, open_flags);
      if not Unix.file_connected (fd) then
         Event.emit_with_strerror ("cannot load fingerprints from " & full_path);
         result.hash_type := HASH_UNKNOWN;
         result.hash := SUS ("hash-error");
         return result;
      end if;

      parser := Ucl.ucl_parser_new_nofilevars;
      if not Ucl.ucl_parser_add_fd (parser, fd) then
         Event.emit_error ("cannot parse fingerprints: " & Ucl.ucl_parser_get_error (parser));
         onward := False;
      end if;

      if onward then
         declare
            obj : access libucl.ucl_object_t;
         begin
            obj := Ucl.ucl_parser_get_object (parser);

            if Ucl.type_is_object (obj) then
               result := parse_fingerprint (obj);
            end if;
            libucl.ucl_object_unref (obj);
         end;
      end if;

      libucl.ucl_parser_free (parser);
      if not Unix.close_file (fd) then
         Event.emit_error ("failed to close file descriptor of " & full_path);
      end if;
      return result;
   end load_fingerprint;


   --------------------------------------------------------------------
   --  load_fingerprints
   --------------------------------------------------------------------
   function load_fingerprints (my_repo : Repo_Cursor) return Action_Result is
   begin
      if load_fingerprints_by_type (my_repo, trusted) /= RESULT_OK then
         Event.emit_error ("Error loading trusted certificates");
         return RESULT_FATAL;
      end if;

      if count_of_trusted_fingerprints (my_repo) = 0 then
         Event.emit_error ("No trusted certificates");
         return RESULT_FATAL;
      end if;

      if load_fingerprints_by_type (my_repo, revoked) /= RESULT_OK then
         Event.emit_error ("Error loading revoked certificates");
         return RESULT_FATAL;
      end if;
      return RESULT_OK;
   end load_fingerprints;


   --------------------------------------------------------------------
   --  load_fingerprints_by_type
   --------------------------------------------------------------------
   function load_fingerprints_by_type
     (my_repo  : Repo_Cursor;
      validity : Cert_Validity)
      return Action_Result
   is
      function get_path return String;
      function get_path return String is
      begin
         case validity is
            when revoked => return repo_fingerprints (my_repo) & "/revoked";
            when trusted => return repo_fingerprints (my_repo) & "/trusted";
         end case;
      end get_path;

      path : constant String := get_path;
   begin
      if not DIR.Exists (path) then
         case validity is
            when revoked => return RESULT_OK;
            when trusted => return RESULT_FATAL;
         end case;
      end if;
      case DIR.Kind (path) is
         when DIR.Directory => null;
         when others => return RESULT_FATAL;
      end case;

      declare
         Inner_Search : DIR.Search_Type;
         Inner_Dirent : DIR.Directory_Entry_Type;
      begin
         DIR.Start_Search (Search    => Inner_Search,
                           Directory => path,
                           Filter    => (DIR.Ordinary_File => True, others => False),
                           Pattern   => "*");

         while DIR.More_Entries (Inner_Search) loop
            DIR.Get_Next_Entry (Search => Inner_Search, Directory_Entry => Inner_Dirent);
            declare
               procedure add_trusted_key (Key : text; Element : in out A_repo);
               procedure add_revoked_key (Key : text; Element : in out A_repo);

               dsn : constant String := DIR.Simple_Name (Inner_Dirent);
               fingerprint : A_fingerprint;

               procedure add_trusted_key (Key : text; Element : in out A_repo) is
               begin
                  Element.trusted_fprint.Append (fingerprint);
               end add_trusted_key;

               procedure add_revoked_key (Key : text; Element : in out A_repo) is
               begin
                  Element.revoked_fprint.Append (fingerprint);
               end add_revoked_key;
            begin
               if dsn /= "." and then dsn /= ".." then
                  fingerprint := load_fingerprint (path, dsn);
                  if fingerprint.hash_type = HASH_UNKNOWN or else
                    IsBlank (fingerprint.hash)
                  then
                     null;
                  else
                     case validity is
                        when trusted =>
                           repositories.Update_Element (my_repo.position, add_trusted_key'Access);
                        when revoked =>
                           repositories.Update_Element (my_repo.position, add_revoked_key'Access);
                     end case;
                  end if;
               end if;
            end;
         end loop;
         DIR.End_Search (Inner_Search);
      end;
      return RESULT_OK;

   end load_fingerprints_by_type;


   --------------------------------------------------------------------
   --  extract_public_key
   --------------------------------------------------------------------
   function extract_public_key (metafd : Unix.File_Descriptor;
                                name   : String;
                                rc     : out Action_Result) return Text
   is
      parser : Ucl.T_parser;
      top    : access libucl.ucl_object_t;
      pubkey : Text;
   begin
      parser := Ucl.ucl_parser_new_nofilevars;
      if not Ucl.ucl_parser_add_fd (parser, metafd) then
         Event.emit_error ("cannot parse fingerprints: " & Ucl.ucl_parser_get_error (parser));
         libucl.ucl_parser_free (parser);
         rc := RESULT_FATAL;
         return blank;
      end if;

      top := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      --  Now search for the required key
      declare
         obj  : access constant libucl.ucl_object_t;
         item : access constant libucl.ucl_object_t;
         iter : aliased libucl.ucl_object_iter_t;
      begin
         obj := Ucl.ucl_object_find_key (top, "cert");
         if not Ucl.type_is_object (obj) then
            Event.emit_error ("cannot find key for signature " & name & " in meta");
            libucl.ucl_object_unref (top);
            rc := RESULT_FATAL;
            return blank;
         end if;

         iter := libucl.ucl_object_iter_t (System.Null_Address);
         loop
            item := Ucl.ucl_object_iterate (obj, iter'Access, True);
            exit when item = null;

            declare
               elt : access constant libucl.ucl_object_t;
               dat : access constant libucl.ucl_object_t;
            begin
               elt := Ucl.ucl_object_find_key (item, "name");
               if Ucl.type_is_string (elt) then
                  if Ucl.ucl_object_tostring (elt) = name then
                     dat := Ucl.ucl_object_find_key (item, "data");
                     if Ucl.type_is_string (dat) then
                        pubkey := SUS (Ucl.ucl_object_tostring (dat));
                        exit;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end;

      libucl.ucl_object_unref (top);

      --  don't close metafd
      rc := RESULT_OK;
      return pubkey;

   end extract_public_key;

end Core.Repo.Keys;
