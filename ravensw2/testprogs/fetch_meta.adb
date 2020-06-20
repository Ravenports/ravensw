with Interfaces.C;
with Ada.Text_IO;
with Libfetch;

procedure fetch_meta is

  package TIO renames Ada.Text_IO;

  my_url : constant String := "http://www.ravenports.com/repository/dragonfly:5.8:x86:64/meta.tzst";
  my_opts : constant String := "iv";
  
  url        : Libfetch.URL_Component_Set;
  remote     : Libfetch.Fetch_Stream;
  chars_read : Natural;
  one_char   : constant Positive := 1;
  requested  : constant Positive := 8192;

begin  
  url := Libfetch.parse_url (my_url);
  if not Libfetch.url_is_valid (url) then
     TIO.Put_Line ("failed to parse " & my_url);
     return;
  end if;

  Libfetch.initialize_estreams;
  remote := Libfetch.fx_XGet (url, my_opts);
  if not Libfetch.stream_is_active (remote) then
     TIO.Put_Line ("failed to reach resource");
     return;
  else
     TIO.Put_Line ("succeeded to reach resource");
     TIO.Put_Line ("size is " & Libfetch.get_fetched_file_size (url)'Img);   
  end if;

  TIO.Put_Line (LibFetch.url_scheme (url));
  TIO.Put_Line (LibFetch.url_host (url) & LibFetch.url_port (url)'Img);
  TIO.Put_Line (LibFetch.url_doc (url));
  
  loop
     chars_read := 0;
     declare
        line : String := Libfetch.fx_fread (fstream => remote,
                                            number_chunks => requested,
                                            chunk_size => one_char,
                                            chunks_read => chars_read);
     begin
        if chars_read > 0 then
           TIO.Put_Line ("fetched" & chars_read'Img & " chars");
        end if;
     end;
     exit when chars_read = 0;
  end loop;
  
  Libfetch.free_url (url);  
  
end fetch_meta;