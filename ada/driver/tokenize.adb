with uncomment;
with text_io,ada.strings.fixed,ada.strings.maps,ada.integer_text_io,digsimio,global;
use  text_io,ada.strings.fixed,ada.strings.maps,ada.integer_text_io,digsimio;
function tokenize(input : file_type) return string_ptr_list
is
   first       : positive;
   last        : natural;
   index       : natural := 0;
   token       : string_ptr_list(1..5) := (others => new string'(""));
   whitespace  : character_set := to_set( character'val( 9)   -- tab
                                        & character'val(10)   -- line feed
                                        & character'val(13)   -- carriage return
                                        & character'val(32)); -- space
   procedure parse (s : string) is
      cursor : natural := s'first;
   begin
      loop
         find_token(source => s(cursor..s'last),
                    set    => whitespace,
                    test   => ada.strings.outside,
                    first  => first,
                    last   => last);
         exit when last = 0;
         exit when s(first) = '#';  -- comment character
         cursor := last  + 1;
         index  := index + 1;
         token(index) := new string'(s(first..last));
      end loop;
   end parse;
begin
   while not end_of_file(input) loop
      parse(uncomment(get_line(input)));
      exit when index > 0;
   end loop;
   return token;
end tokenize;
