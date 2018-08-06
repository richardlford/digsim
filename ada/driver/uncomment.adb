with ada.characters.handling, ada.strings.maps,ada.strings.fixed;
use  ada.characters.handling, ada.strings.maps,ada.strings.fixed;
with global;
function uncomment(card : string) return string is
   commentchar : character := '#';
   first       : positive;
   last        : natural;
begin
   if card'length = 0 then
      return "";
   elsif card(card'first) = commentchar then
      return "";
   else
      find_token(source => card,
                 set    => to_set(commentchar),
                 test   => ada.strings.outside,
                 first  => first,
                 last   => last);
      if first = last then
         return "";
      else
         return card(card'first .. last);
      end if;
   end if;
end uncomment;
