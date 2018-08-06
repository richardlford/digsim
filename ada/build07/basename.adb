with ada.strings.fixed;
use  ada.strings.fixed;
function basename(batch,runno : natural) return string is
begin
   if batch = 0 and runno = 0 then
      return "output";
   else
      return "output-" & trim(integer'image(batch),ada.strings.left)
        & "."  & trim(integer'image(runno),ada.strings.left);
   end if;
end basename;
