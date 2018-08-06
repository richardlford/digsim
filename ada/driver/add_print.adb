with framework,digsimio;
use  framework,digsimio;
procedure add_print(name : names; index : out positive) is
   found : boolean := false;
begin
   for i in 1 .. nprints loop
      if print_list(i) = name then
         index := i;
         found := true;
         exit;
      end if;
   end loop;
   if not found then
      nprints := nprints + 1;
      index   := nprints;
      print_list(nprints) := name;
   end if;
end add_print;
