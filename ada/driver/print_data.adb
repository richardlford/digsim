with schedule;
with framework,global,text_io, ada.float_text_io,digsimio;
use  framework,global,text_io, ada.float_text_io;
procedure print_data(output : file_type) is
begin
   -- Print states for this time
   -- put(output,item=>time,fore=>3,aft=>3,exp=>0);
   for n in  1 .. nprints loop
      put(output," ");
      put(output,item => digsimio.cell(print_list(n)).r.all,fore=>2,aft=>5,exp=>3);
   end loop;
   new_line(output);
   schedule((log_print_data,float(integer(time/dtprint) + 1) * dtprint));
end print_data;


