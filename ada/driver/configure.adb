with add_print;
with basename;
with plotfile;
with reset;
with tokenize;
with text_io,framework,digsimio,global;
use  text_io,framework,digsimio;
procedure configure ( input  : in     file_type
                    ; output : in out file_type)
is
   -- This procedure reads input data cards; then calls the appropriate
   -- processing routine or performs the processing directly.
   -- Commands:
   --     PRINT - Prints selected variable                   (PRINT <Variable> <Location>)
   --     SET   - Assigns variable for initialization        (SET {Variable} = {Value} {Location})
   --     RUN   - Runs simulation and returns for more input (RUN)
   --     STOP  - Stops the driver                           (STOP)
   -- RUN is optional at the end of the input file.
   command : commands;
   name    : names;
   idnum   : positive;
   last    : natural;
   card    : string_ptr_list(1..5);
   runno   : natural renames global.runno;
   batch   : natural renames global.batch;
   over    : boolean renames global.over;
begin
    loop
      card := tokenize(input);
      if card(1).all = "" then
         if runno = 0 and nprints > 0 then
            create(output,out_file,basename(batch,runno) & ".dat");
            if nplots > 0 then
               plotfile(basename(batch,runno));
            end if;
         elsif runno > 0 and nprints > 0 then
            create(output,out_file,basename(batch,runno) & ".dat");
            if nplots > 0 then
               plotfile(basename(batch,runno));
            end if;
         else
            over := true;
         end if;
         exit;
      end if;
      declare
         token1 : string := card(1).all;
         token2 : string := card(2).all;
         token3 : string := card(3).all;
         token4 : string := card(4).all;
         token5 : string := card(5).all;
         index  : positive;
      begin
         commands_io.get(from => token1,item => command, last => last);
         case command is
            when plot =>
               nplots := nplots + 1;
               -- get and add x axis veriable to plotx list
               names_io.get(from => token2, item => name, last => last);
               idnum  := integer'value(token3);
               add_print(name,index);
               plotx_list(nplots) := index;
               -- get and add y axis veriable to ploty list
               names_io.get(from => token4, item => name, last => last);
               idnum := integer'value(token5);
               add_print(name,index);
               ploty_list(nplots) := index;
            when run =>
               if runno = 0 then
                  runno := 1;
                  batch := 1;
               end if;
               create(output,out_file,basename(batch,runno) & ".dat");
               if nplots > 0 then
                  plotfile(basename(batch,runno));
               end if;
               runno := runno + 1;
            when stop  =>
               runno := 1;
               batch := batch + 1;
            when print =>
               if nprints = 0 then
                  add_print(time,index);
               end if;
               names_io.get(from => token2, item => name, last => last);
               idnum   := integer'value(token3);
               add_print(name,index);
            when set =>
               names_io.get(from => token2,item => name,last => last);
               idnum := integer'value(token3);
               if idnum /= cell(name).numb then
                  raise data_error;
               end if;
               nsets := nsets + 1;
               case cell(name).kind is
                  when real => set_list(nsets) := (real,name,float  'value(token4));
                  when int8 => set_list(nsets) := (int8,name,integer'value(token4));
                  when bool => set_list(nsets) := (bool,name,boolean'value(token4));
               end case;
         end case;
      end;
      exit when command = run;
   end loop;
exception
   when data_error =>
      put_line("############################################################");
      put_line("#################### bad input.dat file ####################");
      put_line("############################################################");
      raise;
end configure;
