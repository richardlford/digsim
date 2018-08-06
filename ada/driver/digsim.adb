with advance_states;
with calculate;
with configure;
with doevents;
with initialize;
with print_data;
with reset;
with set_defaults;
with termination;
with global,text_io;
use  global,text_io;
procedure digsim is
   -- DigSim provides an Ada architecture required to simulate continuous systems described by
   -- simultaneous first-order differential equations.
   input  : file_type;                      -- Simulation input file handle
   output : file_type;                      -- Simulation output file handle
begin
   open(input,in_file,"input.dat");
   loop
      reset;
      configure(input,output);              -- Get configuration data and setup user IO
      exit when over;
      set_defaults;                         -- Set the default data for this run
      initialize;                           -- Setup & initialize the states
      calculate;                            -- Determine inital values for state derivatives
      doevents(output);                     -- Process discrete events based on current information
      if recalc then
         calculate;                         -- Recalculate derivatives if so directed by user
         recalc := false;
      end if;
      while not termination loop            -- Advance simulation states until termination
         advance_states;                    -- Advance states to new time
         calculate;                         -- Calculate state derivatives at new time
         doevents(output);                  -- Process discrete events based on current information
         if recalc then
            calculate;                      -- Recalculate derivatives if so directed by user
            recalc:= false;
         end if;
      end loop;
      close(output);                        -- Close output*.dat for the run
   end loop;
   close(input);                            -- Close input.dat for the batch
end digsim;
