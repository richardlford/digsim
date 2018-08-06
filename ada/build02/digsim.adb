with default_data;
with initialization;
with differential_equations;
with termination_conditions;
with advance_states;
with differential_equations;
with termination_conditions;
with cleanup;
procedure digsim is
   -- DigSim provides an Ada architecture required to simulate continuous systems
   -- described by sets of simultaneous first-order differential equations.
   --
   -- Variables:
   --   End Of Run
   --
   -- Exceptions to default type
   end_of_run : boolean := false; -- Flag set by user to stop the s{mulation [Logical]
begin
  default_data;                          -- Set the default data for this run
  initialization;                        -- Setup & initialize the states
  differential_equations;                -- Determine the inital value for the state derivatives
  termination_conditions(end_of_run);    -- Get the initial value for the run termination flag
  while not end_of_run loop              -- Advance the simulation states until termination
     advance_states;                     -- Advance states to new time
     differential_equations;             -- Calculate state derivatives at new time
     termination_conditions(end_of_run); -- Check for end of run
  end loop;
  cleanup;
exception
   when others => cleanup;
end digsim;
