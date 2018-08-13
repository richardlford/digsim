with default_data;
with initialization;
with differential_equations;
with termination_conditions;
with advance_states;
procedure digsim is
   -- DigSim provides an Ada architecture required to simulate continuous systems
   -- described by sets of simultaneous first-order differential equations.
begin
  default_data;                          -- Set the default data for this run
  initialization;                        -- Setup & initialize the states
  differential_equations;                -- Determine the inital value for the state derivatives
  while not termination_conditions loop  -- Advance the simulation states until termination
     advance_states;                     -- Advance states to new time
     differential_equations;             -- Calculate state derivatives at new time
  end loop;
end digsim;
