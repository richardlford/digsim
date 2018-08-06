with global; use global;
procedure termination_conditions(quit : in out boolean) is
   -- Quit : Stop simulation run [Boolean]
begin
  quit := time >= tstop;
end termination_conditions;
