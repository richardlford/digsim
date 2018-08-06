with limit;
with global;
use  global;
package body actuator is
   procedure actuator_data is
      -- Fin actuators model default data
   begin
      fin_limit_dg := 20.0;
   end actuator_data;

   procedure actuator_init is
   begin
      -- Convert from input units to simulation units
      fin_limit := fin_limit_dg/rdtodg;
   end actuator_init;

   procedure actuator_response is
      -- This model simulates four perfect fin actuators, with position limits.
   begin
      -- Perfect actuator response to position commands
      fin_1_position := limit (fin_1_cmd, -fin_limit, fin_limit);
      fin_2_position := limit (fin_2_cmd, -fin_limit, fin_limit);
      fin_3_position := limit (fin_3_cmd, -fin_limit, fin_limit);
      fin_4_position := limit (fin_4_cmd, -fin_limit, fin_limit);
   end actuator_response;
end actuator;
