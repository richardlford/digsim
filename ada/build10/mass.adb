with global;
use  global;
package body mass is
   procedure mass_data is
      -- This procedure sets the default data for the missile mass characteristics.
   begin
      ixx_b  :=  0.10;
      iyy_b  :=  0.75;
      izz_b  :=  0.75;
      mass_b := 10.0;
   end mass_data;

   procedure mass_init is
      -- This procedure initializes the variables the missile mass characteristics.
   begin
      null;  -- No initialization required.
   end mass_init;

   procedure mass_response is
      -- This procedure processes the dynamic missile mass characteristics.
   begin
      null;  -- No dynamic processing required.
   end mass_response;
end mass;
