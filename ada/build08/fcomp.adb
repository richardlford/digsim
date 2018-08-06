with global;
use  global;
package body fcomp is
   procedure flight_computer_data is
      -- This subroutine sets the default data for the flight computer.
   begin
      guidance_gain := 3.0;
   end flight_computer_data;

   procedure flight_computer_init is
   begin
      null;
   end flight_computer_init;

   procedure flight_computer_response is
      -- This subroutine determines the commanded airframe rate.
   begin
      q_b_cmd := guidance_gain*q_s_meas; -- Calculate guidance command
   end flight_computer_response;
end fcomp;

