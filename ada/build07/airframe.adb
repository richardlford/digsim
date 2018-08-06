with global;
use  global;
package body airframe is
   procedure airframe_response_data is
      -- This subroutine sets the default data for determining the airframe response
      -- to a rate command.
   begin
      null;
   end airframe_response_data;

   procedure airframe_response_init is
   begin
      -- This subroutine initializes the variables for determining the airframe
      -- response to a rate command.
      null;
   end airframe_response_init;

   procedure airframe_response is
      -- This subroutine determines the airframe response to a rate command.
   begin
      thetadot_b := thetadot_b_cmd; -- Calculate airframe rate
   end airframe_response;

end airframe;
