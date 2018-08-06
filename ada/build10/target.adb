with global;
use  global;
package body target is
   procedure target_data is
      -- This procedure sets the default data for the target motion model.
   begin
      -- Declare global common and assign variable locations
      x_ti_i_ic   :=  500.0;
      y_ti_i_ic   := -250.0;
      z_ti_i_ic   :=    0.0;
      xd_ti_i_ic  :=  -25.0;
      yd_ti_i_ic  :=   25.0;
      zd_ti_i_ic  :=    0.0;
   end target_data;

   procedure target_init is
      -- This procedure initializes the variables for the target motion model.
   begin
      -- Initialize target states
      x_ti_i  := x_ti_i_ic;
      y_ti_i  := y_ti_i_ic;
      z_ti_i  := z_ti_i_ic;
      xd_ti_i := xd_ti_i_ic;
      yd_ti_i := yd_ti_i_ic;
      zd_ti_i := zd_ti_i_ic;
   end target_init;

   procedure target_response is
      -- This procedure determines the target motion.
   begin
      -- Set target states
      x_ti_i := x_ti_i_ic + xd_ti_i*time;
      y_ti_i := y_ti_i_ic + yd_ti_i*time;
      z_ti_i := z_ti_i_ic + zd_ti_i*time;
   end target_response;
end target;
