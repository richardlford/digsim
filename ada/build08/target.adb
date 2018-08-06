with global;
use  global;
package body target is
   procedure target_data is
   begin
      x_ti_i_ic := 0.0;
      z_ti_i_ic := 0.0;
      xd_ti_i_ic := 0.0;
      zd_ti_i_ic := 0.0;
   end target_data;

   procedure target_init is
   begin
      x_ti_i  := x_ti_i_ic;
      z_ti_i  := z_ti_i_ic;
      xd_ti_i := xd_ti_i_ic;
      zd_ti_i := zd_ti_i_ic;
   end target_init;

   procedure target_response is
   begin
      -- Calculate current target position
      x_ti_i := x_ti_i_ic + xd_ti_i*time;
      z_ti_i := z_ti_i_ic + zd_ti_i*time;
   end target_response;
end target;

