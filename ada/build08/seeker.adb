with global;
use  global;
package body seeker is
   procedure seeker_data is
      -- This subroutine sets the default data for the seeker model.
   begin
      null;
   end seeker_data;

   procedure seeker_init is
     -- This subroutine initializes the variables for the seeker model.
   begin
      null;
   end seeker_init;

   procedure seeker_response is
      -- This subroutine models a perfect one-axis seeker/tracker.
   begin
      -- Target relative position and velocity in ICS
      x_bt_i  := x_bi_i  - x_ti_i;
      z_bt_i  := z_bi_i  - z_ti_i;
      xd_bt_i := xd_bi_i - xd_ti_i;
      zd_bt_i := zd_bi_i - zd_ti_i;
      -- Calculate true LOS rate
      q_s := (z_bt_i*xd_bt_i - x_bt_i*zd_bt_i)/(x_bt_i*x_bt_i + z_bt_i*z_bt_i);
      -- Calculate measured L08 rate
      q_s_meas := q_s;
   end seeker_response;
end seeker;
