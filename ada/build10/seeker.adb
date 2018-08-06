with global;
use  global;
package body seeker is
   procedure seeker_data is
      -- Seeker model default data
   begin
      null;  -- No default data required.
   end seeker_data;

   procedure seeker_init is
      -- Seeker model initialization
   begin
      null;  -- No initialization required.
   end seeker_init;

   procedure seeker_response is
      -- Perfect seeker/tracker model
   begin
      -- Target relative position and velocity in ICS
      x_tb_i  := x_ti_i  - x_bi_i;
      y_tb_i  := y_ti_i  - y_bi_i;
      z_tb_i  := z_ti_i  - z_bi_i;
      xd_tb_i := xd_ti_i - xd_bi_i;
      yd_tb_i := yd_ti_i - yd_bi_i;
      zd_tb_i := zd_ti_i - zd_bi_i;
      -- Missile/target range, squared & limited
      range_tb_sq := float'max(small, (x_tb_i*x_tb_i + y_tb_i*y_tb_i + z_tb_i*z_tb_i));
      -- Beam rotation rate WRT inertial space in ICS
      p_s := (y_tb_i*zd_tb_i - z_tb_i*yd_tb_i)/range_tb_sq;
      q_s := (z_tb_i*xd_tb_i - x_tb_i*zd_tb_i)/range_tb_sq;
      r_s := (x_tb_i*yd_tb_i - y_tb_i*xd_tb_i)/range_tb_sq;
      -- Beam rotation rate WRT inertial space in BCS
      q_si_b_meas := tib21*p_s + tib22*q_s + tib23*r_s;
      r_si_b_meas := tib31*p_s + tib32*q_s + tib33*r_s;
   end seeker_response;
end seeker;
