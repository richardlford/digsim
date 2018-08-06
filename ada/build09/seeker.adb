with global;
use global;

package body seeker is
   procedure seeker_data is
      -- Seeker model default data
   begin
      null;
   end seeker_data;

   procedure seeker_init is
      -- Seeker model initialization
   begin
      null;
   end seeker_init;

   procedure seeker_response is
      -- Perfect seeker/tracker model
      --
      -- Inputs:
      --   Tibij   - ICS to BCS transformation matrix [Real]
      --   X_bi_i  - Missile terminal position WRT ICS [m]
      --   Y_bi_i
      --   Z_bi_i
      --   X_ti_i  - Position of target WRT ICS in ICS [m]
      --   Y_ti_i
      --   Z_ti_i
      --   Xd_bi_i - Missile velocity in ICS [m/sec]
      --   Yd_bi_i
      --   Zd_bi_i
      --   Xd_ti_i - Velocity of target WRT ICS in ICS [m/sec]
      --   Yd_ti_i
      --   Zd_ti_i
      --
      -- Outputs:
      --   P_s         - Inertially referenced LOS rates, expressed in the ICS
      --   Q_s           [rad/sec]
      --   R_s
      --   Q_si_b_Meas - LOS rates expressed in the BCS [rad/sec]
      --   R_si_b_Meas
      --   X_tb_i      - Position of the target w.r.t. the BCS, expressed in the ICS
      --   Y_tb_i        [m]
      --   Z_tb_i
      --   Xd_tb_i     - Velocity of the target w.r.t. the BCS, expressed in the ICS
      --   Yd_tb_i       [m/sec]
      --   Zd_tb_i
   begin
      -- Target relative position and velocity in ICS
      x_tb_i  :=  x_ti_i -  x_bi_i;
      y_tb_i  :=  y_ti_i -  y_bi_i;
      z_tb_i  :=  z_ti_i -  z_bi_i;
      xd_tb_i := xd_ti_i - xd_bi_i;
      yd_tb_i := yd_ti_i - yd_bi_i;
      zd_tb_i := zd_ti_i - zd_bi_i;
      -- Missile/target range, squared & limited
      range_tb_sq := float'max(small, (x_tb_i**2 + y_tb_i**2 + z_tb_i**2));
      -- Beam rotation rate WRT inertial space in ICS
      p_s := (y_tb_i*zd_tb_i - z_tb_i*yd_tb_i)/range_tb_sq;
      q_s := (z_tb_i*xd_tb_i - x_tb_i*zd_tb_i)/range_tb_sq;
      r_s := (x_tb_i*yd_tb_i - y_tb_i*xd_tb_i)/range_tb_sq;
      -- Beam rotation rate WRT inertial space in BCS
      q_si_b_meas := tib21*p_s + tib22*q_s + tib23*r_s;
      r_si_b_meas := tib31*p_s + tib32*q_s + tib33*r_s;
   end  seeker_response;
end seeker;
