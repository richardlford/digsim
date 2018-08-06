with global;
use  global;
package body gyro is
   procedure gyro_data is
   begin
      -- Angular rate sensors model default data
      null;
   end gyro_data;

   procedure gyro_init is
      -- Angular rate sensors model initialization
   begin
      null;
   end gyro_init;

   procedure gyro_response is
      -- This model simulates signals from three axis of ideal rate gyros.
      --
      -- Inputs:
      --   P_b - Missile inertial angular velocity [rad/sec]
      --   Q_b
      --   R_b
      --
      -- Outputs:
      --   P_g_Meas  - Measured angular velocity [rad/sec]
      --   R_g_Meas
      --   Q_g_Meas
   begin
      -- Attitude rate signals
      p_g_meas := p_b;
      q_g_meas := q_b;
      r_g_meas := r_b;
   end  gyro_response;
end gyro;
