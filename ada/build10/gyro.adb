with global;
use  global;
package body gyro is

   procedure gyro_data is
      -- Angular rate sensors model default data
   begin
      null;  -- No default data required.
   end gyro_data;

   procedure gyro_init is
      -- Angular rate sensors model initialization
   begin
      null;  -- No initialization required.
   end gyro_init;

   procedure gyro_response is
      -- This model simulates signals from three axis of ideal rate gyros.
   begin
      -- Attitude rate signals
      p_g_meas := p_b;
      q_g_meas := q_b;
      r_g_meas := r_b;
   end gyro_response;
end gyro;
