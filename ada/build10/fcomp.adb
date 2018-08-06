with limit;
with define_state;
with global,digsimio;
with ada.numerics.elementary_functions;
use  ada.numerics.elementary_functions;
package body fcomp is
   procedure flight_computer_data is
      -- This procedure sets the default data for the flight computer.
      use global;
   begin
      -- Guidance gains
      roll_guidance_gain  := -20.0;
      pitch_guidance_gain :=   4.0;
      yaw_guidance_gain   :=   4.0;
      -- Autopilot gains
      del_cmd_per_p_cmd   := 0.01;
      del_cmd_per_q_cmd   := 1.0;
      del_cmd_per_r_cmd   := 1.0;
      -- Initial Euler angle estimates
      psi_b_est_dg        := 0.0;
      theta_b_est_dg      := 0.0;
      phi_b_est_dg        := 0.0;
      -- Pitch rate command bias (gravity compensation)
      q_b_cmd_bias := 0.0988;
   end flight_computer_data;

   procedure flight_computer_init is
      -- This procedure sets the initial conditions for the flight computer.
      -- Internal variables and constants:
      cpsio2    : float;  -- Cosines of half Euler angles [Real]
      cthetao2  : float;  -- Cosines of half Euler angles [Real]
      cphio2    : float;  -- Cosines of half Euler angles [Real]
      spsio2    : float;  -- Sines of half Euler angles [Real]
      sthetao2  : float;  -- Sines of half Euler angles [Real]
      sphio2    : float;  -- Sines of half Euler angles [Real]
   begin
      -- Define flight computer states
      declare
         use digsimio;
      begin
         define_state (q0_b_est , q0d_b_est);
         define_state (q1_b_est , q1d_b_est);
         define_state (q2_b_est , q2d_b_est);
         define_state (q3_b_est , q3d_b_est);
      end;
      -- Initial flight computer states
      declare
         use global;
      begin
         -- Trig functions of half Euler angles
         cpsio2   := cos (0.5*psi_b_est_dg  /rdtodg);
         cthetao2 := cos (0.5*theta_b_est_dg/rdtodg);
         cphio2   := cos (0.5*phi_b_est_dg  /rdtodg);
         spsio2   := sin (0.5*psi_b_est_dg  /rdtodg);
         sthetao2 := sin (0.5*theta_b_est_dg/rdtodg);
         sphio2   := sin (0.5*phi_b_est_dg  /rdtodg);

         -- Initial quaternion values
         q0_b_est := cpsio2*cthetao2*cphio2 + spsio2*sthetao2*sphio2;
         q1_b_est := cpsio2*cthetao2*sphio2 + spsio2*sthetao2*cphio2;
         q2_b_est := cpsio2*sthetao2*cphio2 + spsio2*cthetao2*sphio2;
         q3_b_est := spsio2*cthetao2*cphio2 + cpsio2*sthetao2*sphio2;
      end;
   end flight_computer_init;

   procedure flight_computer_response is
      -- This procedure provides a continuous time emulation of the MLRS/TGSM
      -- flight computer processing.
      use global;
      -- Internal variables and constants:
      del_p_cmd    : float;          -- Commanded equivalent fin deflections [rad]
      del_q_cmd    : float;          -- Commanded equivalent fin deflections [rad]
      del_r_cmd    : float;          -- Commanded equivalent fin deflections [rad]
      qmag_est     : float;          -- Magnitude of estimated quaternion [Real]
      stheta_b_est : float;          -- Sine of estimated pitch Euler angle
      tib11_est,tib12_est,tib13_est, -- Estimated elements of the ICS to BCS transformation matrix [Real]
        tib21_est,tib22_est,tib23_est,
        tib31_est,tib32_est,tib33_est : float;
   begin
      -- Navigation section

      -- Evaluate the ICS to BCS transformation matrix
      tib11_est := q0_b_est**2 + q1_b_est**2 - q2_b_est**2 - q3_b_est**2;
      tib12_est := 2.0e0*(q1_b_est*q2_b_est + q0_b_est*q3_b_est);
      tib13_est := 2.0e0*(q1_b_est*q3_b_est - q0_b_est*q2_b_est);
      tib21_est := 2.0e0*(q1_b_est*q2_b_est - q0_b_est*q3_b_est);
      tib22_est := q0_b_est**2 + q2_b_est**2 - q1_b_est**2 - q3_b_est**2;
      tib23_est := 2.0e0*(q2_b_est*q3_b_est + q0_b_est*q1_b_est);
      tib31_est := 2.0e0*(q1_b_est*q3_b_est + q0_b_est*q2_b_est);
      tib32_est := 2.0e0*(q2_b_est*q3_b_est - q0_b_est*q1_b_est);
      tib33_est := q0_b_est**2 + q3_b_est**2 - q1_b_est**2 - q2_b_est**2;
      -- Evaluate Euler roll angle
      stheta_b_est := limit (-tib13_est, -1.0, 1.0);
      psi_b_est    := arctan (tib12_est, tib11_est);
      theta_b_est  := arcsin (stheta_b_est);
      phi_b_est    := arctan (tib23_est, tib33_est);
      -- Quaternion constraint equation
      qmag_est := sqrt (q0_b_est*q0_b_est + q1_b_est*q1_b_est + q2_b_est*q2_b_est + q3_b_est*q3_b_est);
      q0_b_est := q0_b_est/qmag_est;
      q1_b_est := q1_b_est/qmag_est;
      q2_b_est := q2_b_est/qmag_est;
      q3_b_est := q3_b_est/qmag_est;
      -- Quaternion derivative
      q0d_b_est := -0.5e0*(q1_b_est*p_g_meas + q2_b_est*q_g_meas + q3_b_est*r_g_meas);
      q1d_b_est :=  0.5e0*(q0_b_est*p_g_meas + q2_b_est*r_g_meas - q3_b_est*q_g_meas);
      q2d_b_est :=  0.5e0*(q0_b_est*q_g_meas + q3_b_est*p_g_meas - q1_b_est*r_g_meas);
      q3d_b_est :=  0.5e0*(q0_b_est*r_g_meas + q1_b_est*q_g_meas - q2_b_est*p_g_meas);
      -- Maintain zero roll angle
      p_b_cmd := roll_guidance_gain*phi_b_est;
      -- Compute yaw & pitch rate command via proportional navigation
      q_b_cmd := pitch_guidance_gain*q_si_b_meas + q_b_cmd_bias*cos(theta_b_est);
      r_b_cmd := yaw_guidance_gain*r_si_b_meas;
      -- Autopilot
      del_p_cmd := del_cmd_per_p_cmd*p_b_cmd;
      del_q_cmd := del_cmd_per_q_cmd*q_b_cmd;
      del_r_cmd := del_cmd_per_r_cmd*r_b_cmd;
      -- Fin mixing
      fin_1_cmd := -del_q_cmd - del_p_cmd;
      fin_2_cmd := -del_r_cmd - del_p_cmd;
      fin_3_cmd :=  del_q_cmd - del_p_cmd;
      fin_4_cmd :=  del_r_cmd - del_p_cmd;
   end flight_computer_response;
end fcomp;
