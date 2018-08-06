with define_state;
with limit;
with ada.numerics.elementary_functions;
use  ada.numerics.elementary_functions;
with global,digsimio;
package body kinematics is
   procedure kinematics_data is
      -- Missile Kinematics model default data
      use global;
   begin
      -- Variables for determining initial attitude
      psi_b_ic_dg   := 0.0;
      theta_b_ic_dg := 0.0;
      phi_b_ic_dg   := 0.0;
      --  Variables for determining initial angular velocity
      p_b_ic_dg := 0.0;
      q_b_ic_dg := 0.0;
      r_b_ic_dg := 0.0;
      -- Variables for determining initial translational velocity of the BCS w.r.t. the ICS
      xd_bi_i_ic := 100.0;
      yd_bi_i_ic :=   0.0;
      zd_bi_i_ic :=   0.0;
      -- Variables for determining initial translational position of the BCS w.r.t. the ICS
      x_bi_i_ic :=    0.0;
      y_bi_i_ic :=    0.0;
      z_bi_i_ic := -100.0;
      -- Acceleration due to gravity
      acc_gravity := 9.88;
   end kinematics_data;

   procedure kinematics_init is
      -- Missile kinematics model initialization; initializes quaternions from
      -- initial missile Euler angles.
      cpsio2   : float;  -- Cosines of half Euler angles [Real]
      cthetao2 : float;  -- Cosines of half Euler angles [Real]
      cphio2   : float;  -- Cosines of half Euler angles [Real]
      spsio2   : float;  -- Sines of half Euler angles [Real]
      sthetao2 : float;  -- Sines of half Euler angles [Real]
      sphio2   : float;  -- Sines of half Euler angles [Real]
   begin
      -- Assign state and state derivative pointers
      declare
         use digsimio;
      begin
         define_state ( x_bi_i  , xd_bi_i  );
         define_state ( xd_bi_i , xdd_bi_i );
         define_state ( y_bi_i  , yd_bi_i  );
         define_state ( yd_bi_i , ydd_bi_i );
         define_state ( z_bi_i  , zd_bi_i  );
         define_state ( zd_bi_i , zdd_bi_i );
         define_state ( p_b     , pd_b     );
         define_state ( q_b     , qd_b     );
         define_state ( r_b     , rd_b     );
         define_state ( q0_b    , q0d_b    );
         define_state ( q1_b    , q1d_b    );
         define_state ( q2_b    , q2d_b    );
         define_state ( q3_b    , q3d_b    );
      end;

      declare
         use global;
      begin
         -- Initial Euler angles: (BCS) := [ROLL] [PITCH] [YAW] (ICS)
         psi_b   := psi_b_ic_dg  /rdtodg;
         theta_b := theta_b_ic_dg/rdtodg;
         phi_b   := phi_b_ic_dg  /rdtodg;
         -- Initial angular velocity of BCS w.r.t. ICS, expressed in BCS
         p_b := p_b_ic_dg/rdtodg;
         q_b := q_b_ic_dg/rdtodg;
         r_b := r_b_ic_dg/rdtodg;
         -- Initial translational position of the BCS w.r.t. the ICS
         x_bi_i := x_bi_i_ic;
         y_bi_i := y_bi_i_ic;
         z_bi_i := z_bi_i_ic;
         -- Initial translational velocity of the BCS w.r.t. the ICS
         xd_bi_i := xd_bi_i_ic;
         yd_bi_i := yd_bi_i_ic;
         zd_bi_i := zd_bi_i_ic;
         -- Trig functions of half Euler angles
         cpsio2   := cos (0.5*psi_b);
         cthetao2 := cos (0.5*theta_b);
         cphio2   := cos (0.5*phi_b);
         spsio2   := sin (0.5*psi_b);
         sthetao2 := sin (0.5*theta_b);
         sphio2   := sin (0.5*phi_b);
         -- Initial quaternion values
         q0_b := cpsio2*cthetao2*cphio2 + spsio2*sthetao2*sphio2;
         q1_b := cpsio2*cthetao2*sphio2 - spsio2*sthetao2*cphio2;
         q2_b := cpsio2*sthetao2*cphio2 + spsio2*cthetao2*sphio2;
         q3_b := spsio2*cthetao2*cphio2 - cpsio2*sthetao2*sphio2;
      end;
   end kinematics_init;

   procedure kinematics_response is
      -- This module evaluates the translational and rotational derivatives
      --  which describe the missile's motion WRT an earth fixed coordinate
      --  system for a non-rotating earth.
      use global;
      qmag     : float;  -- Magnitude of ICS to BCS quaternion [Real]
      stheta_b : float;  -- Sine of pitch Euler angle [Real]
   begin
      --  Evaluate Euler angles
      stheta_b := limit (-tib13, -1.0, 1.0);
      theta_b  := arcsin (stheta_b);
      psi_b    := arctan (tib12, tib11);
      phi_b    := arctan (tib23, tib33);
      -- Missile velocity rate in ICS WRT ICS
      xdd_bi_i := (tib11*faerox_bi_b + tib21*faeroy_bi_b + tib31*faeroz_bi_b)/mass_b;
      ydd_bi_i := (tib12*faerox_bi_b + tib22*faeroy_bi_b + tib32*faeroz_bi_b)/mass_b;
      zdd_bi_i := (tib13*faerox_bi_b + tib23*faeroy_bi_b + tib33*faeroz_bi_b)/mass_b + acc_gravity;
      --  Angular rate derivatives
      pd_b := maerox_bi_b/ixx_b;
      qd_b := (maeroy_bi_b + p_b*r_b*(izz_b - ixx_b))/iyy_b;
      rd_b := (maeroz_bi_b + q_b*p_b*(ixx_b - iyy_b))/izz_b;
      -- Quaternion constraint equation
      qmag := sqrt(q0_b*q0_b + q1_b*q1_b + q2_b*q2_b + q3_b*q3_b);
      q0_b := q0_b/qmag;
      q1_b := q1_b/qmag;
      q2_b := q2_b/qmag;
      q3_b := q3_b/qmag;
      -- Quaternion derivative
      q0d_b := -0.5*(q1_b*p_b + q2_b*q_b + q3_b*r_b);
      q1d_b :=  0.5*(q0_b*p_b + q2_b*r_b - q3_b*q_b);
      q2d_b :=  0.5*(q0_b*q_b + q3_b*p_b - q1_b*r_b);
      q3d_b :=  0.5*(q0_b*r_b + q1_b*q_b - q2_b*p_b);
   end kinematics_response;
end kinematics;
