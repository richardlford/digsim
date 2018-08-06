with limit;
with define_state;
with global,digsimio;
with ada.numerics.elementary_functions;
use  ada.numerics.elementary_functions;
package body kinematics is
   procedure kinematics_data is
      use global;
      -- Outputs:
      --   Acc_Gravity   - Acceleration due to gravity [m/sec**2]
      --   Psi_b_IC_dg   - Initial Euler angles [deg]
      --   Theta_b_IC_dg
      --   Phi_b_IC_dg
      --   P_b_IC_dg     - Initial missile rotation velocity [deg/sec]
      --   Q_b_IC_dg
      --   R_b_IC_dg
      --   Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
      --   Yd_bi_i_IC
      --   Zd_bi_i_IC
      --   X_bi_i_IC     - Initial missile position [m]
      --   Y_bi_i_IC
      --   Z_bi_i_IC
   begin
      -- Variables for determining initial attitude
      psi_b_ic_dg := 0.0;
      theta_b_ic_dg := 0.0;
      phi_b_ic_dg := 0.0;
      -- Variables for determining initial angular velocity
      p_b_ic_dg := 0.0;
      q_b_ic_dg := 0.0;
      r_b_ic_dg := 0.0;
      -- Variables for determining initial translational velocity of the BCS w.r.t. the ICS
      xd_bi_i_ic := 100.0;
      yd_bi_i_ic := 0.0;
      zd_bi_i_ic := 0.0;
      -- Variables for determining initial translational position of the BCS w.r.t. the ICS
      x_bi_i_ic := 0.0;
      y_bi_i_ic := 0.0;
      z_bi_i_ic := -100.0;
      -- Acceleration due to gravity
      acc_gravity := 9.88;
   end kinematics_data;

   procedure kinematics_init is
      -- Missile kinematics model initialization; initializes quaternions from
      -- initial missile Euler angles.
      --
      -- Inputs:
      --   Psi_b_IC_dg   - Initial Euler angles [deg]
      --   Theta_b_IC_dg
      --   Phi_b_IC_dg
      --   P_b_IC_dg     - Initial missile rotational velocity [deg/sec]
      --   Q_b_IC_dg
      --   R_b_IC_dg
      --   Xd_bi_i_IC    - Initial missile velocity WRT ICS [m/sec]
      --   Yd_bi_i_IC
      --   Zd_bi_i_IC
      --   X_bi_i_IC     - Initial missile position [m]
      --   Y_bi_i_IC
      --   Z_bi_i_IC
      --
      -- Outputs:
      --   P_b     - Missile inertial angular velocity [rad/sec]
      --   Q_b
      --   R_b
      --   Q0_b    - ICS to BCS quaternion [Real]
      --   Q1_b
      --   Q2_b
      --   Q3_b
      --   X_bi_i  - Missile terminal position WRT ICS [m]
      --   Y_bi_i
      --   Z_bi_i
      --   Xd_bi_i - Missile velocity in ICS [m/sec]
      --   Yd_bi_i
      --   Zd_bi_i
      --
      -- Internal variables and constants:
      cpsio2   : float; -- Cosines of half Euler angles [Real]
      cthetao2 : float;
      cphio2   : float;
      spsio2   : float; -- Sines of half Euler angles [Real]
      sthetao2 : float;
      sphio2   : float;
   begin

      -- Assign state and state derivative pointers
      declare
         use digsimio;
      begin
      define_state(x_bi_i  ,    xd_bi_i );    --0  501
      define_state(xd_bi_i ,   xdd_bi_i );    --1  502
      define_state(y_bi_i  ,    yd_bi_i );    --3  504
      define_state(yd_bi_i ,    ydd_bi_i);    --4  505
      define_state(z_bi_i  ,    zd_bi_i );    --6  507
      define_state(zd_bi_i ,   zdd_bi_i );    --7 508
      define_state(p_b     ,   pd_b     );    --9 510
      define_state(q_b     ,    qd_b    );    --1  512
      define_state(r_b     ,    rd_b    );    --3  514
      define_state(q0_b    ,    q0d_b   );    --8  519
      define_state(q1_b    ,    q1d_b   );    --0  521
      define_state(q2_b    ,    q2d_b   );    --2  523
      define_state(q3_b    ,    q3d_b   );    --4  525
      end;
      declare
         use global;
      begin
         -- Initial Euler angles: (BCS) := [ROLL][PITCH][YAW](ICS)
         psi_b := psi_b_ic_dg/rdtodg;
         theta_b := theta_b_ic_dg/rdtodg;
         phi_b := phi_b_ic_dg/rdtodg;
         -- Initial angular velocity of BCS w.r.t. ICS, expressed in BCS
         p_b := p_b_ic_dg/rdtodg;
         q_b := q_b_ic_dg/rdtodg;
         r_b := r_b_ic_dg/rdtodg;
         -- Initial translational position of the BCS w.r.t. the ICS
         x_bi_i := x_bi_i_ic;
         y_bi_i := y_bi_i_ic;
         z_bi_i := z_bi_i_ic;
         -- Initial translational velocity of the BCS w.r.t. to the ICS
         xd_bi_i := xd_bi_i_ic;
         yd_bi_i := yd_bi_i_ic;
         zd_bi_i := zd_bi_i_ic;
         -- Trig functions of half Euler angles
         cpsio2 := cos(0.5*psi_b);
         cthetao2 := cos(0.5*theta_b);
         cphio2 := cos(0.5*phi_b);
         spsio2 := sin(0.5*psi_b);
         sthetao2 := sin(0.5*theta_b);
         sphio2 := sin(0.5*phi_b);
         -- Initial quaternion values
         q0_b := cpsio2*cthetao2*cphio2 + spsio2*sthetao2*sphio2;
         q1_b := cpsio2*cthetao2*sphio2 - spsio2*sthetao2*cphio2;
         q2_b := cpsio2*sthetao2*cphio2 + spsio2*cthetao2*sphio2;
         q3_b := spsio2*cthetao2*cphio2 - cpsio2*sthetao2*sphio2;
      end;
   end kinematics_init;

   procedure kinematics_response is
      use global;
      -- This module evaluates the translational and rotational derivatives which
      -- describe the missile's motion WRT an earth fixed coordinate system for
      -- a non-rotating earth.
      --
      -- Inputs:
      --   AaeroX_bi_b - Accelerations due to aero in BCS [m/sec**2]
      --   AaeroY_bi_b
      --   AaeroZ_bi_b
      --   Acc_Gravity - Acceleration due to gravity [m/sec**2]
      --   P_b         - Missile inertial angular velocity [rad/sec]
      --   Q_b
      --   R_b
      --   Q0_b        - ICS to BCS quaternion [Real]
      --   Q1_b
      --   Q2_b
      --   Q3_b
      --   Tibij       - ICS to BCS transformation matrix [Real]
      --
      -- Outputs:
      --   Q0_b     - ICS to BCS quaternion [Real]
      --   Q1_b
      --   Q2_b
      --   Q3_b
      --   Qd0_b    - ICS to BCS quaternion derivatives [Real]
      --   Qd1_b
      --   Qd2_b
      --   Qd3_b
      --   Psi_b    - Missile Euler angles [rad]
      --   Theta_b
      --   Phi_b
      --   Xdd_bi_i - Missile acceleration WRT ICS in ICS [m/sec**2]
      --   Ydd_bi_i
      --   Zdd_bi_i
      --
      -- Internal variables and constants:
      --   Qmag     - Magnitude of ICS to BCS quaternion [Real]
      --   RDTODG   - Radians to degrees conversion factor [deg/rad]
      --   STheta_b - Sine of pitch Euler angle [Real]
      --
      qmag     : float;  -- Magnitude of ICS to BCS quaternion [Real]
      stheta_b : float;  -- Sine of pitch Euler angle [Real]
      -- Declare global common and assign variable locations
   begin
      -- Evaluate Euler angles
      stheta_b := limit(-tib13,-1.0,1.0);
      theta_b := arcsin(stheta_b);
      psi_b := arctan(tib12,tib11);
      phi_b := arctan(tib23,tib33);
      -- Missile velocity rate in ICS WRT ICS
      xdd_bi_i := tib11*aaerox_bi_b + tib21*aaeroy_bi_b + tib31*aaeroz_bi_b;
      ydd_bi_i := tib12*aaerox_bi_b + tib22*aaeroy_bi_b + tib32*aaeroz_bi_b;
      zdd_bi_i := tib13*aaerox_bi_b + tib23*aaeroy_bi_b + tib33*aaeroz_bi_b + acc_gravity;
      -- Quaternion constraint equation
      qmag := sqrt(q0_b*q0_b+q1_b*q1_b+q2_b*q2_b+q3_b*q3_b);
      q0_b := q0_b/qmag;
      q1_b := q1_b/qmag;
      q2_b := q2_b/qmag;
      q3_b := q3_b/qmag;
      -- Quaternion derivative
      q0d_b := -0.5*(q1_b*p_b+q2_b*q_b+q3_b*r_b);
      q1d_b := 0.5*(q0_b*p_b+q2_b*r_b-q3_b*q_b);
      q2d_b := 0.5*(q0_b*q_b+q3_b*p_b-q1_b*r_b);
      q3d_b := 0.5*(q0_b*r_b+q1_b*q_b-q2_b*p_b);
   end  kinematics_response;
end  kinematics;
