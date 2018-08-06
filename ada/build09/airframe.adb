with limit;
with global,ada.numerics.elementary_functions;
use  global,ada.numerics.elementary_functions;
with ada.numerics;
use  ada.numerics;
package body airframe is
   procedure airframe_data is
   begin
      -- Aero coefficients
      drag_per_velsq := 0.00021;
      -- Time constant for roll channel; natural frequency of stabilized airframe
      tau_p   := 0.02;
      freq0_q := 8.0;
      freq0_r := 8.0;
      -- Damping ratios for pitch and yaw channels
      zeta_r := 0.5;
      zeta_q := 0.5;
      -- Minimum turning radii for x-y and x-z planes
      rmin_xy := 200.0;
      rmin_xz := 200.0;
      -- Aerodynamic reference angles-of-attack
      alpha_ref_dg := 15.0;
      beta_ref_dg  := 15.0;
   end airframe_data;

   procedure airframe_init is
   begin
      --  Convert from input units to simulation units
      alpha_ref := alpha_ref_dg/rdtodg;
      beta_ref  := beta_ref_dg /rdtodg;
      omega0_q  := 2.0*pi*freq0_q;
      omega0_r  := 2.0*pi*freq0_r;
   end airframe_init;

   procedure airframe_response is
      -- This model determines the rate of change of the angular and translational
      -- velocities due to aerodynamic forces resulting from acceleration commands.
      -- Transfer functions approximate the behavior of a stable
      -- aerodynamics/autopilot combination. This version models the aerodynamic
      -- drag as a function of total velocity.
      acc_alpha      : float;  -- Pitch acceleration due to alpha [m/sec**2]
      acc_beta       : float;  -- Yaw acceleration due to beta [m/sec**2]
      acc_per_alpha  : float;  -- Pitch acceleration per rad of alpha [m/sec**2/rad]
      acc_per_beta   : float;  -- Yaw acceleration per rad of beta [m/sec**2/rad]
      alpha_cmd      : float;  -- Commanded alpha [rad]
      adrag          : float;  -- X-axis (BCS) deceleration due to drag [m/sec**2]
      beta_cmd       : float;  -- Commanded beta [rad]
      beta_prime     : float;  -- Out-of-plane sideslip angle [rad]
      calpha         : float;  -- Cosine of alpha [Real]
      cbetap         : float;  -- Cosine of beta [Real]
      velsq          : float;  -- The square of Vmag [m**2/sec**2]
      xd_bi_b        : float;  -- Missile velocity WRT Earth in BCS [m/sec]
      yd_bi_b        : float;  -- Missile velocity WRT Earth in BCS [m/sec]
      zd_bi_b        : float;  -- Missile velocity WRT Earth in BCS [m/sec]
      y_acc_max      : float;  -- Yaw acceleration limit [m/sec**2]
      z_acc_max      : float;  -- Pitch acceleration limit [m/sec**2]
      tstb11,tstb12,tstb13,tstb21,tstb22,tstb23,tstb31,tstb32,tstb33 : float;
      aq11,aq21,aq22 : float;
      ar11,ar21,ar22 : float;
      salpha         : float;
      sbetap         : float;
      y_acc_cmd      : float;
      z_acc_cmd      : float;
      br2            : float;
      bq2            : float;
      vmag           : float;
   begin
      -- Evaluate ICS to BCS transformation matrix
      tib11 := q0_b**2 + q1_b**2 - q2_b**2 - q3_b**2;
      tib12 := 2.0*(q1_b*q2_b+q0_b*q3_b);
      tib13 := 2.0*(q1_b*q3_b-q0_b*q2_b);
      tib21 := 2.0*(q1_b*q2_b-q0_b*q3_b);
      tib22 := q0_b**2 + q2_b**2 - q1_b**2 - q3_b**2;
      tib23 := 2.0*(q2_b*q3_b+q0_b*q1_b);
      tib31 := 2.0*(q1_b*q3_b+q0_b*q2_b);
      tib32 := 2.0*(q2_b*q3_b-q0_b*q1_b);
      tib33 := q0_b**2 + q3_b**2 - q1_b**2 - q2_b**2;
      -- Missile velocity WRT ICS origin in BCS;
      xd_bi_b := xd_bi_i*tib11 + yd_bi_i*tib12 + zd_bi_i*tib13;
      yd_bi_b := xd_bi_i*tib21 + yd_bi_i*tib22 + zd_bi_i*tib23;
      zd_bi_b := xd_bi_i*tib31 + yd_bi_i*tib32 + zd_bi_i*tib33;
      velsq   := xd_bi_i**2 + yd_bi_i**2 + zd_bi_i**2;
      vmag    := sqrt(velsq);
      -- Angle of attack and sideslip angle;
      alpha      := arctan(zd_bi_b,xd_bi_b);
      beta       := arctan(yd_bi_b,xd_bi_b);
      beta_prime := arctan(yd_bi_b,sqrt(xd_bi_b**2 + zd_bi_b**2));
      -- Aero angle trig functions;
      calpha := cos(alpha);
      cbetap := cos(beta_prime);
      salpha := sin(alpha);
      sbetap := sin(beta_prime);
      -- Stability axis to body axis transformation matrix;
      tstb11 :=  calpha*cbetap;
      tstb12 := -calpha*sbetap;
      tstb13 := -salpha;
      tstb21 :=  sbetap;
      tstb22 :=  cbetap;
      tstb23 := 0.0;
      tstb31 :=  salpha*cbetap;
      tstb32 := -salpha*sbetap;
      tstb33 :=  calpha;
      -- Translational accelerations due to aero in stability axis;
      adrag         := -velsq*drag_per_velsq;
      z_acc_max     :=  velsq/rmin_xz;
      acc_per_alpha := -float'max(small,z_acc_max/alpha_ref);
      acc_alpha     :=  acc_per_alpha*alpha;
      y_acc_max     :=  velsq/rmin_xy;
      acc_per_beta  := -float'max(small,y_acc_max/beta_ref);
      acc_beta      :=  acc_per_beta*beta;
      -- Translational acceleration due to aero in BCS;
      aaerox_bi_b := tstb11*adrag + tstb12*acc_beta + tstb13*acc_alpha;
      aaeroy_bi_b := tstb21*adrag + tstb22*acc_beta + tstb23*acc_alpha;
      aaeroz_bi_b := tstb31*adrag + tstb32*acc_beta + tstb33*acc_alpha;
      -- Roll response to roll command;
      pd_b := (p_b_cmd-p_b)/tau_p;
      -- Yaw response to a yaw command;
      y_acc_cmd :=  r_b_cmd*vmag;
      beta_cmd  :=  limit(y_acc_cmd,-y_acc_max,y_acc_max)/acc_per_beta;
      br2       := -omega0_r*omega0_r;
      ar11      := -vmag/(beta_ref*rmin_xy);
      ar21      :=  ar11*(ar11-2.0*zeta_r*omega0_r) - br2;
      ar22      :=  ar11 - 2.0*zeta_r*omega0_r;
      rd_b      :=  ar21*beta + ar22*r_b + br2*beta_cmd;
      -- Pitch response to a pitch command;
      z_acc_cmd := -q_b_cmd*vmag;
      alpha_cmd :=  limit(z_acc_cmd,-z_acc_max,z_acc_max)/acc_per_alpha;
      bq2       :=  omega0_q**2;
      aq11      := -vmag/(alpha_ref*rmin_xz);
      aq21      := -(aq11*(aq11-2.0*zeta_q*omega0_q)+bq2);
      aq22      :=  aq11 - 2.0*zeta_q*omega0_q;
      qd_b      :=  aq21*alpha + aq22*q_b + bq2*alpha_cmd;
   end airframe_response;
end airframe;
