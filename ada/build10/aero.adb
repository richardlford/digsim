with ada.numerics.elementary_functions,global;
use  ada.numerics.elementary_functions,global;
package body aero is
   procedure aerodynamics_data is
      -- Aerodynamics model default data
   begin
      -- Local atmospheric density
      air_density := 1.0;
      -- Base axial drag coefficient
      cx_base := 0.3;
      -- Force coefficients per aerodynamic angles
      cx_per_alpha_total :=  -0.5;
      cy_per_beta        := -50.0;
      cz_per_alpha       := -50.0;
      -- Moment coefficients per aerodynamic angles
      cmy_per_alpha := -50.0;
      cmz_per_beta  :=  50.0;
      -- Moment coefficients per effective fin deflections
      cmx_per_delp := 10.0;
      cmy_per_delq := 40.0;
      cmz_per_delr := 40.0;
      --  Damping coefficients
      cmp :=  -20.0;
      cmq := -200.0;
      cmr := -200.0;
      -- Aerodynamic reference values
      ref_area   := 0.01;
      ref_length := 1.0;
   end aerodynamics_data;

   procedure aerodynamics_init is
      -- Aerodynamics model initialization
   begin
      null;  -- No initialization required.
   end aerodynamics_init;

   procedure aerodynamics_response is
      -- This model determines the aerodynamic forces and moments acting on
      -- the missile.
      -- Internal variables and constants:
      del_eff_p     : float; -- Effective fin deflections [rad]
      del_eff_q     : float; -- Effective fin deflections [rad]
      del_eff_r     : float; -- Effective fin deflections [rad]
      lrefo2vmag    : float; -- Reference length divided by twice the total velocity [sec]
      qbar_b        : float; -- Dynamic pressure [N/m**2]
      qbarsref      : float; -- Product of dynamic pressure and reference area [N]
      qbarsreflref  : float; -- Product of dynamic pressure, reference area, and reference length [N*m]
      velsq         : float; -- Total missile velocity, squared [m**2/s**2]
      vmag          : float; -- Total missile velocity WRT earth [m/sec]
      xd_bi_b       : float; -- Missile velocity WRT Earth in BCS [m/sec]
      yd_bi_b       : float; -- Missile velocity WRT Earth in BCS [m/sec]
      zd_bi_b       : float; -- Missile velocity WRT Earth in BCS [m/sec]
   begin
      -- Evaluate ICS to BCS transformation matrix
      tib11 := q0_b**2 + q1_b**2 - q2_b**2 - q3_b**2;
      tib12 := 2.0*(q1_b*q2_b + q0_b*q3_b);
      tib13 := 2.0*(q1_b*q3_b - q0_b*q2_b);
      tib21 := 2.0*(q1_b*q2_b - q0_b*q3_b);
      tib22 := q0_b**2 + q2_b**2 - q1_b**2 - q3_b**2;
      tib23 := 2.0*(q2_b*q3_b + q0_b*q1_b);
      tib31 := 2.0*(q1_b*q3_b + q0_b*q2_b);
      tib32 := 2.0*(q2_b*q3_b - q0_b*q1_b);
      tib33 := q0_b**2 + q3_b**2 - q1_b**2 - q2_b**2;
      -- Missile velocity WRT ICS origin in BCS
      xd_bi_b := xd_bi_i*tib11 + yd_bi_i*tib12 + zd_bi_i*tib13;
      yd_bi_b := xd_bi_i*tib21 + yd_bi_i*tib22 + zd_bi_i*tib23;
      zd_bi_b := xd_bi_i*tib31 + yd_bi_i*tib32 + zd_bi_i*tib33;
      velsq   := xd_bi_i*xd_bi_i + yd_bi_i*yd_bi_i + zd_bi_i*zd_bi_i;
      vmag    := sqrt(velsq);
      -- Angle of attack, sideslip angle, and total angle-of-attack
      alpha       := arctan (zd_bi_b, xd_bi_b);
      beta        := arctan (yd_bi_b, xd_bi_b);
      alpha_total := arctan (sqrt (yd_bi_b*yd_bi_b + zd_bi_b*zd_bi_b), xd_bi_b);
      --  Dynamic pressure and multipliers for forces and moments
      qbar_b       := 0.5*air_density*velsq;
      qbarsref     := qbar_b*ref_area;
      qbarsreflref := qbarsref*ref_length;
      lrefo2vmag   := ref_length/ (2.0*vmag);
      -- Equivalent fin deflections
      del_eff_p := -0.25* (fin_1_position + fin_2_position + fin_3_position + fin_4_position);
      del_eff_q :=  0.5 * (fin_3_position - fin_1_position);
      del_eff_r :=  0.5 * (fin_4_position - fin_2_position);
      -- Aerodynamics forces
      faerox_bi_b := (cx_base + cx_per_alpha_total*alpha_total)*qbarsref;
      faeroy_bi_b := cy_per_beta*beta*qbarsref;
      faeroz_bi_b := cz_per_alpha*alpha*qbarsref;
      -- Aerodynamics moments
      maerox_bi_b := qbarsreflref*(cmx_per_delp*del_eff_p + cmp*p_b*lrefo2vmag);
      maeroy_bi_b := qbarsreflref*(cmy_per_alpha*alpha + cmy_per_delq*del_eff_q + cmq*q_b*lrefo2vmag);
      maeroz_bi_b := qbarsreflref*(cmz_per_beta*beta +cmz_per_delr*del_eff_r + cmr*r_b*lrefo2vmag);
   end aerodynamics_response;
end aero;
