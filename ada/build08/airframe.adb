with limit;
with global,ada.numerics.elementary_functions;
use  global,ada.numerics.elementary_functions;
package body airframe is
   procedure airframe_response_data is
  -- Airframe response model default data
   begin
      drag_per_velsq := 0.00021; -- Aero coefficients
      omega0_q       := 30.0;    -- Natural frequency and damping
      zeta_q         := 0.5;     -- ratio of stabilized airframe
      rmin_xz        := 200.0;   -- Minimum turning radii for x-z plane
      alpha_ref      := 0.2618;  -- Aerodynamic reference angle-of-attack
   end airframe_response_data;

   procedure airframe_response_init is
      -- Airframe response model initialization
   begin
      null;
   end airframe_response_init;

   procedure airframe_response is
      -- This model determines the rate of change of the angular and translational velocities due to
      -- aerodynamic forces resulting from acceleration commands.  Transfer functions approximate
      -- the behavior of a stable aerodynamics/autopilot combination. This version models the
      -- aerodynamic drag as a function of total velocity.
   begin
      -- Missile velocity in missile axis
      xd_bi_b := xd_bi_i*cos(theta_b) - zd_bi_i*sin(theta_b);
      zd_bi_b := xd_bi_i*sin(theta_b) + zd_bi_i*cos(theta_b);
      -- Missile velocity WRT ICS origin in BCS
      velsq := xd_bi_i*xd_bi_i + zd_bi_i*zd_bi_i;
      vmag  := sqrt(velsq);
      -- Angle of attack
      alpha := arctan(zd_bi_b,xd_bi_b);
      -- Translational accelerations due to aero in stability axis
      adrag := velsq*drag_per_velsq;
      z_acc_max := velsq/rmin_xz;
      acc_per_alpha := -float'max(small,z_acc_max/alpha_ref);
      acc_alpha := acc_per_alpha*alpha;
      -- Translational accelerations due to aero in BCS
      aaerox_bi_b := adrag*cos(alpha) - acc_alpha*sin(alpha);
      aaeroz_bi_b := adrag*sin(alpha) + acc_alpha*cos(alpha);
      -- Pitch response to a pitch command
      alpha_cmd := limit(-q_b_cmd*vmag,-z_acc_max,z_acc_max)/acc_per_alpha;
      bq2       := omega0_q*omega0_q;
      aq11      := -vmag/(alpha_ref*rmin_xz);
      aq21      := -(aq11*(aq11-2.0*zeta_q*omega0_q)+bq2);
      aq22      := aq11 - 2.0*zeta_q*omega0_q;
      qd_b      := aq21*alpha + aq22*q_b + bq2*alpha_cmd;
   end airframe_response;
end airframe;
