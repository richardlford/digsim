with define_state,ada.numerics.elementary_functions;
with global,digsimio;
package body kinematics is
   procedure kinematics_data is
      -- This subroutine sets the Default data for the kinematics routine.
      use global;
   begin
      theta_b_ic_dg :=    0.0;
      q_b_ic_dg     :=    0.0;
      x_bi_i_ic     := -500.0;
      z_bi_i_ic     := -100.0;
      xd_bi_i_ic    :=  100.0;
      zd_bi_i_ic    :=    0.0;
      gravity       :=   9.88;
   end kinematics_data;

   procedure kinematics_init is
   begin
      declare
         use digsimio;
      begin
         define_state(x_bi_i,xd_bi_i);
         define_state(xd_bi_i,xdd_bi_i);
         define_state(z_bi_i,zd_bi_i);
         define_state(zd_bi_i,zdd_bi_i);
         define_state(theta_b,q_b);
         define_state(q_b,qd_b);
      end;
      declare
         use global;
      begin
         x_bi_i  := x_bi_i_ic;
         z_bi_i  := z_bi_i_ic;
         xd_bi_i := xd_bi_i_ic;
         zd_bi_i := zd_bi_i_ic;
         theta_b := theta_b_ic_dg/rdtodg;
         q_b     := q_b_ic_dg/rdtodg;
      end;
   end kinematics_init;

   procedure kinematics_response is
      -- This subroutine determined the kinematic state derivatives.
      use global,ada.numerics.elementary_functions;
   begin
      -- Calculate accelerations (attitude rate derivative comes from aero)
      xdd_bi_i :=  aaerox_bi_b*cos(theta_b) + aaeroz_bi_b*sin(theta_b);
      zdd_bi_i := -aaerox_bi_b*sin(theta_b) + aaeroz_bi_b*cos(theta_b)  + gravity;
   end kinematics_response;

end kinematics;
