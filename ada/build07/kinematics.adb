with define_state;
with ada.numerics.elementary_functions,digsimio,global;
use  ada.numerics.elementary_functions;
package body kinematics is
   procedure kinematics_data is
      -- This subroutine sets the default data for the kinematics routine.
      use global;
   begin
      theta_b_ic_dg :=    0.0;
      velocity      :=  100.0;
      x_bi_i_ic     := -500.0;
      z_bi_i_ic     := -100.0;
   end kinematics_data;

   procedure kinematics_init is
      -- Begin math model initialization:
   begin
      declare
         use digsimio;
      begin
         define_state(x_bi_i,xd_bi_i);
         define_state(z_bi_i,zd_bi_i);
         define_state(theta_b,thetadot_b);
      end;
      declare
         use global;
      begin
         -- Set the initial conditions
         x_bi_i  := x_bi_i_ic;
         z_bi_i  := z_bi_i_ic;
         theta_b := theta_b_ic_dg/rdtodg;
      end;
   end kinematics_init;

   procedure kinematics_response is
      -- This subroutine determines the kinematics state derivatives
      use global;
   begin
      -- Calculate velocities (attitude rate comes directly from airframe response)
      xd_bi_i :=  velocity*cos(theta_b);
      zd_bi_i := -velocity*sin(theta_b);
   end kinematics_response;
end kinematics;
