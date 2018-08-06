with text_io,ada.numerics;
package global is
   batch      : natural;    -- Simulation Batch Number incremented by STOP command
   runno      : natural;    -- Simulation Run Number incremented by RUN command

   big        : constant := 10_000_000_000.0;
   small      : constant := 0.000_001;
   rdtodg     : constant := 180.0/ada.numerics.pi;

   time                 : aliased float;    -- Acceleration of suspended mass [m/sec**2]
   time0                : aliased float;    -- Simulation time [sec]
   tstop                : aliased float;    -- Simulation stop time [sec]
   dt                   : aliased float;    -- Simulation time step [sec]
   dtmax                : aliased float;    -- Maximum allowable simulation time step [sec]
   dtmin                : aliased float;    -- Minimum allowable simulation time step [sec]
   dtprint              : aliased float;    -- Time step between printing data [sec]
   recalc               : aliased boolean;  -- Indicates states changed value at a discrete event [Logical]
   quit                 : aliased boolean;  -- Flag set simulation events to done [Logical]
   over                 : aliased boolean;  -- Indicates no more input to processt [Logical]

   AaeroX_bi_b		    : aliased float;  --
   AaeroY_bi_b		    : aliased float;  --
   AaeroZ_bi_b		    : aliased float;  --
   Alpha		        : aliased float;  --
   Alpha_Ref		    : aliased float;  --
   Beta		            : aliased float;  --
   Beta_Ref		        : aliased float;  --
   Drag_Per_VelSq		: aliased float;  --
   Rmin_xy		        : aliased float;  --
   Rmin_xz		        : aliased float;  --
   Tau_p		        : aliased float;  --
   Omega0_q		        : aliased float;  --
   Omega0_r		        : aliased float;  --
   Zeta_q		        : aliased float;  --
   Zeta_r		        : aliased float;  --
   Alpha_Ref_dg		    : aliased float;  --
   Beta_Ref_dg		    : aliased float;  --
   Freq0_q		        : aliased float;  --
   Freq0_r		        : aliased float;  --
   P_g_Meas		        : aliased float;  --
   Q_g_Meas		        : aliased float;  --
   R_g_Meas		        : aliased float;  --
   P_b_Cmd		        : aliased float;  --
   Q_b_Cmd		        : aliased float;  --
   R_b_Cmd		        : aliased float;  --
   Q0_b_Est		        : aliased float;  --
   Q1_b_Est		        : aliased float;  --
   Q2_b_Est		        : aliased float;  --
   Q3_b_Est		        : aliased float;  --
   Q0d_b_Est		    : aliased float;  --
   Q1d_b_Est		    : aliased float;  --
   Q2d_b_Est		    : aliased float;  --
   Q3d_b_Est		    : aliased float;  --
   Pitch_Guidance_Gain	: aliased float;  --
   Roll_Guidance_Gain	: aliased float;  --
   Yaw_Guidance_Gain	: aliased float;  --
   Psi_b_Est_dg		    : aliased float;  --
   Theta_b_Est_dg		: aliased float;  --
   Phi_b_Est_dg		    : aliased float;  --
   Psi_b_Est		    : aliased float;  --
   Theta_b_Est		    : aliased float;  --
   Phi_b_Est		    : aliased float;  --
   Q_b_Cmd_Bias	        : aliased float;  --
   X_bi_i		        : aliased float;  --
   Xd_bi_i		        : aliased float;  --
   Xdd_bi_i		        : aliased float;  --
   Y_bi_i		        : aliased float;  --
   Yd_bi_i		        : aliased float;  --
   Ydd_bi_i		        : aliased float;  --
   Z_bi_i		        : aliased float;  --
   Zd_bi_i		        : aliased float;  --
   Zdd_bi_i		        : aliased float;  --
   P_b		            : aliased float;  --
   Pd_b		            : aliased float;  --
   Q_b		            : aliased float;  --
   Qd_b		            : aliased float;  --
   R_b		            : aliased float;  --
   Rd_b		            : aliased float;  --
   Psi_b		        : aliased float;  --
   Theta_b		        : aliased float;  --
   Phi_b		        : aliased float;  --
   Q0_b		            : aliased float;  --
   Q0d_b		        : aliased float;  --
   Q1_b		            : aliased float;  --
   Q1d_b		        : aliased float;  --
   Q2_b		            : aliased float;  --
   Q2d_b		        : aliased float;  --
   Q3_b		            : aliased float;  --
   Q3d_b		        : aliased float;  --
   Tib11		        : aliased float;  --
   Tib12		        : aliased float;  --
   Tib13		        : aliased float;  --
   Tib21		        : aliased float;  --
   Tib22		        : aliased float;  --
   Tib23		        : aliased float;  --
   Tib31		        : aliased float;  --
   Tib32		        : aliased float;  --
   Tib33		        : aliased float;  --
   Psi_b_IC_dg		    : aliased float;  --
   Theta_b_IC_dg	    : aliased float;  --
   Phi_b_IC_dg		    : aliased float;  --
   P_b_IC_dg		    : aliased float;  --
   Q_b_IC_dg		    : aliased float;  --
   R_b_IC_dg		    : aliased float;  --
   Xd_bi_i_IC		    : aliased float;  --
   Yd_bi_i_IC		    : aliased float;  --
   Zd_bi_i_IC		    : aliased float;  --
   X_bi_i_IC		    : aliased float;  --
   Y_bi_i_IC		    : aliased float;  --
   Z_bi_i_IC		    : aliased float;  --
   Acc_Gravity		    : aliased float;  --
   Q_si_b_Meas		    : aliased float;  --
   R_si_b_Meas		    : aliased float;  --
   P_s		            : aliased float;  --
   Q_s		            : aliased float;  --
   R_s		            : aliased float;  --
   Range_tb_Sq		    : aliased float;  --
   X_tb_i		        : aliased float;  --
   Xd_tb_i		        : aliased float;  --
   Y_tb_i		        : aliased float;  --
   Yd_tb_i		        : aliased float;  --
   Z_tb_i		        : aliased float;  --
   Zd_tb_i		        : aliased float;  --
   X_ti_i		        : aliased float;  --
   Xd_ti_i		        : aliased float;  --
   Y_ti_i		        : aliased float;  --
   Yd_ti_i		        : aliased float;  --
   Z_ti_i		        : aliased float;  --
   Zd_ti_i		        : aliased float;  --
   X_ti_i_IC		    : aliased float;  --
   Y_ti_i_IC		    : aliased float;  --
   Z_ti_i_IC		    : aliased float;  --
   Xd_ti_i_IC		    : aliased float;  --
   Yd_ti_i_IC		    : aliased float;  --
   Zd_ti_i_IC		    : aliased float;  --
end global;
