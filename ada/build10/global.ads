with ada.numerics;
package global is
   batch    : natural;    -- Simulation Batch Number incremented by STOP command
   runno    : natural;    -- Simulation Run Number incremented by RUN command

   big      : constant float := 10_000_000_000.0;
   small    : constant float := 0.000_001;
   rdtodg   : constant float := 180.0/ada.numerics.pi;
   ----------------------------------------------------------------------------------------------------------------
   recalc, quit, over : aliased boolean;
   time, time0, tstop, dt, dtmax, dtmin, dtprint,
     faerox_bi_b, faeroy_bi_b, faeroz_bi_b, maerox_bi_b, maeroy_bi_b, maeroz_bi_b,
     alpha, beta, alpha_total, air_density, ref_area, ref_length, cx_base,
     cx_per_alpha_total, cy_per_beta, cz_per_alpha, cmy_per_alpha, cmz_per_beta,
     cmx_per_delp, cmy_per_delq, cmz_per_delr, cmp, cmq, cmr, p_g_meas, q_g_meas,
     r_g_meas, fin_1_cmd, fin_2_cmd, fin_3_cmd, fin_4_cmd, q0_b_est, q1_b_est,
     q2_b_est, q3_b_est, q0d_b_est, q1d_b_est, q2d_b_est, q3d_b_est,
     pitch_guidance_gain, roll_guidance_gain, yaw_guidance_gain, psi_b_est_dg,
     theta_b_est_dg, phi_b_est_dg, psi_b_est, theta_b_est, phi_b_est,
     q_b_cmd_bias, del_cmd_per_p_cmd, del_cmd_per_q_cmd, del_cmd_per_r_cmd,
     p_b_cmd, q_b_cmd, r_b_cmd, ixx_b, iyy_b, izz_b, mass_b, x_bi_i, xd_bi_i,
     xdd_bi_i, y_bi_i, yd_bi_i, ydd_bi_i, z_bi_i, zd_bi_i, zdd_bi_i, p_b, pd_b,
     q_b, qd_b, r_b, rd_b, phi_b, psi_b, theta_b, q0_b, q0d_b, q1_b, q1d_b,
     q2_b, q2d_b, q3_b, q3d_b, tib11, tib12, tib13, tib21, tib22, tib23, tib31,
     tib32, tib33, psi_b_ic_dg, theta_b_ic_dg, phi_b_ic_dg, p_b_ic_dg, q_b_ic_dg,
     r_b_ic_dg, xd_bi_i_ic, yd_bi_i_ic, zd_bi_i_ic, x_bi_i_ic, y_bi_i_ic,
     z_bi_i_ic, acc_gravity, q_si_b_meas, r_si_b_meas, p_s, q_s, r_s,
     range_tb_sq, x_tb_i, xd_tb_i, y_tb_i, yd_tb_i, z_tb_i, zd_tb_i, x_ti_i,
     xd_ti_i, y_ti_i, yd_ti_i, z_ti_i, zd_ti_i, x_ti_i_ic, y_ti_i_ic, z_ti_i_ic,
     xd_ti_i_ic, yd_ti_i_ic, zd_ti_i_ic, fin_1_position, fin_2_position,
     fin_3_position, fin_4_position, fin_limit_dg, fin_limit
     : aliased float;
end global;
