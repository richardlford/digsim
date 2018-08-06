SUBROUTINE SEEKER_DATA
  IMPLICIT NONE
  -- This subroutine sets the default data for the seeker model.
  --
  -- Inputs:
  --   None.
  --
  -- Outputs:
  --   None.
  --
  -- Internal variables and constants:
  --   None.
  --
  -- Begin default data definitions:
  --   No default data required.
  --
END SUBROUTINE SEEKER_DATA

SUBROUTINE SEEKER_INIT
  IMPLICIT NONE
  -- This subroutine initializes the variables for the seeker model.
END SUBROUTINE SEEKER_INIT

SUBROUTINE SEEKER
  -- This subroutine models a perfect one-axis seeker/tracker.
  --
  -- Inputs:
  --   X_bi_i  - X position of the BCS w.r.t. ICS, expressed in the ICS [m]
  --   X_ti_i  - X position of the target, w.r.t. ICS in ICS[m]
  --   Xd_bi_i - X velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  --   Xd_ti_i - X velocity of the target, w.r.t. ICS in ICS [m/sec]
  --   Z_bi_i  - Z position of the BCS w.r.t. ICS, expressed in the ICS [m]
  --   Z_ti_i  - Z position of the target, w.r.t. ICS in ICS [m]
  --   Zd_bi_i - Z velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  --   Zd_ti_i - Z velocity of the target, w.r.t. ICS in ICS [m/sec]
  --
  -- Outputs:
  --   Q_s             - LOS rate [rad/sec]
  --   Q_s_Meas        - Measured LOS rate [rad/sec]
  --   X_bt_i          - X position of the BCS w.r.t. ICS, expressed in the ICS [m]
  --   Xd_bt_i         - X velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  --   Z_bt_i          - Z position of the BCS w.r.t. ICS, expressed in the ICS [m]
  --   Zd_bt_i         - Z velocity of the BCS w.r.t. ICS, expressed in the ICS [m/sec]
  IMPLICIT NONE
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  REAL q_s, q_s_meas, xd_bi_i, xd_bt_i, xd_ti_i,  &
       & x_bi_i, x_bt_i, x_ti_i, zd_bi_i, zd_bt_i, zd_ti_i,   &
       & z_bi_i, z_bt_i, z_ti_i
  EQUIVALENCE (real_array(500),x_bi_i)
  EQUIVALENCE (real_array(503),z_bi_i)
  EQUIVALENCE (real_array(501),xd_bi_i)
  EQUIVALENCE (real_array(504),zd_bi_i)
  EQUIVALENCE (real_array(600),q_s)
  EQUIVALENCE (real_array(601),q_s_meas)
  EQUIVALENCE (real_array(602),x_bt_i)
  EQUIVALENCE (real_array(603),z_bt_i)
  EQUIVALENCE (real_array(604),xd_bt_i)
  EQUIVALENCE (real_array(605),zd_bt_i)
  EQUIVALENCE (real_array(700),x_ti_i)
  EQUIVALENCE (real_array(701),xd_ti_i)
  EQUIVALENCE (real_array(702),z_ti_i)
  EQUIVALENCE (real_array(703),zd_ti_i)
  -- Begin math model:
  -- Target relative position and velocity in ICS
  x_bt_i = x_bi_i - x_ti_i
  z_bt_i = z_bi_i - z_ti_i
  xd_bt_i = xd_bi_i - xd_ti_i
  zd_bt_i = zd_bi_i - zd_ti_i
  -- Calculate true LOS rate
  q_s = (z_bt_i*xd_bt_i-x_bt_i*zd_bt_i)/(x_bt_i*x_bt_i+z_bt_i*z_bt_i)
  -- Calculate measured L08 rate
  q_s_meas = q_s
END SUBROUTINE SEEKER
