SUBROUTINE TARGET_DATA
  IMPLICIT NONE
  REAL  xd_ti_i_ic, x_ti_i_ic, zd_ti_i_ic, z_ti_i_ic
  --*** End of declarations inserted by SPAG
  --
  --     This subroutine sets the default data for the target motion
  --
  --     Inputs:
  --       None.
  --
  --     Outputs:
  --       X_ti_i_IC  - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  --       Xd_ti_i_IC - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --       Z_ti_i_IC  - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  --       Zd_ti_i_IC - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --
  --     Internal variables and constants
  --       None.
  --
  --     Declare global common and assign variable locations
  --
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  EQUIVALENCE (real_array(704),x_ti_i_ic)
  EQUIVALENCE (real_array(705),xd_ti_i_ic)
  EQUIVALENCE (real_array(706),z_ti_i_ic)
  EQUIVALENCE (real_array(707),zd_ti_i_ic)
  --
  --       Begin default data definition:
  --
  x_ti_i_ic = 0.0E0
  z_ti_i_ic = 0.0E0
  xd_ti_i_ic = 0.0E0
  zd_ti_i_ic = 0.0E0
  --
END SUBROUTINE TARGET_DATA
--*==TARGET_INIT.spg  processed by SPAG 6.72Dc at 23:38 on 23 Jan 2016

SUBROUTINE TARGET_INIT
  IMPLICIT NONE
  --*--TARGET_INIT45
  --*** Start of declarations inserted by SPAG
  REAL xd_ti_i, xd_ti_i_ic, x_ti_i, x_ti_i_ic,     &
       & zd_ti_i, zd_ti_i_ic, z_ti_i, z_ti_i_ic
  --*** End of declarations inserted by SPAG
  --
  --     This subroutine initializes the variables for the target motion model.
  --
  --     Inputs:
  --       X_ti_i_IC  - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  --       Xd_ti_i_IC - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --       Z_ti_i_IC  - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  --       Zd_ti_i_IC - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --     Outputs:
  --       X_ti_i  - X position of target w.r.t. ICS, expressed in ICS [m]
  --       Xd_ti_i - X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --       Z_ti_i  - Z position of target w.r.t. ICS, expressed in ICS [m]
  --       Zd_ti_i - Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --
  --       Internal variables and constants:
  --           None.
  --
  --       Declare global common and assign variable locations
  --
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  EQUIVALENCE (real_array(700),x_ti_i)
  EQUIVALENCE (real_array(701),xd_ti_i)
  EQUIVALENCE (real_array(702),z_ti_i)
  EQUIVALENCE (real_array(703),zd_ti_i)
  EQUIVALENCE (real_array(704),x_ti_i_ic)
  EQUIVALENCE (real_array(705),xd_ti_i_ic)
  EQUIVALENCE (real_array(706),z_ti_i_ic)
  EQUIVALENCE (real_array(707),zd_ti_i_ic)
  --
  --       Begin math model initialization:
  --
  --       Initialize target states
  --
  x_ti_i = x_ti_i_ic
  z_ti_i = z_ti_i_ic
  xd_ti_i = xd_ti_i_ic
  zd_ti_i = zd_ti_i_ic
  --
END SUBROUTINE TARGET_INIT
--*==TARGET.spg  processed by SPAG 6.72Dc at 23:38 on 23 Jan 2016

SUBROUTINE TARGET
  -- This subroutine determined the target motion.
  --
  -- Inputs:
  --     X_ti_i_IC       - Initial X position of target w.r.t. ICS, expressed in ICS [m]
  --     Z_ti_i_IC       - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
  --
  -- Outputs:
  --     X_ti_i  - X position of target w.r.t. ICS, expressed in ICS [m]
  --     Xd_ti_i - X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  --     Z_ti_i  - Z position of target w.r.t. ICS, expressed in ICS [m]
  --     Zd_ti_i - Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
  IMPLICIT NONE
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  REAL xd_ti_i, x_ti_i, x_ti_i_ic, zd_ti_i,  z_ti_i, z_ti_i_ic
  EQUIVALENCE (real_array(700),x_ti_i)
  EQUIVALENCE (real_array(701),xd_ti_i)
  EQUIVALENCE (real_array(702),z_ti_i)
  EQUIVALENCE (real_array(703),zd_ti_i)
  EQUIVALENCE (real_array(704),x_ti_i_ic)
  EQUIVALENCE (real_array(706),z_ti_i_ic)
  -- Begin math model:
  -- Calculate current target position
  x_ti_i = x_ti_i_ic + xd_ti_i*time
  z_ti_i = z_ti_i_ic + zd_ti_i*time
END SUBROUTINE TARGET
