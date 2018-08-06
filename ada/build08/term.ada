SUBROUTINE TERMINATION_CONDITIONS(Quit)
  -- This module determines if the run termination conditions have been met
  --
  -- Inputs:
  --     X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
  --     Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS
  --                       [m/sec]
  --     Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
  --     Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  --
  -- Outputs:
  --     Quit - Stop simulation run [Boolean]
  --
  -- Internal variables and constants:
  --     Time_To_Go - Time until closest approach [sec]
  --
  -- Declare global common and assign variable locations
  IMPLICIT NONE
  REAL time_to_go, xd_bt_i, x_bt_i, zd_bt_i, z_bt_i
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  LOGICAL Quit
  EQUIVALENCE (real_array(602),x_bt_i)
  EQUIVALENCE (real_array(603),z_bt_i)
  EQUIVALENCE (real_array(604),xd_bt_i)
  EQUIVALENCE (real_array(605),zd_bt_i)
  -- Begin consition check:
  -- Evaluate closest approach time and miss distance
  time_to_go = -(x_bt_i*xd_bt_i+z_bt_i*zd_bt_i)/(xd_bt_i**2+zd_bt_i**2)
  IF ( time_to_go.LT.0.0E0 ) THEN
     CALL MISS
     Quit = .TRUE.
  ENDIF
END SUBROUTINE TERMINATION_CONDITIONS

SUBROUTINE MISS
  -- Miss distance calculation routine
  --
  -- Inputs:
  --     X_bt_i  - X position of the BCS w.r.t. the target, expressed in the ICS [m]
  --     Xd_bt_i - X velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  --     Z_bt_i  - Z position of the BCS w.r.t. the target, expressed in the ICS [m]
  --     Zd_bt_i - Z velocity of the BCS w.r.t. the target, expressed in the ICS [m/sec]
  --
  -- Outputs:
  --     DtMiss  - Time step to closest approach [sec]
  --     RMiss   - Missile miss distance [m]
  IMPLICIT NONE
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  REAL dtmiss, rmiss, xd_bt_i, x_bt_i, zd_bt_i, z_bt_i
  EQUIVALENCE (real_array(602),x_bt_i)
  EQUIVALENCE (real_array(603),z_bt_i)
  EQUIVALENCE (real_array(604),xd_bt_i)
  EQUIVALENCE (real_array(605),zd_bt_i)
  -- Begin miss distance calculation:
  -- Evaluate closest approach time and miss distance
  dtmiss = (x_bt_i*xd_bt_i+z_bt_i*zd_bt_i)/(xd_bt_i**2+zd_bt_i**2)
  rmiss = SQRT((x_bt_i-xd_bt_i*dtmiss)**2+(z_bt_i-zd_bt_i*dtmiss)**2)
  OPEN (UNIT=98,FILE='miss.dat')
  WRITE (98,*) 'RMiss = ', rmiss
  WRITE (98,*) 'DtMiss = ', dtmiss
  CLOSE (98)
END SUBROUTINE MISS
