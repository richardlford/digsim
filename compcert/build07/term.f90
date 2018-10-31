SUBROUTINE TERMINATION_CONDITIONS(Quit)
  IMPLICIT NONE
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  REAL z_bi_i
  LOGICAL Quit
  ! Termination module
  !
  ! Inputs:
  !   Z_bi_i - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  !
  ! Outputs:
  !   Quit - Stop simulation run [Boolean]
  EQUIVALENCE (real_array(501),z_bi_i)
  IF (z_bi_i.GE.0.0) THEN
     CALL MISS
     Quit = .TRUE.
  ENDIF
END SUBROUTINE TERMINATION_CONDITIONS

SUBROUTINE MISS
  ! Miss distance calculation routine
  !
  ! Inputs:
  !   X_bi_i  - X position of BCS w.r.t. ICS, expressed in the ICS [m]
  !   Xd_bi_i - X velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  !   Z_bi_i  - Z position of BCS w.r.t. ICS, expressed in the ICS [m]
  !   Zd_bi_i - Z velocity of BCS w.r.t. ICS, expressed in the ICS [m/sec]
  !
  ! Outputs:
  !   DtMiss - Time step to closest approach [sec]
  !   RMiss  - Missile miss distance [m]
  IMPLICIT NONE
  INCLUDE '../driver/global.inc'
  INCLUDE '../driver/sysvars.inc'
  REAL dtmiss, rmiss, xd_bi_i, x_bi_i, zd_bi_i, z_bi_i
  EQUIVALENCE (real_array(500),x_bi_i)
  EQUIVALENCE (real_array(501),z_bi_i)
  EQUIVALENCE (real_array(502),xd_bi_i)
  EQUIVALENCE (real_array(503),zd_bi_i)
  ! Begin miss distance calculation:
  ! Evaluate closest approach time and miss distance
  dtmiss = (x_bi_i*xd_bi_i+z_bi_i*zd_bi_i)/(xd_bi_i**2+zd_bi_i**2)
  rmiss = SQRT((x_bi_i-xd_bi_i*dtmiss)**2+(z_bi_i-zd_bi_i*dtmiss)**2)
  OPEN (UNIT=98,FILE='miss.dat')
  WRITE (98,*) 'Rmiss  = ' , rmiss
  WRITE (98,*) 'DtMiss = ' , dtmiss
  CLOSE (98)
END SUBROUTINE MISS
