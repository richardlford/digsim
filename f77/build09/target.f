C
        Subroutine Target_Data
C
C       This subroutine sets the default data for the target motion
C
C       Inputs:
C           None.
C
C       Outputs:
C           X_ti_i_IC       - Initial X position of target w.r.t. ICS, expressed in ICS [m]
C           Xd_ti_i_IC      - Initial X velocity of target w.r.t. ICS, expressed in ICS [m/sec]
C           Z_ti_i_IC       - Initial Z position of target w.r.t. ICS, expressed in ICS [m]
C           Zd_ti_i_IC      - Initial Z velocity of target w.r.t. ICS, expressed in ICS [m/sec]
C
C       Internal variables and constants
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(706), X_ti_i_IC)
      Equivalence (Real_Array(707), Y_ti_i_IC)
      Equivalence (Real_Array(708), Z_ti_i_IC)
      Equivalence (Real_Array(709), Xd_ti_i_IC)
      Equivalence (Real_Array(710), Yd_ti_i_IC)
      Equivalence (Real_Array(711), Zd_ti_i_IC)
C
C       Begin default data definition:
C
      X_ti_i_IC               =  500.0e0
      y_ti_i_IC               = -250.0e0
      Z_ti_i_IC               =    0.0e0
      Xd_ti_i_IC              =  -25.0e0
      Yd_ti_i_IC              =   25.0e0
      Zd_ti_i_IC              =    0.0e0
C
      Return
      End

      Subroutine Target_Init
C
C       This subroutine initializes the variables for the target motion model.
C
C       Inputs:
C           X_ti_i_IC       - Initial position of target w.r.t. ICS, expressed in ICS [m]
C           Y_ti_i_IC
C           Z_ti_i_IC
C           Xd_ti_i_IC      - Initial velocity of target w.r.t. ICS, expressed in ICS [m/sec]
C           Yd_ti_i_IC
C           Zd_ti_i_IC

C       Outputs:
C           X_ti_i  - position of target w.r.t. ICS, expressed in ICS [m]
C           Y_ti_i
C           Z_ti_i
C           Xd_ti_i - velocity of target w.r.t. ICS, expressed in ICS [m/sec]
C           Yd_ti_i
C           Zd_ti_i
C
C       Internal variables and constants:
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(700), X_ti_i)
      Equivalence (Real_Array(701), Xd_ti_i)
      Equivalence (Real_Array(702), Y_ti_i)
      Equivalence (Real_Array(703), Yd_ti_i)
      Equivalence (Real_Array(704), Z_ti_i)
      Equivalence (Real_Array(705), Zd_ti_i)
      Equivalence (Real_Array(706), X_ti_i_IC)
      Equivalence (Real_Array(707), Y_ti_i_IC)
      Equivalence (Real_Array(708), Z_ti_i_IC)
      Equivalence (Real_Array(709), Xd_ti_i_IC)
      Equivalence (Real_Array(710), Yd_ti_i_IC)
      Equivalence (Real_Array(711), Zd_ti_i_IC)
C
C       Begin math model initialization:
C
C       Initialize target states
C
      X_ti_i  = X_ti_i_IC
      Y_ti_i  = Y_ti_i_IC
      Z_ti_i  = Z_ti_i_IC
      Xd_ti_i = Xd_ti_i_IC
      Yd_ti_i = Yd_ti_i_IC
      Zd_ti_i = Zd_ti_i_IC
C
      Return
      End

      Subroutine Target
C
C       This subroutine determined the target motion.
C
C       Inputs:
C           Time       - Simulation time [sec]
C           X_ti_i_IC  - Initial position of target w.r.t. ICS, expressed in ICS [m]
C           Y_ti_i_IC
C           Z_ti_i_IC
C           Xd_ti_i - velocity of target w.r.t. ICS, expressed in ICS [m/sec]
C           Yd_ti_i
C           Zd_ti_i
C
C       Outputs:
C           X_ti_i  - position of target w.r.t. ICS, expressed in ICS [m]
C           Y_ti_i
C           Z_ti_i
C
C       Internal variables and constants
C           None.
C
C       Declare global common and assign variable locations
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(700), X_ti_i)
      Equivalence (Real_Array(701), Xd_ti_i)
      Equivalence (Real_Array(702), Y_ti_i)
      Equivalence (Real_Array(703), Yd_ti_i)
      Equivalence (Real_Array(704), Z_ti_i)
      Equivalence (Real_Array(705), Zd_ti_i)
      Equivalence (Real_Array(706), X_ti_i_IC)
      Equivalence (Real_Array(707), Y_ti_i_IC)
      Equivalence (Real_Array(708), Z_ti_i_IC)
C
C       Begin math model:
C
C       Calculate current target position
C
      X_ti_i = X_ti_i_IC + Xd_ti_i*Time
      Y_ti_i = Y_ti_i_IC + Yd_ti_i*Time
      Z_ti_i = Z_ti_i_IC + Zd_ti_i*Time
C
      Return
      End
