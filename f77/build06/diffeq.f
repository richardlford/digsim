C
      Subroutine Differential_Equations
C
C     This routine evaluates the state derivatives for the terminal homing
C     simulation
C
C     Inputs:
C       Guidance_Gain - Commanded angular rate per measured LOS rate [Real]
C       Velocity      - Velocity of missile [m/sec]
C
C     Outputs:
C       Q_s           - LOS rate [rad/sec]
C       Q_s_Meas      - Measured LOS rate {rad/sec]
C       Theta         - Attitude of missile [rad]
C       ThetaDot      - Attitude rate missile [rad/sec]
C       ThetaDot_Cmd  - Commanded attitude rate of missile [rad/sec]
C       X             - X position of missile [m]
C       Xd            - X velocity of missile [m/sec]
C       Z             - Z position of missile [m]
C       Zd            - Z velocity of missile [m/sec]
C
C     Internal variables and constants:
C         None.
C
C     Declare global common
C
      Include '../driver/global.inc'
      Include '../driver/sysvars.inc'
      Equivalence (Real_Array(13), Velocity)
      Equivalence (Real_Array(16), X)
      Equivalence (Real_Array(17), Z)
      Equivalence (Real_Array(18), Theta)
      Equivalence (Real_Array(19), Xd)
      Equivalence (Real_Array(20), Zd)
      Equivalence (Real_Array(21), ThetaDot)
      Equivalence (Real_Array(22), ThetaDot_Cmd)
      Equivalence (Real_Array(23), Q_s)
      Equivalence (Real_Array(24), Q_s_Meas)
      Equivalence (Real_Array(25), Guidance_Gain)
C
C     Begin math model
C
C     Calculate velocities
C
      Xd = Velocity*cos(Theta)
      Zd = -Velocity*sin(Theta)
C
C     Calculate true LOS rate
C
      Q_s = (z*Xd - X*Zd)/(X*X + Z*Z)
C
C     Calculate measured LOS rate
C
      Q_s_Meas = Q_s
C
C     Calculate guidance comand
C
      ThetaDot_Cmd = Guidance_Gain*Q_s_Meas
C
C     Calculate airframe rate
C
      ThetaDot = ThetaDot_Cmd

C
      Return
      End
