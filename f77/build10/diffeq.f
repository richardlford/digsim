C
      Subroutine Differential_Equations
C
C     This routine calls the routines that evaluate the state derivatives
C
      Call Actuator
      Call Aerodynamics
      Call Mass_Data
      Call Kinematics
      Call Target
      Call Seeker
      Call Gyro
      Call Flight_Computer
C
      Return
      End
