C
C This program simulates a mass-spring-damper system.
C
C
C Variables:
C Damping_Coefficient  - Damping force per velocity [Nimls]
C Dt                   - Simulation time step [sec]
C Gravity              - Acceleration due to gravity [mlsec**2)
C Mass                 - Mass suspended from spring [Kg)
C Spring_Coefficient   - Restoring force per position [Nim]
C Time                 - Simulation time [sec]
C Tstop                - Simulation stop time [sec]
C X                    - Position of suspended mass [m]
C X IC                 - Initial position of suspended mass [m]
C Xd                   - Velocity of suspended mass [mlsec]
C Xd IC                - Initial velocity of suspended mass [mlsec]
C Xdd                  - Acceleration of suspended mass [mlsec**2]
C
      Program DigSim
C
C Variable declarations
C
      Implicit None
      Real Damping_Coefficient,
     &     Dt,
     &     Gravity,
     &     Mass,
     &     Spring_Coefficient,
     &     Time,
     &     Tstop,
     &     X,
     &     X_IC,
     &     Xd,
     &     Xd_IC,
     &     Xdd
C
C     System parameters
C
      Data Damping_Coefficient / 8.88e0/,
     &     Dt                  / 0.01e0/,
     &     Gravity             / 9.88e0/,
     &     Mass                / 1.0e0/,
     &     Spring_Coefficient  /39.47e0/,
     &     Tstop               / 2.50e0/,
     &     X_IC                / 0.0e0 /,
     &     Xd_IC               / 0.0e0 /
C
C     Set initial conditions
C
      X    = X_IC
      Xd   = Xd_IC
      Time = 0.0e0
C
C     Main simulation loop
C
      Do While (Time .le. Tstop)
C
C     Print states for this time
C
C         Print *, Time, X, Xd
 100     Format(' ',3ES15.5)
         Write(*,100) Time, X, Xd
C
C     Calculate derivative at current time
C
         Xdd = -(Spring_Coefficient*X + Damping_Coefficient*Xd)/Mass-
     &        Gravity
C
C       Advance states one time step
C
         X  = X + Xd*Dt
         Xd = Xd + Xdd*Dt
C
C     Advance time and continue simulation loop
C
         Time = Time + Dt
C
      End Do
C
      Stop
      End
