C
C  State and state derivative pointers
C
      Integer Ix_Real, IxDot_Real, Ndes_Real,
     &     MAX NUMBER_OF_REAL_STATES
      Common /Deriv_Common/ Ndes_Real, Ix_Real(5), IxDot_Real(5)
      Parameter (MAX_NUMBER_OF_REAL_STATES = 5)
