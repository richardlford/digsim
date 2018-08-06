C
C   State and state derivative pointers
C
        Integer Ix_Real,
     &          IxDot_Real,
     &          Ndes_Real,
     &          MAX_NUMBER_OF_REAL_STATES
        Common /Deriv_Common/ Ndes_Real,
     &                        Ix_Real (25),
     &                        IxDot_Real (25)
        Parameter (MAX_NUMBER_OF_REAL_STATES = 25)
