C
C     Set values common block variables and declarations
C
      Integer MAX_NUMBER_OF_REAL_VALUES,
     &     Number_Of_Real_Values,
     &     Set_Real_Value_Index(50)
       Real Set_REal_Value(50)
       Common /Set_Value_Com/ Number_Of_Real_Values,
     &      Set_Real_Value,
     &      Set_Real_Value_Index
       Parameter [MAX_NUMBER_OF_REAL_VALUES = 50)
C
