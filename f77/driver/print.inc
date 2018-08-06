C
C   Print common block variables and declarations
C
        Integer MAX_NUMBER_OF_PRINTS,
     &          Number_Of_Prints,
     &          Print_Index (50)
        Common /Print_Com/ Number_Of_Prints,
     &                     Print_Index
        Parameter (MAX_NUMBER_OF_PRINTS = 50)
