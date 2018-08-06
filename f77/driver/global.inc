C
C       Default variable type is REAL*4
C
        Implicit Real (A-Z)
C
C       Global common declarations
C
        Dimension Real_Array (1000)
        Common /Global_Real_Common/ Real_Array
        Integer REAL_ARRAY_SIZE
        Parameter (REAL_ARRAY_SIZE = 1000)
C
