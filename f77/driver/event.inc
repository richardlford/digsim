C
C       Event common variables and declaration
C
        Integer Events (100),
     &          Number_Of_Events,
     &          MAX_NUMBER_OF_EVENTS
        Dimension Time_OF_Events (100)
        Common /Event_Com/ Time_Of_Events,
     &                     Events,
     &                     Number_Of_Events
        Parameter (MAX_NUMBER_OF_EVENTS = 100)
