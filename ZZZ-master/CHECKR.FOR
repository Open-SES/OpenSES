      SUBROUTINE CHECKR(THING,REMIN,REMAX,ERTYP)                        
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      INTEGER ERTYP                                                     
      IF((REMAX-THING)*(THING-REMIN)+0.0001) 10,20,20                   
   10 CALL EERROR(ERTYP)                                                 
   20 RETURN                                                            
      END                                                               
