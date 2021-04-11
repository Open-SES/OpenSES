      SUBROUTINE CHECKI(ITEM,INTMIN,INTMAX,ERTYP)                       
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      INTEGER ERTYP                                                     
C                                                                       
      IF( INTMIN - ITEM ) 6,20,10                                       
    6 IF( ITEM - INTMAX ) 20,20,10                                      
C                                                                       
   10 CALL EERROR(ERTYP)                                                 
C                                                                       
   20 RETURN                                                            
C                                                                       
      END                                                               
