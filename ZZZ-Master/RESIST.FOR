      SUBROUTINE RESIST(UTRN,ITYP,OHMS,NUMV)                            
C                                                                       
C     THIS SUBROUTINE COMPUTES THE ELECTRICAL RESISTANCE IN OHMS        
C         (SUM OF INTERNAL AND EXTERNAL RESISTANCE PER MOTOR)           
C         AS A FUNCTION OF THE TRAIN SPEED IN FT/SEC.                   
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C**** THE EXTERNAL RESISTANCE IN THE MOTOR CIRCUIT IS ADDED             
C     ONLY DURING FULL POWER ACCELERATION IF TPOPT=1                    
C                                                                       
      IF (TPOPT-1)  20,20,40                                            
C                                                                       
   20 MOD2 = MODEV(NUMV)+1                                              
      GO TO (300,300,40,300,300,300,300,40), MOD2                       
40    IF (UTRN-U3M(ITYP)) 50,300,300                                    
C                                                                       
                                                                        
   50 IF(UTRN - U2M(ITYP))100,200,200                                   
C****                                                                   
C**** TRAIN SPEED IS BETWEEN 0 AND U2M FPS                              
C****                                                                   
  100 OHMS=RE1M(ITYP) + UTRN*SRE1M(ITYP)                                
      RETURN                                                            
C****                                                                   
C**** TRAIN SPEED IS BETWEEN U2M AND U3M FPS                            
C****                                                                   
  200 OHMS=RE2M(ITYP)+(UTRN - U2M(ITYP))*SRE2M(ITYP)                    
      RETURN                                                            
C****                                                                   
C**** TRAIN SPEED IS ON FLAT PORTION OF CURVE ABOVE U3M FPS             
C****     INTERNAL MOTOR RESISTANCE ONLY                                
C****                                                                   
  300 OHMS=RE3M(ITYP)                                                   
      RETURN                                                            
      END