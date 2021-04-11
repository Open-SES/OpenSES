      SUBROUTINE  HEAT( NUMV, ITYP, TE, AMPS, AMPL )                    
C                                                                       
C*****THIS SUBROUTINE COMPUTES THE RATE OF HEAT INPUT (IN BTU/SEC/ENTIRE
C*****TRAIN) INTO BOTH THE ACCLERATION RESISTOR GRIDS (QACC OR QACCV)   
C*****AND THE DECELERATION RESISTOR GRIDS (QDEC OR QDECV) FOR TRAIN     
C*****PERFORMANCE OPTIONS 1 AND 2. IN ADDITION THE MOTOR CURRENT (AMPS  
C*****OR AMPV) AND THE LINE CURRENT (AMPL OR AMPLV) ARE SET TO ZERO IF  
C*****THE TRAIN MOTORS ARE NOT BEING USED                               
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      QACC=0.0                                                          
      QDEC=0.0                                                          
C*****COMPUTE THE ENERGY INPUT (IN BTU/SEC/ENTIRE TRAIN) TO THE TRAIN   
C*****CAUSED BY FORCES ACTING UPON IT                                   
      DELPE= -RSISTV(NUMV) * UV(NUMV) * FLBBTU                          
C*****FIND THE CHOPPER LOSS COEFFICIENT - IF ANY                        
      IF ( UV(NUMV) - UCHPRV(ITYP) ) 12,12,11                           
11    CHOPER = CHPR2V(ITYP)                                             
      GO TO 13                                                          
12    CHOPER = CHPR1V(ITYP)                                             
13    GO TO( 20, 600, 700 ), TPOPT                                      
C                                                                       
C*****IMPLICIT TRAIN PERFORMANCE                                        
C                                                                       
   20 NUMY1=MODEV(NUMV)+1                                               
      GO TO (200,200,300,400,400,202,300,300),NUMY1                     
  200 IF(DELPE) 210,215,215                                             
  202 IF(DELPE) 225,204,204                                             
  204 IF( TE ) 220,225,225                                              
  210 DELPE = 0.0                                                       
  215 IF( TE ) 220,220,300                                              
  220 QDEC = DELPE                                                      
  225 AMPS = 0.0                                                        
      AMPL = 0.0                                                        
      GO TO 680                                                         
C*****TRAIN IS USING MOTORS - FIND MOTOR CIRCUIT RESISTANCE             
  300 CALL RESIST( UV(NUMV), ITYP, OHMS, NUMV )                         
C*****COMPUTE HEAT INPUT TO ACCELERATION RESISTOR GRIDS                 
      QACC=(AMPS**2*OHMS*MOTORV(ITYP)+AMPL*VOLTSV(ITYP)*CHOPER*         
     1     NPCARV(ITYP))*WTBTUS                                         
      GO TO 680                                                         
C*****TRAIN IS BRAKING - FIND APPARENT MASS                             
  400 TMASS = WV(ITYP)*RRACC(ITYP) + WPATV(NUMV)/GRACC                  
C*****COMPUTE HEAT INPUT TO DECELERATION RESISTOR GRIDS                 
      QDEC = -TMASS * UV(NUMV) * DUDTV(NUMV) * FLBBTU + DELPE           
      IF(QDEC) 420,680,680                                              
  420 QDEC = 0.0                                                        
      GO TO 680                                                         
C                                                                       
C*****EXPLICIT TRAIN PERFORMANCE                                        
C                                                                       
  600 TMASS = WV(ITYP) * RRACC(ITYP) + WPATV(NUMV) /GRACC               
      QDEC = -TMASS *UV(NUMV) * DUDTV(NUMV) * FLBBTU + DELPE            
      IF( QDEC ) 610,620,620                                            
  610 QDEC = 0.0                                                        
  620 IF( TE ) 640,640,630                                              
  630 CALL RESIST( UV(NUMV), ITYP, OHMS, NUMV )                         
      QACC=(AMPS**2*OHMS*MOTORV(ITYP)+AMPL*VOLTSV(ITYP)*CHOPER*         
     1     NPCARV(ITYP))*WTBTUS                                         
      GO TO 680                                                         
  640 TE = 0.0                                                          
      AMPS = 0.0                                                        
      AMPL = 0.0                                                        
C*****INCORPORATE THE EFFECTS OF REGENERATIVE BRAKING                   
  680 QACCV(NUMV) = QACC                                                
      QDECV(NUMV) = QDEC * REGENV(ITYP)                                 
C---- COMPUTE PROPULSION POWER DRAWN FROM/AND REGENERATED TO THIRD RAIL 
      PPROPV(NUMV) = AMPL*VOLTSV(ITYP)*NPCARV(ITYP)                     
      PREGNV(NUMV) = QDEC*(1.0-REGENV(ITYP))/WTBTUS                     
  700 RETURN                                                            
      END                                                               
