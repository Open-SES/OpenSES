      SUBROUTINE FLYWHL (ITYP,NUMV)                                     
C                                                                       
C**** THIS SUBROUTINE SIMULATES THE DYNAMIC BEHAVIOR OF A FLYWHEEL.     
C     INPUT TO THIS SUBROUTINE ARE THE TRAIN SPEED, RATE OF ACCELERATION
C     (DECELERATION), MODE OF OPERATION, ETC.                           
C     OUTPUT FROM THE SUBROUTINE ARE --                                 
C     1- AMOUNT OF ENERGY STORED IN THE FLYWHEEL, (FT-LBS)/ TRAIN.      
C     2- THE FLYWHEEL ROTATIONAL SPEED, (REV/SEC).                      
C     3- THE QUANTITY OF HEAT RESULTING FROM THE INEFFICIENCIES         
C        OF POWER CONVERSION AND IS RELEASED INSTANTANEOUSLY,           
C        (BTU/(SEC-TRAIN)).                                             
C     4- THE QUANTITY OF HEAT TO THE BRAKING GRIDS, (BTU/(SEC-TRAIN))   
C     5- THE TRACTION POWER DRAWN FROM THE THIRD RAIL, (WATTS/TRAIN)    
C     6- THE TRACTION POWER DRAWN FROM THE FLYWHEEL, (WATTS/TRAIN)      
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      POWREQ = 0.0                                                      
C**** CALCULATE THE EFFECTIVE WEIGHT OF THE TRAIN                       
      WTRN = WV(ITYP) * RRACC(ITYP) + WPATV(NUMV) / GRACC               
C**** COMPUTE THE INTERNAL LOSSES OF THE FLYWHEEL ASSEMBLY              
      ROT = (ALOSSV(ITYP)*RPS(NUMV)**DLOSSV(ITYP) + BLOSSV(ITYP)*RPS(NUM
     1V)**ELOSSV(ITYP) + CLOSSV(ITYP)) * 1000.0   * WTBTUS * WEELSV(ITYP
     2)*NPCARV(ITYP)*BTUFLB                                             
C**** BRANCH ON TRAIN MODE OF OPERATION TO DETERMINE THE STATE OF THE   
C     FLYWHEEL                                                          
C                                                                       
      NUMY1 = MODEV(NUMV) + 1                                           
      GO TO (300,100,100,200,200,300,100,100),NUMY1                     
100   IF( TEV(NUMV) ) 200,300,110                                       
C                                                                       
C**** DISCHARGING CYCLE - THE TRAIN IS MOTORING, THEREFORE ENERGY IS    
C     BEING TRANSFERRED FROM THE FLYWHEEL TO THE TRACTION MOTORS.       
C                                                                       
C**** CALCULATE THE RATE OF CHANGE OF TRAIN ENERGY                      
110   DEDT = (WTRN * DUDTV(NUMV) + RSISTV(NUMV)) * UV(NUMV)             
C                                                                       
C**** CALCULATE THE RATE OF CHANGE OF KINETIC ENERGY IN THE FLYWHEEL    
      POW2= ( - DEDT / ETA2V(ITYP)) - ROT                               
C                                                                       
C**** COMPARE THE OLD ROTATIONAL SPEED WITH THE MINIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPS(NUMV) - RPSMNV(ITYP) ) 140,140,120                        
C**** CALCULATE AND STORE THE NEW VALUE OF KINETIC ENERGY OF THE        
C     FLYWHEEL                                                          
120   FKENEW(NUMV) = FKENEW(NUMV) + 0.5 * DELT * (POW2 + POW1(NUMV))    
C                                                                       
C**** CALCULATE THE NEW ROTATIONAL SPEED.                               
      RPSNEW = SQRT(2.0*GRACC*FKENEW(NUMV)/FLYJV(ITYP))/(2.0*PI)        
C**** COMPARE THE NEW ROTATIONAL SPEED WITH THE MINIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPSNEW - RPSMNV(ITYP) ) 130,135,135                           
130   RPSNEW = RPSMNV(ITYP)                                             
C**** CALCULATE THE QUANTITY OF HEAT GENERATED BY THE INEFFICIENCIES IN 
C     POWER CONVERSION PLUS THE FLYWHEEL WINDAGE AND BEARINGS LOSSES.   
135   QINST = (DEDT * (1.0 - ETA2V(ITYP))/ ETA2V(ITYP) + ROT) * FLBBTU  
      QGRIDS = 0.0                                                      
      RPS(NUMV) = RPSNEW                                                
      FKENEW(NUMV) = (2.0*PI*RPSNEW)**2*FLYJV(ITYP) / ( 2.0 * GRACC )   
      POW1(NUMV) = POW2                                                 
      AMPEQV = AMPLV(NUMV)                                              
      AMPLV(NUMV)= 0.0                                                  
C**** CALCULATE THE POWER BEING DRAWN FROM THE FLYWHEEL BY THE TRACTION 
C     MOTORS                                                            
      PFLYV(NUMV) = AMPEQV * VOLTSV(ITYP) * NPCARV(ITYP)                
      GO TO 500                                                         
C                                                                       
C**** THE FLYWHEEL CANNOT GIVE UP ANY MORE ENERGY- SWITCH TO 3RD RAIL   
C     FOR TRACTION POWER PLUS THE POWER TO OVERCOME THE FLYWHEEL        
C     INTERNAL LOSSES, THUS MAINTAINING THE FLYWHEEL AT THE MINIMUM RPM.
C                                                                       
140   RPSNEW = RPSMNV(ITYP)                                             
      RPS(NUMV) = RPSNEW                                                
      FKENEW(NUMV) = (2.0*PI*RPSNEW)**2*FLYJV(ITYP) / ( 2.0 * GRACC )   
      POW2= 0.0                                                         
      POW1(NUMV) = 0.0                                                  
      PFLYV(NUMV) = 0.0                                                 
      QGRIDS = 0.0                                                      
C**** FIND THE CHOPPER LOSS COEFFICIENT                                 
      IF( UV(NUMV) - UCHPRV(ITYP) ) 142,142,141                         
141   CHOPER = CHPR2V(ITYP)                                             
      GO TO 143                                                         
142   CHOPER = CHPR1V(ITYP)                                             
C**** CALCULATE THE INSTANTANEOUS HEAT RELEASED - ( BTU/(SEC-TRAIN) )   
C     NOTE-- THE FACTOR 0.85 IS AN ASSUMED EFFICIENCY FOR THE           
C     FLYWHEEL MOTOR AND GEAR TRAIN.                                    
143   QINST = (AMPV(NUMV)**2*RE3M(ITYP)*MOTORV(ITYP) + AMPLV(NUMV) *    
     1VOLTSV(ITYP) * CHOPER * NPCARV(ITYP)) * WTBTUS + ROT * FLBBTU/0.85
C                                                                       
C**** CALCULATE THE POWER REQUIRED FROM THE 3RD RAIL - (WATTS/TRAIN)    
C     NOTE-- THE FACTOR 0.85 IS AN ASSUMED EFFICIENCY FOR THE           
C     FLYWHEEL MOTOR AND GEAR TRAIN.                                    
C                                                                       
      POWREQ=(VOLTSV(ITYP) * AMPLV(NUMV) * NPCARV(ITYP)) +  ROT/0.85 *  
     1FLBBTU / WTBTUS                                                   
      GO TO 500                                                         
C                                                                       
C**** CHARGING CYCLE - THE TRAIN IS BRAKING, THEREFORE ENERGY IS BEING  
C     TRANSFERRED FROM THE TRAIN TO THE FLYWHEEL.                       
C                                                                       
C**** CALCULATE THE RATE OF CHANGE OF TRAIN ENERGY                      
200   DEDT = (- WTRN * DUDTV(NUMV) - RSISTV(NUMV)) * UV(NUMV)           
C                                                                       
C**** CALCULATE THE RATE OF CHANGE OF KINETIC ENERGY IN THE FLYWHEEL    
      POW2 = DEDT * ETA1V(ITYP) - ROT                                   
C                                                                       
C**** COMPARE THE OLD ROTATIONAL SPEED WITH THE MAXIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPS(NUMV) - RPSMXV(ITYP) ) 210,250,250                        
C                                                                       
C**** THE FLYWHEEL CAN ACCEPT ENERGY --                                 
C     CALCULATE AND STORE THE NEW VALUE OF KINETIC ENERGY OF THE        
C     FLYWHEEL.                                                         
210   FKENEW(NUMV) = FKENEW(NUMV) + 0.5 * DELT * (POW2 + POW1(NUMV))    
C**** CALCULATE THE NEW ROTATIONAL SPEED.                               
      RPSNEW = SQRT(2.0*GRACC*FKENEW(NUMV)/FLYJV(ITYP))/(2.0*PI)        
C**** COMPARE THE NEW ROTATIONAL SPEED WITH THE MAXIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPSNEW - RPSMXV(ITYP) ) 230,230,220                           
C                                                                       
C**** THE FLYWHEEL HAS JUST REACHED ITS STORAGE CAPACITY.               
C                                                                       
220   RPSNEW = RPSMXV(ITYP)                                             
C**** COMPUTE THE QUANTITY OF HEAT GENERATED BY THE INEFFICIENCIES IN   
C     POWER CONVERSION PLUS THE INTERNAL FLYWHEEL LOSSES.               
230   QGRIDS = 0.0                                                      
      QINST = (DEDT * (1.0 - ETA1V(ITYP))+ ROT) * FLBBTU                
      RPS(NUMV) = RPSNEW                                                
      FKENEW(NUMV) = (2.0*PI*RPSNEW)**2*FLYJV(ITYP) / ( 2.0 * GRACC )   
      POW1(NUMV) = POW2                                                 
      AMPV(NUMV) = 0.0                                                  
      AMPLV(NUMV)= 0.0                                                  
      PFLYV(NUMV) = 0.0                                                 
      GO TO 500                                                         
C                                                                       
C**** THE FLYWHEEL CANNOT ACCEPT ANYMORE ENERGY --                      
C                                                                       
250   RPS(NUMV) = RPSMXV(ITYP)                                          
C                                                                       
C**** CALCULATE THE QUANTITY OF HEAT GENERATED --                       
C                                                                       
      QINST = ( ROT / ETA1V(ITYP) ) * FLBBTU                            
      QGRIDS = DEDT * FLBBTU - QINST                                    
      POW2 = 0.0                                                        
      POW1(NUMV) = POW2                                                 
      AMPLV(NUMV)= 0.0                                                  
      AMPV(NUMV) = 0.0                                                  
      FKENEW(NUMV) = (2.0*PI*RPS(NUMV))**2*FLYJV(ITYP) / ( 2.0*GRACC )  
      PFLYV(NUMV) = 0.0                                                 
      GO TO 500                                                         
C                                                                       
C**** STORAGE CYCLE - THE FLYWHEEL IS IDLING. IF THE FLYWHEEL REACHES   
C     MINUMUM ALLOWABLE ROTATIONAL SPEED, POWER WILL BE DRAWN FROM THE  
C     3RD RAIL TO OFF-SET THE INTERNAL LOSSES OF THE FLYWHEEL.          
C                                                                       
300   DRAW = 0.0                                                        
      IF( MODEV(NUMV) - 5 ) 320,310,320                                 
C                                                                       
C**** CHECK IF BRAKES ARE BEING APPLIED TO MAINTAIN A SPEED LIMIT.      
310   IF( TEV(NUMV) ) 315,320,320                                       
C                                                                       
C****  THE TRAIN IS BRAKING - SWITCH TO CHARGING CYCLE                  
315   GO TO 200                                                         
C                                                                       
C**** CALCULATE THE RATE OF CHANGE OF KINETIC ENERGY STORED IN THE      
C     FLYWHEEL                                                          
320   POW2 = - ROT                                                      
C**** COMPARE THE OLD ROTATIONAL SPEED WITH THE MINIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPS(NUMV) - RPSMNV(ITYP) ) 340,340,330                        
C**** CALCULATE AND STORE THE NEW VALUE OF KINETIC ENERGY OF THE        
C     FLYWHEEL.                                                         
330   FKENEW(NUMV) = FKENEW(NUMV) + 0.5 * DELT * ( POW2 + POW1(NUMV))   
C**** CALCULATE THE NEW ROTATIONAL SPEED                                
      RPSNEW = SQRT(2.0*GRACC*FKENEW(NUMV)/FLYJV(ITYP))/(2.0*PI)        
C**** COMPARE THE NEW ROTATIONAL SPEED WITH THE MINIMUM ALLOWABLE       
C     ROTATIONAL SPEED.                                                 
      IF( RPSNEW - RPSMNV(ITYP) ) 340,350,350                           
C                                                                       
C**** THE FLYWHEEL IS DRAWING POWER FROM THE 3RD RAIL TO OFF-SET THE    
C     INTERNAL LOSSES.                                                  
340   DRAW = 1.0                                                        
      RPSNEW = RPSMNV(ITYP)                                             
      FKENEW(NUMV) = (2.0*PI*RPSNEW)**2*FLYJV(ITYP) / ( 2.0 * GRACC )   
      POW2 = 0.0                                                        
C**** CALCULATE THE INSTANTANEOUS HEAT RELEASED                         
C     NOTE-- THE FACTOR 0.85 IS AN ASSUMED EFFICIENCY FOR THE           
C     FLYWHEEL MOTOR AND GEAR TRAIN.                                    
C                                                                       
350   QINST = ROT* (1.0 + (1.0/0.85-1.0)* DRAW ) * FLBBTU               
C**** CALCULATE THE POWER REQUIRED FROM THE 3RD RAIL                    
      POWREQ = ROT / 0.85* DRAW                                         
      POW1(NUMV) = POW2                                                 
      AMPLV(NUMV) = 0.0                                                 
      AMPV(NUMV)  = 0.0                                                 
      PFLYV(NUMV) = 0.0                                                 
      QGRIDS = 0.0                                                      
C                                                                       
C**** STORE THE VALUES OF POWER FROM THE 3RD RAIL, HEAT TO BRAKING GRIDS
C     AND HEAT RELEASED INSTANTANEOUSLY                                 
500   PPROPV(NUMV) = POWREQ                                             
      QACCV(NUMV) = QINST                                               
      QDECV(NUMV) = QGRIDS                                              
C     WRITE(OUT,91) RPS(NUMV),FKENEW(NUMV),POW1(NUMV),ROT,PPROPV(NUMV), 
C    1 PFLYV(NUMV),QDECV(NUMV),QACCV(NUMV),AMPV(NUMV),AMPLV(NUMV)       
C 91  FORMAT(F10.2,2X,5E10.4,2X,4F10.2)                             
      RETURN                                                            
      END                                                               
