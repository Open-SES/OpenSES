      SUBROUTINE AMPERE(TE,TEREQ,UTRN,ITYP)                             
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
C     INPUT TO THIS SUBROUTINE IS TRACTIVE EFFORT AVAILABLE, TRACTIVE   
C     EFFORT REQUIRED AND TRAIN SPEED.                                  
C     OUTPUT IS THE AMPERAGE REQUIRED BY THE MOTOR TO PRODUCE THIS      
C     TRACTIVE EFFORT AND THE CORRESPONDING LINE CURRENT DRAWN BY EACH  
C     POWERED CAR.                                                      
C     TRACTIVE EFFORT IS IN LBS AT THE WHEEL RIM AND TRAIN SPEED IS IN  
C     FEET PER SECOND.                                                  
C                                                                       
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      REAL     IBASE, ILMAX                                             
C                                                                       
C**** BASE SPEED, BASE CURRENT AND BASE TRACTIVE EFFORT                 
      UBASE = SP1(ITYP)                                                 
      IBASE = COAMP(ITYP,4)                                             
C**** MAXIMUM LINE CURRENT                                              
      IF ( NOPTV(ITYP) - 2 ) 2,4,4                                      
2     ILMAX = 2.0 * COAMP(ITYP,4)                                       
      GO TO 6                                                           
4     ILMAX = COAMPL(ITYP,4)                                            
C**** NORMALIZE SPEED AND TRACTIVE EFFORT ON BASE CONDITIONS            
6     TRATIO = TEREQ/TE                                                 
      IF(TRATIO .LT. 0.0 ) TRATIO=0.0                                   
      IF(TRATIO .GT. 1.0 ) TRATIO=1.0                                   
      IF (TE-TEREQ) 10,10,100                                           
C                                                                       
C**** THE TRAIN IS MOTORING AT FULL POWER - COMPUTE MOTOR CURRENT       
C                                                                       
C**** CHECK IF THE TRAIN SPEED IS GREATER THAN OR EQUAL TO BASE SPEED   
10     IF (UTRN - UBASE) 12,11,11                                       
11    IMOTOR = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN+COAMP(ITYP,3)/UTRN**2   
C**** CHECK IF AMPERAGE EXCEEDS MAXIMUM VALUE                           
      IF (IMOTOR-IBASE) 14,14,12                                        
12    IMOTOR = IBASE                                                    
C**** COMPUTE LINE CURRENT                                              
14    NUMY1 = NOPTV(ITYP)                                               
      GO TO (16,30), NUMY1                                              
C**** CAM CONTROLLER -- CHECK IF TRAIN SPEED IS ABOVE OR BELOW THE      
C     TRANSITION SPEED.                                                 
16    IF (UTRN-U2M(ITYP)) 18,20,20                                      
C**** THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE     
C     TRACTION MOTORS ARE IN SERIES.                                    
18    ILINE = IMOTOR                                                    
      GO TO 200                                                         
C**** THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE     
C     TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL                   
20    ILINE = IMOTOR*2.0                                                
      GO TO 200                                                         
C**** CHOPPER CONTROLLER -- CHECK IF TRAIN IS ABOVE OR BELOW BASE SPEED 
30    IF (UTRN-UBASE) 32,34,34                                          
C**** TRAIN IS BELOW BASE SPEED                                         
32    ILINE = COAMPL(ITYP,6)+SLOPEV(ITYP)*UTRN                          
      GO TO 200                                                         
C**** TRAIN IS ABOVE BASE SPEED                                         
34    ILINE = COAMPL(ITYP,1)+COAMPL(ITYP,2)/UTRN+COAMPL(ITYP,3)/UTRN**2 
C**** DETERMINE IF THE MAXIMUM LINE CURRENT HAS BEEN EXCEEDED           
      IF (ILINE-ILMAX) 200,200,36                                       
36    ILINE = ILMAX                                                     
      GO TO 200                                                         
C                                                                       
C**** THE TRAIN IS MOTORING AT PARTIAL POWER - COMPUTE MOTOR CURRENT    
C                                                                       
100   IF (UTRN-UBASE) 110,160,160                                       
C                                                                       
C**** THE TRAIN IS OPERATING BELOW BASE SPEED                           
110   IMOTOR = IBASE * SQRT(TRATIO)                                     
C**** COMPUTE LINE CURRENT                                              
      NUMY1 =NOPTV(ITYP)                                                
      GO TO (130,120), NUMY1                                            
C**** CHOPPER CONTROLLER                                                
120   ILINE = (COAMPL(ITYP,6)+SLOPEV(ITYP)*UTRN)*TRATIO               
      GO TO 200                                                         
C**** CAM CONTROLLER -- CHECK IF THE TRAIN SPEED IS ABOVE OR BELOW THE  
C     TRANSITION SPEED.                                                 
130   IF (UTRN-U2M(ITYP)) 140,150,150                                   
C --  THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE     
C     TRACTION MOTORS ARE IN SERIES.                                    
140   ILINE = IMOTOR                                                    
      GO TO 200                                                         
C --  THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE     
C     TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL                   
150   ILINE = IMOTOR*2.0                                                
      GO TO 200                                                         
C                                                                       
C**** THE TRAIN IS OPERATING ABOVE BASE SPEED.                          
C***                                                                    
  160 CURRNT = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN+COAMP(ITYP,3)/UTRN**2   
      ILINE = COAMPL(ITYP,1)+COAMPL(ITYP,2)/UTRN+COAMPL(ITYP,3)/UTRN**2 
      IMOTOR = CURRNT * SQRT(TRATIO)                                    
      ILINE  = ILINE  *      TRATIO                                     
C --  COMPUTE THE MAXIMUM CURRENT AT THIS TRAIN SPEED                   
C --  COMPARE THE CURRENT DRAWN DURING PARTIAL POWER MOTORING WITH      
C --  MAXIMUM CURRENT.                                                  
C***                                                                    
C --  COMPUTED THE MAXIMUM CURRENT AT THIS TRAIN SPEED                  
C --  COMPARE THE CURRENT DRAWN DURING PARTIAL POWER MOTORING WITH      
C --  MAXIMUM CURRENT.                                                  
      IF( CURRNT - IMOTOR ) 170,171,171                                 
170   IMOTOR = CURRNT                                                   
171   IF (ILINE-ILMAX) 200,200,190                                      
190   ILINE = ILMAX                                                     
200   RETURN                                                            
      END                                                               
