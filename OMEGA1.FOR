      SUBROUTINE OMEGA1                                                 
C                                                                       
C*****THIS SUBROUTINE COMPUTES THE AERODYNAMIC FORCING FUNCTION FOR EACH
C*****VENTILATION SHAFT SECTION. THE INERTIAL TERM IS NOT COMPUTED HERE 
C*****SINCE IT REMAINS CONSTANT. IT IS COMPUTED IN SUBROUTINE VSINS.    
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
5     FORMAT(T21,'VENTILATION SHAFT NUMBER',I3,' -',I3,'  FLOW CFM',
     1G12.2)                                                            
      TADJST = 1.0                                                      
C*****LOOP OVER ALL VENTILATION SHAFT SECTIONS                          
      DO 170 ISCT=NLSCP1,NSEC                                           
      Q=QS(ISCT)                                                        
      VLOWS(ISCT)=Q/ALOWS(ISCT)                                         
      VHIGHS(ISCT)=Q/AHIGHS(ISCT)                                       
C**** OBTAIN VENT SHAFT NUMBER                                          
      IVS = IABS(TABL9(ISCT))                                           
C                                                                       
C**** CHECK THE FIRE SIMULATION OPTION -- IF A FIRE IS BEING SIMULATED, 
C**** ADJUST VARIABLES BY TEMPERATURE RATIO                             
C                                                                       
      IF (FIROP2 .LE. 0)  GO TO 8                                       
C                                                                       
C**** FIRE SIMULATION                                                   
C                                                                       
      ISSLVS = TABL5(IVS)                                               
      TADJST = TSSTAB(ISSLVS)                                           
      Q = Q*TADJST                                                      
      ABSQ = ABS(Q)                                                     
      TAVGVS = TADJST*ABSAMB - ABTEMP                                   
      VISCOS = AIR(TAVGVS,1)                                            
      REVS(IVS) = ABSQ*RNFVS(IVS)*(VISAIR/RHOMAS)/VISCOS                
      SUM1 = -Q*(CEPS(ISCT)*(Q+ABSQ)-CENS(ISCT)*(Q-ABSQ))/TADJST        
      GO TO 9                                                           
C                                                                       
C*****REYNOLDS NUMBER COMPUTATION                                       
C                                                                       
8     ABSQ = ABS(Q)                                                     
      REVS(IVS)=ABSQ*RNFVS(IVS)                                         
C                                                                       
C*****COMPUTE SUM OF ALL HEAD CHANGES OCCURRING AT THE ENDS OF ALL      
C*****VENTILATION SHAFT SEGMENTS IN THIS VENTILATION SHAFT SECTION      
C                                                                       
      SUM1=-Q*(CEPS(ISCT)*(Q+ABSQ)-CENS(ISCT)*(Q-ABSQ))                 
9     OMEGS(ISCT)=SUM1                                                  
C                                                                       
C*****FAN GAIN COMPUTATION                                              
C                                                                       
      IFT=FTYPVS(IVS)                                                   
C*****CHECK FOR POWERED OR UNPOWERED VENTILATION SHAFT                  
      IF(IFT) 140,140,10                                                
C*****CHECK FAN OPERATING TIME TO SEE IF IT'S TURNED ON                 
10    NUMY1=NTIME-FTONVS(IVS)                                           
      IF(NUMY1) 140,20,20                                               
C*****FAN OPERATING DIRECTION COMPUTATION                               
20    IDIR=FDIRVS(IVS)                                                  
C*****CHECK FAN OPERATING TIME TO SEE IF IT'S TURNED OFF                
      NUMY2=NTIME-FTOFVS(IVS)                                           
      IF(NUMY2) 30,30,60                                                
30    DUMY1=IDIR*Q                                                      
C*****CHECK FOR FAN WINDMILLING OR STOPPING                             
      IF((CFMHFT(IFT)-DUMY1)*(DUMY1-CFMLFT(IFT))+0.001) 40,80,80        
C*****IF FAN IS WINDMILLING OR STOPPING, SHUT FAN OFF AND PRINTOUT      
C*****ERROR MESSAGE                                                     
40    FTOFVS(IVS)=NTIME-1                                               
      DUMY1=60.0*Q                                                      
      WRITE(OUT,5) TABL11(ISCT), NUMVS(IVS), DUMY1                      
      CALL SIMERR(6)                                                    
C*****DECIDE WHETHER TO TERMINATE SIMULATION OR NOT                     
      GO TO (50,140), FSCFT                                             
C*****SIMULATION TERMINATION                                            
50    NEW=1                                                             
      GO TO 180                                                         
C*****SIMULATION CONTINUATION                                           
C*****SPOOL IS ACTUAL FAN PRESSURE/THEORETICAL FAN PRESSURE AND GOES TO 
C*****0 ASYMPTOTICALLY AS THE FAN RUNS DOWN IN THE MANNER OF E**(K*T**2)
C*****WHERE K IS NEGATIVE AND T TIME IS ELAPSED FROM FAN SHUTDOWN       
60    SPOOL=0.0                                                         
      DUMY1=TIMFT(IFT)*((FLOAT(NUMY2+1))**2)                            
C*****THE FOLLOWING STATEMENT IS USED TO PREVENT OVERFLOW OF THE EXP    
C*****FUNCTION                                                          
      IF(DUMY1+80) 100,70,70                                            
70    SPOOL=EXP(DUMY1)                                                  
      GO TO 100                                                         
C*****SPOOL IS ACTUAL FAN PRESSURE/THEORETICAL FAN PRESSURE AND GOES TO 
C*****1 ASYMPTOTICALLY AS THE FAN RUNS UP IN THE MANNER OF 1-E**(K*T**2)
C*****WHERE K IS NEGATIVE AND T TIME IS ELAPSED FROM FAN STARTUP        
80    SPOOL=1.0                                                         
      DUMY1=TIMFT(IFT)*((FLOAT(NUMY1+1))**2)                            
C*****THE FOLLOWING STATEMENT IS USED TO PREVENT OVERFLOW OF THE EXP    
C*****FUNCTION                                                          
      IF(DUMY1+80.0) 100,90,90                                          
90    SPOOL=1.0-EXP(DUMY1)                                              
100   IF(IDIR) 110,120,120                                              
C*****FAN GAIN COMPUTATION FOR FAN OPERATING IN INFLOW(SUPPLY) DIRECTION
110   SUM2=Q*(Q*(Q*FN1IFT(IFT)+FN2IFT(IFT))+FN3IFT(IFT))+FN4IFT(IFT)    
      GO TO 130                                                         
C*****FAN GAIN COMPUTATION FOR FAN OPERATING IN                         
C*****OUTFLOW(EXHAUST) DIRECTION                                        
120   SUM2=Q*(Q*(Q*FN1OFT(IFT)+FN2OFT(IFT))+FN3OFT(IFT))+FN4OFT(IFT)    
130   OMEGS(ISCT)=OMEGS(ISCT)+SPOOL*SUM2/TADJST                         
C                                                                       
C**** ADD BUOYANCY GAIN (COMPUTED IN PINPNT)                            
C                                                                       
  140 OMEGS(ISCT) = OMEGS(ISCT) + BUOYS(ISCT)                           
C                                                                       
C*****STORE MAXIMUM POSITIVE AND NEGATIVE PRESSURE CHANGES AND THE TIMES
C*****OF THEIR OCCURANCES FOR SUMMARY USE                               
C     CALL MMAX (OMEGS(ISCT),FPMXVS(IVS),MXFPVS(IVS))                    
C     CALL MMIN (OMEGS(ISCT),FPMNVS(IVS),MNFPVS(IVS))                    
170   CONTINUE                                                          
180   RETURN                                                            
      END                                                               
