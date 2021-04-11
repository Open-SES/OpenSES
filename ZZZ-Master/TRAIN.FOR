      SUBROUTINE TRAIN                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C*****THIS SUBROUTINE IS THE DRIVING PORTION OF THE SES TRAIN           
C*****PERFORMANCE SUBPROGRAM. IT HAS THREE MAJOR PARTS. THEY ARE        
C***** 1) THE TRAIN 'DISPATCHER' WHICH PLACES TRAINS IN OPERATION ON ALL
C*****    ROUTES AND REMOVES FROM OPERATION ANY TRAINS THAT HAVE GONE   
C*****    BEYOND THE LAST TRACK SECTION OF THEIR ROUTE                  
C***** 2) THE IMPLICIT TRAIN PERFORMANCE WHICH COMPUTES THE LOCATION,   
C*****    SPEED, ACCELERATION, ETC. OF EACH TRAIN WHEN THE IMPLICIT     
C*****    TRAIN PERFORMANCE OPTION IS SPECIFIED (TPOPT=1)               
C***** 3) THE EXPLICIT TRAIN PERFORMANCE WHICH COMPUTES THE LOCATION,   
C*****    SPEED, ACCELERATION, ETC. OF EACH TRAIN WHEN THE EXPLICIT     
C*****    TRAIN PERFORMANCE OPTION IS SPECIFIED (TPOPT=2,3)             
C                                                                       
C*****TRAIN OPERATING MODES                                             
C***** IMPLICIT TRAIN PERFORMANCE - TPOPT=1                             
C*****    MODE 0 = STOPPED                                              
C*****         1 = MAINTAINING CONSTANT SPEED                           
C*****         2 = ACCELERATING AT FULL AVAILABLE POWER                 
C*****         3 = BRAKING (DECELERATION RATE VARIES LINEARLY WITH      
C*****                      RESPECT TO TRAIN SPEED)                     
C*****         4 = BRAKING (CONSTANT DECELERATION RATE)                 
C*****         5 = COASTING                                             
C*****         6 = MAINTAINING MINIMUM SPEED IN A TRACK SECTION WHERE   
C*****             COASTING IS PERMITTED                                
C*****         7 = ATTEMPTING TO MAINTAIN CONSTANT MINIMUM SPEED IN A   
C*****             TRACK SECTION WHERE COASTING IS PERMITTED            
C***** EXPLICIT TRAIN PERFORMANCE - HEAT REJECTION COMPUTED - TPOPT=2   
C*****    MODE 1 = ALL ACTIVITIES                                       
C***** EXPLICIT TRAIN PERFORMANCE - HEAT REJECTION INPUT - TPOPT=3      
C*****    MODE 1 = ALL ACTIVITIES                                       
C                                                                       
C*****INITIALIZE NUMBER OF TRAINS IN EACH SEGMENT TO ZERO               
      IF (NLS) 1280,30,10                                               
   10 DO 20 ILS=1,NLS                                                   
   20 NTRNLS(ILS)=0                                                     
C                                                                       
C                                                                       
C*****DISPATCHER - FIRST 'REMOVE', THEN 'DISPATCH'                      
C                                                                       
C                                                                       
C*****SKIP DISPATCHER IF NOT TIME FOR COMPLETE TRAIN EVALUATION         
   30 IF (NTIME-TRTIME) 240,40,40                                       
C*****IF NO TRAINS ARE OPERATING THEN NONE MUST BE REMOVED              
   40 IF (NTRAN) 150,150,50                                             
C*****LOOP OVER OPERATING TRAINS CHECKING IF THEY SHOULD BE REMOVED     
C*****INITIALIZE INDEX OF OPERATING TRAINS                              
   50 JTRAIN=0                                                          
C*****INCREMENT INDEX OF OPERATING TRAINS                               
   60 JTRAIN=JTRAIN+1                                                   
C*****FIND TRAIN NUMBER, ROUTE, AND NUMBER OF TRACK SECTIONS IN ROUTE   
   70 NUMV=IINDEX(JTRAIN)                                                
      IROUTE=IROTEV(NUMV)                                               
      NTS=NTSRT(IROUTE)                                                 
C*****REMOVE TRAIN FROM OPERATION WHEN FRONT IS BEYOND LAST T.S.        
      IF (SNGL( XV(NUMV))-FSTS(NTS,IROUTE)) 80,80,100                   
   80 GO TO (140,90,90), TPOPT                                          
C*****REMOVE FROM OPERATION TRAINS THAT HAVE PASSED LAST POINT          
C*****IN SPEED-TIME PROFILE                                             
   90 IF (NEXPDP(IROUTE)-DPV(NUMV)) 100,100,140                         
C*****REMOVE TRAIN FROM OPERATION - REDUCE INDEX OF ACTIVE TRAINS       
C*****AND RE-PACK ARRAY 'IINDEX'                                         
  100 IF (JTRAIN-LMTRAN) 110,130,130                                    
  110 I1=LMTRAN-1                                                       
      DO 120 I=JTRAIN,I1                                                
  120 IINDEX(I)=IINDEX(I+1)                                               
C*****THIS ENTRY MEANS THAT THE ENTRIES IN IINDEX FROM IINDEX(NTRAN+1)    
C*****TO IINDEX(LMTRAN) CONTAIN THE 'NUMV' THAT ARE AVAILABLE FOR REUSE  
      IINDEX(LMTRAN)=NUMV                                                
C*****DECREASE THE NUMBER OF OPERATING TRAINS BY ONE                    
  130 NTRAN=NTRAN-1                                                     
C*****CHECK IF TRAIN REMOVED WAS LAST TRAIN NEEDING REMOVAL CHECK       
      IF (JTRAIN-NTRAN) 70,70,150                                       
C*****CHECK IF ALL TRAINS HAVE BEEN CHECKED FOR REMOVAL                 
  140 IF (JTRAIN-NTRAN) 60,150,150                                      
C*****TRAIN DISPATCHING FOR ALL TRAIN ROUTES                            
  150 DO 230 IROUTE=1,NTRRTE                                            
      IGROUP=JGROUP(IROUTE)                                             
C*****CHECK IF TRAIN GROUP INFORMATION FOR ROUTE HAS BEEN EXHAUSTED     
      IF (IGROUP-NGRV(IROUTE)) 160,160,230                              
  160 IF (NTIME-NTIMTR(IROUTE)) 230,170,170                             
C*****PLACE A NEW TRAIN INTO OPERATION                                  
  170 NTRAN=NTRAN+1                                                     
C*****CHECK IF AN ATTEMPT IS BEING MADE TO PLACE TOO MANY TRAINS IN     
C*****OPERATION AT ONCE - IF SO, DO NOT PLACE TRAIN IN OPERATION AND    
C*****HOLD ALL DISPATCHING ON THIS ROUTE FOR ONE HEADWAY                
      IF (NTRAN-LMTRAN) 190,190,180                                     
  180 CALL SIMERR (1)                                                   
      NTRAN=NTRAN-1                                                     
      NTIMTR(IROUTE)=NTIME+GRHDWY(IGROUP,IROUTE)                        
      GO TO 230                                                         
C*****FIND INTERNAL NUMBER OF NEWLY DISPATCHED TRAIN                    
  190 NUMV=IINDEX(NTRAN)                                                 
C*****INCREMENT TOTAL NUMBER OF TRAINS THAT HAVE ENTERED SYSTEM         
      NUMBTR=NUMBTR+1                                                   
C*****ASSIGN TRAIN EXTERNAL NUMBER - STORED ALPHAMERICALLY              
      CALL ALCHAR (NUMBTR,TNUMV(NUMV))                                  
C*****INCREMENT NUMBER OF TRAINS DISPATCHED FOR THIS GROUP              
      NMTRGR(IROUTE)=NMTRGR(IROUTE)+1                                   
C*****CHECK IF ALL TRAINS IN THIS GROUP HAVE BEEN DISPATCHED            
      IF (NMTRGR(IROUTE)-NTRGR(IGROUP,IROUTE)) 210,200,200              
  200 JGROUP(IROUTE)=JGROUP(IROUTE)+1                                   
C*****INITIALIZE NUMBER OF TRAINS DISPATCHED IN NEW GROUP               
      NMTRGR(IROUTE)=0                                                  
C*****COMPUTE TIME OF NEXT TRAIN DISPATCH FOR THIS ROUTE                
  210 KGROUP=JGROUP(IROUTE)                                             
      NTIMTR(IROUTE)=NTIME+GRHDWY(KGROUP,IROUTE)                        
C*****INITIALIZE OPERATIONAL TRAIN PARAMETERS ASSUMING IMPLICIT TRAIN   
C*****PERFORMANCE                                                       
      IROTEV(NUMV)=IROUTE                                               
      XV(NUMV)=DBLE(ORIGRT(IROUTE))                                     
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      UV(NUMV)=0.0                                                      
      TSTRTV(NUMV)=0                                                    
      ITYPV(NUMV)=GRTYPV(IGROUP,IROUTE)                                 
      ITYP=ITYPV(NUMV)                                                  
      WPATV(NUMV)=NPETTS(1,IROUTE)*PATWHT                               
      DUDTV(NUMV)=0.0                                                   
      MODEV(NUMV)=0                                                     
      TSTRTV(NUMV)=0                                                    
      QERDCV(NUMV)=0.0                                                  
      QERACV(NUMV)=0.0                                                  
      TGACCV(NUMV)=TIACCV(ITYP)                                         
      TGDECV(NUMV)=TIDECV(ITYP)                                         
      TLPSV(NUMV)=NDELT                                                 
      NTSV(NUMV)=2                                                      
      NTSV2(NUMV)=2                                                     
      DRAGV(NUMV)=0.0                                                   
      QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=0.0                                                   
      RMHTV(NUMV)=0.0                                                   
      RGSUMV(NUMV)=0.0                                                  
      TAAVGV(NUMV)=0.0                                                  
      TWAVGV(NUMV)=0.0                                                  
      ANVV(NUMV)=0.0                                                    
      RPS(NUMV) = RPSOV(ITYP)                                           
      FKENEW(NUMV) = SPINV(ITYP)                                        
      POW1(NUMV) = 0.0                                                  
      PREGNV(NUMV) = 0.0                                                
      PAUXV(NUMV) = PAUXEV(ITYP) + PAUXPV(ITYP)*NPETTS(1,IROUTE)        
      PFLYV(NUMV) = 0.0                                                 
      PPROPV(NUMV) = 0.0                                                
      QPRPV(NUMV) = 0.0                                                 
      QAXSV(NUMV) = SHREV(ITYP) + SHRPV(ITYP) * NPETTS(1,IROUTE)        
      QAXLV(NUMV) = LHREV(ITYP) + LHRPV(ITYP) * NPETTS(1,IROUTE)        
      GO TO (230,220,220), TPOPT                                        
C*****INITIALIZE OR MODIFY OPERATIONAL TRAIN PARAMETERS FOR EXPLICIT    
C*****TRAIN PERFORMANCE                                                 
  220 MODEV(NUMV)=1                                                     
      DPV(NUMV)=1                                                       
      UV(NUMV)=SPEEDV(1,IROUTE)                                         
      XLASTV(NUMV)=ORIGRT(IROUTE)                                       
      DUDTV(NUMV)=(SPEEDV(1,IROUTE)-SPEEDV(2,IROUTE))/(TIMEV(1,IROUTE)-T
     1IMEV(2,IROUTE))*100.0                                             
  230 CONTINUE                                                          
C*****IF NO TRAINS ARE OPERATIONAL THEN NO PERFORMANCE MUST BE COMPUTED 
  240 IF (NTRAN) 1280,1270,250                                          
  250 GO TO (260,1090,1090), TPOPT                                      
C                                                                       
C                                                                       
C*****IMPLICIT TRAIN PERFORMANCE                TPOPT = 1               
C                                                                       
C                                                                       
C*****LOOP OVER ALL OPERATING TRAINS                                    
  260 DO 1085 ITRAN=1,NTRAN                                             
C*****FIND TRAIN NUMBER, TYPE, ROUTE, AND TRACK SECTION                 
      NUMV=IINDEX(ITRAN)                                                 
      ITYP=ITYPV(NUMV)                                                  
      IROUTE=IROTEV(NUMV)                                               
      JTS=NTSV(NUMV)                                                    
C*****BRANCH ON MODE OF TRAIN                                           
      NUMY1=MODEV(NUMV)+1                                               
      GO TO (270,310,310,460,460,560,680,760), NUMY1                    
C                                                                       
C*****TRAIN IS STOPPED - MODE=0                                         
C                                                                       
C*****FIND TRAIN POSITION, AND ITS GRADE AND CURVE RESISTANCE           
  270 CALL LOCATE (NUMV,ITYP,IROUTE,RG,RC)                              
C*****CHECK IF TIME TO RESTART TRAIN                                    
      IF (TSTRTV(NUMV)-NTIME) 280,280,1080                              
C*****RESTART THE TRAIN                                                 
  280 MODEV(NUMV)=2                                                     
C*****FIND TRAIN TOTAL WEIGHT, GRADE AND CURVE RESISTANCES AND SUM OF   
C*****GRADE CURVE, AND ROLLING RESISTANCES                              
      WTRN=WV(ITYP)+WPATV(NUMV)                                         
      RG=RG*WTRN                                                        
      RC=RC*WTRN/TONLB                                                  
      RSISTV(NUMV)=CORMV(ITYP,1)*WTRN+CORMV(ITYP,2)+RG+RC               
C*****FIND TRACTIVE EFFORT AVAILABLE                                    
      TEV(NUMV)=TORQUE(0.0,ITYP)                                        
C*****FIND ACCELERATION TRAIN IS CAPABLE OF                             
      DUDTV(NUMV)=(TEV(NUMV)*MOTORV(ITYP)-RSISTV(NUMV))/(WV(ITYP)*RRACC(
     1ITYP)+WPATV(NUMV)/GRACC)                                          
C*****CHECK IF MAXIMUM ACCELERATION ALLOWED WOULD BE EXCEEDED           
      IF (DUDTV(NUMV)-ACCV(ITYP)) 300,300,290                           
C*****SET ACCELERATION TO MAXIMUM ALLOWED AND RECOMPUTE TRACTIVE EFFORT 
  290 DUDTV(NUMV)=ACCV(ITYP)                                            
      TEV(NUMV)=(DUDTV(NUMV)*(WV(ITYP)*RRACC(ITYP)+WPATV(NUMV)/GRACC)+RS
     1ISTV(NUMV))/MOTORV(ITYP)                                          
C*****COMPUTE MOTOR CURRENT AND LINE CURRENT                            
  300 CALL  AMPERE(TEV(NUMV),TEV(NUMV),0.0,ITYP)                        
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
C---- COMPUTE SENSIBLE AND LATENT HEAT FROM TRAIN AUXILIARIES           
      QAXSV(NUMV) = SHREV(ITYP)+SHRPV(ITYP)*WPATV(NUMV)/PATWHT          
      QAXLV(NUMV) = LHREV(ITYP)+LHRPV(ITYP)*WPATV(NUMV)/PATWHT          
C --  COMPUTE POWER DRAWN BY TRAIN AUXILIARIES                          
      PAUXV(NUMV) = PAUXEV(ITYP) + PAUXPV(ITYP)*WPATV(NUMV)/PATWHT      
      GO TO 1070                                                        
C                                                                       
C*****TRAIN IS MAINTAINING CONSTANT SPEED OR ACCELERATING - MODE=1,2    
C                                                                       
C*****APPROXIMATE NEW SPEED USING LAST VALUE OF ACCELERATION            
  310 UNEW=UV(NUMV)+DUDTV(NUMV)*DELT                                    
      UAVG=0.5*(UV(NUMV)+UNEW)                                          
C*****FIND TRACTIVE EFFORT AVAILABLE                                    
      TE=TORQUE(UAVG,ITYP)                                              
      TEV(NUMV)=TE                                                      
      IF (MODEV(NUMV)-1) 1280,320,390                                   
C                                                                       
C*****MAINTAINING CONSTANT SPEED  -  MODE=1                             
C                                                                       
C*****COMPUTE T.E. REQUIRED TO MAINTAIN SPEED                           
  320 TEREQ=RSISTV(NUMV)/MOTORV(ITYP)                                   
C*****CHECK IF AVAILABLE T.E. IS GREATER THAN REQUIRED T.E.             
      IF (TE-TEREQ) 330,340,340                                         
C*****SWITCH TO ACCELERATION MODE TO USE ALL AVAILABLE POWER - MODE=2   
  330 MODEV(NUMV)=2                                                     
      GO TO 390                                                         
C*****ENOUGH POWER IS AVAILABLE - COMPUTE MOTOR CURRENT AND LINE CURRENT
  340 CALL  AMPERE(TE,TEREQ,UAVG,ITYP)                                  
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      TEV(NUMV)=TEREQ                                                   
C*****COMPUTE NEW TRAIN LOCATION                                        
      XV(NUMV)=XV(NUMV)+DBLE(UV(NUMV)*DELT)                             
C*****CHECK IF TRAIN HAS EXCEEDED CURRENT SPEED RESTRICTION             
      IF (UNEW-ABS(UMXTS(JTS,IROUTE))+0.01) 370,350,350                 
C*****CHECK IF TRAIN HAS JUST ENTERED TRACK SECTION WHERE COASTING IS   
C*****PERMITTED WHILE RUNNING AT CONSTANT SPEED                         
  350 IF (UMXTS(JTS,IROUTE)) 360,820,820                                
C*****SWITCH TO COASTING MODE - MODE=5                                  
  360 MODEV(NUMV)=5                                                     
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      GO TO 820                                                         
C*****TRAIN IS BELOW MAXIMUM SPEED ALLOWED IN THIS TRACK SECTION. SWITCH
C*****TO ACCELERATION ONLY IF TRAIN IS BEYOND THE TRACK SECTION THAT IT 
C*****BRAKED FOR, OR, IF IT IS IN THE TRACK SECTION THAT IT BRAKED FOR  
C*****IT IS GOING TOO SLOW                                              
  370 IF (NTSV(NUMV)-NTSV2(NUMV)) 820,380,380                           
  380 MODEV(NUMV)=2                                                     
      GO TO 820                                                         
C                                                                       
C*****ACCELERATING MODE - MODE=2                                        
C                                                                       
  390 DUDTV(NUMV)=(TE*MOTORV(ITYP)-RSISTV(NUMV))/(WV(ITYP)*RRACC(ITYP)+W
     1PATV(NUMV)/GRACC)                                                 
C*****CHECK IF ACCELERATION EXCEEDS MAXIMUM ALLOWABLE                   
      IF (DUDTV(NUMV)-ACCV(ITYP)) 410,410,400                           
C*****SET ACCELERATION TO MAXIMUM ALLOWED AND RECOMPUTE TRACTIVE EFFORT 
  400 DUDTV(NUMV)=ACCV(ITYP)                                            
      TEV(NUMV)=(DUDTV(NUMV)*(WV(ITYP)*RRACC(ITYP)+WPATV(NUMV)/GRACC)+RS
     1ISTV(NUMV))/MOTORV(ITYP)                                          
C*****COMPUTE MOTOR CURRENT AND LINE CURRENT                            
  410 CALL  AMPERE(TE,TEV(NUMV),UAVG,ITYP)                              
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
C*****COMPUTE NEW TRAIN SPEED                                           
      UNEW=UV(NUMV)+DUDTV(NUMV)*DELT                                    
C*****COMPARE TRAIN SPEED TO MAXIMUM ALLOWED IN CURRENT TRACK SECTION   
      IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 420,430,430                      
C*****CONTINUE ACCELERATION                                             
C*****COMPUTE NEW TRAIN LOCATION                                        
  420 XV(NUMV)=XV(NUMV)+DBLE(0.5*DELT*(UV(NUMV)+UNEW))                  
C*****STORE NEW TRAIN SPEED                                             
      UV(NUMV)=UNEW                                                     
      GO TO 820                                                         
C*****COMPUTE NEW TRAIN LOCATION                                        
  430 DUDTV(NUMV)=0.0                                                   
      XV(NUMV)=XV(NUMV)+DBLE(0.5*DELT*(UV(NUMV)+ABS(UMXTS(JTS,IROUTE))))
C*****SET TRAIN SPEED EQUAL TO CURRENT OR ANTICIPATED SPEED             
C*****RESTRICTION                                                       
      UV(NUMV)=ABS(UMXTS(JTS,IROUTE))                                   
C*****CHECK IF SWITCH IS TO MAINTAINING CONSTANT SPEED OR COASTING MODE 
      IF (UMXTS(JTS,IROUTE)) 440,450,450                                
C*****SWITCH TO COASTING MODE - MODE=5                                  
  440 MODEV(NUMV)=5                                                     
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      GO TO 820                                                         
C*****SWITCH TO MAINTAINING CONSTANT SPEED MODE - MODE=1                
  450 MODEV(NUMV)=1                                                     
C*****SET TRACK SECTION TRAIN HAS BRAKED FOR TO CURRENT TRACK SECTION   
      NTSV2(NUMV)=JTS                                                   
      GO TO 820                                                         
C                                                                       
C*****BRAKING MODES - MODE=3,4                                          
C                                                                       
  460 UNEW=UV(NUMV)+DUDTV(NUMV)*DELT                                    
C*****CHECK IF TRAIN HAS STOPPED                                        
      IF (UNEW) 550,550,470                                             
C*****CHECK IF GOVERNING SPEED RESTRICTION HAS BEEN MET                 
  470 JTS=NTSV2(NUMV)                                                   
      IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 430,430,480                      
  480 XV(NUMV)=XV(NUMV)+DBLE(0.5*DELT*(UV(NUMV)+UNEW))                  
      UV(NUMV)=UNEW                                                     
      IF (MODEV(NUMV)-4) 490,820,820                                    
C*****COMPUTE NEW BRAKING RATE                                          
  490 IF (UNEW-DECV1V(ITYP)) 510,500,500                                
C*****ABOVE SPEED V1 - VARYING PORTION OF DECELERATION RATE CURVE       
  500 DUDTV(NUMV)=-DECBV(ITYP)-DECAV(ITYP)*UNEW                         
      GO TO 820                                                         
C*****BELOW SPEED V1 - CONSTANT DECELERATION RATE                       
  510 JTS2=NTSV2(NUMV)                                                  
      IF (ABS(UMXTS(JTS2,IROUTE))-0.1) 520,530,530                      
C*****TRAIN IS STOPPING FOR A STATION                                   
C*****COMPUTE STOPPING TIME AND DECELERATION RATE                       
  520 STTIM=(FSTS(JTS2-1,IROUTE)- SNGL(XV(NUMV)))*2.0/UV(NUMV)          
      IF( STTIM ) 537,537,525                                           
  525 DUDTV(NUMV)=-UV(NUMV)/STTIM                                       
      GO TO 540                                                         
C*****TRAIN IS SLOWING FOR A SPEED RESTRICTION                          
C*****ADJUST STOPPING TIME TO AN INTEGER DELTA T                        
  530 STTIM=(UV(NUMV)-ABS(UMXTS(JTS2,IROUTE)))/DECV(ITYP)               
      IINT=INT(STTIM/DELT+0.9999)                                       
      STTIM=FLOAT(IINT)*DELT                                             
      IF( STTIM ) 537,537,535                                           
  535 DUDTV(NUMV)=(ABS(UMXTS(JTS2,IROUTE))-UV(NUMV))/STTIM              
      GO TO 540                                                         
C**** USE MAXIMUM DECELERATION RATE                                     
  537 DUDTV(NUMV)=-DECV(ITYP)                                           
C*****SWITCH TO MODE 4                                                  
C*****USE THIS DECELERATION RATE FOR REMAINDER OF BRAKING CYCLE         
  540 MODEV(NUMV)=4                                                     
      GO TO 820                                                         
C*****TRAIN HAS STOPPED                                                 
  550 MODEV(NUMV)=0                                                     
      DUDTV(NUMV)=0.0                                                   
      UV(NUMV)=0.0                                                      
      JTS=NTSV2(NUMV)                                                   
      XV(NUMV)=DBLE(FSTS(JTS,IROUTE))                                   
C*****COMPUTE TIME OF RESTART                                           
      TSTRTV(NUMV)=NTIME+DWLTS(JTS,IROUTE)                              
      NTSV(NUMV)=NTSV2(NUMV)+1                                          
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=0.0                                                   
      RMHTV(NUMV)=0.0                                                   
C*****COMPUTE NEW WEIGHT OF PATRONS ON TRAIN                            
      WPATV(NUMV)=WPATV(NUMV)+NPETTS(JTS,IROUTE)*PATWHT                 
      GO TO 820                                                         
C                                                                       
C*****COASTING MODE - MODE=5                                            
C                                                                       
C*****COMPUTE NEW ACCELERATION                                          
  560 DUDTV(NUMV)=-RSISTV(NUMV)/(WV(ITYP)*RRACC(ITYP)+WPATV(NUMV)/GRACC)
C*****COMPUTE ANTICIPATED SPEED                                         
      UNEW=UV(NUMV)+DUDTV(NUMV)*DELT                                    
C*****IF ANTICIPATED SPEED IS LESS THAN UMIN THEN SWITCH TO ANOTHER MODE
      IF (UNEW-UMINRT(IROUTE)) 570,570,600                              
  570 UNEW=UMINRT(IROUTE)                                               
C*****COMPUTE NEW TRAIN LOCATION AND STORE NEW TRAIN SPEED              
      XV(NUMV)=XV(NUMV)+DBLE(0.5*DELT*(UV(NUMV)+UNEW))                  
      UV(NUMV)=UNEW                                                     
      DUDTV(NUMV)=0.0                                                   
      IF (COPTRT(IROUTE)) 1280,590,580                                  
C*****SWITCH TO ACCELERATING MODE - MODE=2                              
  580 MODEV(NUMV)=2                                                     
      GO TO 820                                                         
C*****SWITCH TO COASTING CONSTANT SPEED MODE - MODE=6                   
  590 MODEV(NUMV)=6                                                     
      GO TO 640                                                         
C*****IF ANTICIPATED SPEED IS GREATER THAN MAXIMUM ALLOWED THEN MAINTAIN
C*****MAXIMUM SPEED                                                     
  600 IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 620,610,610                      
  610 UNEW=ABS(UMXTS(JTS,IROUTE))                                       
C*****FIND 'NEGATIVE' TRACTIVE' EFFORT (BRAKING EFFORT) REQUIRED TO KEEP
C*****TRAIN FROM GOING TOO FAST                                         
      TEV(NUMV)=RSISTV(NUMV)/MOTORV(ITYP)                               
      DUDTV(NUMV)=0.0                                                   
      GO TO 630                                                         
  620 TEV(NUMV)=0.0                                                     
C*****COMPUTE NEW TRAIN LOCATION AND SPEED                              
  630 XV(NUMV)=XV(NUMV)+DBLE(0.5*DELT*(UV(NUMV)+UNEW))                  
      UV(NUMV)=UNEW                                                     
C*****CHECK IF TRAIN HAS LEFT COASTING TRACK SECTION                    
  640 IF (UMXTS(JTS,IROUTE)) 820,650,650                                
C*****TRAIN CAN NO LONGER COAST - SWITCH TO ANOTHER MODE                
  650 IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 670,660,660                      
C*****SWITCH TO MAINTAINING CONSTANT SPEED MODE - MODE=1                
  660 MODEV(NUMV)=1                                                     
      UV(NUMV)=ABS(UMXTS(JTS,IROUTE))                                   
      GO TO 820                                                         
C*****SWITCH TO ACCELERATING MODE - MODE=2                              
  670 MODEV(NUMV)=2                                                     
      GO TO 820                                                         
C                                                                       
C*****MAINTAINING CONSTANT MINIMUM SPEED IN A TRACK SECTION WHERE       
C*****COASTING IS PERMITTED MODE - MODE=6                               
C                                                                       
C*****FIND TRAIN SPEED                                                  
  680 UNEW=UV(NUMV)                                                     
C*****FIND TRACTIVE EFFORT REQUIRED                                     
      TEREQ=RSISTV(NUMV)/MOTORV(ITYP)                                   
C*****IF TRACTIVE EFFORT REQUIRED IS NEGATIVE THEN SWITCH TO COASTING   
C*****MODE - MODE=5                                                     
      IF (TEREQ) 690,690,700                                            
  690 MODEV(NUMV)=5                                                     
      TEV(NUMV)=0.0                                                     
      GO TO 560                                                         
C*****FIND TRACTIVE EFFORT AVAILABLE                                    
  700 TE=TORQUE(UNEW,ITYP)                                              
C*****CHECK IF AVAILABLE T.E. IS GREATER THAN REQUIRED T.E.             
      IF (TE-TEREQ) 750,710,710                                         
C*****COMPUTE AVAILABLE TRACTIVE EFFORT, LINE CURRENT AND MOTOR CURRENT 
  710 TEV(NUMV)=TEREQ                                                   
      CALL  AMPERE(TE,TEREQ,UNEW,ITYP)                                  
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
C*****COMPUTE NEW TRAIN LOCATION                                        
      XV(NUMV)=XV(NUMV)+DBLE(UNEW*DELT)                                 
C*****CHECK IF TRAIN HAS LEFT COASTING SECTION                          
      IF (UMXTS(JTS,IROUTE)) 820,720,720                                
  720 IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 730,740,740                      
C*****SWITCH TO ACCELERATION MODE - MODE=2                              
  730 MODEV(NUMV)=2                                                     
      GO TO 820                                                         
C*****SWITCH TO MAINTAINING CONSTANT SPEED MODE - MODE=1                
  740 MODEV(NUMV)=1                                                     
      UV(NUMV)=ABS(UMXTS(JTS,IROUTE))                                   
      GO TO 820                                                         
C*****SWITCH TO ATTEMPTING TO MAINTAIN MINIMUM SPEED MODE - MODE=7      
  750 MODEV(NUMV)=7                                                     
C                                                                       
C*****ATTEMPTING TO MAINTAIN CONSTANT MINIMUM SPEED IN A TRACK SECTION  
C*****WHERE COASTING IS PERMITTED - MODE=7                              
C                                                                       
C*****COMPUTE AVAILABLE TRACTIVE EFFORT, LINE CURRENT AND MOTOR CURRENT 
  760 TE=TORQUE(UV(NUMV),ITYP)                                          
      TEV(NUMV)=TE                                                      
      CALL  AMPERE(TE,TE,UV(NUMV),ITYP)                                 
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
C*****CHECK IF TIME TO DISPATCH NEXT TRAIN ON THIS ROUTE                
C*****FIND ACCELERATION TRAIN IS CAPABLE OF                             
      DUDTV(NUMV)=(TE*MOTORV(ITYP)-RSISTV(NUMV))/(WV(ITYP)*RRACC(ITYP)+W
     1PATV(NUMV)/GRACC)                                                 
C*****FIND NEW TRAIN SPEED                                              
      UNEW=UV(NUMV)+DUDTV(NUMV)*DELT                                    
C*****IF TRAIN SPEED IS GREATER THAN UMIN, SET SPEED TO UMIN AND        
C*****SWITCH BACK TO CONSTANT MINIMUM SPEED MODE                        
      IF (UNEW-UMINRT(IROUTE)) 780,770,770                              
  770 UNEW=UMINRT(IROUTE)                                               
      MODEV(NUMV)=6                                                     
      DUDTV(NUMV)=0.0                                                   
C*****COMPUTE NEW TRAIN LOCATION AND STORE NEW TRAIN SPEED              
  780 XV(NUMV)=XV(NUMV)+DBLE(0.5*(UV(NUMV)+UNEW) *DELT)                 
      UV(NUMV)=UNEW                                                     
C*****CHECK IF TRAIN HAS LEFT COASTING TRACK SECTION                    
      IF (UMXTS(JTS,IROUTE)) 820,790,790                                
C*****TRAIN CAN NO LONGER COAST - SWITCH TO ANOTHER MODE                
  790 IF (UNEW-ABS(UMXTS(JTS,IROUTE))) 800,810,810                      
C*****SWITCH TO ACCELERATION MODE - MODE=2                              
  800 MODEV(NUMV)=2                                                     
      GO TO 820                                                         
C*****SWITCH TO MAINTAINING CONSTANT SPEED MODE - MODE=1                
  810 MODEV(NUMV)=1                                                     
      UV(NUMV)=ABS(UMXTS(JTS,IROUTE))                                   
C                                                                       
C*****CONVERGENCE POINT FOR MODES   1,2,3,4,5,6,7     ( TPOPT=1 )       
C                                                                       
  820 RSISTV(NUMV)=DRAGV(NUMV)                                          
C*****FIND TRAIN POSITION, AND ITS GRADE AND CURVE RESISTANCE           
      CALL LOCATE (NUMV,ITYP,IROUTE,RG,RC)                              
C*****FIND TRAIN TOTAL WEIGHT, GRADE AND CURVE RESISTANCES AND SUM OF   
C*****GRADE, CURVE, AND ROLLING RESISTANCES                             
      WTRN=WV(ITYP)+WPATV(NUMV)                                         
      RG=RG*WTRN                                                        
      RC=RC*WTRN/TONLB                                                  
C*****COMPUTE MECHANICAL ROLLING RESISTANCE OF TRAIN                    
      RM=CORMV(ITYP,2)+(CORMV(ITYP,1)+CORMV(ITYP,3)*UV(NUMV))*WTRN      
C*****COMPUTE HEAT REJECTION CAUSED BY MECHANICAL RESISTANCE            
      RMHTV(NUMV)=(RM+RC)*UV(NUMV)*FLBBTU                               
C*****IF AERO IS NOT RUNNING COMPUTE AIR DRAG ON VEHICLE                
      IF (JRATAE) 850,850,830                                           
C*****IF IT IS NOT TIME FOR AERO STEP REPLACE AIR DRAG                  
  830 IF (AETIME-NTIME) 860,860,840                                     
  840 DRAGV(NUMV)=RSISTV(NUMV)                                          
      GO TO 860                                                         
C*****COMPUTE AIR DRAG ON TRAIN RUNNING IN OPEN AIR                     
  850 DRAGV(NUMV)=(AV(ITYP)*(CDFVOV(ITYP)+CDBVOV(ITYP))+LAMDAV(ITYP)*LV(
     1ITYP)/4.0)*0.5*RHOMAS*UV(NUMV)**2                                 
      RSISTV(NUMV)=DRAGV(NUMV)                                          
  860 RSISTV(NUMV)=RSISTV(NUMV)+RG+RC+RM                                
C*****CHECK IF TIME FOR COMPLETE TRAIN BRAKING CHECK                    
      IF (NTIME-TRTIME) 1070,870,870                                    
C*****BYPASS COMPLETE TRAIN BRAKING CHECK IF MODES 3 OR 4               
  870 NUMY1=MODEV(NUMV)+1                                               
      GO TO (1070,880,880,1070,1070,880,880,880), NUMY1                 
C                                                                       
C*****COMPLETE TRAIN BRAKING CHECK - INITIATES BRAKING                  
C                                                                       
C*****CHECK SPEED RESTICTIONS IN FRONT OF TRAIN, CONTINUING UNTIL A     
C*****STATION (0 MPH) OR THE END OF THE ROUTE IS FOUND                  
  880 JTS=NTSV(NUMV)+1                                                  
      NTS=NTSRT(IROUTE)                                                 
C*****CHECK FOR END OF TRACK SECTIONS                                   
  890 IF (NTS-JTS) 1070,900,900                                         
C*****COMPUTE ANTICIPATED SPEED AT NEXT TIME OF COMPLETE TRAIN          
C*****BRAKING CHECK. THIS POINT IS WORKED WITH TO CAUSE THE TRAIN TO    
C*****BRAKE 'EARLY' RATHER THAN 'LATE'                                  
  900 UNEXT=UV(NUMV)+DUDTV(NUMV)*DELTR                                  
C*****COMPUTE SPEED REDUCTION REQUIRED                                  
      SPDIF= UV(NUMV) - ABS(UMXTS(JTS,IROUTE))                          
      IF (SPDIF) 1050,1050,910                                          
C*****SPEED RESTRICTION IS LOWER THAN CURRENT TRAIN SPEED, THEREFORE    
C*****COMPUTE BRAKING TIME AND DISTANCE. BEGIN BY DETERMINING IF IN     
C*****CONSTANT OR VARYING DECELERATION RATE RANGE                       
  910 IF (UNEXT-DECV1V(ITYP)) 920,920,930                               
C*****CONSTANT DECELERATION RATE                                        
  920 STTIM= (UNEXT-ABS(UMXTS(JTS,IROUTE))) / DECV(ITYP)                
      STDIS=UNEXT*STTIM-0.5*DECV(ITYP)*STTIM**2                         
      GO TO 990                                                         
C*****VARYING DECELERATION RATE RANGE, COMPUTE LOWEST SPEED THAT WILL   
C*****OCCUR IN IT                                                       
  930 IF (ABS(UMXTS(JTS,IROUTE))-DECV1V(ITYP)) 950,950,940              
  940 V1T=ABS(UMXTS(JTS,IROUTE))                                        
      GO TO 960                                                         
  950 V1T=DECV1V(ITYP)                                                  
C*****COMPUTE BRAKING TIME AND DISTANCE FOR VARYING PORTION OF CURVE    
  960 T1= -LOG((DECAV(ITYP)*V1T+DECBV(ITYP))/(DECAV(ITYP)*UNEXT+DECBV(IT
     1YP)))/DECAV(ITYP)                                                 
      S1=(DECAV(ITYP)*UNEXT+DECBV(ITYP))*(1.0-EXP(-DECAV(ITYP)*T1))/DECA
     1V(ITYP)**2-DECBV(ITYP)*T1/DECAV(ITYP)                             
      IF (ABS(UMXTS(JTS,IROUTE))-DECV1V(ITYP)) 980,980,970              
C*****CONSTANT DECELERATION PORTION OF CURVE IS NOT BEING USED          
C*****BECAUSE SPEED RESTRICTION 'UMXTS' IS ABOVE 'DECV1V'               
  970 STDIS=S1                                                          
      GO TO 990                                                         
C*****COMPUTE BRAKING TIME AND DISTANCE ON CONSTANT DECELERATION        
C*****PORTION OF CURVE                                                  
  980 T2=(DECV1V(ITYP)-ABS(UMXTS(JTS,IROUTE)))/DECV(ITYP)               
      S2=DECV1V(ITYP)*T2-0.5*DECV(ITYP)*T2**2                           
      STDIS=S1+S2                                                       
C*****CHECK IF NECESSARY TO BRAKE FOR THIS SPEED RESTRICTION BEFORE     
C*****NEXT COMPLETE TRAIN EVALUATION                                    
  990 IF (STDIS+UV(NUMV)*DELTR+0.5*DUDTV(NUMV)*DELTR**2-(FSTS(JTS-1,IROU
     1TE)- SNGL(XV(NUMV)))) 1050,1050,1000                              
C*****IT IS NECESSARY TO BRAKE NOW - SWITCH TO MODE 3 OR 4              
C*****SET TRACK SECTION TRAIN IS BRAKING FOR                            
 1000 NTSV2(NUMV)=JTS                                                   
C*****TURN TRAIN POWER OFF                                              
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
C*****COMPUTE DECELERATION RATE                                         
      IF (UV(NUMV)-DECV1V(ITYP)) 1020,1020,1010                         
C*****ABOVE SPEED V1 - VARYING DECELERATION RATE                        
 1010 MODEV(NUMV)=3                                                     
      DUDTV(NUMV)=-DECBV(ITYP)-DECAV(ITYP)*UV(NUMV)                     
      GO TO 1070                                                        
C*****BELOW SPEED V1 - CONSTANT DECELERATION RATE                       
 1020 MODEV(NUMV)=4                                                     
      IF (ABS(UMXTS(JTS,IROUTE))-0.1) 1030,1040,1040                    
C*****TRAIN IS STOPPING FOR A STATION                                   
C*****COMPUTE STOPPING TIME AND DECELERATION RATE                       
 1030 STTIM=(FSTS(JTS-1,IROUTE)- SNGL(XV(NUMV)))*2.0/UV(NUMV)           
      IF( STTIM ) 1047,1047,1035                                        
 1035 DUDTV(NUMV)=-UV(NUMV)/STTIM                                       
      GO TO 1070                                                        
C*****TRAIN IS SLOWING FOR A SPEED RESTRICTION                          
C*****ADJUST STOPPING TIME TO AN INTEGER DELTA T                        
 1040 STTIM=(UV(NUMV)-ABS(UMXTS(JTS,IROUTE)))/DECV(ITYP)                
      IINT=INT(STTIM/DELT+0.9999)                                       
      STTIM=FLOAT(IINT)*DELT                                             
      IF( STTIM ) 1047,1047,1045                                        
 1045 DUDTV(NUMV)=(ABS(UMXTS(JTS,IROUTE))-UV(NUMV))/STTIM               
      GO TO 1070                                                        
C*****USE MAXIMUM DECELERATION RATE                                     
 1047 DUDTV(NUMV)=-DECV(ITYP)                                           
      GO TO 1070                                                        
C*****SPEED RESTRICTION IS HIGHER THAN CURRENT SPEED OR THERE IS        
C*****SUFFICIENT BRAKING DISTANCE                                       
C*****CHECK FOR A STATION (MAXIMUM SPEED OF ZERO)                       
 1050 IF (ABS(UMXTS(JTS,IROUTE))-0.1) 1070,1070,1060                    
 1060 JTS=JTS+1                                                         
      GO TO 890                                                         
 1070 CONTINUE                                                          
C                                                                       
C                                                                       
C     PRINT DATA FOR DEBUGGING SUBROUTINE FLYWHL                        
C                                                                       
C     WRITE (OUT,1300) NTIME, MODEV(NUMV), UV(NUMV), DUDTV(NUMV),       
C    1 RSISTV(NUMV), TEV(NUMV)                                          
C1300 FORMAT (I6,2X,I1,2X,F6.2,2X,F6.2,2X,F10.2,2X,F10.2)           
C                                                                       
C**** BRANCH ON FLYWHEEL SIMULATION OPTION                              
      NUMY1 = FLYOPV(ITYP)                                              
      GO TO (1074,1072), NUMY1                                          
 1072 CALL  FLYWHL(ITYP,NUMV)                                           
      GO TO 1080                                                        
1074  CALL HEAT (NUMV,ITYP,TEV(NUMV),AMPV(NUMV),AMPLV(NUMV))            
C                                                                       
C                                                                       
C     PRINT DATA FOR DEBUGGING SUBROUTINE FLYWHL                        
C                                                                       
C     WRITE (UNIT=OUT,FMT=1301,ADVANCE='NO') QDECV(NUMV),QACCV(NUMV),
C                                            AMPV(NUMV), AMPLV(NUMV) 
C1301 FORMAT(T53,4F10.2)                                            
C                                                                       
C     TRAIN ENERGY SUMMARY  -  TPOPT=1                                  
C                                                                       
 1080 IF( NOSUMY ) 1085,1085,1081                                       
 1081 JTS = NTSV(NUMV)                                                  
      ISEG2 = NSEGTS(JTS,IROUTE) / 128                                  
      ISTR = IABS(NSEGTS(JTS,IROUTE) - ISEG2 * 128 )                    
      IF( ISTR .EQ. 0 ) GO TO 1085                                      
      EAUXSR(ISTR) = EAUXSR(ISTR) + PAUXV(NUMV) * DELT                  
      ERGNSR(ISTR) = ERGNSR(ISTR) + PREGNV(NUMV) * DELT                 
      EPR3SR(ISTR) = EPR3SR(ISTR) + PPROPV(NUMV) * DELT                 
      EPRFSR(ISTR) = EPRFSR(ISTR) + PFLYV(NUMV) * DELT                  
 1085 CONTINUE                                                          
      ESPTSR = ESPTSR + 1                                               
C*****END OF IMPLICIT TRAIN PERFORMANCE                                 
      RETURN                                                            
C                                                                       
C                                                                       
C*****EXPLICIT TRAIN PERFORMANCE                  TPOPT = 2 + 3         
C                                                                       
C                                                                       
 1090 DO 1260 ITRAN=1,NTRAN                                             
C*****FIND TRAIN NUMBER, TYPE, AND ROUTE                                
      NUMV=IINDEX(ITRAN)                                                 
      ITYP=ITYPV(NUMV)                                                  
      IROUTE=IROTEV(NUMV)                                               
C*****COMPUTE THE TRAIN POSITION, SPEED, ACCELERATION AND POWER         
C*****REJECTIONS (TPOPT=3 ONLY) BY INTERPOLATION OF THE USER-SUPPLIED   
C*****SPEED AND POWER REJECTIONS VS. TIME                               
      CALL RATES (NUMV,IROUTE,QACC,QDEC)                                
      RSISTV(NUMV)=DRAGV(NUMV)                                          
C*****FIND TRAIN POSITION, AND ITS GRADE AND CURVE RESISTANCE           
      CALL LOCATE (NUMV,ITYP,IROUTE,RG,RC)                              
      GO TO (1280,1100,1200), TPOPT                                     
C                                                                       
C*****IMPLICIT HEAT REJECTION                     TPOPT = 2             
C                                                                       
C*****COMPUTE HEAT REJECTION FROM MOTOR CURVES                          
C*****COMPUTE MECHANICAL ROLLING RESISTANCE OF TRAIN                    
 1100 RM=CORMV(ITYP,2)+(CORMV(ITYP,1)+CORMV(ITYP,3)*UV(NUMV))*WV(ITYP)  
      RMHTV(NUMV)=(RM+RC)*UV(NUMV)*FLBBTU                               
C*****IF AERO IS NOT RUNNING COMPUTE AIR DRAG ON VEHICLE                
      IF (JRATAE) 1130,1130,1110                                        
C*****IF IS NOT TIME FOR AERO STEP REPLACE AIR DRAG                     
 1110 IF (AETIME-NTIME) 1140,1140,1120                                  
 1120 DRAGV(NUMV)=RSISTV(NUMV)                                          
      GO TO 1140                                                        
 1130 DRAGV(NUMV)=(AV(ITYP)*(CDFVOV(ITYP)+CDBVOV(ITYP))+LAMDAV(ITYP)*LV(
     1ITYP)/4.0)*0.5*RHOMAS*UV(NUMV)**2                                 
      RSISTV(NUMV)=DRAGV(NUMV)                                          
 1140 RSISTV(NUMV)=RSISTV(NUMV)+RG+RC+RM                                
C*****COMPUTE TRACTIVE EFFORT REQUIRED TO PRODUCE DESIRED ACCELERATION  
      TEV(NUMV)=(DUDTV(NUMV)*(WV(ITYP)*RRACC(ITYP)+WPATV(NUMV)/GRACC)+RS
     1ISTV(NUMV))/MOTORV(ITYP)                                          
      IF (UV(NUMV)-0.01) 1170,1150,1150                                 
 1150 IF (TEV(NUMV)) 1180,1180,1160                                     
 1160 TE = TORQUE(UV(NUMV),ITYP)                                        
      CALL  AMPERE(TE,TEV(NUMV),UV(NUMV),ITYP)                          
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      GO TO 1190                                                        
 1170 TEV(NUMV)=0.0                                                     
 1180 AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
 1190 CALL HEAT (NUMV,ITYP,TEV(NUMV),AMPV(NUMV),AMPLV(NUMV))            
C     TRAIN ENERGY SUMMARY  -  TPOPT=2 (ONLY)                           
C                                                                       
      IF( NOSUMY ) 1250,1250,1191                                       
 1191 JTS = NTSV(NUMV)                                                  
      ISEG2 = NSEGTS(JTS,IROUTE) / 128                                  
      ISTR = IABS(NSEGTS(JTS,IROUTE) - ISEG2 * 128 )                    
      IF( ISTR .EQ. 0 ) GO TO 1250                                      
      EAUXSR(ISTR) = EAUXSR(ISTR) + PAUXV(NUMV) * DELT                  
      ERGNSR(ISTR) = ERGNSR(ISTR) + PREGNV(NUMV) * DELT                 
      EPR3SR(ISTR) = EPR3SR(ISTR) + PPROPV(NUMV) * DELT                 
C     EPRFSR(ISTR) = 0          NO FLYWHEELS WHEN TPOPT=2,3             
      GO TO 1250                                                        
C                                                                       
C*****EXPLICIT HEAT REJECTION                     TPOPT = 3             
C                                                                       
C*****COMPUTE HEAT REJECTIONS IN BTU FROM POWER REJECTIONS IN KW        
 1200 QACCV(NUMV)=QACC*WTBTUS*1000.0                                    
      QDECV(NUMV)=QDEC*WTBTUS*1000.0                                    
C*****COMPUTE HEAT CREATED BY MECHANICAL ROLLING FRICTION               
      RM=CORMV(ITYP,2)+(CORMV(ITYP,1)+CORMV(ITYP,3)*UV(NUMV))*WV(ITYP)  
      RMHTV(NUMV)=(RM+RC)*UV(NUMV)*FLBBTU                               
C*****IF AERO IS NOT RUNNING COMPUTE AIR DRAG ON VEHICLE                
      IF (JRATAE) 1230,1230,1210                                        
C*****IF IT IS NOT TIME FOR AERO STEP REPLACE AIR DRAG                  
 1210 IF (AETIME-NTIME) 1240,1240,1220                                  
 1220 DRAGV(NUMV)=RSISTV(NUMV)                                          
      GO TO 1240                                                        
 1230 DRAGV(NUMV)=(AV(ITYP)*(CDFVOV(ITYP)+CDBVOV(ITYP))+LAMDAV(ITYP)*LV(
     1ITYP)/4.0)*0.5*RHOMAS*UV(NUMV)**2                                 
      RSISTV(NUMV)=DRAGV(NUMV)                                          
 1240 RSISTV(NUMV)=RSISTV(NUMV)+RG+RC+RM                                
C*****INCREMENT TIME SINCE TRAIN DISPATCH                               
 1250 TLPSV(NUMV)=TLPSV(NUMV)+NDELT                                     
 1260 CONTINUE                                                          
      ESPTSR = ESPTSR + 1                                               
C*****END OF EXPLICIT TRAIN PERFORMANCE                                 
 1270 RETURN                                                            
C                                                                       
C*****IMPOSSIBLE SITUATION                                              
C                                                                       
 1280 WRITE (OUT,1290)CHAR(12)                                                  
 1290 FORMAT (A1,/,'IMPOSSIBLE SITUATION - SUBROUTINE TRAIN')            
      CALL EXIT (EXITCODE)                                                              
      END                                                               
