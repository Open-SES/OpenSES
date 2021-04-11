      SUBROUTINE RSTREAD
C                                                                       
C *** THIS SUBROUTINE OPENS AND READS THE RESTART FILE SPECIFIED AT THE 
C *** END OF THE NORMAL INPUT FILE.  THIS FILE IS USED TO INITIALIZE THE
C *** SYSTEM IN ACCORDANCE WITH A PREVIOUS SIMULATION OF THE SAME SYSTEM.
C *** THE LEVEL OF INFORMATION THAT IS READ DEPENDS UPON THE VALUE ASSIGNED 
C *** TO THE INITIALIZATION FILE OPTION (RSTROPT). THESE LEVELS ARE AS
C *** FOLLOWS:                                                   
C                                                                       
C     RSTROPT = 1 AERO                                                   
C                                                                        
C             = 2 TRAINS                                                 
C                                                                       
C             = 3 THERMO                                                 
C                                                                       
C             = 4 AERO AND TRAINS                                        
C                                                                       
C             = 5 AERO, TRAINS, AND THERMO                               
C                                                                       
C             = 6 UNDEFINED                                              
C                                                                       
C             = 7 UNDEFINED                                              
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C
      CHARACTER*132 SYSTEMI
      IF (RST .EQ. IN)THEN
C**** THE RESTART FILE HAS BEEN ATTACHED TO THE INPUT FILE.  THEREFORE
C**** THE FIRST 80 CHARACTERS OF VARIABLE RESTART CONTAIN THE 
C**** IDENTIFICATION OF THE INITIALIZATION DATA FILE.  ALSO VARIABLE
C**** TITLE CONTAINS THE INITIALIZATION OPTION FOR THE RESTART FILE 
        SYSTEMI=RESTART(1:80)
        READ (TITLE,FMT=1480)IRSTOPT
      ELSE
C**** THE RESTART FILE WAS NOT ATTACHED TO THE INPUT FILE.  THEREFORE
C**** VARIABLE RESTART CONTAINS THE NAME OF THE RESTART FILE
	  CALL OPENRRST
        READ(RST,1520) SYSTEMI                                        
        READ(RST,1480) IRSTOPT                                              
      END IF
      WRITE(OUT,1710) SYSTEMI                                      
      WRITE(OUT,1490) IRSTOPT                                            
      IF(IRSTOPT-RSTROPT) 10,20,10                                        
   10 CALL EERROR(223)                                                   
   20 GO TO (100,500,900,100,100,100,100), IRSTOPT                       
C                                                                       
C---- AERODYNAMIC INITIALIZATION                                        
C                                                                       
  100 WRITE(OUT,1530)                                                   
      READ(RST,1550) NSECI                                               
      WRITE(OUT,1540) NSECI                                             
      IF(NSECI-NSEC) 130,140,130                                        
  130 CALL EERROR(224)                                                   
  140 WRITE(OUT,1560)                                                   
      DO 150 I =  1, NSECI                                              
      READ(RST,1580) ISECTX, DUMY1                                       
      ISCT=TABL14(ISECTX)                                               
      WRITE(OUT,1570) ISECTX,ISCT,DUMY1                                 
      CALL CHECKI( ISECTX, 1, LMSCTX, 153 )                             
      CALL CHECKI( ISCT, 1, LMSECT, 227 )                               
  150 QS(ISCT) =DUMY1/60.                                               
C*****CHECK INITIAL AIRFLOW CONDITIONS INPUT BY USER                    
C                                                                       
C*****LOOP OVER ALL NODES IN SYSTEM                                     
 160  DO  210 INODE=1,NNODE                                             
C*****FIND NUMBER OF SECTIONS(BRANCHES) THAT MEET AT THE NODE           
      NBR=TABL16(INODE)                                                 
C*****CHECK IF NUMBER OF SECTIONS MEETING AT THE NODE IS GREATER THAN   
C*****ONE OR NOT                                                        
      IF (NBR-1) 210,210,170                                            
C*****FOR NODES HAVING MORE THAN ONE SECTION MEETING AT THEM COMPUTE    
C*****FLOW ENTERING NODE                                                
 170  DUMY1=0.0                                                         
      DO  180 IBR=1,NBR                                                 
      ISCT=TABL17(INODE,IBR)                                            
      IISCT=IABS(ISCT)                                                  
 180  DUMY1=DUMY1+ISCT*QS(IISCT)/IISCT                                  
C*****IF THE NET FLOW ENTERING THE NODE IS ESSENTIALLY NON-ZERO THEN THE
C*****INPUT HAS VIOLATED CONTINUITY                                     
      IF (ABS(DUMY1)-0.1) 210,190,190                                   
 190  CALL EERROR (175)                                                  
      WRITE (OUT,430) TABL15(INODE)                                     
C*****AS A DEFAULT, SET INITIAL FLOWS EQUAL TO ZERO                     
      DO  200 ILOOP=1,NLOOP                                             
      QERRLP(ILOOP)=0.0                                                 
      DQDTLP(ILOOP)=0.0                                                 
 200  QLP(ILOOP)=0.0                                                    
      GO TO  390                                                        
 210  CONTINUE                                                          
C                                                                       
C*****INITIALIZATION OF LOOP FLOWS FROM SECTION FLOWS                   
C                                                                       
C*****FIRST, INITIALIZE LOOP FLOWS TO 1000000 CUBIC FEET PER SECOND AND 
C*****SET THEIR TIME DERIVATIVES AND AERO ACCUMULATED ERRORS TO ZERO    
      DO  220 ILOOP=1,NLOOP                                             
      QERRLP(ILOOP)=0.0                                                 
      DQDTLP(ILOOP)=0.0                                                 
 220  QLP(ILOOP)=1000000.0                                              
C*****FOR ALL SECTIONS HAVING ONLY ONE LOOP PASSING THROUGH THEM SET THE
C*****INITIAL LOOP FLOW EQUAL TO THE INITIAL SECTION FLOW, SIGNING THE  
C*****LOOP FLOW IN ACCORDANCE WITH THE DIRECTION OF THE LOOP THROUGH THE
C*****SECTION                                                           
      DO  240 ISCT=1,NSEC                                               
      NUMY1=NDSS(ISCT)                                                  
      NUMY2=NDSS(ISCT+1)-1                                              
      IF (NUMY1-NUMY2) 240,230,240                                      
 230  ILP1=LKAN(NUMY1)                                                  
      IILP1=IABS(ILP1)                                                  
      QLP(IILP1)=ILP1*QS(ISCT)/IILP1                                    
 240  CONTINUE                                                          
C*****BEGINNING OF ITERATIVE TYPE PROCEDURE TO INITIALIZE LOOP FLOWS NOT
C*****YET INITIALIZED                                                   
C*****SET ITERATION COUNTER EQUAL TO ZERO                               
      NUMY3=0                                                           
C*****CHECK IF ALL LOOP FLOWS HAVE BEEN INITIALIZED                     
 250  DO  260 ILOOP=1,NLOOP                                             
      IF (999999.0-ABS(QLP(ILOOP))) 270,260,260                         
 260  CONTINUE                                                          
C*****LOOP FLOW INITIALIZATION COMPLETE                                 
      GO TO  390                                                        
C*****CHECK FOR FAILURE OF ITERATION PROCEDURE, ASSUMING THAT IT        
C*****NEVER REQUIRES MORE THAN KBND1+1 ITERATIONS                       
 270  IF (KBND1+1-NUMY3) 280,290,290                                    
 280  WRITE (OUT,440)                                                   
      GO TO 3150                                                        
C*****BEGINNING OF NEXT ITERATION - INCREMENT ITERATION COUNTER BY ONE  
 290  NUMY3=NUMY3+1                                                     
C*****LOOP OVER ALL SECTIONS IN SYSTEM                                  
      DO  380 ISCT=1,NSEC                                               
      NUMY1=NDSS(ISCT)                                                  
      NUMY2=NDSS(ISCT+1)-1                                              
C*****IF SECTION HAS ONLY ONE LOOP PASSING THROUGH IT, THEN THAT LOOP   
C*****FLOW HAS BEEN INITIALIZED AND NO FURTHER INFORMATION MAY BE       
C*****OBTAINED FROM THIS INITIAL SECTION FLOW                           
      IF (NUMY1-NUMY2) 300,380,3150                                     
C*****IF MORE THAN ONE LOOP PASSES THROUGH THIS SECTION, LOOP OVER      
C*****LOOPS PASSING THROUGH IT, SEARCHING FOR ONE WHOSE FLOW HAS        
C*****NOT YET BEEN INITIALIZED                                          
 300  DO  310 NUMY4=NUMY1,NUMY2                                         
      ILP1=LKAN(NUMY4)                                                  
      IILP1=IABS(ILP1)                                                  
      IF (999999.0-ABS(QLP(IILP1))) 320,310,310                         
 310  CONTINUE                                                          
C*****LOOP FLOWS HAVE BEEN INITIALIZED FOR ALL LOOPS PASSING THROUGH    
C*****THIS SECTION, THUS, NO FURTHER INFORMATION MAY BE OBTAINED FROM   
C*****ITS INITIAL FLOW                                                  
      GO TO  380                                                        
C*****AT LEAST ONE LOOP PASSING THROUGH THIS SECTION HAS NOT BEEN       
 320  NUMY4=NUMY4+1                                                     
      IF (NUMY4-NUMY2) 330,330,350                                      
C*****LOOP OVER REMAINING LOOPS PASSING THROUGH THIS SECTION (IF ANY)   
C*****CHECKING TO SEE IF ANY OF THEM HAVE NOT YET BEEN INITIALIZED      
 330  DO  340 NUMY5=NUMY4,NUMY2                                         
      NUMY6=LKAN(NUMY5)                                                 
      IILP2=IABS(NUMY6)                                                 
C*****IF ANOTHER UNINITIALIZED LOOP PASSES THROUGH THIS SECTION THEN    
C*****NO FURTHER USE CAN BE MADE OF THIS SECTION INITIAL FLOW AT        
C*****THIS TIME                                                         
      IF (999999.0-ABS(QLP(IILP2))) 380,380,340                         
 340  CONTINUE                                                          
C*****INITIALIZE THE ONE UNINITIALIZED LOOP PASSING THROUGH THIS SECTION
C*****BY TAKING THE SECTION INITIAL FLOW AND SUBTRACTING THE OTHER LOOP 
C*****FLOWS FROM IT                                                     
 350  QLP(IILP1)=ILP1*QS(ISCT)/IILP1                                    
      DO  370 NUMY4=NUMY1,NUMY2                                         
      ILP2=LKAN(NUMY4)                                                  
      IILP2=IABS(ILP2)                                                  
      IF (IILP1-IILP2) 360,370,360                                      
 360  QLP(IILP1)=QLP(IILP1)-ILP2*ILP1*QLP(IILP2)/(IILP2*IILP1)          
 370  CONTINUE                                                          
 380  CONTINUE                                                          
C*****END OF THIS ITERATION                                             
      GO TO  250                                                        
C*****INITIALIZATION OF LOOP FLOWS COMPLETE. RECOMPUTE SECTION FLOWS    
C*****FROM LOOP FLOWS TO ELIMINATE ANY MINOR DISCREPANCIES (ONE CFM OR  
C*****LESS) IN CONTINUITY AT THE NODES. THIS ALLOWS THE INPUT OF        
C*****SECTION FLOWS TO THE NEAREST CFM.                                 
 390  KAERO=0                                                           
      CALL QSQLP                                                        
C*****OPTIONAL PRINTOUT OF INITIALIZED LOOP FLOWS AND RECOMPUTED SECTION
C*****FLOWS                                                             
      GO TO (420,400,400,400,400,400,420,420), SUPOPT                   
 400  WRITE (OUT,450)                                                   
      DO  410 ISCT=1,NSEC                                               
 410  BLP(ISCT)=QS(ISCT)*60.0                                           
      WRITE (OUT,460) (BLP(ISCT),ISCT=1,NSEC)                           
      WRITE (OUT,465)                                                   
      WRITE (OUT,470) (QLP(ILP),ILP=1,NLOOP)                            
 420  CONTINUE                                                          
C                                                                       
  500 GO TO (1000,505,900,505,505,505,505), IRSTOPT                      
C                                                                       
C---- TRAIN INITIALIZATION                                              
C                                                                       
  505 WRITE(OUT,1600)                                                   
      READ (RST,1550) NTRAN                                              
      WRITE(OUT,1610) NTRAN                                             
      WRITE(OUT,1620)                                                   
C---- INITIALIZE TRAIN INDEX ARRAY                                      
      DO 520  I=1,LMTRAN                                                
  520 IINDEX(I)=I                                                        
C---- INITIALIZE NUMBER OF TRAINS IN EACH LINE SEGMENT TO ZERO          
      IF (NLS) 524,524,521                                              
  521 DO 522 ILS=1, NLS                                                 
  522 NTRNLS(ILS)=0                                                     
C --  INITIALIZE THE NUMBER OF THE LAST TRAIN DISPATCHED INTO THE SYSTEM
  524 NUMBTR=0                                                          
      IF(NTRAN)900,900,530                                              
  530 DO 840  ITRAN=1,NTRAN                                             
      NUMV=IINDEX(ITRAN)                                                 
      READ(RST,1635) XV(NUMV),UV(NUMV),NUMY1,NUMY2,TGACCV(NUMV),TGDECV(N
     1UMV),MODEV(NUMV),DUMY4,DUMY5,NUMY6,DUMY8                           
      WRITE(OUT,1630) XV(NUMV),UV(NUMV),NUMY1,NUMY2,TGACCV(NUMV),TGDECV(
     1NUMV),MODEV(NUMV),DUMY4,DUMY5,NUMY6,DUMY8                         
      UV(NUMV)=UV(NUMV)*MPHFPS                                          
      IROTEV(NUMV)=NUMY1                                                
      IROUTE=IROTEV(NUMV)                                               
      ITYPV(NUMV)=NUMY2                                                 
      ITYP=ITYPV(NUMV)                                                  
      NUMBTR=NUMBTR+1                                                   
      CALL ALCHAR (NUMBTR,TNUMV(NUMV))                                  
      GO TO (710,535,535), TPOPT                                        
C---- LOCATE TRAIN ON SPEED-TIME PROFILE         TPOPT =2,3             
  535 NEXP=NEXPDP(IROUTE)                                               
C --  INTEGRATE SPEED-TIME PROFILE UNTIL TRAIN IS LOCATED               
      DUMY1=ORIGRT(IROUTE)                                              
      DO 550  I=2,NEXP                                                  
      DUMY2=DUMY1+(SPEEDV(I,IROUTE)+SPEEDV(I-1,IROUTE))*(TIMEV(I,IROUTE)
     1-TIMEV(I-1,IROUTE))/(2.*100.0)                                    
      IF (SNGL(XV(NUMV)) -DUMY2) 560, 560, 540                          
  540 DUMY1=DUMY2                                                       
  550 CONTINUE                                                          
C---- TRAIN IS NOT LOCATED WITHIN LIMITS OF SPEED-TIME PROFILE          
C --  THIS WILL CAUSE ERROR 144 TO BE WRITTEN                           
      TLPSV(NUMV)=0                                                     
      MODEV(NUMV)=1                                                     
      DPV(NUMV)=1                                                       
      XLASTV(NUMV)=SNGL( XV(NUMV))                                      
      DUDTV(NUMV)=0.0                                                   
      DGRDQV(NUMV)=0.0                                                  
      GO TO 710                                                         
C---- TRAIN IS LOCATED BETWEEN POINTS I-1 AND I                         
  560 MODEV(NUMV)=1                                                     
      DPV(NUMV)=I-1                                                     
      XLASTV(NUMV)=DUMY1                                                
      DTIME=(TIMEV(I,IROUTE)-TIMEV(I-1,IROUTE))/100.0                   
      DUDTV(NUMV)=(SPEEDV(I,IROUTE)-SPEEDV(I-1,IROUTE))/DTIME           
      DGRDQV(NUMV)=(QGRIDV(I,IROUTE)-QGRIDV(I-1,IROUTE))/DTIME          
      IF (ABS(DUDTV(NUMV))-0.005) 580,580,570                           
  570 DUMY3=(-SPEEDV(I-1,IROUTE)+SQRT(SPEEDV(I-1,IROUTE)**2-2.0*DUDTV(NU
     1MV)*(DUMY1-SNGL(XV(NUMV)) )))/DUDTV(NUMV)                         
      GO TO 590                                                         
  580 DUMY3=(SNGL(XV(NUMV)) -DUMY1)/SPEEDV(I-1,IROUTE)                  
  590 TLPSV(NUMV)=TIMEV(I-1,IROUTE)+DUMY3*100.0                         
      UV(NUMV)=SPEEDV(I-1,IROUTE)+DUDTV(NUMV)*(TLPSV(NUMV)-TIMEV(I-1,IRO
     1UTE))/100.0                                                       
      QGRID=QGRIDV(I-1,IROUTE)+DGRDQV(NUMV)*(TLPSV(NUMV)-TIMEV(I-1,IROUT
     1E))/100.0                                                         
      IF (QGRID) 610,600,600                                            
  600 QACCV(NUMV)=QGRID                                                 
      QDECV(NUMV)=0.0                                                   
      GO TO 620                                                         
  610 QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=-QGRID                                                
  620 WPATV(NUMV)=NPETTS(1,IROUTE)*PATWHT                               
C                                              TPOPT = 1,2,3            
  710 NUMY1=IROTEV(NUMV)                                                
      CALL CHECKI (NUMY1,1,NTRRTE,142)                                  
      NUMY1=ITYPV(NUMV)                                                 
      CALL CHECKI (NUMY1,1,NTRTYP,143)                                  
      NTS=NTSRT(IROUTE)                                                 
      IF (SNGL(XV(NUMV)) -ORIGRT(IROUTE)) 720,730,730                   
  720 CALL EERROR (18)                                                   
  730 CALL CHECKR (SNGL( XV(NUMV)),FSTS(1,IROUTE),FSTS(NTS,IROUTE),144) 
      CALL CHECKR (UV(NUMV),0.0,250.0,145)                              
C                                                                       
C---- INITIALIZE TRAIN PARAMETERS                                       
      GO TO (740, 810, 810 ), TPOPT                                     
C --  COMPUTE NUMBER OF PERSONS ABOARD TRAIN                            
  740 ITOT=0                                                            
      DO 750  ITS=1,NTS                                                 
      IF (SNGL(XV(NUMV)) -FSTS(ITS,IROUTE)) 751,750,750                 
  750 ITOT=ITOT+NPETTS(ITS,IROUTE)                                      
  751 WPATV(NUMV)=ITOT*PATWHT                                           
      RSISTV(NUMV)=CORMV(ITYP,2)+(CORMV(ITYP,1)+CORMV(ITYP,3)*UV(NUMV))*
     1(WV(ITYP)+WPATV(NUMV))                                            
      DUDTV(NUMV)=DUMY5*MPHFPS                                          
      TLPSV(NUMV)=NDELT                                                 
      QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=0.0                                                   
      NTSV(NUMV)=2                                                      
      NTSV2(NUMV)=NUMY6                                                 
      TSTRTV(NUMV)=0                                                    
      MODE =MODEV(NUMV)                                                 
      RPS(NUMV) = DUMY8/60.0                                            
      FKENEW(NUMV) = (2.0*PI*RPS(NUMV))**2*FLYJV(ITYP)/(2.0*GRACC)      
      POW1(NUMV) = 0.0                                                  
      IF (MODE) 760,760,752                                             
  752 GO TO ( 762,764,766,766,768,780,782 ),MODE                        
C---- TRAIN IS STOPPED   (MODE 0)                                       
  760 RSISTV(NUMV) = 0.0                                                
      DUDTV(NUMV)  = 0.0                                                
      TEV(NUMV)    = 0.0                                                
      AMPV(NUMV)   = 0.0                                                
      AMPLV(NUMV)  = 0.0                                                
C---- IF A NEGATIVE VALUE IS ENTERED FOR THE DWELL TIME, PRINT MESSAGE
C---- OTHERWISE CHECK IF DWELL TIME IS WITHIN LIMITS.
        IF (DUMY4 .LT. 0.0)THEN
        TSTRTV(NUMV) = IZAP
        WRITE (OUT,1636)
      ELSE
        TSTRTV(NUMV) = DUMY4*100.0
      END IF                                          
      GO TO 835                                                         
C---- CONSTANT SPEED MODE   (MODE 1)                                    
  762 TEREQ = RSISTV(NUMV)/MOTORV(ITYP)                                 
      TEV(NUMV) = TEREQ                                                 
      CALL AMPERE(TEREQ,TEREQ,UV(NUMV),ITYP)                            
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      GO TO 835                                                         
C---- ACCELERATING MODE   (MODE 2)                                      
  764 TE= TORQUE(UV(NUMV),ITYP)                                         
      TEV(NUMV)=TE                                                      
      CALL AMPERE(TE,TE,UV(NUMV),ITYP)                                  
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      GO TO 835                                                         
C---- BRAKING MODE   (MODE 3 AND 4)                                     
  766 TEV(NUMV)    = 0.0                                                
      AMPV(NUMV)   = 0.0                                                
      AMPLV(NUMV)  = 0.0                                                
C---- SET MODE TO 3 TO FORCE RE-COMPUTATION OF BRAKING RATE             
      MODEV(NUMV)=3                                                     
      GO TO 835                                                         
C---- COASTING MODE   (MODE5)                                           
  768 TEV(NUMV)    = 0.0                                                
      AMPV(NUMV)   = 0.0                                                
      AMPLV(NUMV)  = 0.0                                                
      GO TO 835                                                         
C---- COASTING MODE - MAINT. MINIMUM SPEED   (MODE 6)                   
  780 TEREQ = RSISTV(NUMV)/MOTORV(ITYP)                                 
      TEV(NUMV) = TEREQ                                                 
      CALL AMPERE(TEREQ,TEREQ,UV(NUMV),ITYP)                            
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      GO TO 835                                                         
C---- COASTING MODE - ATTEMPTING TO MAINT. MINIMUM SPEED   (MODE 7)     
  782 TE= TORQUE(UV(NUMV),ITYP)                                         
      TEV(NUMV)=TE                                                      
      CALL AMPERE(TE,TE,UV(NUMV),ITYP)                                  
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      GO TO 835                                                         
  810 TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      TSTRTV(NUMV)=0                                                    
      NTSV(NUMV)=2                                                      
      NTSV2(NUMV)=2                                                     
      WPATV(NUMV) = 0.0                                                 
  835 QERDCV(NUMV)=0.0                                                  
      QERACV(NUMV)=0.0                                                  
      DRAGV(NUMV)=0.0                                                   
      RMHTV(NUMV)=0.0                                                   
      PPROPV(NUMV) = 0.0                                                
      PREGNV(NUMV) = 0.0                                                
      PAUXV(NUMV) = PAUXEV(ITYP)+PAUXPV(ITYP)*WPATV(NUMV)/PATWHT        
      PFLYV(NUMV) = 0.0                                                 
      QPRPV(NUMV) = 0.0                                                 
      QAXSV(NUMV) = SHREV(ITYP)+SHRPV(ITYP)*WPATV(NUMV)/PATWHT          
      QAXLV(NUMV) = LHREV(ITYP)+LHRPV(ITYP)*WPATV(NUMV)/PATWHT          
  840 CALL LOCATE (NUMV,ITYP,IROUTE,RG,RC)                              
C                                                                       
  850 CONTINUE                                                          
C                                                                       
  900 GO TO (1000,1000,905,1000,905,905,905), IRSTOPT                    
C                                                                       
C---- THERMODYNAMIC INITIALIZATION                                      
C                                                                       
  905 WRITE (OUT,1640)                                                  
      READ(RST,1550) JTNLSS                                              
      WRITE(OUT,1650) JTNLSS                                            
      IF(JTNLSS-TNLSS) 910,920,910                                      
  910 CALL EERROR (225)                                                  
  920 IF(JTNLSS) 950,950,930                                            
  930 WRITE(OUT,1660)                                                   
      DO 940 ILSSX=1,JTNLSS                                             
      READ(RST,1680) ISCTX,ILSX,ISSX,DUMY5,DUMY6,DUMY7,DUMY1,DUMY2       
      CALL EXTINT(ISCTX,ILSX,ISSX,ILSS,IERR)                            
      WRITE(OUT,1670) ISCTX,ILSX,ISSX,ILSS,DUMY5,DUMY6,DUMY7,DUMY1,DUMY2
      GO TO (935,932), IERR                                             
  932 CALL  EERROR (227)                                                 
      GO TO 940                                                         
  935 TDBSS(ILSS)=DUMY5                                                 
      TTMPSS(ILSS)= DUMY5                                               
      HUMSS(ILSS)= DUMY6                                                
      HTMPSS(ILSS)= DUMY6                                               
      TSFSS(ILSS)= DUMY7                                                
      SHSS = SHLSS(ILSS)-SACLSS(ILSS)                                   
      SACLSS(ILSS)= DUMY1/3600.                                         
      SHLSS(ILSS)=SHSS+SACLSS(ILSS)                                     
      SHLTSS(ILSS)= SHLSS(ILSS)                                         
      HLSS = LHLSS(ILSS) - LACLSS(ILSS)                                 
      LACLSS(ILSS)= DUMY2/3600.                                         
      LHLSS(ILSS) = HLSS + LACLSS(ILSS)                                 
      LHLTSS(ILSS)= LHLSS(ILSS)                                         
  940 CONTINUE                                                          
  950 READ(RST,1550) ITNVSS                                              
      WRITE(OUT,1690) ITNVSS                                            
      IF(TNSS-TNLSS-ITNVSS) 960,970,960                                 
  960 CALL EERROR (226)                                                  
  970 IF(ITNVSS) 990,990,980                                            
  980 DO 985 IVSSX = 1,ITNVSS                                           
      READ(RST,1720) ISCTX,IVSX,ISSX,DUMY1,DUMY2,DUMY3                   
      CALL EXTINT(ISCTX,IVSX,ISSX,IVSS,IERR)                            
      WRITE(OUT,1715) ISCTX,IVSX,ISSX,IVSS,DUMY1,DUMY2,DUMY3            
      GO TO (983,982), IERR                                             
  982 CALL  EERROR (227)                                                 
      GO TO 985                                                         
  983 TDBSS(IVSS)=DUMY1                                                 
      TTMPSS(IVSS)= DUMY1                                               
      HUMSS(IVSS) = DUMY2                                               
      HTMPSS(IVSS) = DUMY2                                              
      TSFSS(IVSS)= DUMY3                                                
  985 CONTINUE                                                          
  990 CONTINUE                                                          
 1000 CLOSE (7)
      RETURN                                                            
C                                                                       
430   FORMAT (35X,'NODE NUMBER',I4)                                 
440   FORMAT (30X,'POOR INITIALIZATION')                            
450   FORMAT (////T53,'FLOW INITIALIZATION CHECK'///'INTERNAL SECTION AI
     1RFLOW RATE (CFM)',/,35('-'))                                  
460   FORMAT (/,10F12.1)                                           
465   FORMAT (///,'LOOP AIRFLOW RATE (CFS)',/,23('-'))               
470   FORMAT (/,10F12.3)                                           
 1480 FORMAT(I5)                                                        
 1490 FORMAT(/,'INITIALIZATION FILE OPTION =',I2)            
 1510 FORMAT (A1)
 1520 FORMAT(A132)                                            
 1530 FORMAT(/,'AERODYNAMIC INITIALIZATION DATA',/,
     1         '----------- -------------- ----')                                                
 1540 FORMAT(/,'TOTAL NUMBER OF SECTIONS = ',I3)                      
 1550 FORMAT(I5)                                                        
 1560 FORMAT(/,'EXTERNAL INTERNAL',5X,'AIR FLOW RATE',/,2X,
     1         'SECT',5X,'SECT',8X,'(FT**3/MIN)',/)                                              
 1570 FORMAT(2X,I3,6X,I3,4X,F15.1)                                      
 1580 FORMAT(I5,F15.1)                                                  
 1600 FORMAT(/,'TRAIN INITIALIZATION DATA',/,'----- -------------- ----'
     1)                                                            
 1610 FORMAT(/,'TOTAL NUMBER OF TRAINS OPERATING =',I3)               
 1620 FORMAT(/,1X,'DISTANCE  SPEED  ROUTE  TYPE  ACC GRID TEMP  DEC GRID
     1 TEMP  MODE  REMAINING DWELL  ACCELERATION RATE',6X,'FLYWHEEL',/,  
     2'  (FEET)   (MPH)',T33,'(DEGREE F)',5X,'(DEGREE F)',T73,'(SEC)'   
     3,T89,'(MPH/SEC)',T110,'(RPM)',/)                                  
 1630 FORMAT(F9.2,F7.2,I5,T27,I2,T34,F6.1,T49,F6.1,T63,I2,T70,F8.2,T90,
     1F6.3,I10,F10.1)                                                    
 1635 FORMAT(2F10.2,2I5,2F10.2,I5,F10.2,F10.3,I5,/,F10.2)               
 1636 FORMAT (/,'A NEGATIVE VALUE FOR THE REMAINING DWELL TIME INDICATES
     1 THAT THE TRAIN IS TO REMAIN IN THE INITIALIZED LOCATION',/,'FOR T   
     2HE DURATION OF THE SIMULATION.')
 1640 FORMAT(/,'THERMODYNAMIC INITIALIZATION DATA',/,'------------- ----
     1---------- ----')                                            
 1650 FORMAT(/,'TOTAL NUMBER OF LINE SUBSEGMENTS =',T35,I4)           
 1660 FORMAT(/,T3,'EXTERNAL   INTERNAL  DRY BULB TEMP  HUMIDITY  WALL SU
     1RF TEMP   A/C SENSIBLE    A/C LATENT',/                         
     2,'SECT SEG SUB',3X,'SUB',6X,'(DEGREE F)',4X,'(LB/LB)',5X,'(DEGREE 
     3F)',8X,'BTU/HR',9X,'BTU/HR',/)                                   
 1670 FORMAT(I3,'-',I3,'-',I3,4X,I3,8X,F6.2,6X,F7.5,7X,F6.2,8X,F9.0,6X,F
     19.0)                                                              
 1680 FORMAT(3I5,F8.2,F9.5,F8.2,2F10.0)                                 
 1690 FORMAT(/,'TOTAL NUMBER OF VENT SUBSEGMENTS =',I4,/)             
 1710 FORMAT('IDENTIFICATION OF INITIALIZATION DATA SOURCE',/,
     1       '--------------------------------------------',//,A132)
 1715 FORMAT(I3,'-',I3,'-',I3,4X,I3,8X,F6.2,6X,F7.5,7X,F6.2)            
 1720 FORMAT(3I5,F8.2,F9.5,F8.2)                                        
 1730 FORMAT('IMPOSSIBLE SITUATION - SUBROUTINE RSTREAD (AERO)')   
 1740 FORMAT(A256)
 3150 WRITE (OUT,1510) CHAR(12)
      WRITE (OUT,1730)
      CALL EXIT (EXITCODE)                                                              
      END