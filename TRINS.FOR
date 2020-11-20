      SUBROUTINE TRINS (IROUTE)                                         
C                                                                       
C        TRAIN ROUTE INPUT SUBROUTINE                                   
C                                                                       
C                                                                       
C     THIS SUBROUTINE READS THE DATA DESCRIBING THE TRAIN ROUTE.        
C     IT READS AND INPUT VERIFIES  TRAIN SCHEDULING DATA                
C                                  TRACK SECTION DATA                   
C                                  SCHEDULED STOPS                      
C                                  EXP. T.P. SPEED-TIME PROFILE         
C                                  PATH THROUGH SYSTEM                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'
C                                                                       
   10 FORMAT (A1)   
   20 FORMAT ('INPUT VERIFICATION FOR TRAIN ROUTE',I3,3X,A68,T112,
     1'FORM 8A')                                                       
   30 FORMAT ('INPUT VERIFICATION FOR TRAIN ROUTE',I3,3X,A68,T112,
     1        'CONTINUED')                                                       
   40 FORMAT (//,T48,'TRAIN SCHEDULING DATA'/)                        
   50 FORMAT (/,'TRAIN SCHEDULING ORIGIN',T79,F14.1,T96,'FT')        
   60 FORMAT (/,'NUMBER OF GROUPS OF TRAINS THAT COULD ENTER ROUTE',T79
     1,I14)                                                           
   70 FORMAT (/,'NUMBER OF TRACK SECTIONS IN THIS ROUTE',T79,I14)    
   80 FORMAT (/,'DELAY TIME BEFORE DISPATCHING FIRST TRAIN',T79,I14,T96
     1,'SECONDS')                                                     
   81 FORMAT (/ ,'MINIMUM TRAIN SPEED ALLOWED ON THIS ROUTE DURING COAST
     1ING',T79,F14.1,T96,'MPH')                                       
   83 FORMAT (/,'ROUTE COASTING OPTION',T83,I10,3X,
     1          'MAINTAIN MINIMUM SPEED')                         
   84 FORMAT (/,'ROUTE COASTING OPTION',T83,I10,3X,
     1          'ACCELERATE FROM',/,T96,'MINIMUM SPEED')          
   90 FORMAT (//,T46,'TRAIN GROUP INFORMATION',T112,'FORM 8B',//,T11,
     1'GROUP',T30,'NUMBER OF',T50,'TRAIN',T85,'TIME LAST TRAIN IN GROUP'
     2,/T11,'NUMBER',T31,'TRAINS',T50,'TYPE',T67,'HEADWAY',T87,'BECOMES 
     3OPERATIONAL',/,T66,'(SECONDS)',T92,'(SECONDS)'/)
  100 FORMAT (/,T12,I3,T32,I3,T50,I3,T67,I5,T89,I9)                   
  120 FORMAT(//,T51,'TRACK SECTION DATA',T112,'FORM 8C'/              
     1 /T2,'TRACK',T15,'LOCATION OF',T58,'ELEVATION',T88,'MAXIMUM'/, 
     2 'SECTION',T13,'FORWARD END OF',T43,'RADIUS OF',T58,           
     3 'OF FORWARD',T87,'ALLOWABLE',T104,'ENERGY',T116,'COASTING'/,     
     4 'NUMBER',T13,'TRACK SECTION',T31,'LENGTH',T43,'CURVATURE',    
     5 T61,'END',T75,'GRADE',T85,'TRAIN VELOCITY',T104,'SECTOR',        
     6 T116,'PARAMETER' /                                               
     7 T16,'(FEET)',T44,'(FEET)',T60,'(FEET)',                          
     8 T73,'(PERCENT)',T90,'(MPH)' / )                                  
  125 FORMAT(/I4,F17.1,F15.1,12X,F16.1,2F15.2,I13,8X,'COASTING')
  126 FORMAT(/I4,F17.1,F15.1,12X,F16.1,2F15.2,I13)
  130 FORMAT(/I4,F17.1,F15.1,F12.1,F16.1,2F15.2,I13,8X,'COASTING')         
  140 FORMAT(/I4,F17.1,F15.1,F12.1,F16.1,2F15.2,I13)         
  150 FORMAT (//,T49,'SCHEDULED STOPS DATA',T112,'FORM 8D',//,'NUMBER OF
     1 SCHEDULED STOPS',T79,I14)                                  
  160 FORMAT (/,'NUMBER OF PERSONS ABOARD TRAIN AT SCHEDULING ORIGIN',
     1T79,I14)                                                         
  170 FORMAT (/,T16,'LOCATION ON ROUTE WHERE',T87,'NUMBER OF PERSONS'/
     1T16,' FRONT OF TRAINS STOP   ',T61,'DWELL TIME',T88,'ENTERING TRAI
     2N'/T25,'(FEET)',T61,'(SECONDS)',T92,'AT STOP'/)                   
  180 FORMAT (/,T15,F16.1,T60,F8.2,T86,I11)                           
  190 FORMAT (//,T35,'EXPLICIT TRAIN PERFORMANCE TRAIN SPEED VS TIME PRO
     1FILE',T112,'FORM 8E',/,/'NUMBER OF SPEED VS TIME PROFILE POINTS',
     2I6)                                                          
  200 FORMAT (/,T21,'TIME AFTER START',T88,'DISTANCE'/T23,'OF OPERATION'
     1,T59,'SPEED',T88,'TRAVELED'/T24,'(SECONDS)',T59,'(MPH)',T89,'(FEET
     2)'/)                                                            
  210 FORMAT (/,T20,F11.1,T55,F9.1,T79,F15.1)                         
  220 FORMAT (/,T15,'TIME AFTER START',T98,'DISTANCE'/T17,'OF OPERATION'
     1,T39,'SPEED',T57,'POWER INPUT TO RESISTOR GRIDS',T98,'TRAVELED'/
     2T18,'(SECONDS)',T39,'(MPH)',T61,'(KILOWATTS PER TRAIN)',T99,'(FEET
     3)',/T57,'ACCELERATION',T79,'BRAKING'/)                           
  230 FORMAT (/,T14,F11.1,F19.1,F23.1,F19.1,F19.1)                    
  240 FORMAT (//,T43,'SECTION SEQUENCING FOR ROUTE',T112,'FORM 8F'/)  
  250 FORMAT (//'NUMBER OF SECTIONS THROUGH WHICH ROUTE PASSES',T79,
     1I14)                                                              
  260 FORMAT (/ ,'DISTANCE FROM ROUTE ORIGIN TO PORTAL OR FIRST NODE ON 
     1ROUTE',T79,F14.1,T96,'FT')                                      
  270 FORMAT (/,T33,'SECTIONS AND SEGMENTS THROUGH WHICH ROUTE PASSES')
  280 FORMAT (/,T27,'SECTION',T47,'SEGMENT',T65,'LOCATION OF SEGMENT ALO
     1NG ROUTE'/T27,'NUMBER',T47,'NUMBER',T68,'(FEET)',T86,'(FEET)'/)
  290 FORMAT (/,T25,I6,T46,I5,T66,F8.1,4X,'TO',F12.1)                 
  300 FORMAT (T46,I5,T66,F8.1,4X,'TO',F12.1)                        
  310 FORMAT (/,T25,I6,T46,I6)                                        
  320 FORMAT (///,T4,'AN ENTRY OF ZERO SECTIONS INDICATES THAT THIS ROUT
     1E REMAINS IN OPEN AIR AND DOES NOT ENTER THE SYSTEM.')            
  330 FORMAT (A68)                                                     
  340 FORMAT (8F10.0)                                                   
  341 FORMAT(T25,'DUMP OF INTERNAL TRACK SECTION DATA - ROUTE',I3)  
  342 FORMAT(/,T16,'FSTS',T25,'RADTS',T35,'GRDTS',T45,'UMXTS',        
     1  T55,'DWLTS',T64,'NPETTS',T74,'NSEGTS' / )                       
  343 FORMAT(I5,F14.2, F10.1, F10.5, F10.2, 3I10 )                      
 1011 FORMAT(/,10X,'SECTION',I4 )                                     
C                                                                       
C                                                                       
C                                                                       
C***** INPUT FORM 8A - ROUTE NAME + TRAIN DISPATCHER DATA - TPOPT=1,2,3 
C                                                                       
C                                                                       
C*****READ ROUTE NAME                                                   
      READ (IN,330) TITLE                                        
C*****WRITE ROUTE NUMBER AND NAME                                       
      WRITE (OUT,20) IROUTE,TITLE                                  
      WRITE (OUT,40)                                                    
C*****READ TRAIN SCHEDULING ORIGIN, NUMBER OF TRAIN GROUPS TO BE        
C*****DISPATCHED, NUMBER OF TRACK SECTIONS (USED FOR TPOPT=1,2), DELAY  
C*****TIME, FIRST TRAIN TYPE, MINIMUM COASTING SPEED, AND ROUTE COASTING
C*****OPTION                                                            
      READ (IN,340) ORIGRT(IROUTE), DUMY1, DUMY2, DUMY3, DUMY4, UMINRT(I
     1ROUTE), DUMY5                                                     
C*****WRITE AND CHECK TRAIN SCHEDULING ORIGIN                           
      WRITE (OUT,50) ORIGRT(IROUTE)                                     
      CALL CHECKR (ORIGRT(IROUTE),0.0,1000000.0,120)                    
C*****STORE, WRITE AND CHECK NUMBER OF TRAIN GROUPS TO BE DISPATCHED    
      NGRV(IROUTE)=DUMY1                                                
      NUMY1=DUMY1                                                       
      WRITE (OUT,60) NUMY1                                              
      CALL CHECKI (NUMY1,1,LMTRGP,64)                                   
      IF (NEW .EQ. 1)GOTO 1370                                             
      GO TO (360,360,370), TPOPT                                        
C*****STORE, WRITE, AND CHECK NUMBER OF TRACK SECTIONS IF TPOPT=1,2     
  360 NUMY1=DUMY2                                                       
      NTSRT(IROUTE)=NUMY1                                               
      WRITE (OUT,70) NUMY1                                              
      CALL CHECKI (NUMY1,1,LMTSRT-1,3)                                  
C*****THE NUMBER OF TRACK SECTIONS IN A ROUTE THAT THE PROGRAM STORE IS 
C*****ALWAYS GREATER THEN THE NUMBER INPUT (EXTERNAL) BY THE USER.      
C*****'DUMMY' TRACK SECTIONS MUST BE CREATED FOR REASONS SUCH AS (1) TO 
C*****STORE THE LOCATION AND ELEVATION OF THE BACK END OF THE FIRST     
C*****EXTERNAL TRACK SECTION IN THE ROUTE, (2) TO STORE THE LOCATIONS OF
C*****SCHEDULED STOPS, AND (3) TO STORE THE LOCATION OF THE TRANSITION  
C*****FROM ONE SEGMENT TO ANOTHER.                                      
C*****INCREMENT THE NUMBER OF TRACK SECTIONS BY ONE TO STORE THE        
C*****LOCATION AND ELEVATION OF THE BACK END OF THE FIRST TRACK SECTION 
C*****IN THE ROUTE                                                      
      NTSRT(IROUTE)=NTSRT(IROUTE)+1                                     
      IF (NEW .EQ. 1)GOTO 1370                                             
      GOTO 380                                             
C*****SET NUMBER OF TRACK SECTIONS TO ZERO IF TPOPT=3                   
  370 NTSRT(IROUTE)=0                                                   
C*****DATA FOR FIRST TRAIN GROUP WHICH HAS ONLY ONE TRAIN               
  380 IA=DUMY3                                                          
C*****STORE, WRITE, AND CHECK TRAIN DISPATCH (DELAY) TIME IN 100TH'S    
C*****OF SECONDS                                                        
      GRHDWY(1,IROUTE)=IA*100                                           
      WRITE (OUT,80) IA                                                 
      CALL CHECKI (IA,0,10000,67)                                       
      GO TO (381,389,389),TPOPT                                         
C*****WRITE AND CHECK MINIMUM SPEED ALLOWED IN A TRACK SECTION THAT     
C*****PERMITS COASTING                                                  
  381 WRITE (OUT,81)  UMINRT(IROUTE)                                    
      CALL CHECKR ( UMINRT(IROUTE),0.0,250.0,211 )                      
C*****CONVERT MINIMUM COASTING SPEED TO FPS                             
      UMINRT(IROUTE) = UMINRT(IROUTE) * MPHFPS                          
C*****STORE, WRITE, AND CHECK ROUTE COASTING OPTION                     
      NUMY1=DUMY5                                                       
      COPTRT(IROUTE)=NUMY1                                              
      IF(NUMY1) 383,382,383                                             
  382 WRITE(OUT,83)NUMY1                                                
      GO TO 389                                                         
  383 IF(NUMY1-1) 385,384,385                                           
  384 WRITE(OUT,84)NUMY1                                                
      GO TO 389                                                         
  385 CALL EERROR(215)                                                   
      COPTRT(IROUTE)=0                                                  
C*****SET NUMBER OF TRAINS IN FIRST TRAIN GROUP TO ONE                  
  389 NTRGR(1,IROUTE)=1                                                 
C*****STORE TYPE OF TRAINS IN FIRST TRAIN GROUP                         
      GRTYPV(1,IROUTE)=DUMY4                                            
      NUMY3=DUMY4                                                       
      NUMY1=1                                                           
      NUMY2=IA                                                          
      WRITE (OUT,90)                                                    
C*****WRITE FIRST TRAIN GROUP INFORMATION                               
      WRITE (OUT,100) NUMY1,NTRGR(1,IROUTE),NUMY3,IA,NUMY2              
C*****CHECK TRAIN TYPE                                                  
      CALL CHECKI (NUMY3,1,NTRTYP,65)                                   
      IF (NEW .EQ. 1)GOTO 1370                                             
C*****INITIALIZE TRAIN DISPATCHER GROUP COUNTER, NUMBER OF TRAINS       
C*****ALREADY DISPATCHED FOR THIS GROUP, AND TIME OF NEXT (FIRST) TRAIN 
C*****DISPATCH                                                          
      JGROUP(IROUTE) = 1                                                
      NMTRGR(IROUTE)=0                                                  
      NTIMTR(IROUTE)=GRHDWY(1,IROUTE)                                   
C*****CHECK FOR READING OF ADDITIONAL TRAIN GROUP DATA                  
      IB=NGRV(IROUTE)                                                   
      IF (IB-2) 410,390,390                                             
C                                                                       
C                                                                       
C***** INPUT FORM 8B - TRAIN GROUP DATA FOR REST OF GROUPS - TPOPT=1,2,3
C                                                                       
C                                                                       
C*****LOOP OVER REST OF TRAIN GROUPS                                    
  390 DO 400 IC=2,IB                                                    
C*****READ AND STORE NUMBER OF TRAINS IN GROUP, THEIR TYPE, AND HEADWAY 
      READ (IN,340) DUMY1,DUMY2,DUMY3                                   
      NTRGR(IC,IROUTE)=DUMY1                                            
      GRTYPV(IC,IROUTE)=DUMY2                                           
      IA=DUMY3                                                          
C*****HEADWAY IS STORED IN 100TH'S OF SECONDS                           
      GRHDWY(IC,IROUTE)=IA*100                                          
C*****COMPUTE TIME LAST TRAIN IN THIS GROUP IS DISPATCHED               
      NUMY2=NUMY2+NTRGR(IC,IROUTE)*IA                                   
      WRITE (OUT,100) IC,NTRGR(IC,IROUTE),GRTYPV(IC,IROUTE),IA,NUMY2    
C*****CHECK NUMBER OF TRAINS IN GROUP AND THEIR TYPE                    
      NUMY1=NTRGR(IC,IROUTE)                                            
      CALL CHECKI (NUMY1,1,1000,66)                                     
      NUMY1=GRTYPV(IC,IROUTE)                                           
      CALL CHECKI (NUMY1,1,NTRTYP,65)                                   
      IF (NEW .EQ. 1)GOTO 1370                                             
C*****CHECK HEADWAY                                                     
  400 CALL CHECKI (IA,0,10000,67)                                       
  410 GO TO (420,420,670), TPOPT                                        
C                                                                       
C                                                                       
C***** INPUT FORM 8C - TRACK SECTION DATA - TPOPT=1,2                   
C                                                                       
C                                                                       
C     NOTE - THE EXPLICIT TRAIN PERFORMANCE ARRAYS SPEEDV AND TIMEV ARE 
C     TEMPORARILY USED TO STORE TRACK SECTION DATA DURING PROCESSING    
C     OF FORM 8C.                                                       
C*****WRITE TITLES ON NEW PAGE                                          
 420  WRITE (OUT,10) CHAR (12)
      WRITE (OUT,30) IROUTE,TITLE                                      
      WRITE (OUT,120)
C*****SET LOCATION OF BACK END OF FIRST TRACK SECTION TO ZERO           
      FSTS(1,IROUTE)=0.0                                                
C*****SET GRADE AND RADIUS OF THIS 'ZEROTH' TRACK SECTION TO ZERO       
      GRDTS(1,IROUTE)=0.0                                               
      RADTS(1,IROUTE)=0.0                                               
C*****FIND NUMBER OF TRACK SECTIONS IN ROUTE                            
      NTS=NTSRT(IROUTE)                                                 
C*****INITIALIZE NUMBER OF ENERGY SECTORS                               
      NSTR = 0                                                          
C*****LOOP OVER ALL EXTERNAL TRACK SECTIONS IN ROUTE READING ALL DATA - 
C*****NOTE THAT ELEVATION AT FORWARD END OF TRACK SECTION IS TEMPORARILY
C*****STORED IN SPEEDV, AND COASTING PARAMETER IS TEMPORARILY STORED    
C*****IN TIMEV      
      DO 440 I=2,NTS                                                    
      READ (IN,340) FSTS(I,IROUTE),RADTS(I,IROUTE),GRDTS(I,IROUTE),SPEED
     1V(I,IROUTE),UMXTS(I,IROUTE),DUMY6,DUMY5                           
      TIMEV(I,IROUTE) = DUMY5                                           
  440 NSEGTS(I,IROUTE) = DUMY6                                          
      NSEGTS(1,IROUTE) = 0                                              
      TIMEV(1,IROUTE) = TIMEV(2,IROUTE)                                 
C*****SET ELEVATION AT BACK END OF FIRST TRACK SECTION EQUAL TO THAT OF 
C*****FORWARD END                                                       
      SPEEDV(1,IROUTE)=SPEEDV(2,IROUTE)                                 
C*****SET SPEED LIMIT OF THIS 'ZEROTH' TRACK SECTION EQUAL TO THAT OF   
C*****FIRST                                                             
      UMXTS(1,IROUTE)=UMXTS(2,IROUTE)                                   
C*****LOOP OVER ALL EXTERNAL TRACK SECTIONS IN ROUTE                    
      DO 550 I=2,NTS                                                    
C*****COMPUTE TRACK SECTION LENGTH                                      
      DUMY2=FSTS(I,IROUTE)-FSTS(I-1,IROUTE)                             
C*****CHECK IF ELEVATION AT FORWARD END OF TRACK SECTION IS ABOUT ZERO  
      IF (ABS(SPEEDV(I,IROUTE))-0.01) 480,480,470                       
C*****IF ELEVATION IS NON-ZERO COMPUTE GRADE USING TRACK SECTION LENGTH 
C*****AND ELEVATION AT FORWARD END OF PREVIOUS TRACK SECTION - NOTE     
C*****THAT IF A NON-ZERO ELEVATION IS ENTERED GRADE INPUT IS OVERRRIDDEN
  470 GRDTS(I,IROUTE)=(SPEEDV(I,IROUTE)-SPEEDV(I-1,IROUTE))*100.0/DUMY2 
      GO TO 490                                                         
C*****IF ELEVATION IS ZERO, COMPUTE IT USING GRADE AND ELEVATION AT     
C*****FORWARD END OF PREVIOUS TRACK SECTION                             
  480 SPEEDV(I,IROUTE)=SPEEDV(I-1,IROUTE)+DUMY2*GRDTS(I,IROUTE)/100.0   
C*****FIND EXTERNAL NUMBER OF TRACK SECTION                             
  490 J=I-1                                                             
C*****CHECK IF RADIUS OF CURVATURE IS ZERO                              
      IF (ABS(RADTS(I,IROUTE))-0.01) 500,500,510                        
C*****RADIUS OF CURVATURE IS ABOUT ZERO, WRITE TRACK SECTION DATA    
C*****EXCLUDING RADIUS AND INDICATE WHETHER COASTING IS ALLOWED                                                 
  500 IF ( TIMEV(I,IROUTE) - 1 ) 505,501,505                            
C*****COASTING IN THIS TRACK SECTION                                    
  501 WRITE (OUT,125) J,FSTS(I,IROUTE),DUMY2,
     1SPEEDV(I,IROUTE),GRDTS(I,IROUTE),UMXTS(I,IROUTE),NSEGTS(I,IROUTE)
      GO TO (506,502,502) , TPOPT                                       
C*****COASTING IS PERMITTED FOR TRAIN PERFORMANCE OPTION 1, ONLY        
  502 CALL EERROR(213)                                                   
      TIMEV(I,IROUTE) = 0                                               
C*****NO COASTING IN THIS TRACK SECTION                                    
  505 WRITE (OUT,126) J,FSTS(I,IROUTE),DUMY2,
     1SPEEDV(I,IROUTE),GRDTS(I,IROUTE),UMXTS(I,IROUTE),NSEGTS(I,IROUTE)
      CALL CHECKI ( TIMEV(I,IROUTE),0,1,212 )                           
C*****SET RADIUS TO EXACTLY ZERO                                        
  506 RADTS(I,IROUTE)=0.0                                               
      GO TO 535                                                         
C*****RADIUS OF CURVATURE IS NON-ZERO WRITE ALL TRACK SECTION DATA AND
C*****INDICATE WHETHER COASTING IS ALLOWED
  510 IF ( TIMEV(I,IROUTE) - 1 ) 515,511,515                            
C*****COASTING IN THIS TRACK SECTION
  511 WRITE (OUT,130) J,FSTS(I,IROUTE),DUMY2,
     1RADTS(I,IROUTE),SPEEDV(I,IROUTE),GRDTS(I,IROUTE),UMXTS(I,IROUTE),
     2NSEGTS(I,IROUTE)            
      GO TO (516,512,512) , TPOPT                                       
C*****COASTING IS PERMITTED FOR TRAIN PERFORMANCE OPTION 1, ONLY        
  512 CALL EERROR(213)                                                   
      TIMEV(I,IROUTE) = 0                                               
C*****NO COASTING IN THIS TRACK SECTION
  515 WRITE (OUT,140) J,FSTS(I,IROUTE),DUMY2,
     1RADTS(I,IROUTE),SPEEDV(I,IROUTE),GRDTS(I,IROUTE),UMXTS(I,IROUTE),
     2NSEGTS(I,IROUTE)            
      CALL CHECKI ( TIMEV(I,IROUTE),0,1,212 )                           
C*****CHECK RADIUS                                                      
  516 IF (RADTS(I,IROUTE)-75.0) 520,530,530                             
  520 CALL EERROR (14)                                                   
C---- CHECK IF THE CENTRAL ANGLE IS MORE THAN 180 DEGREES               
  530 DUMY7 = DUMY2/RADTS(I,IROUTE)                                     
      IF (DUMY7 .GT. PI ) CALL EERROR(245)                               
C*****CHECK GRADE                                                       
  535 CALL CHECKR (GRDTS(I,IROUTE),-10.0,+10.0,15)                      
C*****CONVERT GRADE TO SINE OF ANGLE WITH HORIZONTAL FOR INTERNAL       
C*****PROGRAM USE                                                       
      GRDTS(I,IROUTE)=SIN(ATAN(GRDTS(I,IROUTE)/100.0))                  
C*****CHECK LOCATION OF FORWARD END OF TRACK SECTION                    
      CALL CHECKR (FSTS(I,IROUTE),0.0,1000000.0,12)                     
C*****CHECK TRACK SECTION LENGTH                                        
      CALL CHECKR (DUMY2,10.0,.1E08,13)                                 
C*****CHECK MAXIMUM VELOCITY ALLOWED IN TRACK SECTION IF TPOPT=1        
      GO TO (540,545,550), TPOPT                                        
  540 CALL CHECKR (UMXTS(I,IROUTE),1.0,250.0,16)                        
C**** IF COASTING HAS BEEN INDICATED FOR THIS TRACK SECTION, STORE      
C     THE SPEED LIMIT AS A NEGATIVE NUMBER                              
      IF( TIMEV(I,IROUTE) - 1 ) 545,541,545                             
  541 UMXTS(I,IROUTE)=-UMXTS(I,IROUTE)                                  
C---- CHECK + STORE ENERGY SECTOR                                       
  545 CALL CHECKI(NSEGTS(I,IROUTE),0,LMSTR, 243 )                       
      IF( NSEGTS(I,IROUTE) .GT. NSTR ) NSTR=NSEGTS(I,IROUTE)            
  550 CONTINUE                                                          
      UMXTS(1,IROUTE) = UMXTS(2,IROUTE)                                 
C*****LOOP OVER ALL TRACK SECTIONS IN ROUTE                             
      DO 560 I=1,NTS                                                    
C*****CONVERT MAXIMUM VELOCITY ALLOWED FROM MPH TO FPS                  
      UMXTS(I,IROUTE)=UMXTS(I,IROUTE)*MPHFPS                            
      SPEEDV(I,IROUTE)=0.0                                              
      TIMEV(I,IROUTE) = 0                                               
      DWLTS(I,IROUTE)=0                                                 
  560 NPETTS(I,IROUTE)=0                                                
      GO TO (570,670,670), TPOPT                                        
C                                                                       
C                                                                       
C***** INPUT FORM 8D - SCHEDULED STOPS - TPOPT=1                        
C                                                                       
C                                                                       
  570 WRITE (OUT,10) CHAR(12)
      WRITE (OUT,30) IROUTE,TITLE                                  
      READ (IN,340) DUMY1,DUMY2                                         
      NSCHED=DUMY1                                                      
      WRITE (OUT,150) NSCHED                                            
      NTS=NTSRT(IROUTE)                                                 
      NUMY2=2*NSCHED+NTSRT(IROUTE)                                      
      CALL CHECKI (NUMY2,NTS,LMTSRT,48)                                 
      IF (NEW .EQ. 1)GOTO 1370                                             
      NUMY1=DUMY2                                                       
      NPETTS(1,IROUTE)=NUMY1                                            
      WRITE (OUT,160) NUMY1                                             
      CALL CHECKI (NUMY1,-4000,+4000,121)                               
      CALL CHECKI (NUMY1,0,4000,9)                                      
      ITOTAL=NUMY1                                                      
      IF (NSCHED) 660,660,590                                           
  590 WRITE (OUT,170)                                                   
      DO 650 I=1,NSCHED                                                 
      READ (IN,340) DUMY1,DUMY2,DUMY3                                   
C --  TRUNCATE DWELL TIME TO NEAREST 0.01 SECOND                        
      NUMY2=DUMY2*100.0+0.01                                            
      DUMY2=FLOAT(NUMY2)/100.0                                          
      NUMY3=DUMY3                                                       
      WRITE (OUT,180) DUMY1,DUMY2,NUMY3                                 
      CALL CHECKR (DUMY1,FSTS(1,IROUTE),FSTS(NTS,IROUTE),49)            
      CALL CHECKI (NUMY2,0,900*100,50)                                  
      CALL CHECKI (NUMY3,-4000,+4000,121)                               
      ITOTAL=ITOTAL+NUMY3                                               
      IF (ITOTAL) 600,610,610                                           
  600 CALL EERROR (9)                                                    
  610 DO 620 J=2,NTS                                                    
      IF (FSTS(J,IROUTE)-DUMY1) 620,620,630                             
  620 CONTINUE                                                          
      J=NTS                                                             
  630 NUMY4=NTS-J+1                                                     
C---- SHIFT TRACK SECTION ARRAYS DOWN BY 2 TO ALLOW ADDITION OF         
C----    DUMMY TRACK SECTION WITH ZERO MPH SPEED WHICH FORMS THE STOP   
      DO 640 K=1,NUMY4                                                  
      NUMY5=NTS-K+1                                                     
      DWLTS(NUMY5+2,IROUTE)=DWLTS(NUMY5,IROUTE)                         
      UMXTS(NUMY5+2,IROUTE)=UMXTS(NUMY5,IROUTE)                         
      GRDTS(NUMY5+2,IROUTE)=GRDTS(NUMY5,IROUTE)                         
      FSTS(NUMY5+2,IROUTE)=FSTS(NUMY5,IROUTE)                           
      NSEGTS(NUMY5+2,IROUTE) = NSEGTS(NUMY5,IROUTE)                     
      NPETTS(NUMY5+2,IROUTE) = NPETTS(NUMY5,IROUTE)                     
  640 RADTS(NUMY5+2,IROUTE)=RADTS(NUMY5,IROUTE)                         
      NTS=NTS+2                                                         
C --- INSERT NEW T.S. AT POINT J AND J+1                                
      FSTS(J,IROUTE)=DUMY1                                              
      FSTS(J+1,IROUTE)=DUMY1                                            
      DWLTS(J,IROUTE)=0                                                 
      DWLTS(J+1,IROUTE)=NUMY2                                           
      NPETTS(J,IROUTE)=0                                                
      NPETTS(J+1,IROUTE)=NUMY3                                          
      NSEGTS(J,IROUTE)=NSEGTS(J+2,IROUTE)                               
      NSEGTS(J+1,IROUTE)=NSEGTS(J+2,IROUTE)                             
      UMXTS(J,IROUTE)=UMXTS(J+2,IROUTE)                                 
      UMXTS(J+1,IROUTE)=0.0                                             
      GRDTS(J,IROUTE)=GRDTS(J+2,IROUTE)                                 
      GRDTS(J+1,IROUTE)=GRDTS(J+2,IROUTE)                               
      RADTS(J,IROUTE)=RADTS(J+2,IROUTE)                                 
  650 RADTS(J+1,IROUTE)=RADTS(J+2,IROUTE)                               
      NTSRT(IROUTE)=NTS                                                 
  660 CONTINUE                                                          
      GO TO 890                                                         
C                                                                       
C     EXPLICIT TRAIN PERFORMANCE  -  SPEED-TIME PROFILE     TPOPT=2,3   
C                                                                       
C---- INPUT FORM 8E 
  670 WRITE (OUT,10) CHAR (12)
      WRITE (OUT,30) IROUTE,TITLE                                  
      READ (IN,340) DUMY1                                               
      NEXPDP(IROUTE)=DUMY1                                              
      NEXP=NEXPDP(IROUTE)                                               
      WRITE (OUT,190) NEXP                                              
      CALL CHECKI (NEXP,2,LMEXPD-1,45)                                  
      IF (NEW .EQ. 1)GOTO 1370                                             
      DUMY1=0.0                                                         
      READ (IN,340) DUMY2,SPEEDV(1,IROUTE),DUMY3,DUMY4                  
      DUMY2=0.0                                                         
      GO TO (690,690,700), TPOPT                                        
  690 WRITE (OUT,200)                                                   
      WRITE (OUT,210) DUMY2,SPEEDV(1,IROUTE),DUMY1                      
      QGRIDV(1,IROUTE)=0.0                                              
      GO TO 750                                                         
  700 WRITE (OUT,220)                                                   
      WRITE (OUT,230) DUMY2,SPEEDV(1,IROUTE),DUMY3,DUMY4,DUMY1          
      CALL CHECKR (DUMY3,0.0,20000.,90)                                 
      CALL CHECKR (DUMY4,0.0,20000.,91)                                 
      IF (ABS(DUMY3)-0.0001) 740,740,710                                
  710 IF (ABS(DUMY4)-0.0001) 730,730,720                                
  720 CALL EERROR (92)                                                   
  730 QGRIDV(1,IROUTE)=DUMY3                                            
      GO TO 750                                                         
  740 QGRIDV(1,IROUTE)=-DUMY4                                           
  750 CALL CHECKR (SPEEDV(1,IROUTE),0.0,250.0,47)                       
      TIMEV(1,IROUTE)=0                                                 
      NPETTS(1,IROUTE) = 0                                              
C                                                                       
      DO 830 I=2,NEXP                                                   
      READ (IN,340) DUMY2,SPEEDV(I,IROUTE),DUMY3,DUMY4                  
      TIMEV(I,IROUTE)=(DUMY2*100.0)+.5                                  
      DUMY1=DUMY1+(SPEEDV(I,IROUTE)+SPEEDV(I-1,IROUTE))*(TIMEV(I,IROUTE)
     1-TIMEV(I-1,IROUTE))*MPHFPS/200.0                                  
      GO TO (760,760,770), TPOPT                                        
  760 WRITE (OUT,210) DUMY2,SPEEDV(I,IROUTE),DUMY1                      
      QGRIDV(I,IROUTE)=0.0                                              
      GO TO 820                                                         
  770 WRITE (OUT,230) DUMY2,SPEEDV(I,IROUTE),DUMY3,DUMY4,DUMY1          
      CALL CHECKR (DUMY3,0.0,20000.,90)                                 
      CALL CHECKR (DUMY4,0.0,20000.,91)                                 
      IF (ABS(DUMY3)-0.0001) 810,810,780                                
  780 IF (ABS(DUMY4)-0.0001) 800,800,790                                
  790 CALL EERROR (92)                                                   
  800 QGRIDV(I,IROUTE)=DUMY3                                            
      GO TO 820                                                         
  810 QGRIDV(I,IROUTE)=-DUMY4                                           
  820 CALL CHECKI (TIMEV(I,IROUTE),TIMEV(I-1,IROUTE),8640000,46)        
  830 CALL CHECKR (SPEEDV(I,IROUTE),0.0,250.0,47)                       
      DO 840 I=1,NEXP                                                   
  840 SPEEDV(I,IROUTE)=SPEEDV(I,IROUTE)*MPHFPS                          
      SPEEDV(NEXP+1,IROUTE)=SPEEDV(NEXP,IROUTE)                         
      TIMEV(NEXP+1,IROUTE)=TIMEV(NEXP,IROUTE)+8640000                   
      QGRIDV(NEXP+1,IROUTE)=0.0                                         
      GO TO (880,850,870), TPOPT                                        
  850 IF (FSTS(NTS,IROUTE)-DUMY1-ORIGRT(IROUTE)-50.) 880,880,860        
  860 CALL EERROR (71)                                                   
      GO TO 880                                                         
  870 NTSRT(IROUTE)=2                                                   
      FSTS(1,IROUTE)=0.0                                                
      FSTS(2,IROUTE)=DUMY1+ORIGRT(IROUTE)                               
      GRDTS(1,IROUTE) = 0                                               
      GRDTS(2,IROUTE) = 0                                               
      RADTS(1,IROUTE) = 0                                               
      RADTS(2,IROUTE) = 0                                               
      NSEGTS(1,IROUTE) = 0                                              
      NSEGTS(2,IROUTE) = 0                                              
  880 CONTINUE                                                          
C                                                                       
C     SECTION SEQUENCING FOR ROUTE                         TPOPT = 1,2,3
C                                                                       
C---- INPUT FORM 8F                                                     
C---- SKIP IF NO SYSTEM HAS BEEN DEFINED                                
  890 IF (NOSYS) 900,1360,900                                           
  900 WRITE (OUT,10) CHAR (12)
      WRITE (OUT,30) IROUTE,TITLE                                  
      WRITE (OUT,240)                                                   
      READ (IN,340) DUMY1,DIST                                          
      NTS=NTSRT(IROUTE)                                                 
      KSECT=DUMY1                                                       
      WRITE (OUT,250) KSECT                                             
      IF (KSECT) 910,910,920                                            
  910 WRITE (OUT,320)                                                   
      GO TO 1360                                                        
  920 WRITE (OUT,260) DIST                                              
      CALL CHECKR (DIST,0.0,FSTS(NTS,IROUTE),122)                       
      WRITE (OUT,270)                                                   
      WRITE (OUT,280)                                                   
C---- IF DIST IS ZERO, ROUTE STARTS AT PORTAL OR BRANCHED JUNCTION      
      JTS=1                                                             
      IF (ABS(DIST)-0.001) 990,990,930                                  
  930 DO 940 JTS=1,NTS                                                  
      IF (FSTS(JTS,IROUTE)-DIST) 940,990,950                            
  940 CONTINUE                                                          
      GO TO 1330                                                        
  950 IF (NTS+1-LMTSRT) 970,970,960                                     
  960 CALL EERROR (48)                                                   
      GO TO 1370                                                        
C---- SHIFT TRACK SECTION ARRAYS DOWN BY 1 TO ALLOW ADDITION OF         
C----    DUMMY TRACK SECTION WITH FORWARD STATION LOCATED AT PORTAL     
  970 NUMY4=NTS-JTS+1                                                   
      DO 980 K=1,NUMY4                                                  
      NUMY5=NTS-K+1                                                     
      DWLTS(NUMY5+1,IROUTE)=DWLTS(NUMY5,IROUTE)                         
      NPETTS(NUMY5+1,IROUTE)=NPETTS(NUMY5,IROUTE)                       
      UMXTS(NUMY5+1,IROUTE)=UMXTS(NUMY5,IROUTE)                         
      GRDTS(NUMY5+1,IROUTE)=GRDTS(NUMY5,IROUTE)                         
      FSTS(NUMY5+1,IROUTE)=FSTS(NUMY5,IROUTE)                           
      NSEGTS(NUMY5+1,IROUTE)=NSEGTS(NUMY5,IROUTE)                       
  980 RADTS(NUMY5+1,IROUTE)=RADTS(NUMY5,IROUTE)                         
C --  INSERT TRACK SECTION BOUNDARY AT 'DIST'                           
C --  ARRAYS UMXTS, GRDTS, AND NSEGTS ARE SAME FOR JTS AND JTS+1        
      DWLTS(JTS,IROUTE)=0                                               
      NPETTS(JTS,IROUTE)=0                                              
      FSTS(JTS,IROUTE)=DIST                                             
      NTSRT(IROUTE)=NTSRT(IROUTE)+1                                     
      NTS=NTSRT(IROUTE)                                                 
C                                                                       
C --  READ SECTIONS AND INSERT SEGMENTS INTO TRACK SECTION ARRAYS       
  990 ISCT=0                                                            
      ISIGN=0                                                           
      JTS = JTS + 1                                                     
      DO 1270 ISEC=1,KSECT                                              
      ISCT2=ISCT                                                        
      ISIGN2=ISIGN                                                      
      READ (IN,340) DUMY1                                               
      ISCTX=ABS(DUMY1)                                                  
      ISIGN=DUMY1/(ABS(DUMY1)-.01)                                      
      CALL CHECKI (ISCTX,1,LMSCTX,153)                                  
      IF (NEW .EQ. 1)GOTO 1010                                             
      ISCT = TABL14(ISCTX)                                              
      IF (ISCT) 1010,1010,1000                                          
 1000 IF( TABL9(ISCT) ) 1020,1010,1020                                  
 1010 WRITE(OUT,1011) ISCTX                                             
      CALL EERROR( 124 )                                                 
      GO TO 1370                                                        
 1020 IA=ISCTX*ISIGN                                                    
      K2=0                                                              
      ISEGL=TABL9(ISCT)                                                 
      ISEGH=TABL10(ISCT)                                                
      IF (ISEGL) 1030,1030,1040                                         
 1030 WRITE(OUT,1011) ISCTX                                             
      CALL EERROR ( 180 )                                                
      GO TO 1370                                                        
C---- LOOP OVER SEGMENTS IN THIS SECTION                                
C --  THIS 'DO 1260' RUNS FORWARD IF ISIGN IS +, BACKWARD IF ISIGN IS - 
 1040 IF (ISIGN) 1060,1270,1050                                         
 1050 ISEG=ISEGL                                                        
      ISEG2=ISEGH                                                       
      INC=+1                                                            
      GO TO 1070                                                        
 1060 ISEG=ISEGH                                                        
      ISEG2=ISEGL                                                       
      INC=-1                                                            
C                                                                       
 1070 DIST1=DIST                                                        
      DIST=DIST+LLS(ISEG)                                               
      IB=ISIGN*NUMLS(ISEG)                                              
      IF (K2) 1080,1080,1170                                            
 1080 WRITE (OUT,290) IA,IB,DIST1,DIST                                  
      K2=1                                                              
      CALL CHECKI (ISCT,1,NSEC,124)                                     
C---- CHECK SECTION CONSISTANCY                                         
      IF (ISEC-1) 1180,1180,1090                                        
 1090 IF (ISIGN) 1110,1100,1100                                         
 1100 NODE=TABL12(ISCT)                                                 
      GO TO 1120                                                        
 1110 NODE=TABL13(ISCT)                                                 
 1120 IF (ISIGN2) 1130,1140,1140                                        
 1130 NODE2=TABL12(ISCT2)                                               
      GO TO 1150                                                        
 1140 NODE2=TABL13(ISCT2)                                               
C --  CHECK THAT BOTH SECTIONS ARE CONNECTED TO THE SAME NODE           
 1150 IF (NODE-NODE2) 1160,1180,1160                                    
 1160 CALL EERROR (164)                                                  
      GO TO 1180                                                        
 1170 WRITE (OUT,300) IB,DIST1,DIST                                     
C --  SEARCH FORWARD IN TRACK SECTIONS UNTIL PAST 'DIST'                
 1180 IF (FSTS(JTS,IROUTE)-DIST) 1190,1245,1210                         
 1190 NSEGTS(JTS,IROUTE) = ISIGN * ( ISEG*128 + NSEGTS(JTS,IROUTE) )    
      JTS=JTS+1                                                         
      IF (JTS-NTS) 1180,1180,1280                                       
C --  CREATE NEW TRACK SECTION BOUNDARY AT 'DIST'                       
 1210 IF (NTS+1-LMTSRT) 1230,1230,1220                                  
 1220 CALL EERROR (48)                                                   
      GO TO 1370                                                        
 1230 NUMY4=NTS-JTS+1                                                   
C---- SHIFT TRACK SECTION ARRAYS DOWN BY 1 TO ALLOW ADDITION OF         
C----    DUMMY TRACK SECTION WITH FORWARD STATION LOCATED AT SEGMENT    
C----    BOUNDARY                                                       
      DO 1240 K=1,NUMY4                                                 
      NUMY5=NTS-K+1                                                     
      DWLTS(NUMY5+1,IROUTE)=DWLTS(NUMY5,IROUTE)                         
      NPETTS(NUMY5+1,IROUTE)=NPETTS(NUMY5,IROUTE)                       
      UMXTS(NUMY5+1,IROUTE)=UMXTS(NUMY5,IROUTE)                         
      GRDTS(NUMY5+1,IROUTE)=GRDTS(NUMY5,IROUTE)                         
      FSTS(NUMY5+1,IROUTE)=FSTS(NUMY5,IROUTE)                           
      NSEGTS(NUMY5+1,IROUTE) = NSEGTS(NUMY5,IROUTE)                     
 1240 RADTS(NUMY5+1,IROUTE)=RADTS(NUMY5,IROUTE)                         
C --  INSERT TRACK SECTION BOUNDARY AT 'DIST'                           
      DWLTS(JTS,IROUTE)=0                                               
      NPETTS(JTS,IROUTE)=0                                              
      FSTS(JTS,IROUTE)=DIST                                             
      NTSRT(IROUTE)=NTSRT(IROUTE)+1                                     
      NTS=NTSRT(IROUTE)                                                 
 1245 NSEGTS(JTS,IROUTE) = ISIGN * ( ISEG*128 + NSEGTS(JTS,IROUTE) )    
      JTS = JTS + 1                                                     
C --  INCREMENT COUNTER AND TEST                                        
 1250 IF (ISEG-ISEG2) 1260,1270,1260                                    
 1260 ISEG=ISEG+INC                                                     
      GO TO 1070                                                        
C --                                                                    
 1270 CONTINUE                                                          
      GO TO 1360                                                        
C---- ROUTE TERMINATES WITHIN SYSTEM                                    
C --  IF MORE SEGMENTS OR SECTIONS ARE REMAINING WRITE ERROR MESSAGE    
C --  AND PRINT THEM                                                    
 1280 IF ((ISEG-ISEG2)+(ISEC-KSECT)) 1290,1350,1290                     
 1290 CALL EERROR (125)                                                  
 1300 IF (ISEG-ISEG2) 1310,1320,1310                                    
 1310 ISEG=ISEG+INC                                                     
      IB = ISIGN * NUMLS(ISEG)                                          
      WRITE(OUT,310) IA,IB                                              
      GO TO 1300                                                        
 1320 IF (ISEC-KSECT) 1330,1350,1350                                    
 1330 DO  1340  I = ISEC, KSECT                                         
      READ (IN,340) DUMY1                                               
      ISCT=DUMY1                                                        
 1340 WRITE (OUT,300) ISCT                                              
 1350 CONTINUE                                                          
C                                                                       
 1360 CONTINUE                                                          
C                                                                       
C---- DUMP TRACK SECTION ARRAYS                                         
C                                                                       
      GO TO (1370,1365,1365,1365,1365,1365,1370,1370), SUPOPT           
 1365 WRITE (OUT,10) CHAR (12)
      WRITE(OUT,341) IROUTE                                             
      WRITE(OUT,342)                                                    
      NTS = NTSRT(IROUTE)                                               
      WRITE(OUT,343) (I,FSTS(I,IROUTE),RADTS(I,IROUTE),GRDTS(I,IROUTE), 
     1  UMXTS(I,IROUTE),DWLTS(I,IROUTE),NPETTS(I,IROUTE),               
     2   NSEGTS(I,IROUTE), I=1,NTS)                                     
C                                                                       
C                                                                       
 1370 RETURN                                                            
      END                                                               
