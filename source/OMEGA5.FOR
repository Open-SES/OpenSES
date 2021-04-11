      SUBROUTINE OMEGA5( ILS, ISCT )                                    
C                                                                       
C*****THIS SUBROUTINE COMPUTES THE AERODYNAMIC FORCING FUNCTION, TRAIN  
C*****HEAD CHANGE (WHEN NECESSARY), AND INERTIAL TERM FOR EACH LINE     
C*****SECTION                                                           
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C*****ARRAY TRLIST IS DIMENSIONED LMTRSG                                
      INTEGER TRLIST(LMTRSG), AHTRRG, AHTDRG                                 
      REAL LRG, KRVF, KRVB                                              
C*****ARRAY TRDIST IS DIMENSIONED 2*LMTRSG+1                            
      DIMENSION TRDIST(2*LMTRSG+1)                                              
C                                                                       
C                                                                       
C---- INITIALIZE SECTION PARAMETERS                                     
      Q = QS(ISCT)                                                      
      NLOWS = TABL9(ISCT)                                               
      NHIGHS = TABL10(ISCT)                                             
C---- FIND THE TEMPERATURE CORRECTION FOR THE SUBSEGMENT OR NODE AT     
C---- EITHER END OF THIS SEGMENT (SIMILAR CODE IN PINPNT)               
      ILSSB = TABL3(NLOWS)                                              
      ILSSH = TABL4(NHIGHS)                                             
      ILOWSS = TABL3(ILS)                                               
      IHISS = TABL4(ILS)                                                
C --  CHECK BACK (LOW) END                                              
      IF( ILOWSS .NE. ILSSB ) GO TO 30                                  
      NODBB = TABL20(ISCT) / 64                                         
      TADJB = ( TDBTN(NODBB) + ABTEMP ) / ABSAMB                        
      GO TO 50                                                          
30    TADJB = TSSTAB(ILOWSS - 1 )                                       
C --  CHECK FORWARD (HIGH) END                                          
50    IF( IHISS .NE. ILSSH ) GO TO 60                                   
      NODFF = TABL21(ISCT) / 64                                         
      TADJF = ( TDBTN(NODFF) + ABTEMP ) / ABSAMB                        
      GO TO 70                                                          
60    TADJF = TSSTAB( IHISS + 1 )                                       
70    CONTINUE                                                          
C                                                                       
C*****INITIALIZE LINE SEGMENT AND REGION PARAMETERS THAT MUST ONLY BE   
C*****INITIALIZED ONCE FOR EACH LINE SEGMENT                            
C*****NUMBER OF TRAINS IN LINE SEGMENT - EQUALS NUMBER OF LOCATIONS IN  
C*****USE IN ARRAY TRLIST                                               
      NTRLS=NTRNLS(ILS)                                                 
C*****NUMBER OF LOCATIONS IN USE IN ARRAY TRDIST                        
      NTRLS2=2*NTRLS+1                                                  
C*****NUMBER OF TRAINS IN REGION                                        
      NTRRG=0                                                           
C*****REGION NUMBER                                                     
      NRGN = 0                                                          
      AHTRRG=0                                                          
      ITAHRG=0                                                          
C*****DISTANCE TO BACKWARD END OF REGION AS MEASURED FROM BACKWARD END  
C*****OF LINE SEGMENT                                                   
      XBKRG=0.0                                                         
C*****CREATE LIST OF DISTANCES TO BE CONSIDERED AS DISTANCES TO FORWARD 
C*****END OF REGIONS AS MEASURED FROM BACKWARD END OF LINE SEGMENT.     
C*****DISTANCES NOT TO BE CONSIDERED (I.E. NEGATIVE ONES) ARE SET TO    
C*****1000000.0 AND A COUNT IS MADE OF THE NUMBER OF TRAINS IN THE FIRST
C*****REGION. DISTANCES IN ARRAY POSITIONS 2 TO NTRLS*2+1 ARE ENDS OF   
C*****TRAINS                                                            
      NSS = TABL4(ILS) - TABL3(ILS) + 1                                 
      DLSS = LLS(ILS) / FLOAT( NSS )                                    
      ISS = TABL3(ILS)                                                  
      JSS = 1                                                           
      TRDIST(1) = DLSS * FLOAT( JSS )                                   
      DO 150 IT=1,NTRLS                                                 
      IF (TRNDLS(ILS,2*IT-1)) 130,140,140                               
130   NTRRG=NTRRG+1                                                     
      TRDIST(2*IT)=1000000.0                                            
      NUMY1=TRNNLS(ILS,IT)                                              
      TRLIST(IT)=NUMY1/IABS(NUMY1)                                      
      GO TO 150                                                         
140   TRDIST(2*IT)=TRNDLS(ILS,2*IT-1)                                   
      TRLIST(IT)=0                                                      
150   TRDIST(2*IT+1)=TRNDLS(ILS,2*IT)                                   
C                                                                       
C---- BEGIN REGION ANALYSIS LOOP                                        
C                                                                       
C*****COMPUTE REGION NUMBER                                             
160   NRGN = NRGN + 1                                                   
C*****INITIALIZE REGION PARAMETERS                                      
C*****REGION AREA                                                       
      ANETRG=ALS(ILS)                                                   
C*****TOTAL PERIMETER OF REGION EXPOSED TO AIR FLOW                     
      PNETRG=PERMLS(ILS)                                                
C*****TOTAL TRAIN VOLUME FLOW RATE IN REGION                            
      AVUV=0.0                                                          
C*****TIME RATE OF CHANGE OF TOTAL TRAIN VOLUME FLOWRATE IN REGION      
      AVDUDT=0.0                                                        
C*****LOCATION IN ARRAY TRDIST OF DISTANCE TO FORWARD END OF REGION AS  
C*****MEASURED FROM BACKWARD END OF LINE SEGMENT                        
      JTAHRG=1                                                          
C*****DISTANCE TO FORWARD END OF REGION AS MEASURED FROM BACKWARD END   
C*****OF LINE SEGMENT                                                   
      XAHRG=TRDIST(1)                                                   
C*****COMPUTE DISTANCE TO FORWARD END OF REGION AS MEASURED FROM        
C*****BACKWARD END OF LINE SEGMENT. OF THE LIST OF DISTANCES TO BE      
C*****CONSIDERED THIS IS THE SHORTEST.                                  
      DO 180 IT=2,NTRLS2                                                
      IF (TRDIST(IT)-XAHRG) 170,180,180                                 
170   XAHRG=ABS(TRDIST(IT))                                             
      JTAHRG=IT                                                         
180   CONTINUE                                                          
C*****CHECK IF FORWARD END OF REGION IS FORWARD END OF LINE SEGMENT.    
      IF (JTAHRG-1) 190,190,200                                         
190   ITAHRG=0                                                          
      JTAHRG=0                                                          
      AHTRRG=0                                                          
      GO TO 210                                                         
C*****COMPUTE LOCATION IN ARRAY TRLIST OF TRAIN WHOSE END IS FORWARD END
C*****OF REGION                                                         
200   ITAHRG=JTAHRG/2                                                   
C*****COMPUTE NUMBER OF TRAIN WHOSE END IS FORWARD END OF REGION - IF   
C*****AHTRRG IS ZERO THE FORWARD END OF THE REGION IS ALSO THE FORWARD  
C*****END OF THE LINE SEGMENT                                           
      NUMY1=TRNNLS(ILS,ITAHRG)                                          
      AHTRRG=IABS(NUMY1)                                                
C*****COMPUTE DIRECTION OF THIS TRAIN WITH RESPECT TO THE LINE SEGMENT  
      AHTDRG=AHTRRG/NUMY1                                               
C*****ADJUST REGION PARAMETERS FOR PRESENCE OF TRAINS - IF ANY          
210   IF (NTRRG) 250,250,220                                            
C*****LOOP OVER TRAINS IN LINE SEGMENT                                  
220   DO 240 IT=1,NTRLS                                                 
C*****CHECK TO SEE IF TRAIN IS IN REGION - 0 NO, -1 OR +1 YES, WITH SIGN
C*****INDICATING DIRECTION OF TRAIN WITH RESPECT TO LINE SEGMENT        
      IF (TRLIST(IT)) 230,240,230                                       
C*****TRAIN NUMBER                                                      
230   NUMV=TRNNLS(ILS,IT)                                               
      NUMV=IABS(NUMV)                                                   
C*****TRAIN TYPE                                                        
      ITYP=ITYPV(NUMV)                                                  
C*****REGION AREA                                                       
      ANETRG=ANETRG-AV(ITYP)                                            
C*****TOTAL PERIMETER OF REGION EXPOSED TO AIR FLOW                     
      PNETRG=PNETRG+PERMV(ITYP)                                         
C*****TOTAL TRAIN VOLUME FLOWRATE FOR REGION                            
      AVUV=AVUV+AV(ITYP)*TRLIST(IT)*UV(NUMV)                            
C*****TIME RATE OF CHANGE OF TOTAL TRAIN VOLUME FLOWRATE FOR REGION     
      AVDUDT=AVDUDT+AV(ITYP)*TRLIST(IT)*DUDTV(NUMV)                     
240   CONTINUE                                                          
C --  COMPUTE REGION PARAMETERS                                         
C*****LENGTH OF REGION                                                  
250   LRG=XAHRG-XBKRG                                                   
C*****HYDRAULIC DIAMETER OF REGION                                      
      DRG=4.0*ANETRG/PNETRG                                             
C*****ADD REGION INERTIAL TERM TO LINE SECTION INERTIAL TERM            
      BS(ISCT)=BS(ISCT)+LRG/ANETRG                                      
C*****AIR FLOW RATE IN REGION                                           
      QNETRG=Q-AVUV                                                     
C*****AIR VELOCITY IN REGION                                            
      VNETRG=QNETRG/ANETRG                                              
C*****ABSOLUTE VELOCITY OF AIR FLOW IN REGION                           
      ABVNRG=ABS(VNETRG)                                                
C                                                                       
C --  CHECK FOR FIRST REGION IN SEGMENT                                 
C                                                                       
      IF( NRGN - 1 ) 260,260,390                                        
C---- FIRST REGION IN LINE SEGMENT                                      
C*****HEAD CHANGES AT BACK END OF LINE SEGMENT - ONLY COMPUTED IF BACK  
C*****END OF REGION IS BACK END OF LINE SEGMENT - OTHERWISE ZERO        
260   SUM3=-VNETRG *(CBPLS(ILS)*(VNETRG+ABVNRG)*TADJB -                 
     1               CBNLS(ILS)*(VNETRG-ABVNRG)*TSSTAB(ILOWSS) )/4.0    
C*****FIND LINE SEGMENT TYPE                                            
      ITYPLS= IABS(TYPLS(ILS))                                          
C*****BRANCH ON SEGMENT TYPE - REGULAR,IMPULSE FAN                      
      GO TO (269,269,269,269,269,269,269,269,261,261,261,261,261,261,   
     1       269,269), ITYPLS                                           
C*****IMPULSE FAN - CHECK IF IT'S FULLY OPERATIONAL                     
261   IFS = ITYPLS-8                                                    
      IF (NTIME-TONIFS(IFS)) 269,262,262                                
262   IF (NTIME-TOFIFS(IFS)) 263,263,269                                
C*****COMPUTE NET GAIN IN TOTAL HEAD CAUSED BY IMPULSE FAN USING        
C*****'THRUST' EQUATION                                                 
C --  IFS IS ASSUMED TO USE OUTSIDE AIR OR BE UPSTREAM OF               
C --  FIRE - HENCE NO DENSITY ADJUSTMENT                                
263   GAIN=QIFS(IFS)*(VIFS(IFS)-VNETRG)/ANETRG                          
      SUM3=SUM3+GAIN                                                    
269   IF (ILS-NLOWS) 400,270,400                                        
C*****STORE AIR VELOCITY AND AREA AT BACKWARD END OF SECTION FOR LATER  
C*****USE BY JUNCTION SUBROUTINES                                       
270   VLOWS(ISCT)=VNETRG                                                
      ALOWS(ISCT)=ANETRG                                                
C*****COMPUTATION AND STORAGE OF TRAIN-AT-NODE DATA FOR USE BY JUNCTION 
C*****SUBROUTINES - COMPUTED ONLY FOR MULTIPLY BRANCHED JUNCTIONS       
C*****FIND NUMBER OF NODE AT BACK END OF SEGMENT                        
      IF(KAERO) 400,275,400                                             
275   INODE=TABL12(ISCT)                                                
C*****FIND RANGE OF LOCATIONS IN ARRAY TABL2 WHERE NODE DATA IS STORED  
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
C*****CHECK IF NODE IS A MULTIPLY BRANCHED JUNCTION                     
      IF (NHIGH-NLOW) 400,280,280                                       
C*****NODE IS A MULTIPLY BRANCHED JUNCTION. COMPUTE LOWER RANGE OF      
C*****LOCATIONS OF TRAIN-AT-NODE DATA IN ARRAY TABL2 (UPPER RANGE       
C*****IS THE SAME). FOR EACH NODE THAT IS A MULTIPLY BRANCHED JUNCTION  
C*****THE TRAIN-AT-NODE DATA IS STORED IN THE UPPER 3*LMTRRT LOCATIONS  
C*****OF THE PORTION OF ARRAY TABL2 SET ASIDE FOR THIS NODE. IF NODE    
C*****INODE IS A MULTIPLY BRANCHED JUNCTION THEN THE NODE-AT-TRAIN DATA 
C*****IS STORED IN ARRAY TABL2 FROM TABL1(INODE+1)-3*LMTRRT TO          
C*****TABL1(INODE+1)-1. EACH TRAIN FOR EACH NODE THAT IT'S AT GENERATES 
C*****THREE CONSECUTIVE ENTRIES IN ARRAY TABL2. THE FIRST IS THE TRAIN  
C*****NUMBER. THE SECOND IS THE SECTION THE TRAIN IS LEAVING. THE THIRD 
C*****IS THE SECTION THE TRAIN IS ENTERING. SINCE LMTRRT TRAINS MAY BE  
C*****PASSING THROUGH A NODE AT THE SAME TIME THEN 3*LMTRRT LOCATIONS   
C*****ARE REQUIRED.                                                     
280   NLOW=NHIGH-3*LMTRRT+1                                             
C*****LOOP OVER ALL TRAINS IN SEGMENT                                   
      DO 380 IT=1,NTRLS                                                 
C*****CHECK IF TRAIN IS IN REGION                                       
      IF (TRLIST(IT)) 290,380,290                                       
C*****TRAIN IS IN REGION, AND ALSO AT NODE, FIND ITS NUMBER             
290   NUMV=TRNNLS(ILS,IT)                                               
      NUMV=IABS(NUMV)                                                   
C*****LOOP OVER POSSIBLE LOCATIONS IN ARRAY TABL2 WHERE TRAIN-AT-NODE   
C*****DATA MAY BE STORED                                                
      DO 370 ITBL2=NLOW,NHIGH,3                                         
C*****CHECK IF ANY TRAIN-AT-NODE DATA HAS BEEN ENTERED IN THIS SET OF   
C*****LOCATIONS                                                         
      IF (TABL2(ITBL2)) 330,300,330                                     
C*****NO TRAIN-AT-NODE DATA HAS BEEN ENTERED -  ENTER TRAIN NUMBER      
300   TABL2(ITBL2)=NUMV                                                 
C*****CHECK TRAIN DIRECTION                                             
      IF (TRLIST(IT)) 310,380,320                                       
C*****TRAIN IS LEAVING SECTION                                          
310   TABL2(ITBL2+1)=ISCT                                               
      GO TO 380                                                         
C*****TRAIN IS ENTERING SECTION                                         
320   TABL2(ITBL2+2)=ISCT                                               
      GO TO 380                                                         
C*****CHECK IF TRAIN ALREADY ENTERED IS THIS TRAIN                      
330   IF (TABL2(ITBL2)-NUMV) 370,340,370                                
C*****CHECK TRAIN DIRECTION                                             
340   IF (TRLIST(IT)) 350,380,360                                       
C*****TRAIN IS LEAVING SECTION                                          
350   TABL2(ITBL2+1)=ISCT                                               
      GO TO 380                                                         
C*****TRAIN IS ENTERING SECTION                                         
360   TABL2(ITBL2+2)=ISCT                                               
      GO TO 380                                                         
C*****PLACE TO ENTER TRAIN DATA HAS NOT YET BEEN FOUND - KEEP LOOKING   
370   CONTINUE                                                          
380   CONTINUE                                                          
      GO TO 400                                                         
C---- NOT FIRST REGION IN LINE SEGMENT                                  
390   SUM3=0.0                                                          
C*****COMPUTE REYNOLDS NUMBER IN REGION                                 
400   RERG=ABVNRG*DRG/NUAIR                                             
C --  ADJUST REYNOLDS NO. FOR HIGH TEMP                                 
      RERG = RERG * (VISAIR /(RHOMAS*AIR(TDBSS(ISS),1)))* TSSTAB(ISS)   
C*****CHECK FOR LAMINAR OR TURBULENT FLOW IN REGION                     
      IF (RERG-2000.0) 410,420,420                                      
C*****REGION VISCOUS HEAD CHANGE FOR LAMINAR FLOW IN REGION AT LINE     
C*****SEGMENT WALL                                                      
410   SUM4=-8.0*PERMLS(ILS)*LRG*AIR(TDBSS(ISS),1)*VNETRG/(ANETRG*DRG)   
     1  * TSSTAB(ISS)                                                   
      GO TO 430                                                         
C*****TURBULENT FLOW DARCY-WEISBACH FRICTION FACTOR BY KENNEDY CURVE FIT
420   FRG=0.0055*(1.0+((19000.0+16000.0*((EPSLS(ILS)/(0.05*DRG))**1.5))*
     1EPSLS(ILS)/DRG+1000000.0/RERG)**0.33333333)                       
C*****REGION VISCOUS HEAD CHANGE FOR TURBULENT FLOW IN REGION AT LINE   
C*****SEGMENT WALL                                                      
      SUM4=-FRG*PERMLS(ILS)*LRG*VNETRG*ABVNRG/(8.0*ANETRG)*TSSTAB(ISS)  
C*****HEAD CHANGE IN REGION CAUSED BY TRAIN ACCELERATION                
430   SUM5=LRG*AVDUDT/ANETRG                                            
C*****INITIALIZE REGION VISCOUS HEAD CHANGE FOR FLOW AT PERIMETERS OF   
C*****ALL TRAINS IN REGION                                              
      SUM7=0.0                                                          
C*****LOOP OVER TRAINS IN LINE SEGMENT                                  
      DO 470 IT=1,NTRLS                                                 
C*****CHECK IF TRAIN IS IN REGION                                       
      IF (TRLIST(IT)) 440,470,440                                       
C*****TRAIN NUMBER                                                      
440   NUMV=TRNNLS(ILS,IT)                                               
      NUMV=IABS(NUMV)                                                   
C*****TRAIN TYPE                                                        
      ITYP=ITYPV(NUMV)                                                  
C*****AIR VELOCITY IN REGION RELATIVE TO TRAIN                          
      VRVRG=VNETRG-TRLIST(IT)*UV(NUMV)                                  
C*****ABSOLUTE AIR VELOCITY IN REGION RELATIVE TO TRAIN                 
      ABVVRG=ABS(VRVRG)                                                 
C*****COMPUTE (TRAIN SKIN VISCOUS FORCE/AIR MASS DENSITY)               
      SUM7A=-LAMDAV(ITYP)*LRG*VRVRG*ABVVRG* TSSTAB(ISS) / 8.0           
C*****CHECK FOR NECESSITY OF TRAIN DRAG COMPUTATION - SEE DEFINITION OF 
C*****KAERO IN DICTIONARY OF VARIABLES FOR PHILOSOPHY BEHIND THIS       
      IF (KAERO) 460,450,460                                            
C*****ADD TRAIN HEAD CHANGE CAUSED BY TRAIN SKIN VISCOUS FORCE TO TOTAL 
C*****TRAIN HEAD CHANGE. ADD ESTIMATED TRAIN HEAD CHANGE CAUSED BY      
C*****AIRFLOW ACCELERATION TO TOTAL TRAIN HEAD CHANGE USING PREVIOUSLY  
C*****KNOWN VALUE OF AIRFLOW ACCELERATION.                              
450   DRAGV(NUMV)=DRAGV(NUMV)+TRLIST(IT)*(SUM7A/AV(ITYP)-LRG*DQDTS(ISCT)
     1/ANETRG)                                                          
C*****COMPUTE REGION VISCOUS HEAD CHANGE FOR FLOW AT PERIMETER OF ALL   
C*****TRAINS IN REGION BY ADDING COMPONENT FOR A TRAIN                  
460   SUM7=SUM7+SUM7A/ANETRG                                            
470   CONTINUE                                                          
C---- CHECK IF END OF REGION IS CAUSED BY                               
C     1. END OF SUBSEGMENT -   GO TO 711                                
C     2. END OF TRAIN -        GO TO 610                                
C     3. END OF SEGMENT -      GO TO 480                                
C                                                                       
      IF( AHTRRG ) 610,475,610                                          
  475 IF( NSS - JSS ) 480,480,711                                       
C --  END OF REGION IS CAUSED BY END OF LINE SEGMENT                    
C*****HEAD CHANGES AT FRONT END OF LINE SEGMENT - ONLY COMPUTED IF FRONT
C*****END OF REGION IS FRONT END OF LINE SEGMENT - OTHERWISE ZERO       
480   SUM8=-VNETRG *(CFPLS(ILS)*(VNETRG+ABVNRG)*TSSTAB(IHISS) -         
     1               CFNLS(ILS)*(VNETRG-ABVNRG)*TADJF ) / 4.0           
C*****SET HEAD CHANGE AT FORWARD END OF REGION CAUSED BY END OF TRAIN   
C*****TO ZERO                                                           
      SUM9=0.0                                                          
C --  IF SEGMENT IS AT END OF SECTION STORE TRAIN DATA WITH NODE        
      IF (ILS-NHIGHS) 720,490,720                                       
C*****STORE AIR VELOCITY AND AREA AT FORWARD END OF SECTION FOR LATER   
C*****USE BY JUNCTION SUBROUTINES                                       
490   VHIGHS(ISCT)=VNETRG                                               
      AHIGHS(ISCT)=ANETRG                                               
C*****COMPUTATION AND STORAGE OF TRAIN-AT-NODE DATA FOR USE BY JUNCTION 
C*****SUBROUTINES - COMPUTED ONLY FOR MULTIPLY BRANCHED JUNCTIONS       
C*****FIND NUMBER OF NODE AT FORWARD END OF SEGMENT                     
      IF(KAERO) 720,495,720                                             
495   INODE=TABL13(ISCT)                                                
C*****FIND RANGE OF LOCATIONS IN ARRAY TABL2 WHERE NODE DATA IS STORED  
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
C*****CHECK IF NODE IS A MULTIPLY BRANCHED JUNCTION                     
      IF (NHIGH-NLOW) 720,500,500                                       
C*****NODE IS A MULTIPLY BRANCHED JUNCTION. COMPUTE LOWER RANGE OF      
C*****LOCATIONS OF TRAIN-AT-NODE DATA IN ARRAY TABL2 (UPPER RANGE       
C*****IS THE SAME). FOR EACH NODE THAT IS A MULTIPLY BRANCHED JUNCTION  
C*****THE TRAIN-AT-NODE DATA IS STORED IN THE UPPER 3*LMTRRT LOCATIONS  
C*****OF THE PORTION OF ARRAY TABL2 SET ASIDE FOR THIS NODE. IF NODE    
C*****INODE IS A MULTIPLY BRANCHED JUNCTION THEN THE NODE-AT-TRAIN DATA 
C*****IS STORED IN ARRAY TABL2 FROM TABL1(INODE+1)-3*LMTRRT TO          
C*****TABL1(INODE+1)-1. EACH TRAIN FOR EACH NODE THAT IT'S AT GENERATES 
C*****THREE CONSECUTIVE ENTRIES IN ARRAY TABL2. THE FIRST IS THE TRAIN  
C*****NUMBER. THE SECOND IS THE SECTION THE TRAIN IS LEAVING. THE THIRD 
C*****IS THE SECTION THE TRAIN IS ENTERING. SINCE LMTRRT TRAINS MAY BE  
C*****PASSING THROUGH A NODE AT THE SAME TIME THEN 3*LMTRRT LOCATIONS   
C*****ARE REQUIRED.                                                     
500   NLOW=NHIGH-3*LMTRRT+1                                             
C*****LOOP OVER ALL TRAINS IN SEGMENT                                   
      DO 600 IT=1,NTRLS                                                 
C*****CHECK IF TRAIN IS IN REGION                                       
      IF (TRLIST(IT)) 510,600,510                                       
C*****TRAIN IS IN REGION, AND ALSO AT NODE, FIND ITS NUMBER             
510   NUMV=TRNNLS(ILS,IT)                                               
      NUMV=IABS(NUMV)                                                   
C*****LOOP OVER POSSIBLE LOCATIONS IN ARRAY TABL2 WHERE TRAIN-AT-NODE   
C*****DATA MAY BE STORED                                                
      DO 590 ITBL2=NLOW,NHIGH,3                                         
C*****CHECK IF ANY TRAIN-AT-NODE DATA HAS BEEN ENTERED IN THIS SET OF   
C*****LOCATIONS                                                         
      IF (TABL2(ITBL2)) 550,520,550                                     
C*****NO TRAIN-AT-NODE DATA HAS BEEN ENTERED -  ENTER TRAIN NUMBER      
520   TABL2(ITBL2)=NUMV                                                 
C*****CHECK TRAIN DIRECTION                                             
      IF (TRLIST(IT)) 530,600,540                                       
C*****TRAIN IS ENTERING SECTION                                         
530   TABL2(ITBL2+2)=ISCT                                               
      GO TO 600                                                         
C*****TRAIN IS LEAVING SECTION                                          
540   TABL2(ITBL2+1)=ISCT                                               
      GO TO 600                                                         
C*****CHECK IF TRAIN ALREADY ENTERED IS THIS TRAIN                      
550   IF (TABL2(ITBL2)-NUMV) 590,560,590                                
C*****CHECK TRAIN DIRECTION                                             
560   IF (TRLIST(IT)) 570,600,580                                       
C*****TRAIN IS ENTERING SECTION                                         
570   TABL2(ITBL2+2)=ISCT                                               
      GO TO 600                                                         
C*****TRAIN IS LEAVING SECTION                                          
580   TABL2(ITBL2+1)=ISCT                                               
      GO TO 600                                                         
C*****PLACE TO ENTER TRAIN DATA HAS NOT YET BEEN FOUND - KEEP LOOKING   
590   CONTINUE                                                          
600   CONTINUE                                                          
      GO TO 720                                                         
C                                                                       
C---- END OF REGION IS CAUSED BY END OF TRAIN (EITHER FRONT OR REAR )   
610   SUM8=0.0                                                          
C*****FIND THE TYPE OF TRAIN THAT FORMS THE FORWARD END OF THE REGION   
      ITYP=ITYPV(AHTRRG)                                                
C*****FIND ITS AREA                                                     
      ARV=AV(ITYP)                                                      
C*****FIND ITS VELOCITY RELATIVE TO THE LINE SEGMENT                    
      URV=AHTDRG*UV(AHTRRG)                                             
C*****CHECK WHETHER TRAIN WHOSE END FORMS THE FORWARD END OF THIS REGION
C*****LIES IN THIS REGION OR IN THE NEXT REGION                         
      NUMY1=JTAHRG-2*(JTAHRG/2)                                         
      IF (NUMY1) 620,670,620                                            
C*****TRAIN LIES IN THIS REGION - COMPUTE VELOCITY IN REGION AHEAD OF   
C*****TRAIN                                                             
620   VTRP1=(QNETRG+ARV*URV)/(ANETRG+ARV)                               
C*****COMPUTE AIR VELOCITY RELATIVE TO TRAIN                            
      VRELTR=VTRP1-URV                                                  
C*****CHECK DIRECTION OF AIRFLOW RELATIVE TO TRAIN                      
      IF (VRELTR) 630,630,650                                           
C*****NEGATIVE AIRFLOW RELATIVE TO THE TRAIN, THEREFORE, END OF TRAIN   
C*****MUST BE 'FRONT' END                                               
630   KRVF=ARV*((2.0*ANETRG+ARV)/(ANETRG**2)+CDFVOV(ITYP)/(ANETRG+ARV)) 
C*****COMPUTE FORCING FUNCTION CAUSED BY TRAIN                          
      SUM9=0.5*(VTRP1**2-VNETRG**2+KRVF*VRELTR**2) *TSSTAB(ISS)         
C*****CHECK FOR NECESSITY OF TRAIN AIR DRAG COMPUTATION                 
      IF (KAERO) 720,640,720                                            
640   DRAGV(AHTRRG)=DRAGV(AHTRRG)+AHTDRG*(ARV*(ANETRG+ARV)/(ANETRG**2)+C
     1DFVOV(ITYP))*VRELTR*VRELTR *TSSTAB(ISS) / 2.0                     
      GO TO 720                                                         
C*****POSITIVE AIRFLOW RELATIVE TO THE TRAIN, THEREFORE, END OF TRAIN   
C*****MUST BE 'BACK' END                                                
650   KRVF=ARV*(2.0-CDBVOV(ITYP)*(ANETRG+ARV)/ANETRG)/ANETRG            
C*****COMPUTE FORCING FUNC TION CAUSED BY TRAIN                         
      SUM9=0.5*(VTRP1**2-VNETRG**2+KRVF*VRELTR**2) * TSSTAB(ISS)        
C*****CHECK FOR NECESSITY OF TRAIN AIR DRAG COMPUTATION                 
      IF (KAERO) 720,660,720                                            
660   DRAGV(AHTRRG)=DRAGV(AHTRRG)+AHTDRG*(-CDBVOV(ITYP)*(((ANETRG+ARV)/A
     1NETRG)**2))*VRELTR*VRELTR * TSSTAB(ISS) / 2.0                     
      GO TO 720                                                         
C*****TRAIN LIES IN NEXT REGION - COMPUTE VELOCITY IN NEXT REGION       
670   VTRP1=(QNETRG-ARV*URV)/(ANETRG-ARV)                               
C*****COMPUTE AIR VELOCITY RELATIVE TO TRAIN                            
      VRELTR=VNETRG-URV                                                 
C*****CHECK DIRECTION OF AIRFLOW RELATIVE TO TRAIN                      
      IF (VRELTR) 700,700,680                                           
C*****POSITIVE AIRFLOW RELATIVE TO THE TRAIN, THEREFORE, END OF TRAIN   
C*****MUST BE 'FRONT' END                                               
680   KRVB=-ARV*((2.0*ANETRG-ARV)/((ANETRG-ARV)**2)+CDFVOV(ITYP)/ANETRG)
C*****COMPUTE FORCING FUNCTION CAUSED BY TRAIN                          
      SUM9=0.5*(VTRP1**2-VNETRG**2+KRVB*VRELTR**2) * TSSTAB(ISS)        
C*****CHECK FOR NECESSITY OF TRAIN AIR DRAG COMPUTATION                 
      IF (KAERO) 720,690,720                                            
690   DRAGV(AHTRRG)=DRAGV(AHTRRG)+AHTDRG*(-ANETRG*ARV/((ANETRG-ARV)**2)-
     1CDFVOV(ITYP))*VRELTR*VRELTR * TSSTAB(ISS) / 2.0                   
      GO TO 720                                                         
C*****NEGATIVE AIRFLOW RELATIVE TO THE TRAIN, THEREFORE, END OF TRAIN   
C*****MUST BE 'BACK' END                                                
700   KRVB=ARV*(ANETRG*CDBVOV(ITYP)/(ANETRG-ARV)-2.0)/(ANETRG-ARV)      
C*****COMPUTE FORCING FUNCTION CAUSED BY TRAIN                          
      SUM9=0.5*(VTRP1**2-VNETRG**2+KRVB*VRELTR**2) * TSSTAB(ISS)        
C*****CHECK FOR NECESSITY OF TRAIN AIR DRAG COMPUTATION                 
      IF (KAERO) 720,710,720                                            
710   DRAGV(AHTRRG)=DRAGV(AHTRRG)+AHTDRG*CDBVOV(ITYP)*((ANETRG/(ANETRG-A
     1RV))**2)*VRELTR*VRELTR * TSSTAB(ISS) / 2.0                        
      GO TO 720                                                         
C                                                                       
C---- END OF REGION IS CAUSED BY END OF SUBSEGMENT                      
  711 SUM8 = 0.0                                                        
      SUM9 = 0.0                                                        
C                                                                       
C*****COMPUTE TOTAL HEAD CHANGE FOR REGION                              
720   SUM10=SUM3+SUM4+SUM5+SUM7+SUM8+SUM9                               
C*****ADD TOTAL HEAD CHANGE FOR REGION TO LINE SECTION HEAD CHANGE      
      OMEGS(ISCT)=OMEGS(ISCT)+SUM10                                     
C*****CHECK FOR NECESSITY OF TRAIN AIR DRAG COMPUTATION                 
      IF (KAERO) 780,730,780                                            
C*****LOOP OVER TRAINS IN LINE SEGMENT                                  
730   DO 770 IT=1,NTRLS                                                 
C*****CHECK IF TRAIN IS IN REGION                                       
      IF (TRLIST(IT)) 740,770,740                                       
C*****TRAIN NUMBER                                                      
740   NUMV=TRNNLS(ILS,IT)                                               
      NUMV=IABS(NUMV)                                                   
      IF (NUMV-AHTRRG) 760,750,760                                      
C*****IF THIS TRAIN IS THE SAME AS THE ONE WHOSE END IS THE FORWARD END 
C*****OF THE REGION THE SUM9 IS NOT ADDED TO THE TOTAL TRAIN HEAD       
C*****CHANGE SINCE THIS COMPONENT HAS BEEN ACCOUNTED FOR ABOVE. TO      
C*****ACCOMPLISH THIS, SUM9 IS DEDUCTED HERE AND ADDED BELOW AS PART OF 
C*****SUM10                                                             
750   DRAGV(NUMV)=DRAGV(NUMV)-TRLIST(IT)*SUM9                           
C*****ADD REGION TRAIN HEAD CHANGE TO TOTAL TRAIN HEAD CHANGE           
760   DRAGV(NUMV)=DRAGV(NUMV)+TRLIST(IT)*SUM10                          
770   CONTINUE                                                          
780   CONTINUE                                                          
C***********************************************************************
C*****SPECIAL PRINT SECTION FOR PRINTING PRESSURE CHANGES THROUGH       
C*****SEGMENTS WITH TRAINS IN THEM ONLY - THIS PORTION OF THE SUBROUTINE
C*****MAY BE REMOVED IN TOTO FROM IT WITHOUT AFFECTING ITS OPERATION.   
C*****IN ORDER TO USE THIS OPTION, THIS SUBROUTINE MUST BE              
C*****RECOMPILED WITH NTON AND NTOFF SET ACCORDINGLY                    
C*****NTON - TIME AT WHICH PRINT SECTION BEGINS - 100TH'S OF SECONDS    
      NTON = 1000000                                                    
      IF (NTIME-NTON) 820,820,790                                       
C*****NTOFF - TIME AT WHICH PRINT SECTION ENDS - 100TH'S OF SECONDS     
790   NTOFF=1000000                                                     
      IF (NTOFF-NTIME) 820,820,800                                      
C*****CONSTANT TO CONVERT FORCING FUNCTIONS TO INCHES OF WATER          
800   ALTER=RHOMAS/WGCONS                                               
C*****TIME                                                              
      PSUM1=NTIME/100.0                                                 
C*****SECTION NUMBER                                                    
      NSUM1=TABL11( ISCT )                                              
C*****LINE SEGMENT NUMBER                                               
      NSUM2=NUMLS(ILS)                                                  
C  -  SUBSEGMENT NUMBER = ISS                                           
C  - *REGION NUMBER = NRGN                                              
C*****CFM IN SECTION                                                    
      PSUM2=Q*60.0                                                      
C*****HEAD CHANGE AT BACKWARD END OF REGION IF BACKWARD END OF REGION IS
C*****BACKWARD END OF LINE SEGMENT                                      
      PSUM3=ALTER*SUM3                                                  
C*****REGION VISCOUS HEAD CHANGE FOR FLOW AT LINE SEGMENT WALLS         
      PSUM4=ALTER*SUM4                                                  
C*****REGION VISCOUS HEAD CHANGE FOR FLOW AT PERIMETERS OF ALL TRAINS   
C*****IN REGION                                                         
      PSUM5=ALTER*SUM7                                                  
C*****HEAD CHANGE AT FORWARD END OF REGION IF FORWARD END OF REGION IS  
C*****FORWARD END OF LINE SEGMENT                                       
      PSUM6=ALTER*SUM8                                                  
C*****HEAD CHANGE AT FORWARD END OF REGION CAUSED BY FORWARD END OF     
C*****TRAIN                                                             
      PSUM7=ALTER*SUM9                                                  
C*****TOTAL HEAD CHANGE FOR REGION                                      
      PSUM8=ALTER*SUM10                                                 
      WRITE (OUT,810) PSUM1,NSUM1,NSUM2,NRGN,JSS,                       
     1   PSUM2, PSUM3, PSUM4, PSUM5, PSUM6, PSUM7, PSUM8, LRG           
810   FORMAT (/,F6.2,4I4, '  OMEGA5  ', 8G10.3 )                      
820   CONTINUE                                                          
C*****END SPECIAL PRINT SECTION                                         
C***********************************************************************
C*****CHECK FOR COMPLETION OF LINE SEGMENT COMPUTATIONS                 
      IF( AHTRRG ) 830,825,830                                          
  825 IF( NSS - JSS ) 900,900,860                                       
C*****LINE SEGMENT COMP. NOT COMPLETE - REGION BOUNDED BY TRAIN         
C*****REMOVE DISTANCE TO FORWARD END OF REGION FROM FURTHER             
C*****CONSIDERATION                                                     
830   TRDIST(JTAHRG)=1000000.0                                          
C*****DISTANCE TO FORWARD END OF THIS REGION BECOMES DISTANCE TO        
C*****BACKWARD END OF NEXT REGION                                       
      XBKRG=XAHRG                                                       
C*****CHECK IF TRAIN WHOSE END IS THE FORWARD END OF THIS REGION LIES IN
C*****THIS REGION OR THE NEXT ONE                                       
      NUMY1=JTAHRG-2*(JTAHRG/2)                                         
      IF (NUMY1) 840,850,840                                            
C*****TRAINS IS IN THIS REGION                                          
C*****COMPUTE NUMBER OF TRAINS IN NEXT REGION                           
840   NTRRG=NTRRG-1                                                     
C*****UPDATE ARRAY TRLIST FOR NEXT REGION BY REMOVING TRAIN             
      TRLIST(ITAHRG)=0                                                  
      GO TO 160                                                         
C*****TRAIN IS IN NEXT REGION                                           
C*****COMPUTE NUMBER OF TRAINS IN NEXT REGION                           
850   NTRRG=NTRRG+1                                                     
C*****UPDATE ARRAY TRLIST FOR NEXT REGION BY ADDING TRAIN               
      NUMY1=TRNNLS(ILS,ITAHRG)                                          
      TRLIST(ITAHRG)=IABS(NUMY1)/NUMY1                                  
      GO TO 160                                                         
C*****LINE SEGMENT COMPUTATIONS COMPLETED                               
C---- LINE SEGMENT COMP. NOT COMPLETE - REGION BOUNDED BY SUBSEGMENT    
C     INCREMENT SUBSEGMENT AND ANALYZE NEXT REGION                      
  860 ISS = ISS + 1                                                     
      JSS = JSS + 1                                                     
      TRDIST(1) = DLSS * FLOAT( JSS )                                   
      XBKRG = XAHRG                                                     
      GO TO 160                                                         
900   RETURN                                                            
      END                                                               
