      SUBROUTINE SUMARY (IOPT)                                          
C                                                                       
C                                                                       
C      IOPT = 0   NO SUMMARY, DON'T INITIALIZE                          
C             1   INITIALIZE ONLY                                       
C             2   PRINT SUMMARY ONLY                                    
C             3   PRINT SUMMARY AND INITIALIZE                          
C             4   PRINT SUMMARY,PERF. ENVIRON. LOAD ESTIMATE, INITIALIZE
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      REAL      LHFTAP, LHFSSS, LHFUSS, LHFECS                          
      DIMENSION  DUMMY1(6), SUMQ(3), NODTHI(3)                          
C                                                                       
70    FORMAT (A1)    
80    FORMAT (T32,'SUMMARY OF SIMULATION FROM ',F9.2,' TO ',F9.2,' SECON
     1DS')                                                            
110   FORMAT (/,'( VENTILATION SHAFT )',T42,A36,T95,'FROM NODE ',I3,' TO
     1 NODE ',I3)                                                       
130   FORMAT (/,'( STAIRWAY )',T42,A36,T95,'FROM NODE ',I3,' TO NODE ',I
     13)                                                                
134   FORMAT( / ,'( SPECIAL VENT SHAFT )',T42,A36,T95,'FROM NODE ',I3,   
     1'TO NODE ',I3)                                                     
160   FORMAT (/,'(TUNNEL)',T42,A36,T95,'FROM NODE ',I3,' TO NODE ',I3) 
180   FORMAT (/,'(STATION)',T42,A36,T95,'FROM NODE ',I3,' TO NODE ',I3)
200   FORMAT (/'(SPECIAL TUNNEL)',T42,A36,T95,'FROM NODE ',I3,' TO NODE'
     1,I3)                                                           
220   FORMAT(/'LENGTH',F8.1,' FT'/'AREA',F10.1,' SQ FT',T53,'M  A  X  I
     1 M  U  M',T79,'M  I  N  I  M  U  M',T105,'A V E R A G E'/T40
     2,'SYSTEM',T53,19('-'),T79,19('-'),T104,15('-') /                  
     3     T37,'PARTITIONING',T55,'VALUE',T66,                          
     4   'TIME',T81,'VALUE',T92,'TIME',T109,'VALUE'/T37,12('-'),T53,9('-
     5'),T65,7('-'),T79,9('-'),T91,7('-'),T108,7('-')/T101,'POSITIVE',T1
     613,'NEGATIVE'/T101,8('-'),T113,8('-'))                            
230   FORMAT (/'AIR FLOW RATE',T24,'( CFM )',T36,I3,' -',I3,F17.0,F11.1,
     1        F15.0,F11.1,F11.0,F12.0)                                      
270   FORMAT (/'AIR VELOCITY',T24,'( FPM )',T36,I3,' -',I3,F17.0,F11.1,
     1        F15.0,F11.1,F11.0,F12.0)                                       
  275 FORMAT (/'AIR FLOW DIRECTION',T24,'( PERCENT )',T36,I3,' -',I3,
     1        54X,F11.1,F12.1 )                                               
300   FORMAT (/'PERCENTAGE OF TIME OUTFLOW VELOCITY'/3X,'EXCEEDS',F8.1,
     1        ' FPM',T36,I3,' -',I3,T97,F18.1)                               
350   FORMAT (/'DRY-BULB TEMPERATURE',T24,'( DEG F )',T36,2(I3,' -'),I3,
     1        F12.1,F11.1,F15.1,F11.1,F11.1,F12.1)                          
380   FORMAT (T36,2(I3,' -'),I3,F12.1,F11.1,F15.1,F11.1,F11.1,F12.1)
410   FORMAT (/'HUMIDITY RATIO',T24,'( LB/LB )',T36,2(I3,' -'),I3,F12.4,
     1        F11.1,F15.4,F11.1,F17.4)                                      
440   FORMAT (T36,2(I3,' -'),I3,F12.4,F11.1,F15.4,F11.1,F17.4)      
460   FORMAT (/'WET-BULB TEMPERATURE',T24,'( DEG F )',T36,2(I3,' -'),I3,
     1        F12.1,F11.1,F15.1,F11.1,F17.1)                                
490   FORMAT (T36,2(I3,' -'),I3,F12.1,F11.1,F15.1,F11.1,F17.1)      
510   FORMAT (/'RELATIVE HUMIDITY',T24,'( PERCENT )',T36,2(I3,' -'),I3,
     1        F12.1,F11.1,F15.1,F11.1,F17.1)                                 
  540 FORMAT (T36,2(I3,' -'),I3,F12.1,F11.1,F15.1,F11.1,F17.1)      
590   FORMAT (//T42,'P E R C E N T A G E  O F  T I M E  T E M P E R A T 
     1U R E  I S  A B O V E'//T48,6F11.1/T53,6(8('-'),3X)/)             
620   FORMAT (T36,2(I3,' -'),I3,F10.1,5F11.1)                           
640   FORMAT (//T42,'P E R C E N T A G E  O F  T I M E  T E M P E R A T 
     1U R E  I S  B E L O W'//T48,6F11.1/T53,6(8('-'),3X)/)             
657   FORMAT(//T47,'AVERAGE HEAT GAINS WITHIN THE SEGMENT'/T47, 
     1       '------- ---- ----- ------ --- -------'/,T91,        
     2'SENSIBLE',T107,' LATENT'/T91,'(BTU/HR)',T107, '(BTU/HR)')        
663   FORMAT (/T7,'TRAIN PROPULSION AND BRAKING SYSTEM HEAT',T88,F11.1) 
664   FORMAT (/T7,'TRAIN AUXILIARY SYSTEM AND PASSENGER HEAT'           
     1          ,T88,F11.1,T104,F11.1)                                  
665   FORMAT (/T7,'SEGMENT STEADY-STATE HEAT SOURCES'                   
     1          ,T88,F11.1,T104,F11.1)                                  
668   FORMAT (/T7,'SEGMENT ENVIRONMENTAL CONTROL SYSTEM'                
     1          ,T88,F11.1,T104,F11.1)                                  
669   FORMAT (/T7,'SEGMENT UNSTEADY-STATE HEAT SOURCES, EVAPORATION AND 
     1VISCOUS HEATING',T88,F11.1,T104,F11.1)                            
  672 FORMAT(/T7,'HEAT SINK', T88, F11.1 )                              
  674 FORMAT(/T7,'HEAT TRANSFER TO WALL - CONVECTION', T88, F11.1, // , 
     1  T29,'- RADIATION', T88, F11.1 )                                 
679   FORMAT (/T7,'HEAT CAPTURED BY TRACKWAY EXHAUST SYSTEM',T88,F11.1) 
686   FORMAT (T53,'TRAIN ENERGY SUMMARY'/  T53,'----- ------ -------'//,
     1T26,'TOTAL OF ENERGY CONSUMED FROM SIMULATION TIME ',F9.2,     
     2' TO ',F9.2,' SECONDS' /                                          
     3 T7,'(THIS IS THE AMOUNT OF ENERGY CONSUMED DURING THE SUMMARY INT
     4ERVAL, NOT THE AMOUNT OF ENERGY CONSUMED IN ONE HOUR.)'/)         
687   FORMAT ( / ,T55,'ENERGY SECTOR' ,I3/T55,16('-'))                  
  688 FORMAT( /T7,'PROPULSION ENERGY FROM THIRD RAIL',T83,F15.3,        
     1T101,'KWH')                                                       
689   FORMAT (/T7,'EQUIVALENT THIRD RAIL PROPULSION ENERGY FROM FLYWHEEL
     1',T83,F15.3,T101,'KWH')                                           
  690 FORMAT(/T7, 'AUXILIARY ENERGY',T83,F15.3, T101,'KWH')             
  691 FORMAT(/T7,'REGENERATED ENERGY ACCEPTED BY THIRD RAIL',T83,F15.3, 
     1T101,'KWH'/)                                                      
709   FORMAT (T51,'SES DEEP HEAT SINK ANALYSIS' / T46,38('-'),/,T46,
     1'( OUTPUT OF HEAT SINK PRINT OPTION 1 )',///,'NUMBER OF SECTIONS',
     1T39,I5/'NUMBER OF LINE SEGMENTS',T39,I5/'NUMBER OF VENTILATION SHA
     2FT SECTIONS',T39,I5/'NUMBER OF THERMAL NODES',T39,I5/'TOTAL NUMBER
     3 OF SUBSEGMENTS',T39,I5/'NUMBER OF AERO NODES',T39,I5/'NUMBER OF P
     4OINTS TAKEN IN THE SUMMARY',T39,I5//)                 
710   FORMAT (T14,'LOW',T23,'HIGH'/'SECTION',4X,'SEGMENT',3X,'SEGMENT',
     1 T33,'TABL20',T43,'TABL21',T53,'TABL12',T63,'TABL13',/'-------', 
     24X,7('-'),3X,7('-'),T33,6('-'),T43,6('-'),T53,6('-'),T63,6('-')/) 
730   FORMAT (I5,2I10,I12,I10,I9,I10)                                   
740   FORMAT(/ / ,T14,'LOW',T23,'HIGH',/'LINE SEG',3X,'SUBSEG.',3X,    
     1'SUBSEG.',/,8('-'),3X,7('-'),3X,7('-')/)                         
760   FORMAT (I5,2I10)                                                  
770   FORMAT(/ / ,T14,'LOW',T23,'HIGH',/'VENT SHAFT',1X,'SUBSEG.',3X,  
     1'SUBSEG.'/,10('-'),1X,7('-'),3X,7('-')/)                        
790   FORMAT (I7,I9,I10)                                                
800   FORMAT(/ / , 'LINE SEGMENT',3X,'SUBSEGMENT SURFACE',3X,'PERIMETER'
     1,4X,'AREA',5X,'IDENTIFICATION'/                                   
     2,12('-'),3X,18('-'),3X,9('-'),4X,4('-'),5X,14('-')/)            
820   FORMAT (I6,11X,F10.2,F16.2,F11.2,I9,' -',I3)                      
830   FORMAT(/ / , 'VENT SHAFT',3X,'SUBSEGMENT SURFACE',3X,             
     1'IDENTIFICATION',/,10('-'),3X,18('-'),3X,14('-')/)               
850   FORMAT (I6, 9X,F10.2,9X,I6,' -',I3)                               
860   FORMAT(///,'SUBSEGMENT',5X,'EMTFSS',6X,'EMTBSS',6X,'STDBSS',6X,
     1'HSUMSS',6X,'HTSMSS',6X,'QSUMSS',6X,'LSUMSS',6X,'EMHFSS',         
     2 6X,'EMHBSS',/,10('-'),5X,6('-'),6X,6('-'),6X,6('-'),6X,6('-'), 
     3               6X,6('-'),6X,6('-'),6X,6('-'),6X,6('-'),6X,6('-')/)
872   FORMAT(/ / ,'SUBSEGMENT',T19,'AVERAGE FLOW LEAVING',T50,       
     1'AVERAGE FLOW LEAVING'/'  NUMBER',T19,'FORWARD  END ( CFS )',  
     2T50,'BACKWARD END ( CFS )',11X,'IDENTIFICATION'/                  
     310('-'),T19,20('-'),T50,20('-'),11X,14('-'),/)                 
877   FORMAT (I6,T19,F13.3,T50,F13.3,T81,   2(I3,' -'),I3)              
880   FORMAT( I6,F15.0,F12.0,F12.2,2F12.7,2F12.5,2F12.5 )               
890   FORMAT(//,'THERMAL',T27,'AVERAGE ( FLOW * TEMPERATURE )',T93,
     1 'AVERAGE FLOW LEAVING',/,' NODE',T12,'LEAVING THE THERMAL NODE TH
     2ROUGH EACH BRANCH ( CFS * DEG-F )',T80,'THE THERMAL NODE THROUGH E
     3ACH BRANCH ( CFS )',/,7('-'),T12,60('-'),T77,50('-'),          
     4           /T17,'BR1',T29,'BR2',T41,'BR3',T53,'BR4',T65,'BR5',    
     5 T77,'BR1',T89,'BR2',T101,'BR3',T113,'BR4',T124,'BR5'/)           
910   FORMAT (I4,T7,5F12.0,2X,5F12.3)                                   
911   FORMAT(//,'THERMAL',T15,'AVERAGE ( FLOW * HUMIDITY RATIO ) LEAVING
     1 THE THERMAL',T73,'AVERAGE',T85,'AVERAGE'/                   
     2T2,'NODE', T15,'NODE THROUGH EACH BRANCH (CFS * LB OF WATER/LB OF 
     3AIR)',T71,'TEMPERATURE',T85,'HUMIDITY'/                           
     47('-'),T15,54('-'),T71,11('-'),T85,8('-')/                     
     5            T17,'BR1',T29,'BR2',T41,'BR3',T53,'BR4',T65,'BR5',    
     6 T74,'TAVGTN',T86,'HAVTGN'/)                                      
913   FORMAT (I4,F16.6,4F12.6,F11.3,F13.6)                              
920   FORMAT (//,'AERO NODE', T12,'THERMO NODE TYPE',T30,'TABL22(NODE)',
     1 T44,'EXTERNAL NODE', T59,'THERMO NODES'/,                  
     29('-'), T12,16('-'), T30,12('-'),T44,13('-'),T59,12('-')/)     
940   FORMAT (I5,T12,I8,T30,I6,T44,I7,T59,3I4)                       
C
      IF (IOPT) 2000,2000,10                                            
10    IF (IOPT-2) 990,20,20                                             
C                                                                       
20    TTIME=FLOAT(NTIME)/100.0                                           
      DO 685 ISCTX=1,LMSCTX                                             
      IF (TABL14(ISCTX)) 685,685,30                                     
30    ISCT=TABL14(ISCTX)                                                
      ISEGL=TABL9(ISCT)                                                 
      ISEGH=TABL10(ISCT)                                                
      NODEB=TABL12(ISCT)                                                
      NODEF=TABL13(ISCT)                                                
C                                                                       
      IF (ISEGL) 40,50,50                                               
C---- VENTILATION SHAFT SECTION                                         
40    IVS=-ISEGL                                                        
      ISEGL=IVS                                                         
      ISEGH=IVS                                                         
      ISEGX=NUMVS(IVS)                                                  
      GO TO 60                                                          
50    IVS=0                                                             
C                                                                       
60    DO 680 ILS=ISEGL,ISEGH                                            
C                                                                       
      WRITE (OUT,70) CHAR(12)
      WRITE (OUT,80) SUMTST,TTIME                                        
C                                                                       
      IF (IVS) 140,140,90                                               
C---- VENTILATION SHAFT                                                 
90    IDUMY=TYPVS(IVS)                                                  
      GO TO (100,120,132,132,132,132,132,132,132,132), IDUMY            
100   WRITE (OUT,110) NAMVS(IVS),TABL15(NODEB),TABL15(NODEF)  
      GO TO 135                                                         
120   WRITE (OUT,130) NAMVS(IVS),TABL15(NODEB),TABL15(NODEF)  
      GO TO 135                                                         
132   WRITE(OUT,134)  NAMVS(IVS),TABL15(NODEB),TABL15(NODEF)  
135   WRITE(OUT,220)  LVS(IVS), AVS(IVS)                                
      GO TO 225                                                         
C---- LINE SEGMENTS                                                     
140   ISEGX = NUMLS(ILS)                                                
      IDUMY= IABS(TYPLS(ILS))                                           
      GO TO (150,170,170,170,190,190,190,190,190,190,190,190,190,190,   
     1     190,190), IDUMY                                              
150   WRITE (OUT,160) NAMLS(ILS),TABL15(NODEB),TABL15(NODEF)  
      GO TO 210                                                         
170   WRITE (OUT,180) NAMLS(ILS),TABL15(NODEB),TABL15(NODEF)  
      GO TO 210
190   WRITE (OUT,200) NAMLS(ILS),TABL15(NODEB),TABL15(NODEF)  
210   WRITE (OUT,220)  LLS(ILS), ALS(ILS)                               
225   AVGP=SUMQPS(ISCT)*60.0/MXMNPT                                     
      AVGN=SUMQNS(ISCT)*60.0/MXMNPT                                     
      DUMY3=QSMX(ISCT)*60.0                                             
      DUMY4=QSMN(ISCT)*60.0                                             
      DUMY1=MXQS(ISCT)/100.0                                            
      DUMY2=MNQS(ISCT)/100.0                                            
      WRITE (OUT,230) ISCTX,ISEGX,DUMY3,DUMY1,DUMY4,DUMY2,AVGP,AVGN     
      IF (IVS) 240,240,250                                              
240   A=ALS(ILS)                                                        
      GO TO 260                                                         
250   A=AVS(IVS)                                                        
260   DUMY3=DUMY3/A                                                     
      DUMY4=DUMY4/A                                                     
      AVGP = AVGP / A                                                   
      AVGN = AVGN / A                                                   
      WRITE (OUT,270) ISCTX,ISEGX,DUMY3,DUMY1,DUMY4,DUMY2,AVGP,AVGN     
      DUMY1=(100.0*NTQPS(ISCT))/MXMNPT                                  
      DUMY2 = 100.0 - DUMY1                                             
      WRITE(OUT,275) ISCTX,ISEGX,DUMY1,DUMY2                            
      IF (IVS) 280,280,290                                              
C---- LINE SEGMENT                                                      
280   ISSL=TABL3(ILS)                                                   
      ISSH=TABL4(ILS)                                                   
      GO TO 330                                                         
C---- VENTILATION SHAFT                                                 
290   ISSL=TABL5(IVS)                                                   
      ISSH=TABL6(IVS)                                                   
      DUMY1=(100.0*EXVOVS(IVS))/MXMNPT                                  
      DUMY2=VOMXVS(IVS)*60.0                                            
      WRITE (OUT,300) DUMY2,ISCTX,ISEGX,DUMY1                           
      DUMY2=MXFPVS(IVS)/100.0                                           
      DUMY4=MNFPVS(IVS)/100.0                                           
C*****REMAINING CALCULATIONS FOR MAXIMUM AND MINIMUM                    
C*****STRAIGHT-THROUGH VENTILATION SHAFT PRESSURES (ABOVE               
C*****AND BELOW AMBIENT) TO BE INSERTED HERE                            
C                                                                       
C***** SKIP TEMPERATURE AND HUMIDITY SUMMARY IF THERMO NOT RUNNING      
330   IF (JRATTH) 680,680,340                                           
340   DUMY1=MXTDB(ISSL)/100.0                                           
      DUMY2=MNTDB(ISSL)/100.0                                           
      IF( NTQPS(ISCT) ) 342,342,343                                     
  342 AVGP = 0.0                                                        
      GO TO 344                                                         
  343 AVGP=STDPSS(ISSL)/NTQPS(ISCT)                                     
  344 IF( MXMNPT - NTQPS(ISCT)) 345,345,346                             
  345 AVGN = 0.0                                                        
      GO TO 347                                                         
  346 AVGN=STDNSS(ISSL)/(MXMNPT-NTQPS(ISCT))                            
  347 J=1                                                               
      WRITE (OUT,350) ISCTX,ISEGX,J,TDBMX(ISSL),DUMY1,TDBMN(ISSL),DUMY2,
     1AVGP,AVGN                                                         
      J3=ISSH-ISSL+1                                                    
      J2=ISSL-1                                                         
      IF (J3-2) 390,360,360                                             
360   DO 370 J=2,J3                                                     
      DUMY1=MXTDB(J2+J)/100.0                                           
      DUMY2=MNTDB(J2+J)/100.0                                           
      IF( NTQPS(ISCT) ) 362,362,363                                     
  362 AVGP = 0.0                                                        
      GO TO 364                                                         
  363 AVGP=STDPSS(J2+J)/NTQPS(ISCT)                                     
  364 IF( MXMNPT - NTQPS(ISCT) ) 365,365,366                            
  365 AVGN = 0.0                                                        
      GO TO 370                                                         
  366 AVGN=STDNSS(J2+J)/(MXMNPT-NTQPS(ISCT))                            
370   WRITE (OUT,380) ISCTX,ISEGX,J,TDBMX(J2+J),DUMY1,TDBMN(J2+J),DUMY2,
     1 AVGP,AVGN                                                        
C                                                                       
390   GO TO (400,450,500), HUMOPT                                       
C---- HUMIDITY RATIO                                                    
400   DUMY1=MXHR(ISSL)/100.0                                            
      DUMY2=MNHR(ISSL)/100.0                                            
      AVG=SHRSS(ISSL)/MXMNPT                                            
      J=1                                                               
      WRITE (OUT,410)ISCTX,ISEGX,J,HRMX(ISSL),DUMY1,HRMN(ISSL),DUMY2,AVG
      IF (J3-2) 550,420,420                                             
420   DO 430 J=2,J3                                                     
      DUMY1=MXHR(J2+J)/100.0                                            
      DUMY2=MNHR(J2+J)/100.0                                            
      AVG=SHRSS(J2+J)/MXMNPT                                            
430   WRITE (OUT,440) ISCTX,ISEGX,J,HRMX(J2+J),DUMY1,HRMN(J2+J),DUMY2,AV
     1G                                                                 
      GO TO 550                                                         
C---- WET-BULB TEMPERATURE                                              
450   DUMY1=MXTWB(ISSL)/100.0                                           
      DUMY2=MNTWB(ISSL)/100.0                                           
      AVG=STWBSS(ISSL)/MXMNPT                                           
      J=1                                                               
      WRITE (OUT,460) ISCTX,ISEGX,J,TWBMX(ISSL),DUMY1,TWBMN(ISSL),DUMY2,
     1AVG                                                               
      IF (J3-2) 550,470,470                                             
470   DO 480 J=2,J3                                                     
      DUMY1=MXTWB(J2+J)/100.0                                           
      DUMY2=MNTWB(J2+J)/100.0                                           
      AVG=STWBSS(J2+J)/MXMNPT                                           
480   WRITE (OUT,490) ISCTX,ISEGX,J,TWBMX(J2+J),DUMY1,TWBMN(J2+J),DUMY2,
     1AVG                                                               
      GO TO 550                                                         
C---- RELATIVE HUMIDITY                                                 
500   DUMY1=MXTWB(ISSL)/100.0                                           
      DUMY2=MNTWB(ISSL)/100.0                                           
      AVG=STWBSS(ISSL)/MXMNPT                                           
      J=1                                                               
      WRITE (OUT,510) ISCTX,ISEGX,J,TWBMX(ISSL),DUMY1,TWBMN(ISSL),DUMY2,
     1AVG                                                               
      IF (J3-2) 550,520,520                                             
520   DO 530 J=2,J3                                                     
      DUMY1=MXTWB(J2+J)/100.0                                           
      DUMY2=MNTWB(J2+J)/100.0                                           
      AVG=STWBSS(J2+J)/MXMNPT                                           
530   WRITE (OUT,540) ISCTX,ISEGX,J,TWBMX(J2+J),DUMY1,TWBMN(J2+J),DUMY2,
     1AVG                                                               
C                                                                       
C---- PRINT PERCENTAGE OF TIME ABOVE OR BELOW DESIGN TEMPERATURE        
C----          ( LINE SEG. IN TYPE 1 ZONE  ONLY )                       
550   IF (IVS) 555,555,680                                              
555   IF( HCOPT ) 655,655,560                                           
560   ICLUST = TABL44(ISSL)                                             
      IF (TYPCL(ICLUST)-1) 656,561,656                                  
561   IF( DESPER - 1 ) 562,562,565                                      
C---- MORNING DESIGN                                                    
562   DO  563  I = 1, 6                                                 
563   DUMMY1(I) = DTDMLS(ILS) + WNTSUM*FLOAT(I-3)                       
      GO TO 570                                                         
C---- EVENING DESIGN                                                    
565   DO  567  I = 1, 6                                                 
567   DUMMY1(I) = DTDELS(ILS) + WNTSUM*FLOAT(I-3)                       
570   IF (WNTSUM) 630,580,580                                           
580   WRITE (OUT,590) DUMMY1                                            
      DO 610 J=1,J3                                                     
      ISS=J2+J                                                          
      DO 600 K=1,6                                                      
600   DUMMY1(K)=(100.0*EXDTDB(ISS,K))/MXMNPT                            
610   WRITE (OUT,620) ISCTX,ISEGX,J,DUMMY1                              
      GO TO 655                                                         
630   WRITE (OUT,640) DUMMY1                                            
      DO 652 J=1,J3                                                     
      ISS=J2+J                                                          
      DO 650 K=1,6                                                      
650   DUMMY1(K)=(100.0*EXDTDB(ISS,K))/MXMNPT                            
652   WRITE (OUT,620) ISCTX, ISEGX, J, DUMMY1                           
655   IF (IVS) 656,656,680                                              
656   WRITE (OUT,657)                                                   
      SHFSSS=0.0                                                        
      LHFSSS=0.0                                                        
      SHFECS=0.0                                                        
      LHFECS=0.0                                                        
      HETSNK = 0.0                                                      
      UPLATE = 0.0                                                      
      DO 662 ISS = ISSL, ISSH                                           
C                                                                       
C---- SHFSSS = SENSIBLE HEAT FROM STEADY STATE HEAT SOURCES             
C---- LHFSSS =  LATENT  HEAT FROM STEADY STATE HEAT SOURCES             
C                                                                       
      SHFSSS = SHFSSS+(SHLSS(ISS)-SACLSS(ISS))                          
      LHFSSS = LHFSSS+(LHLSS(ISS)-LACLSS(ISS))                          
C                                                                       
C---- SHFECS = SENSIBLE HEAT FROM THE ENVIRONMENTAL CONTROL SYSTEM      
C---- LHFECS =  LATENT  HEAT FROM THE ENVIRONMENTAL CONTROL SYSTEM      
C                                                                       
      SHFECS = SHFECS+SACLSS(ISS)                                       
      LHFECS = LHFECS+LACLSS(ISS)                                       
C                                                                       
C---- HETSNK = THE AMOUNT OF HEAT ABSORBED BY THE WALLS                 
C                                                                       
      IF ( FIROP2 * TYPLS(ILS) .GE. 0 )  HETSNK = HETSNK +              
     1    ( HTSMSS(ISS)-HSUMSS(ISS)*TSFSS(ISS))*SURLSS(ILS)             
C                                                                       
C---- UPLATE = THE AMOUNT OF TRAIN HEAT RELEASE EXTRACTED BY            
C----          THE UNDERPLATFORM EXHAUST SYSTEM                         
C                                                                       
      IF (OPTUX) 662,662,661                                            
661   UPLATE = UPLATE+QSMXSS(ISS)                                       
662   CONTINUE                                                          
C                                                                       
C---- HRFTPB = HEAT RELEASE FROM TRAIN PROPULSION AND BRAKING           
C                                                                       
      HRFTPB = (SPROLS(ILS)/PINPT ) * 3600.0                            
C                                                                       
C---- SHFTAP = SENSIBLE HEAT FROM TRAIN AUXILIARY SYSTEM AND PASSENGER  
C---- LHFTAP =  LATENT  HEAT FROM TRAIN AUXILIARY SYSTEM AND PASSENGER  
C                                                                       
      SHFTAP = (SAXSLS(ILS) /PINPT)* 3600.0                             
      LHFTAP = (SAXLLS(ILS)/PINPT) * 3600.0                             
C                                                                       
      SHFSSS = SHFSSS*3600.0                                            
      LHFSSS = LHFSSS*3600.0                                            
C                                                                       
      SHFECS = SHFECS*3600.0                                            
      LHFECS = LHFECS*3600.0                                            
C                                                                       
C---- SHFUSS = SENSIBLE HEAT FROM SEGMENT UNSTEADY-STATE HEAT SOURCES,  
C              EVAPORATION AND VISCOUS HEATING.                         
C---- LHFUSS =  LATENT  HEAT FROM SEGMENT UNSTEADY-STATE HEAT SOURCES,  
C               EVAPORATION AND VISCOUS HEATING.                        
C                                                                       
      SHFUSS = (SULSLS(ILS) / PINPT) * 3600.0                           
      LHFUSS = (SULLLS(ILS) / PINPT ) * 3600.0                          
C                                                                       
      HETSNK = -(HETSNK/MXMNPT)*3600.0                                  
      UPLATE = -(UPLATE/MXMNPT)*3600.0                                  
      WRITE (OUT,663) HRFTPB                                            
      WRITE (OUT,664) SHFTAP, LHFTAP                                    
      WRITE (OUT,665) SHFSSS, LHFSSS                                    
      WRITE (OUT,669) SHFUSS, LHFUSS                                    
      WRITE (OUT,668) SHFECS,LHFECS                                     
C --  WRITE HEAT TRANSFER TO WALL DEPENDING UPON FIRE OPTION            
      IF( FIROP2 * TYPLS(ILS) ) 673,671,671                             
C -   NOT FIRE SIMULATION                                               
  671 WRITE(OUT,672) HETSNK                                             
      GO TO 676                                                         
C -   FIRE SIMULATION                                                   
  673 SQWL =(SQWLLS(ILS)*SURLSS(ILS)-SQRDLS(ILS))*3600.0/FLOAT(MXMNPT)  
     1  * ( -1.0 )                                                      
      SQRD = SQRDLS(ILS) * 3600.0 / FLOAT(MXMNPT) * (-1.0)              
      WRITE(OUT,674) SQWL, SQRD                                         
  676 ITYPLS = IABS( TYPLS(ILS) )                                       
      IF( ITYPLS - 2 ) 680,678,675                                      
  675 IF( ITYPLS - 4 ) 678,678,680                                      
678   WRITE(OUT,679) UPLATE                                             
C                                                                       
680   CONTINUE                                                          
C                                                                       
685   CONTINUE                                                          
C                                                                       
C---- PRINT TRAIN ENERGY SUMMARY                                        
C                                                                       
C                                                                       
      IF (TPOPT .GT. 2) GO TO 706                                       
      IF (NSTR .LE. 0) GO TO 706                                        
      ISTR = 1                                                          
692   WRITE (OUT,70) CHAR(12)
      WRITE (OUT,686) SUMTST, TTIME                                      
      ISUM = 0                                                          
C --  ADJ CONVERTS FROM (W-SEC PER SUMMARY INTERVAL) TO                 
C --   (KWH PER SUMMARY INTERVAL)                                       
  693 ADJ = 1000. * 3600.                                               
      EPR3 = EPR3SR(ISTR) / ADJ                                         
      EPRF = EPRFSR(ISTR) / ADJ                                         
      EAUX = EAUXSR(ISTR) / ADJ                                         
      ERGN = ERGNSR(ISTR) / ADJ                                         
      WRITE(OUT,687) ISTR                                               
      WRITE(OUT,688)  EPR3                                              
      WRITE(OUT,689)  EPRF                                              
      WRITE(OUT,690)  EAUX                                              
      WRITE(OUT,691)  ERGN                                              
      ISUM = ISUM+1                                                     
      ISTR = ISTR+1                                                     
      IF (ISTR .GT. NSTR)  GO TO 706                                    
      IF (ISUM .EQ. 4) GO TO 692                                        
      GO TO 693                                                         
C                                                                       
706   CONTINUE                                                          
C                                                                       
C---- PRINT HEAT SINK SUMMARY OPTION 1                                  
C                                                                       
      GO TO (950,708,708,950,950), HSOPT                                
C                                                                       
708   WRITE (OUT,70) CHAR(12)
      WRITE (OUT,709) NSEC,NLS,NVSEC,NTNODE,TNSS,NNODE,MXMNPT           
      WRITE (OUT,710)                                                   
      DO 720 ISCT=1,NSEC                                                
720   WRITE (OUT,730) ISCT,TABL9(ISCT),TABL10(ISCT),TABL20(ISCT),TABL21(
     1ISCT),TABL12(ISCT),TABL13(ISCT)                                   
      WRITE (OUT,740)                                                   
      DO 750 ILS=1,NLS                                                  
750   WRITE (OUT,760) ILS,TABL3(ILS),TABL4(ILS)                         
      IF( NVSEC ) 795,795,765                                           
765   WRITE (OUT,770)                                                   
      DO 780 IVS=1,NVSEC                                                
780   WRITE (OUT,790) IVS,TABL5(IVS),TABL6(IVS)                         
795   WRITE (OUT,800)                                                   
      DO 810 ILS=1,NLS                                                  
      NUMY1=TABL8(ILS)                                                  
      NUMY2=TABL11(NUMY1)                                               
810   WRITE (OUT,820) ILS,SURLSS(ILS),PERMLS(ILS),ALS(ILS),NUMY2,       
     1NUMLS(ILS)                                                        
      IF (NVSEC) 855,855,825                                            
825   WRITE (OUT,830)                                                   
      DO 840 IVS=1,NVSEC                                                
      NUMY1=NLSEC+IVS                                                   
      NUMY2=TABL11(NUMY1)                                               
840   WRITE (OUT,850) IVS,SURVSS(IVS),NUMY2,NUMVS(IVS)                  
855   WRITE (OUT,860)                                                   
      DO 870 ISS=1,TNSS                                                 
      DUMY1 = EMTFSS(ISS) / MXMNPT                                      
      DUMY2 = EMTBSS(ISS) / MXMNPT                                      
C     DUMY3 = STDPSS(ISS) / MXMNPT                                      
C     DUMY3A= STDNSS(ISS) / MXMNPT                                      
      DUMY3 = STDBSS(ISS) / MXMNPT                                      
      DUMY4 = HSUMSS(ISS) / MXMNPT                                      
      DUMY5 = HTSMSS(ISS) / MXMNPT                                      
      DUMY6 = QSUMSS(ISS) / MXMNPT                                      
      DUMY7 = LSUMSS(ISS) / MXMNPT                                      
      DUMY8 = EMHFSS(ISS) / MXMNPT                                      
      DUMY9 = EMHBSS(ISS) / MXMNPT                                      
870   WRITE(OUT,880) ISS,DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,           
     1  DUMY7,DUMY8,DUMY9                                               
      WRITE (OUT,872)                                                   
      DO 875 ISS = 1,TNSS                                               
      DUMY10 = FLOFSS(ISS) / MXMNPT                                     
      DUMY11 = FLOBSS(ISS) / MXMNPT                                     
      CALL INTEXT (ISS,ISCTX,ILSX,ISSX)                                 
875   WRITE (OUT,877) ISS,DUMY10,DUMY11,ISCTX,ILSX,ISSX                 
      WRITE (OUT,890)                                                   
      DO 900 I=1,NTNODE                                                 
      DUMY1 = EMTBTN(I,1) / MXMNPT                                      
      DUMY2 = EMTBTN(I,2) / MXMNPT                                      
      DUMY3 = EMTBTN(I,3) / MXMNPT                                      
      DUMY4 = EMTBTN(I,4) / MXMNPT                                      
      DUMY5 = EMTBTN(I,5) / MXMNPT                                      
      DUMY11 = FLOBTN(I,1) / MXMNPT                                     
      DUMY12 = FLOBTN(I,2) / MXMNPT                                     
      DUMY13 = FLOBTN(I,3) / MXMNPT                                     
      DUMY14 = FLOBTN(I,4) / MXMNPT                                     
      DUMY15 = FLOBTN(I,5) / MXMNPT                                     
900   WRITE(OUT,910) I,DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,                   
     1 DUMY11,DUMY12,DUMY13,DUMY14,DUMY15                               
      WRITE(OUT,911)                                                    
      DO  912  I = 1, NTNODE                                            
      DUMY1 = EMHBTN(I,1) / MXMNPT                                      
      DUMY2 = EMHBTN(I,2) / MXMNPT                                      
      DUMY3 = EMHBTN(I,3) / MXMNPT                                      
      DUMY4 = EMHBTN(I,4) / MXMNPT                                      
      DUMY5 = EMHBTN(I,5) / MXMNPT                                      
      DUMY6 = TAVGTN(I)/MXMNPT                                          
      DUMY7 = HAVGTN(I)/MXMNPT                                          
912   WRITE(OUT,913) I,DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7        
      WRITE(OUT,920)
      DO 930 J=1,NNODE                                                  
      NODTHI(1) = TABL22(J)+1                                           
      NTYPTH = TYPTND(J)                                                
      GO TO (922,924,924,922,922), NTYPTH                               
922   K=1                                                               
      GO TO 930                                                         
924   K=3                                                               
      NODTHI(2) = TABL22(J)+2                                           
      NODTHI(3) = TABL22(J)+3                                           
930   WRITE (OUT,940) J,NTYPTH,TABL22(J),TABL15(J),(NODTHI(I),I=1,K)    
C                                                                       
C                                                                       
C---- SUMMARY TOTAL INITIALIZATION   ***********************************
C                                                                       
  950 IF( IOPT - 2 ) 990,2000,990                                       
  990 IF( NOSYS ) 1500,1500,1000                                        
C                                                                       
C---- LOOP OVER SECTIONS                                                
C                                                                       
 1000 DO  1080  ISCT = 1, NSEC                                          
      QSMN(ISCT) = QSAVES(ISCT)                                         
      QSMX(ISCT) = QSAVES(ISCT)                                         
      MNQS(ISCT) = NTIME                                                
      MXQS(ISCT) = NTIME                                                
      IF( QSAVES(ISCT) ) 1020,1010,1010                                 
C---- Q IS POSITIVE                                                     
 1010 SUMQPS(ISCT) = QSAVES(ISCT)                                       
      SUMQNS(ISCT) = 0.0                                                
      NTQPS(ISCT) = 1                                                   
      GO TO 1025                                                        
C---- Q IS NEGATIVE                                                     
 1020 SUMQNS(ISCT) = QSAVES(ISCT)                                       
      SUMQPS(ISCT) = 0.0                                                
      NTQPS(ISCT) = 0                                                   
C---- CHECK IF VENT SHAFT                                               
1025  IF (TABL9(ISCT)) 1030,1075,1075                                   
C---- VENT SHAFT  -  CHECK MAX OUTFLOW VELOCITY                         
 1030 IVS = - TABL9(ISCT)                                               
      EXVOVS(IVS) = 0                                                   
      IF( VOMXVS(IVS) * AGVS(IVS) - ABS( QSAVES(ISCT))) 1040,1071,1071  
 1040 NODEB = TABL12(ISCT)                                              
      IF( TABL16(NODEB) - 1 ) 1050,1050,1060                            
C---- BACKWARD END OF V.S. IS CONN. TO ATMOS. - OUTFLOW IS NEGATIVE     
 1050 IF( QSAVES(ISCT) ) 1070,1071,1071                                 
C---- FORWARD END OF V.S. IS CONN. TO ATMOS. OR SHAFT IS BETWEEN        
C---- TWO BRANCHED JUNCTIONS  -  OUTFLOW IS POSITIVE                    
 1060 IF( QSAVES(ISCT) ) 1071,1071,1070                                 
 1070 EXVOVS(IVS) = 1                                                   
C---- MAXIMUM AND MINIMUM PRESSURE IN VENT SHAFT                        
 1071 FPMXVS(IVS) = 0.0                                                 
      MXFPVS(IVS) = NTIME                                               
      FPMNVS(IVS) = 0.0                                                 
      MNFPVS(IVS) = NTIME                                               
      GO TO 1080                                                        
C----INITIALIZE SEGMENT HEAT GAINS DATA ( SUMMED IN PINPNT )            
1075  ILSL=TABL9(ISCT)                                                  
      ILSH=TABL10(ISCT)                                                 
      DO 1077 ILS=ILSL,ILSH                                             
      SPROLS(ILS)=0.0                                                   
      SAXSLS(ILS)=0.0                                                   
      SAXLLS(ILS)=0.0                                                   
      SULSLS(ILS)=0.0                                                   
1077  SULLLS(ILS)=0.0                                                   
C --  INITIALIZE NUMBER OF SEGMENT HEAT GAIN POINTS                     
      PINPT = 0                                                         
C                                                                       
 1080 CONTINUE                                                          
C                                                                       
C                                                                       
      IF( JRATTH ) 1420, 1420, 1090                                     
C                                                                       
C---- LINE AND VENT SHAFT SUBSEGMENT INITIALIZATION  *************      
C                                                                       
 1090 DO  1130  ISS = 1, TNSS                                           
      TDBMX(ISS) = TDBSS(ISS)                                           
      TDBMN(ISS) = TDBSS(ISS)                                           
      MXTDB(ISS) = NTIME                                                
      MNTDB(ISS) = NTIME                                                
      HRMX(ISS) = HUMSS(ISS)                                            
      HRMN(ISS) = HUMSS(ISS)                                            
      MXHR(ISS) = NTIME                                                 
      MNHR(ISS) = NTIME                                                 
      STDBSS(ISS) = TDBSS(ISS)                                          
      SHRSS(ISS) = HUMSS(ISS)                                           
      HSUMSS(ISS) = HTRNSS(ISS)                                         
      HTSMSS(ISS) = HTRNSS(ISS)*TDBSS(ISS)                              
      QSUMSS(ISS) = SHLTSS(ISS)                                         
      QSMXSS(ISS) = SHUXSS(ISS)                                         
      LSUMSS(ISS) = LHLTSS(ISS)                                         
      EMTFSS(ISS) = 0.0                                                 
      EMTBSS(ISS) = 0.0                                                 
      EMHFSS(ISS) = 0.0                                                 
      EMHBSS(ISS) = 0.0                                                 
      FLOFSS(ISS) = 0.0                                                 
      FLOBSS(ISS) = 0.0                                                 
      IF( FFSS(ISS) ) 1110,1110,1100                                    
 1100 EMTFSS(ISS) = TDBSS(ISS)*FFSS(ISS)                                
      EMHFSS(ISS) = HUMSS(ISS)*FFSS(ISS)                                
      FLOFSS(ISS) = FFSS(ISS)                                           
 1110 IF( FBSS(ISS) ) 1120,1130,1130                                    
 1120 EMTBSS(ISS) = - TDBSS(ISS)*FBSS(ISS)                              
      EMHBSS(ISS) = - HUMSS(ISS)*FBSS(ISS)                              
      FLOBSS(ISS) = - FBSS(ISS)                                         
 1130 CONTINUE                                                          
      DO  1139  ISCT = 1, NSEC                                          
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL ) 1132,1133,1133                                        
C---- VENT SHAFT SECTION                                                
 1132 ISEGL = - ISEGL                                                   
      ISSL = TABL5(ISEGL)                                               
      ISSH = TABL6(ISEGL)                                               
      GO TO 1134                                                        
C---- LINE SECTION                                                      
 1133 ISSL = TABL3(ISEGL)                                               
      ISSH = TABL4(ISEGH)                                               
 1134 IF( QSAVES(ISCT) ) 1137,1135,1135                                 
 1135 DO  1136  ISS = ISSL,ISSH                                         
      STDPSS(ISS) = TDBSS(ISS)                                          
 1136 STDNSS(ISS) = 0                                                   
      GO TO 1139                                                        
 1137 DO  1138  ISS = ISSL,ISSH                                         
      STDNSS(ISS) = TDBSS(ISS)                                          
 1138 STDPSS(ISS) = 0                                                   
 1139 CONTINUE                                                          
C                                                                       
      GO TO ( 1180, 1140, 1160 ), HUMOPT                                
C---- WET-BULB TEMPERATURE                                              
 1140 DO  1150  ISS = 1, TNSS                                           
      CALL  WETBLB( TDBSS(ISS), HUMSS(ISS), TWBSS(ISS) )                
      TWBMX(ISS) = TWBSS(ISS)                                           
      TWBMN(ISS) = TWBSS(ISS)                                           
      MXTWB(ISS) = NTIME                                                
      MNTWB(ISS) = NTIME                                                
 1150 STWBSS(ISS) = TWBSS(ISS)                                          
      GO TO 1180                                                        
C---- RELATIVE HUMIDITY                                                 
 1160 DO  1170  ISS = 1, TNSS                                           
      CALL  RELHUM( TDBSS(ISS), HUMSS(ISS), TWBSS(ISS) )                
      TWBMX(ISS) = TWBSS(ISS)                                           
      TWBMN(ISS) = TWBSS(ISS)                                           
      MXTWB(ISS) = NTIME                                                
      MNTWB(ISS) = NTIME                                                
 1170 STWBSS(ISS) = TWBSS(ISS)                                          
C                                                                       
C---- DESIGN TEMPERATURE TABULATION                                     
 1180 IF( HCOPT ) 1252,1252,1181                                        
 1181 DO  1250  ILS = 1, NLS                                            
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      ICLUST = TABL44(ISSL)                                             
C*****VERSION 4.1 UPGRADES
C     ICLUST MUST NOT BE ZERO.  
      IF (ICLUST .EQ. 0.0) CALL EERROR (264)  
C     IF ICLUST EQUAL TO ZERO, THE DATA SET DOES NOT INCLUDE ENVIRONMENT CONTROL ZONES.
C     THE FOLLOWING EQUATION CREATES A PROBLEM WITH DOING ENVIRONMENTAL CONTROL
C     LOAD EVALUATION (RUSH AND OFF HOURS). 
C*****
      IF( TYPCL(ICLUST) - 1 ) 1250,1183,1250                            
 1183 IF( DESPER - 1 ) 1185,1185,1241                                   
C                                                                       
C---- MORNING DESIGN PERIOD                                             
 1185 IF( WNTSUM ) 1220,1190,1190                                       
C---- SUMMER DESIGN                                                     
 1190 DO  1210  ILSS = ISSL, ISSH                                       
      DO  1210  I = 1, 6                                                
      EXDTDB(ILSS,I) = 0                                                
      IF( TDBSS(ILSS) - DTDMLS(ILS) - WNTSUM*FLOAT(I-3) ) 1210,1210,1200
 1200 EXDTDB(ILSS,I) = 1                                                
 1210 CONTINUE                                                          
      GO TO 1250                                                        
C---- WINTER DESIGN                                                     
 1220 DO  1240  ILSS = ISSL, ISSH                                       
      DO  1240  I = 1, 6                                                
      EXDTDB(ILSS,I) = 0                                                
      IF( TDBSS(ILSS) - DTDMLS(ILS) - WNTSUM*FLOAT(I-3) ) 1230,1240,1240
 1230 EXDTDB(ILSS,I) = 1                                                
 1240 CONTINUE                                                          
      GO TO 1250                                                        
C                                                                       
C---- EVENING DESIGN PERIOD                                             
 1241 IF( WNTSUM ) 1245,1242,1242                                       
C---- SUMMER DESIGN                                                     
 1242 DO  1244  ILSS = ISSL, ISSH                                       
      DO  1244  I = 1, 6                                                
      EXDTDB(ILSS,I) = 0                                                
      IF( TDBSS(ILSS) - DTDELS(ILS) - WNTSUM*FLOAT(I-3) )1244,1244,1243 
 1243 EXDTDB(ILSS,I) = 1                                                
 1244 CONTINUE                                                          
      GO TO 1250                                                        
C---- WINTER DESIGN                                                     
 1245 DO  1247  ILSS = ISSL, ISSH                                       
      DO  1247  I= 1, 6                                                 
      EXDTDB(ILSS,I) = 0                                                
      IF( TDBSS(ILSS) - DTDELS(ILS) - WNTSUM*FLOAT(I-3) )1246,1247,1247 
 1246 EXDTDB(ILSS,I) = 1                                                
 1247 CONTINUE                                                          
 1250 CONTINUE                                                          
C                                                                       
C---- NODE SUMMARY INITIALIZATION   ************************************
C                                                                       
 1252 DO  1410  NODE = 1, NNODE                                         
      ITYPT = TYPTND(NODE)                                              
      IT22 = TABL22(NODE)                                               
      GO TO ( 1256,1255,1255,1263,1263),ITYPT                           
 1255 K = 3                                                             
      GO TO 1257                                                        
 1256 K = 1                                                             
 1257 DO  1260  I = 1, K                                                
C---- INITIALIZE NODE FLOW SUMMATION                                    
      SUMQ(I) = 0.0                                                     
      NODTH = IT22 + I                                                  
C---- INITIALIZE ENERGY FLOWS AT NODE                                   
      DO  1260  J = 1, LMSCND                                           
      EMHBTN(NODTH,J) = 0.0                                             
      FLOBTN(NODTH,J) = 0.0                                             
 1260 EMTBTN(NODTH,J) = 0.0                                             
      GO TO 1265                                                        
C***** THERMODYNAMIC TYPE 4 AND 5 NODES - INTERNAL TYPES                
 1263 SUMQ(1) = 0.0                                                     
      NODTH = IT22 + 1                                                  
      EMHBTN(NODTH,1) = 0.0                                             
      FLOBTN(NODTH,1) = 0.0                                             
      EMTBTN(NODTH,1) = 0.0                                             
 1265 KSCT = TABL16(NODE)                                               
      DO  1340  I = 1, KSCT                                             
      NUMY1=TABL17(NODE,I)                                              
      ISCT=IABS(NUMY1)                                                  
      ISIGN=ISCT/NUMY1                                                  
      ISS = TABL18(NODE,I)                                              
      IF( ISIGN ) 1310,1280,1280                                        
C---- FORWARD END OF SECTION IS CONNECTED TO NODE                       
 1280 NODTH = TABL21(ISCT) / 64                                         
      IF( FFSS(ISS) ) 1290,1300,1300                                    
 1290 IBRCH = TABL21(ISCT) - NODTH*64                                   
      EMHBTN(NODTH,IBRCH) = EMHBTN(NODTH,IBRCH) - FFSS(ISS)*HUMTN(NODTH)
      EMTBTN(NODTH,IBRCH) = EMTBTN(NODTH,IBRCH) - FFSS(ISS)*TDBTN(NODTH)
      FLOBTN(NODTH,IBRCH) = FLOBTN(NODTH,IBRCH) - FFSS(ISS)             
 1300 J = NODTH - TABL22(NODE)                                          
      SUMQ(J) = SUMQ(J) - FFSS(ISS)                                     
      GO TO 1340                                                        
C---- BACKWARD END OF SECTION IS CONNECTED TO NODE                      
 1310 NODTH = TABL20(ISCT) / 64                                         
      IF( FBSS(ISS) ) 1330,1330,1320                                    
 1320 IBRCH = TABL20(ISCT) - NODTH*64                                   
      EMHBTN(NODTH,IBRCH) = EMHBTN(NODTH,IBRCH) + FBSS(ISS)*HUMTN(NODTH)
      EMTBTN(NODTH,IBRCH) = EMTBTN(NODTH,IBRCH) + FBSS(ISS)*TDBTN(NODTH)
      FLOBTN(NODTH,IBRCH) = FLOBTN(NODTH,IBRCH) + FBSS(ISS)             
 1330 J = NODTH - TABL22(NODE)                                          
      SUMQ(J) = SUMQ(J) + FBSS(ISS)                                     
 1340 CONTINUE                                                          
      ITYPT = TYPTND(NODE)                                              
      GO TO ( 1410, 1350, 1350, 1410, 1410 ), ITYPT                     
C                                                                       
C***** THERMODYNAMIC TYPE 2 AND 3 NODES (NON-MIXING)                    
C --  SUMQ IS POSITIVE FOR AIR ENTERING END NODE FROM CENTER NODE       
C---- COMPUTE INTRA-NODE FLOWS                                          
 1350 IT22 = TABL22(NODE)                                               
      IF( SUMQ(1) ) 1360,1360,1370                                      
C---- AIR IS FLOWING FROM 1(A) TO 2(B)                                  
 1360 EMTBTN(IT22+1,5) = EMTBTN(IT22+1,5) - SUMQ(1)*TDBTN(IT22+1)       
      EMHBTN(IT22+1,5) = EMHBTN(IT22+1,5) - SUMQ(1)*HUMTN(IT22+1)       
      FLOBTN(IT22+1,5) = FLOBTN(IT22+1,5) - SUMQ(1)                     
      GO TO 1380                                                        
C---- AIR IS FLOWING FROM 2(B) TO 1(A)                                  
 1370 EMTBTN(IT22+2,4) = EMTBTN(IT22+2,4) + SUMQ(1)*TDBTN(IT22+2)       
      EMHBTN(IT22+2,4) = EMHBTN(IT22+2,4) + SUMQ(1)*HUMTN(IT22+2)       
      FLOBTN(IT22+2,4) = FLOBTN(IT22+2,4) + SUMQ(1)                     
C                                                                       
 1380 IF( SUMQ(3) ) 1390,1400,1400                                      
C---- AIR IS FLOWING FROM 3(C) TO 2(B)                                  
 1390 EMTBTN(IT22+3,5) = EMTBTN(IT22+3,5) - SUMQ(3)*TDBTN(IT22+3)       
      EMHBTN(IT22+3,5) = EMHBTN(IT22+3,5) - SUMQ(3)*HUMTN(IT22+3)       
      FLOBTN(IT22+3,5) = FLOBTN(IT22+3,5) - SUMQ(3)                     
      GO TO 1410                                                        
C---- AIR IS FLOWING FROM 2(B) TO 3(C)                                  
 1400 EMTBTN(IT22+2,5) = EMTBTN(IT22+2,5) + SUMQ(3)*TDBTN(IT22+2)       
      EMHBTN(IT22+2,5) = EMHBTN(IT22+2,5) + SUMQ(3)*HUMTN(IT22+2)       
      FLOBTN(IT22+2,5) = FLOBTN(IT22+2,5) + SUMQ(3)                     
C                                                                       
 1410 CONTINUE                                                          
      DO 1415 NODTH = 1,NTNODE                                          
      TAVGTN(NODTH) = TDBTN(NODTH)                                      
      HAVGTN(NODTH) = HUMTN(NODTH)                                      
 1415 CONTINUE                                                          
C --  FOR FIRE SIMULATION - INITIALIZE CONVECTION + RADIATION           
C --  HEAT TRANSFER TO WALL                                             
      IF( FIROPT .LE. 0 ) GO TO 1420                                    
      DO  1418  ILS = 1, NLS                                            
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      SQWL = 0.0                                                        
      SQRD = 0.0                                                        
      DO  1417  ISS = ISSL, ISSH                                        
      SQWL = SQWL + QWALSS(ISS)                                         
 1417 SQRD = SQRD + QRADSS(ISS)                                         
      SQWLLS(ILS) = SQWL                                                
 1418 SQRDLS(ILS) = SQRD                                                
C                                                                       
C     TRAIN ENERGY SUMMARY INITIALIZATION  (SUMMATION DONE IN TRAIN)    
C                                                                       
 1420 IF( NSTR ) 1500, 1500, 1440                                       
1440  DO 1450 ISTR = 1,NSTR                                             
      EPR3SR(ISTR) = 0.0                                                
      EPRFSR(ISTR) = 0.0                                                
      EAUXSR(ISTR) = 0.0                                                
1450  ERGNSR(ISTR) = 0.0                                                
C---- INITIALIZE NUMBER OF ENERGY SUMMARY POINTS                        
      ESPTSR =0                                                         
C                                                                       
C     MXMNPT IS A COUNT OF THE NUMBER OF POINTS WHICH                   
C     HAVE BEEN ADDED INTO TOTALS                                       
C                                                                       
 1500 MXMNPT = 1                                                        
C                                                                       
C---- SUMTST CONTAINS THE TIME OF THE LAST SUMMARY INITIALIZATION       
      SUMTST = FLOAT(NTIME)/ 100.0                                      
2000  RETURN                                                            
      END                                                               
