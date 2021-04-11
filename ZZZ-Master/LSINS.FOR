      SUBROUTINE LSINS (ILS)                                            
C                                                                       
C     LINE SEGMENT INPUT SUBROUTINE                                     
C                                                                       
C     THIS SUBROUTINE READS, CHECKS, AND INPUT VERIFIES THE DATA        
C     DESCRIBING LINE SEGMENTS.                                         
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      DIMENSION DUMMY1(8),DUMMY2(8)
      CHARACTER*64 DISPSTR
5     FORMAT (64(' '))                                
10    FORMAT (2F10.0,A36)                                               
20    FORMAT (8F10.0)                                                   
30    FORMAT (5F10.0,A30)                                            
40    FORMAT ('INPUT VERIFICATION FOR LINE SEGMENT',I5,' -',I3,T57,A36
     1        ,T112,'FORM 3A')                                               
50    FORMAT (/,'LINE SEGMENT TYPE',T79,I14,T96,'(TUNNEL)')          
60    FORMAT (/,'LINE SEGMENT TYPE', T26,                            
     1  '(PROP/BRKG AND AUX/PASS TRACKWAY EXHAUSTS)',                   
     2  T79,I14,T96,'(STATION)' )                                       
61    FORMAT (/,'LINE SEGMENT TYPE', T26,                            
     1  '(PROP/BRKG TRACKWAY EXHAUST ONLY)',                            
     2  T79,I14,T96,'(STATION)' )                                       
62    FORMAT (/,'LINE SEGMENT TYPE', T26,                            
     1  '(AUX/PASS TRACKWAY EXHAUST ONLY)',                             
     2  T79,I14,T96,'(STATION)' )                                       
70    FORMAT (/,'LINE SEGMENT TYPE',T79,I14,T96,'(SPECIAL TUNNEL)')  
71    FORMAT (/,T49,15('X'),T79, I14,T96,'(SPECIAL ''FIRE'' SEGMENT)')
80    FORMAT (/,'LENGTH',T79,F14.2,T96,'FT')                         
90    FORMAT (/,'CROSS SECTION AREA',T79,F14.1,T96,'SQ FT')          
91    FORMAT (/, T3,'GRADE OF LINE SEGMENT',T79,F14.3,T96,'PERCENT')  
92    FORMAT (/,'STACK HEIGHT', T79,F14.1,T96,'FT')                  
94    FORMAT (F8.1)
96    FORMAT (F8.4)
100   FORMAT (/,'SEGMENT',T112,'FORM 3B',/,T2,'PERIMETERS',5X,A64,T84
     1         ,'TOTAL PERIMETER',F12.1,2X,'FT')                                 
110   FORMAT (/,'ROUGHNESS ',T84,'WEIGHTED AVERAGE',/,T2,'LENGTHS'8X,
     1           A64,T84,'ROUGHNESS LENGTH',F11.4, 2X,'FT')
120   FORMAT (/,T4,'HYDRAULIC DIAMETER',T79,F14.1,T96,'FT')          
130   FORMAT (/,'SEGMENT TOTAL HEAD LOSS COEFFICIENTS',T54,'POSITIVE'
     1,T75,'NEGATIVE',T112,'FORM 3C'/T56,'FLOW',T77,'FLOW')             
140   FORMAT (/,T4,'AT FORWARD LIMIT',T50,F10.2,T71,F10.2)            
150   FORMAT (T4,'AT BACKWARD LIMIT',T50,F10.2,T71,F10.2)           
160   FORMAT (T4,'FULLY TURBULENT FRICTION FACTOR (FROM AVERAGE ROUGHNES
     1S)',T79,F14.4)                                               
165   FORMAT (T4,'AVERAGE OF INDIVIDUAL WALL FRICTION FACTORS',T79,
     1F14.4)                                                            
170   FORMAT (T4,'RELATIVE ROUGHNESS   ( E/D )',T79,F14.5)         
180   FORMAT (/,'WETTED WALL SURFACE',T79,F14.1,T96,'PERCENT')       
190   FORMAT (/,'NUMBER OF SUBSEGMENTS',T79,I14)                     
200   FORMAT (/,'NUMBER OF STEADY-STATE HEAT SOURCES', T79,I14)      
210   FORMAT (/,1X,'STARTING',T18,'ENDING',T112,'FORM 3D',/, 'SUBSEGMENT
     1',T16,'SUBSEGMENT',T33,'SOURCE',T50,'STEADY-STATE HEAT RATE'/   
     2 2X,'NUMBER',T18,'NUMBER',T34,'TYPE',T50,'SENSIBLE',T66,'LATENT', 
     3 T81,'IDENTIFICATION'/T50,'(BTU/HR)',T65,'(BTU/HR)'/)             
220   FORMAT (I6,5X,'THRU',I6,I15,T43,F14.0,F15.0,T81,A30)            
230   FORMAT (/,1X,'STARTING',T18,'ENDING',T112,'FORM 3E',/,'SUBSEGMENT'
     1,T16,'SUBSEGMENT',T31,'WALL SURFACE',T49,'INITIAL AIR TEMPERATURE'
     2/2X,'NUMBER',T18,'NUMBER',T31,'TEMPERATURE',T49,'DRY-BULB',T64,
     3'WET-BULB'/T33,'(DEG F)',T49,'(DEG F)',T64,'(DEG F)'/)            
240   FORMAT (I6,5X,'THRU',I6,T29,F10.1,6X,F10.1,5X,F10.1)          
250   FORMAT (/,'TUNNEL WALL THICKNESS',T79,F14.1,T96,'FT',T112,'FORM 3F
     1')                                                             
260   FORMAT ('DISTANCE BETWEEN THE INSIDE WALL SURFACES OF ADJACENT TUN
     1NELS',T79,F14.1,T96,'FT')                                    
270   FORMAT ('DISTANCE BETWEEN THE INSIDE WALL SURFACES OF ADJACENT TUN
     1NELS',T79,F14.1,T96,'FT   ( NO ADJ. TUNNEL )')                
280   FORMAT ('TUNNEL WALL THERMAL CONDUCTIVITY',T79,F14.3,T96,'BTU/FT-H
     1R-DEG F')                                                    
290   FORMAT ('TUNNEL WALL THERMAL DIFFUSIVITY',T79,F14.3,T96,'FT SQUARE
     1D/HR')                                                       
300   FORMAT ('SURROUNDING SOIL THERMAL CONDUCTIVITY',T79,F14.3,T96,
     1'BTU/FT-HR-DEG F')                                               
310   FORMAT ('SURROUNDING SOIL THERMAL DIFFUSIVITY',T79,F14.3,T96,
     1'FT SQUARED/HR')                                                  
320   FORMAT ('DEEP SINK TEMPERATURE',T79,F14.1,T96,'DEG F')       
C                                                                       
C**** INITIALIZE VARIABLE DISPSTR
      WRITE(DISPSTR,5)                                                                       
C                                                                       
C     INPUT FORM 3A                                                     
C                                                                       
      READ (IN,10) DUMY1,DUMY2,NAMLS(ILS)                     
      NUMY1=DUMY1                                                       
      NUMLS(ILS)=NUMY1                                                  
      TYPLS(ILS)=DUMY2                                                  
      ISCT=TABL8(ILS)                                                   
      ISCTX=TABL11(ISCT)                                                
      WRITE (OUT,40) ISCTX,NUMY1,NAMLS(ILS)                   
      CALL CHECKI (NUMY1,1,999,149)                                     
C --  CHECK FOR DUPLICATE SEGMENT I.D. NUMBERS                          
      NUMY1=ILS-1                                                       
      IF (NUMY1) 360,360,330                                            
330   DO 350 J=1,NUMY1                                                  
      IF (NUMLS(J)-NUMLS(ILS)) 350,340,350                              
340   CALL EERROR (162)                                                  
350   CONTINUE                                                          
360   ITPSEG = TYPLS(ILS)                                               
      IF (ITPSEG) 420,420,370                                           
370   IF (ITPSEG - 16)  380, 380, 420                                   
380   GO TO (390,400,401,402,410,410,410,410,415,415,415,415,415,415,   
     1       410,410), ITPSEG                                           
390   WRITE (OUT,50) TYPLS(ILS)                                         
      GO TO 430                                                         
400   WRITE(OUT,60) TYPLS(ILS)                                          
      GO TO  430                                                        
401   WRITE(OUT,61) TYPLS(ILS)                                          
      GO TO  430                                                        
402   WRITE(OUT,62) TYPLS(ILS)                                          
      GO TO  430                                                        
410   WRITE (OUT,70) TYPLS(ILS)                                         
      GO TO 430                                                         
415   WRITE (OUT,70) TYPLS(ILS)                                         
      CALL CHECKI (ITPSEG-8,0,NIFTYP,251)                               
      GO TO 430                                                         
420   WRITE (OUT,50) TYPLS(ILS)                                         
      CALL EERROR (10)                                                   
      TYPLS(ILS)=1                                                      
430   CONTINUE                                                          
      READ (IN,20) DUMY1,ALS(ILS),STAKLS(ILS),DUMY2                     
      LLS(ILS)=DUMY1                                                    
      WRITE (OUT,80) LLS(ILS)                                           
      CALL CHECKR (LLS(ILS),10.0,100000.0,19)                           
      WRITE (OUT,90) ALS(ILS)                                           
      CALL CHECKR (ALS(ILS),75.0,10000.0,20)                            
      NUMY1=0                                                           
      PERMLS(ILS)=0.0                                                   
C**** CONVERT THE STACK HEIGHT TO A GRADE(PERCENT), PRINT OUT FOR INPUT 
C     VERIFICATION.  THE AVERAGE STACK HEIGHT PER SUBSEGMENT IS TO BE   
C     STORED AFTER READING INPUT FORM 3C.                               
      WRITE (OUT, 92)  STAKLS(ILS)                                      
      GRADLS = STAKLS(ILS) /DUMY1 * 100.0                               
      WRITE (OUT,91) GRADLS                                             
      ABSTAK = ABS(STAKLS(ILS))                                         
      CALL CHECKR (ABSTAK,0.0,DUMY1,255)                                
      NUMY2 = DUMY2                                                     
      CALL CHECKI (NUMY2,0,1,252)                                       
      IF (NUMY2 .EQ. 0)  GO TO 432                                      
C**** THIS IS A SPECIAL FIRE SEGMENT - STORE SEGMENT TYPE AS A NEGATIVE 
      TYPLS(ILS)= -TYPLS(ILS)                                           
C---- PRINT OUT SPECIAL 'FIRE' SEGMENT                                  
      WRITE( OUT,71) NUMY2                                              
432   CONTINUE                                                          
C                                                                       
C     INPUT FORM 3B                                                     
C                                                                       
      READ (IN,20) DUMMY1,DUMMY2                                        
      DO 450 J=1,8                                                      
      IF (DUMMY1(J)) 450,450,440                                        
440   NUMY1=NUMY1+1                                                     
      PERMLS(ILS)=PERMLS(ILS)+DUMMY1(J)                                 
450   CONTINUE                                                          
      RNFLS(ILS)=4.0*RHOMAS/(PERMLS(ILS)*VISAIR)                        
      C1LS(ILS)=PERMLS(ILS)*LLS(ILS)/(8.0*ALS(ILS)**3)
C**** STORE SEGMENT PERIMETERS IN VARIABLE DISPSTR FOR LATER PRINTING
	DO 455 J=1,NUMY1
	  JJ=1+8*(J-1)
	  KK=JJ+7
	  WRITE(DISPSTR(JJ:KK),94)DUMMY1(J)
455   CONTINUE
      WRITE (OUT,100) DISPSTR, PERMLS(ILS)
      CALL CHECKR (PERMLS(ILS),30.0,1000.0,21)                          
      DUMY1=(4.0*PI*ALS(ILS))**0.5                                      
      IF (PERMLS(ILS)-DUMY1) 460,470,470                                
460   CALL EERROR (70)                                                   
470   EPSLS(ILS)=0.0                                                    
      DO 480 J=1,NUMY1                                                  
480   EPSLS(ILS)=EPSLS(ILS)+DUMMY1(J)*DUMMY2(J)                         
      DUMY1=4.0*ALS(ILS)/PERMLS(ILS)                                    
      EPSLS(ILS)=EPSLS(ILS)/PERMLS(ILS)                                 
C**** STORE SEGMENT ROUGHNESS LENGTHS IN VARIABLE DISPSTR FOR LATER
C**** PRINTING
	DO 485 J=1,NUMY1
	  JJ=1+8*(J-1)
	  KK=JJ+7
	  WRITE(DISPSTR(JJ:KK),96)DUMMY2(J)
485   CONTINUE
      WRITE (OUT,110) DISPSTR, EPSLS(ILS)                  
      CALL CHECKR (EPSLS(ILS),0.0,2.0,22)                               
      WRITE (OUT,120) DUMY1                                             
      DUMY5=EPSLS(ILS)/DUMY1                                            
      WRITE (OUT,170) DUMY5                                             
      FFFLS(ILS)=(19000.0+16000.0*((EPSLS(ILS)/(0.05*DUMY1))**1.5))*EPSL
     1S(ILS)/DUMY1                                                      
      DUMY3=0.0055*(1.0+FFFLS(ILS)**0.33333333)                         
      WRITE (OUT,160) DUMY3                                             
      GO TO (510,490,490,490,490,490,510,510), SUPOPT                   
490   DUMY3=0.0                                                         
      DO 500 J=1,NUMY1                                                  
      DUMMY2(J)=(19000.0+16000.0*((DUMMY2(J)/(0.05*DUMY1))**1.5))*DUMMY2
     1(J)/DUMY1                                                         
500   DUMY3=DUMY3+DUMMY1(J)*0.0055*(1.0+DUMMY2(J)**0.33333333)          
      DUMY3=DUMY3/PERMLS(ILS)                                           
      WRITE (OUT,165) DUMY3                                             
C                                                                       
C     INPUT FORM 3C                                                     
C                                                                       
510   READ (IN,20) CFPLS(ILS),CFNLS(ILS),CBPLS(ILS),CBNLS(ILS),DUMY1,DUM
     1Y2,DUMY3                                                          
      WRITE (OUT,130)                                                   
      WRITE (OUT,140) CFPLS(ILS),CFNLS(ILS)                             
      CALL CHECKR (CFPLS(ILS),0.0,1000.0,28)                             
      CALL CHECKR (CFNLS(ILS),0.0,1000.0,28)                             
      WRITE (OUT,150) CBPLS(ILS),CBNLS(ILS)                             
      CALL CHECKR (CBPLS(ILS),0.0,1000.0,28)                             
      CALL CHECKR (CBNLS(ILS),0.0,1000.0,28)                             
      PDRYLS(ILS)=DUMY1                                                 
      WRITE (OUT,180) PDRYLS(ILS)                                       
      CALL CHECKR (PDRYLS(ILS),0.0,100.0,151)
C     IF THE TEMPERATURE/HUMIDITY OPTION (THOPT) IS NOT EQUAL TO 2
C     SET THE WETTED WALL SURFACE AREA TO ZERO REGARDLESS OF WHAT THE
C     USER HAS ENTERED.
      IF (THOPT .EQ. 2) THEN
	  PDRYLS(ILS)=1.0-(PDRYLS(ILS)/100.0)
	ELSE
	  PDRYLS(ILS)=1.0
	END IF
      NSS=DUMY2                                                         
      WRITE (OUT,190) NSS                                               
      CALL CHECKI  (NSS,1,LMLSS,29)                                     
      ILSL=TNLSS+1                                                      
      TNLSS=TNLSS+NSS                                                   
      CALL CHECKI (TNLSS,1,LMLSS,30)                                    
      IF (NEW .EQ. 1) GOTO 730                                            
C**** STORE THE AVERAGE STACK HEIGHT PER SUBSEGMENT                     
      STAKLS(ILS) = STAKLS(ILS)/FLOAT(NSS)*GRACC                        
      NUMY1 = DUMY3                                                     
      WRITE (OUT,200) NUMY1                                             
C---- INITIALIZE SUBSEGMENT PARAMETERS                                  
      DO 540 ISS=ILSL,TNLSS                                             
      DO 530 IROUTE=1,LMTRRT                                            
530   ITNLSS(ISS,IROUTE)=' '                                          
C                                                                       
      QWALSS(ISS) = 0.0                                                 
      QRADSS(ISS) = 0.0                                                 
      QERTSS(ISS)=0.0                                                   
      QERHSS(ISS)=0.0                                                   
      FFSS(ISS) = 0.0                                                   
      FBSS(ISS) = 0.0                                                   
      HTRNSS(ISS) = 0.0                                                 
      SHUXSS(ISS) = 0.0                                                 
      LHLSS(ISS)=0.0                                                    
      SHLSS(ISS)=0.0                                                    
      LACLSS(ISS)=0.0                                                   
      SACLSS(ISS)=0.0                                                   
      LHLTSS(ISS)=0.0                                                   
540   SHLTSS(ISS)=0.0                                                   
C                                                                       
C     INPUT FORM 3D                                                     
C                                                                       
C     STEADY-STATE HEAT SOURCES                                         
      IF (NUMY1) 610,610,550                                            
550   WRITE (OUT,210)                                                   
      DO 600 J=1,NUMY1                                                  
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,TITLE                 
      NUMY3=DUMY1                                                       
      NUMY4=DUMY2                                                       
      NUMY5=DUMY3                                                       
      WRITE (OUT,220) NUMY3,NUMY4,NUMY5,DUMY4,DUMY5,TITLE              
      NUMY3=NUMY3-1+ILSL                                                
      NUMY4=NUMY4-1+ILSL                                                
      CALL CHECKI (NUMY3,ILSL,TNLSS,31)                                 
      CALL CHECKI (NUMY4,NUMY3,TNLSS,31)                                
      CALL CHECKI (NUMY5,1,2,6)                                         
      IF (NEW .EQ. 1) GOTO 730                                            
      DO 590 K=NUMY3,NUMY4                                              
      DUMY1=DUMY4/(3600.0*FLOAT(NUMY4-NUMY3+1))                         
      DUMY2=DUMY5/(3600.0*FLOAT(NUMY4-NUMY3+1))                         
      IF (NUMY5-1) 580,580,570                                          
C---- HEATING OR COOLING SOURCES  ( TYPE 2)                             
570   SACLSS(K)=SACLSS(K)+DUMY1                                         
      LACLSS(K)=LACLSS(K)+DUMY2                                         
C***** MISC. STEADY-STATE SOURCES - TYPES 1 AND 2                       
580   SHLSS(K)=SHLSS(K)+DUMY1                                           
      LHLSS(K)=LHLSS(K)+DUMY2                                           
      SHLTSS(K)=SHLSS(K)                                                
590   LHLTSS(K)=LHLSS(K)                                                
600   CONTINUE                                                          
C                                                                       
C     INPUT FORM 3E                                                     
C                                                                       
C     WALL SURFACE TEMPERATURE AND INITIAL AIR TEMPERATURE              
610   WRITE (OUT,230)                                                   
      NUMY3=0                                                           
620   READ (IN,20) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5                        
      NUMY1=DUMY1                                                       
      NUMY2=DUMY2                                                       
      WRITE (OUT,240) NUMY1,NUMY2,DUMY3,DUMY4,DUMY5                     
      IF (NUMY3+1-NUMY1) 630,640,630                                    
630   CALL EERROR (150)                                                  
640   CALL CHECKI (NUMY1,1,NSS,139)                                     
      CALL CHECKI (NUMY2,NUMY1,NSS,150)                                 
      IF (NEW .EQ. 1) GOTO 730                                            
      NUMY3=NUMY2                                                       
      NUMY1=NUMY1-1+ILSL                                                
      NUMY2=NUMY2-1+ILSL                                                
      CALL CHECKR (DUMY3,0.0,130.0,23)                                  
      CALL CHECKR (DUMY4,0.0,130.0,24)                                  
      CALL CHECKR (DUMY5,0.0,DUMY4,25)                                  
      CALL LATENT (DUMY4,DUMY5,DUMY6)                                   
      DO 660 ISS=NUMY1,NUMY2                                            
      TSFSS(ISS)=DUMY3                                                  
      TDBSS(ISS)=DUMY4                                                  
      TTMPSS(ISS)=DUMY4                                                 
      TWBSS(ISS)=DUMY5                                                  
      HUMSS(ISS)=DUMY6                                                  
660   HTMPSS(ISS)=DUMY6                                                 
C---- LOOPBACK TO READ ANOTHER LINE IF HIGHEST SUBSEGMENT IS            
C---- LESS THAN THE TOTAL NUMBER OF SUBSEGMENTS                         
      IF (NUMY2-TNLSS) 620,670,670                                      
C                                                                       
C     INPUT FORM 3F                                                     
C                                                                       
C---- HEAT SINK DATA                                                    
670   IF (HCOPT) 720,720,680                                            
680   READ (IN,20) RPRMLS(ILS),DBTLS(ILS),THCNLS(1,ILS),THDFLS(1,ILS),TH
     1CNLS(2,ILS),THDFLS(2,ILS),TSNKLS(ILS)                             
      WRITE (OUT,250) RPRMLS(ILS)                                       
      CALL CHECKR (RPRMLS(ILS),0.0,30.0,63)                             
      IF (DBTLS(ILS)-0.01) 700,700,690                                  
690   WRITE (OUT,260) DBTLS(ILS)                                        
      CALL CHECKR (DBTLS(ILS),0.0, 100.0,131)                           
      GO TO 710                                                         
700   WRITE (OUT,270) DBTLS(ILS)                                        
      CALL CHECKR (DBTLS(ILS),0.0, 100.0,131)                           
      DBTLS(ILS)=0.0                                                    
710   WRITE (OUT,280) THCNLS(1,ILS)                                     
      CALL CHECKR (THCNLS(1,ILS),0.005,2.0,34)                          
      WRITE (OUT,290) THDFLS(1,ILS)                                     
      CALL CHECKR (THDFLS(1,ILS),0.005,1.0,35)                          
      WRITE (OUT,300) THCNLS(2,ILS)                                     
      CALL CHECKR (THCNLS(2,ILS),0.005,2.0,34)                          
      WRITE (OUT,310) THDFLS(2,ILS)                                     
      CALL CHECKR (THDFLS(2,ILS),0.005,1.0,35)                          
      WRITE (OUT,320) TSNKLS(ILS)                                       
      CALL CHECKR (TSNKLS(ILS),0.0,100.0,159)                           
C                                                                       
C---- INITIALIZATION                                                    
720   TABL3(ILS)=ILSL                                                   
      TABL4(ILS)=TNLSS                                                  
      DO 725  ISS = ILSL,TNLSS                                          
  725 TABL7(ISS) = ILS                                                  
      SURLSS(ILS)=PERMLS(ILS)*LLS(ILS)/FLOAT(NSS)                       
      LLSS(ILS)=LLS(ILS)/FLOAT(NSS)                                     
      VOLLSS(ILS)=LLSS(ILS)*ALS(ILS)                                    
      RELS(ILS) = ABS(QS(ISCT))*4.0*RHOMAS/(VISAIR*PERMLS(ILS))         
      DO  727  ISS = ILSL, TNLSS                                        
727   RELSS(ISS) = RELS(ILS)                                            
730   RETURN                                                            
      END                                                               
