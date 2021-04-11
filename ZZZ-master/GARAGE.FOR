      SUBROUTINE GARAGE (I)                                             
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
      DIMENSION  DUMMY7(3), IPIVOT(3), ROWSIZ(3)                        
      CHARACTER*80 TITLE1                                     
      REAL  DUMMY1(4), DUMMY2(4), DUMMY3(4), DUMMY4(9), DUMMY5(3)       
      REAL  DUMMY6(5)                                                   
C                                                                       
C                                                                       
      DATA  RRACC1 / 8.8 /                                              
10    FORMAT (A1)   
110   FORMAT (A36,4X,4F10.0)                                            
120   FORMAT (8F10.0)     
121   FORMAT (2F10.0,3E10.4,2F10.0)                                     
130   FORMAT ('INPUT VERIFICATION FOR TRAIN TYPE',I2,3X,A36,T112,
     1        'FORM 9A' )                                                 
140   FORMAT ('INPUT VERIFICATION FOR TRAIN TYPE',I2,3X,A36,T112,     
     1        'CONTINUED')                                                
150   FORMAT (//,'TOTAL NUMBER OF CARS PER TRAIN',T79,I14)           
155   FORMAT (/,'NUMBER OF POWERED CARS PER TRAIN',T79,I14)          
160   FORMAT (/,'TOTAL LENGTH OF TRAIN',T79,F14.1,T96,'FT')          
170   FORMAT (/,'FRONTAL AREA OF TRAIN',T79,F14.2,T96,'SQ FT')      
180   FORMAT (/,'PERIMETER OF CAR',T79,F14.2,T96,'FT',T112, 'FORM 9B')
190   FORMAT (/,'SKIN FRICTION COEFFICIENT',T79,F14.4)              
195   FORMAT (/,'DRAG COEFFICIENT WEIGHTED TOTAL TRUCK AREA',T79,F14.2,
     1           T96,'SQ FT')                                                    
210   FORMAT (/,'FRONT OF TRAIN DRAG COEFFICIENT',T79,F14.4)         
228   FORMAT (/,'TRAIN HEAT DATA',T112,'FORM 9C')                    
230   FORMAT (/,'SENSIBLE HEAT REJECTION FROM AUXILIARY SYSTEMS FOR AN E
     1MPTY CAR EXCLUDING',/,'PROPULSION SYSTEM',T79,F14.1,T96,'BTU/HR PE     
     2R CAR')                                                
235   FORMAT (/,'LATENT HEAT REJECTION FROM AUXILIARY SYSTEMS FOR AN EMP
     1TY CAR EXCLUDING',/,'PROPULSION SYSTEM',T79,F14.1,T96,'BTU/HR PER 
     2CAR')                                                
236   FORMAT (/,'REGENERATIVE BRAKING EFFECTIVENESS',T79,F14.1,T96,'PERC
     1ENT')                                                          
237   FORMAT (/,'SENSIBLE HEAT REJECTION FROM PASSENGERS AND AUXILIARY S
     1YSTEMS PER PATRON IN CAR',T82,F11.1,T96,'BTU/HR PER PATRON')   
238   FORMAT (/,'LATENT HEAT REJECTION FROM PASSENGERS AND AUXILIARY SYS
     1TEMS PER PATRON IN CAR',T79,F14.1,T96,'BTU/HR PER PATRON')   
239   FORMAT (/,'POWER CONSUMPTION BY VEHICLE AUXILIARY SYSTEMS FOR AN E
     1MPTY CAR',T79,F14.1,T96,'KW PER CAR')                          
240   FORMAT (/,'POWER CONSUMPTION BY VEHICLE AUXILIARY SYSTEMS PER PATR
     1ON IN CAR',T79,F14.3,T96,'KW PER PATRON')                      
248   FORMAT (/,'RESISTOR GRID PHYSICAL CHARACTERISTICS  (PER POWERED CA
     1R)',T66,'ACCELERATION',T85,'DECELERATION',T112,'FORM 9D',/,T70,'GR      
     2ID', T89,'GRID')                                        
250   FORMAT (/,T8,'TOTAL WEIGHT OF RESISTANCE ELEMENTS',T65,F9.1,T79,             
     1F14.1,T96,'LB')                                      
260   FORMAT (/,T8,'EFFECTIVE DIAMETER OF AN ELEMENT',T65,F9.1,T79,   
     1F14.1,T96,'IN.')                                                 
270   FORMAT (/,T8,'EFFECTIVE SURFACE AREA FOR CONVECTION',T65,F9.1,T79
     1,F14.1,T96,'SQ FT')                                  
280   FORMAT (/,T8,'EFFECTIVE SURFACE AREA FOR RADIATION',T65,F9.1,T79,   
     1F14.1,T96,'SQ FT')                                           
290   FORMAT (/,T8,'EMISSIVITY OF RESISTANCE ELEMENT',T65,F9.2,T79,
     1F14.2 )                                                
300   FORMAT (/,T8,'SPECIFIC HEAT OF RESISTANCE ELEMENT',T65,F9.3,T79,        
     1F14.3,T96,'BTU/LB-DEG F')                              
302   FORMAT(/,T8,'INITIAL GRID TEMPERATURES',T65,F9.1,T79,F14.1,T96,
     1'DEG F' )                                                   
310   FORMAT (/,'A VALUE OF ZERO FOR THE ACCELERATION GRID RESISTANCE EL
     1EMENT WEIGHT INDICATES THAT THE HEAT GENERATED FROM TRAIN',/,'ACCE    
     2LERATION IS TO BE INSTANTANEOUSLY REJECTED TO THE SYSTEM.')
320   FORMAT (/,'A VALUE OF ZERO FOR THE DECELERATION GRID RESISTANCE EL
     1EMENT WEIGHT INDICATES THAT THE HEAT GENERATED FROM TRAIN',/,'BRAK
     2ING IS TO BE INSTANTANEOUSLY REJECTED TO THE SYSTEM.')      
325   FORMAT(//,'AVERAGE EMPTY CAR WEIGHT',T79,F14.1,T96,'TONS',T112,           
     1'FORM 9E') 
345   FORMAT(/,'TRAIN ROLLING RESISTANCE COEFFICIENTS ',             
     1     T44,'FIRST',T79,F14.2,T96,'LBS/TON',//,
     2     T44,'SECOND',T79,F14.2,T96,'LBS',//,
     3     T44,'THIRD',T79,F14.4,T96,'LBS/TON-MPH')
340   FORMAT (/,'NUMBER OF MOTORS PER POWERED CAR',T79,F14.0)        
350   FORMAT (/,'ACCELERATION RESISTANCE OF ROTATING PARTS',T79,F14.1,
     1           T96,'(LBS/TON)/(MPH/SEC)')                            
360   FORMAT (A36,4X,2F10.0,/,5F10.0)                                    
370   FORMAT (/,T23,'MOTOR TYPE   ',A36,T112,'FORM 9F')              
380   FORMAT (/,'MANUFACTURER''S MOTOR DATA CORRECTIONS',            
     1 T65,'MANUFACTURER''S',T87,'VEHICLE'/T70,'DATA',T89,'DATA'//,   
     2 T8,'WHEEL DIAMETER',T62,F12.1,T79,F14.1,T96,'IN.')              
390   FORMAT (/,T8,'GEAR RATIO',T62,F12.1,T79,F14.1)                 
395   FORMAT (//,'LINE CURRENT REQUIRED BASED ON MANUFACTURER''S DATA',
     1 T112,'FORM 9H')                                                 
400   FORMAT (/,T8,'SUPPLY VOLTAGE',T62,F12.1,T79,F14.1,T96,'VOLTS')         
401   FORMAT (/,'MOTOR TERMINAL VOLTAGE AT BASE SPEED',T79,F14.1,T96,
     1'VOLTS/MOTOR')                                                    
410   FORMAT (/,'TRACTIVE EFFORT AVAILABLE AND CURRENT REQUIRED BASED ON
     1 MANUFACTURER''S DATA') 
420   FORMAT (/,T8,'TRAIN SPEED',T27,4F12.1,T96,'MPH',T112,'FORM 9G')
430   FORMAT (/,T8,'TRACTIVE EFFORT',T27,4F12.1,T96,'LBS')           
440   FORMAT (/,T8,'MOTOR CURRENT',T27,4F12.1,T96,'AMPS')            
441   FORMAT (/,T8,'LINE CURRENT',T27,5F12.1,T96,'AMPS/POWERED CAR')
443   FORMAT (/'TRAIN CONTROLLER OPTION',T91,I2,3X,'CAM CONTROL'/) 
444   FORMAT (/'TRAIN CONTROLLER OPTION',T91,I2,3X,'CHOPPER CONTROL'/) 
445   FORMAT (//,'CHOPPER EFFICIENCY FOR SPEEDS OF ZERO TO U1',T79,  
     1 F14.1,T96,'PERCENT')                                             
446   FORMAT (/,'SPEED U1',T79,F14.1,T96,'MPH')                      
447   FORMAT (/,'CHOPPER EFFICIENCY FOR SPEEDS GREATER THAN U1',T79,
     1F14.1,T96,'PERCENT')                                               
450   FORMAT (/,'VERIFICATION OF TRACTIVE EFFORT AVAILABLE AND CURRENT R
     1EQUIRED BASED ON THE VEHICLE DATA',//,5X,2(6X,'SPEED',7X,'TRACTIVE
     2 EFFORT',4X,'MOTOR CURRENT' ,3X ) ,T59,'.'/,5X,2( 6X,'(MPH)', 9X,'
     3(LBS/MOTOR)', 6X,'(AMPS/MOTOR)', 4X),T59,'.',/ T59, '.'  )                                          
451   FORMAT (//,'VERIFICATION OF TRACTIVE EFFORT AVAILABLE AND CURRENT  
     1REQUIRED BASED ON THE VEHICLE DATA',//,2(3X,'SPEED',6X,'TRACTIVE E
     2FFORT',4X,'MOTOR CURRENT',3X,'LINE CURRENT',3X) ,T65,'.',/,2(3X,'(
     3MPH)',8X,'(LBS/MOTOR)',6X,'(AMPS/MOTOR)',3X,'(AMPS/PWR CAR)',2X),T
     465,'.',/,T65,'.')                                         
452   FORMAT (2(2X,F6.1,F16.0,2F17.0,6X),T65,'.')                   
460   FORMAT (5X,2(F10.1,F18.0,F17.0,8X) , T60,'.' )                
461   FORMAT (//,'INTERNAL MOTOR RESISTANCE',T79,F14.3,T96,'OHMS/MOTOR',
     1T112,'FORM 9I')                                                
470   FORMAT (//,'MOTOR CIRCUIT RESISTANCE PER MOTOR  (SUM OF MOTOR AND 
     1EXTERNAL RESISTANCES)',T112,'FORM 9I')                        
475   FORMAT(/,T8,'RESISTANCE',T28,3F12.3,T96,'OHMS')                
480   FORMAT (/,T8,'SPEED',T37,'0.0',2F12.1,T96,'MPH')               
490   FORMAT (/,'MAXIMUM ALLOWABLE ACCELERATION RATE',               
     1  T79,F14.1,T96,'MPH/SEC' ,T112,'FORM 9J' )                       
500   FORMAT (/,'NORMAL DECELERATION RATE FROM SPEED V1 TO ZERO',T79,
     1 F14.1,T96,'MPH/SEC')                                             
510   FORMAT (/,'SPEED V1',T79,F14.1,T96,'MPH')                      
520   FORMAT (/,'NORMAL DECELERATION RATE AT SPEED V2',T79,F14.1,T96,
     1  'MPH/SEC')                                                      
530   FORMAT (/,'SPEED V2',T79,F14.1,T96,'MPH')                      
532   FORMAT (/,'ON-BOARD FLYWHEEL SIMULATION OPTION',T79,I14,3X,       
     1          'YES',/)
533   FORMAT (/,'ON-BOARD FLYWHEEL SIMULATION OPTION',T79,I14,3X,       
     1          'BYPASS',/)       
534   FORMAT (/,T46,'ON-BOARD FLYWHEEL',T112,'FORM 9K'/ / ,'POLAR MOMEN
     1T OF INERTIA (J) OF FLYWHEEL ASSEMBLY (ROTOR, SHAFT, ETC.)',T79,
     2F14.1,T96,'LBS-FT SQUARED')                                       
535   FORMAT (/,'NUMBER OF FLYWHEELS PER CAR',T79,I14,//,T38,'FLYWHEEL O
     1PERATING CHARACTERISTICS')                         
536   FORMAT (/'MINIMUM ALLOWABLE ROTATIONAL SPEED',T79,F14.1,T96,'RPM')                                                            
537   FORMAT (/'MAXIMUM ALLOWABLE ROTATIONAL SPEED',T79,F14.1,T96,'RPM')                                                            
538   FORMAT (/,'INITIAL ROTATIONAL SPEED',T79,F14.1,T96,'RPM')      
539   FORMAT (/,'OVERALL EFFICIENCY OF POWER CONVERSION FROM THE TRAIN W
     1HEEL'/'TO THE FLYWHEEL (DURING TRAIN BRAKING)',T79,F14.1,T96,
     2'PERCENT',T112,'FORM 9L')                                        
540   FORMAT (/,'OVERALL EFFICIENCY OF POWER CONVERSION FROM THE FLYWHEE
     1L TO'/,'THE TRAIN WHEEL (DURING TRAIN MOTORING)',T79,F14.1,T96,
     2'PERCENT')         
541   FORMAT (/,'COEFFICIENTS OF EQUATION FOR PARASITIC LOSSES (WINDAGE,
     1 BEARINGS, ETC.)'//,T40,'FIRST COEFFICIENT  - A',T83,E10.4,//,T40,
     2'SECOND COEFFICIENT - B', T83,E10.4//,T40,'THIRD COEFFICIENT  - C'
     3,T83,E10.4)                                                 
542   FORMAT (/,T40,'FIRST EXPONENT     - D',T79,F14.1)                
543   FORMAT (/,T40,'SECOND EXPONENT    - E',T79,F14.1)                
C                                                                       
C                                                                       
C-----   INPUT FORM 9A                                                  
      READ (IN,110) TITLE,DUMY1,DUMY2,LV(I),AV(I)                      
      WRITE (OUT,130) I,TITLE                                          
      NCARV(I)=DUMY1                                                    
      NUMY1=DUMY1                                                       
      WRITE (OUT,150) NUMY1                                             
      CALL CHECKI (NUMY1,1,20,51)                                       
      NPCARV(I)=DUMY2                                                   
      NUMY2=DUMY2                                                       
      WRITE(OUT,155) NUMY2                                              
      CALL CHECKI( NUMY2, 1, NUMY1, 116 )                               
      WRITE (OUT,160) LV(I)                                             
      CALL CHECKR (LV(I),25.0,1500.0,52)                                
      WRITE (OUT,170) AV(I)                                             
      CALL CHECKR (AV(I),25.0,300.0,53)                                 
C                                                                       
C-----   INPUT FORM 9B                                                  
      READ(IN,120) PERMV(I),LAMDAV(I),DUMY2,CDFVOV(I)                   
      WRITE (OUT,180) PERMV(I)                                          
      CALL CHECKR (PERMV(I),20.0,200.0,78)                              
      DUMY1=PERMV(I)-(4.0*PI*AV(I))**0.5                                
      IF (DUMY1) 544,546,546                                            
544   CALL EERROR (70)                                                   
  546 WRITE (OUT,190) LAMDAV(I)                                         
      CALL CHECKR (LAMDAV(I),0.0001,0.20,55)                            
      DUMY1=0.5*LV(I)*LAMDAV(I)*PERMV(I)/(4.0*AV(I))                    
      CDBVOV(I)=0.029/SQRT(DUMY1)                                       
      WRITE(OUT,195) DUMY2                                              
      CALL CHECKR(DUMY2,0.0,500.0,201)                                  
      DUMY2=DUMY2/(4.0*LV(I)*PERMV(I))                                  
      LAMDAV(I)=LAMDAV(I)+DUMY2                                         
      LAMDAV(I)=LAMDAV(I)*PERMV(I)                                      
      WRITE (OUT,210) CDFVOV(I)                                         
      CALL CHECKR ( CDFVOV(I), 0.0, 1.5, 115 )                          
C                                                                       
C**** TRAIN HEAT DATA                                                   
C----    INPUT FORM 9C                                                  
      WRITE (OUT,228)                                                   
      READ (IN,120) SHREV(I),LHREV(I),SHRPV(I),LHRPV(I),PAUXEV(I)       
     1,PAUXPV(I)                                                        
      WRITE (OUT,230) SHREV(I)                                          
      CALL CHECKR (SHREV(I),0.0,0.1E7,57)                               
      WRITE(OUT,235)  LHREV(I)                                          
      CALL CHECKR (LHREV(I),-50000.0,0.2E6,58)                          
      IERR = 0                                                          
C                                                                       
C---- IF TPOPT=2 OR 3, SHREV, LHREV, AND PAUXPV MUST BE EQUAL TO ZERO   
      IF (TPOPT .LT. 2)  GO TO 548                                      
      IF ( SHRPV(I) .EQ. 0. )  GO TO 548                                
      IERR = 1                                                          
      SHREV(I) = 0.                                                     
  548 CONTINUE                                                          
      WRITE(OUT,237) SHRPV(I)                                           
      CALL CHECKR (SHRPV(I),-100.0,1000.0,222)                          
C                                                                       
C---- IF TPOPT=2 OR 3, SHREV, LHREV, AND PAUXPV MUST BE EQUAL TO ZERO   
      IF (TPOPT .LT. 2)  GO TO 550                                      
      IF ( LHRPV(I) .EQ. 0. )  GO TO 550                                
      IERR = 1                                                          
      LHREV(I) = 0.                                                     
  550 CONTINUE                                                          
      WRITE(OUT,238) LHRPV(I)                                           
      CALL CHECKR (LHRPV(I),-100.0,1000.0,222)                          
      WRITE(OUT,239) PAUXEV(I)                                          
      CALL CHECKR (PAUXEV(I),0.0,100.0,241)                             
C---- IF TPOPT=2 OR 3, SHREV, LHREV, AND PAUXPV MUST BE EQUAL TO ZERO   
      IF (TPOPT .LT. 2)  GO TO 552                                      
      IF ( PAUXPV(I) .EQ. 0. )  GO TO 552                               
      IERR = 1                                                          
      PAUXPV(I) = 0.                                                    
  552 CONTINUE                                                          
C                                                                       
      WRITE(OUT,240) PAUXPV(I)                                          
      CALL CHECKR( PAUXPV(I), -2.0, 5.0, 242 )                          
C                                                                       
C---- CHECK IERR, IERR=0 - O.K.,     IERR=1 - WRITE ERROR MESSAGE       
      IF ( IERR .EQ. 1 )  CALL EERROR(244)                               
      IF( LV(I) .LT. 0.001 )  GO TO  557                                
      SHREV(I)=SHREV(I)*NCARV(I)/(3600.0*LV(I))                         
      LHREV(I)=LHREV(I)*NCARV(I)/(3600.0*LV(I))                         
C*****TO CALCULATE HEAT REJECTION RATE IN BTU PER SECOND PER PATRON PER 
C*****FOOT OF TRAIN LENGTH                                              
      SHRPV(I)=SHRPV(I)/(3600.0*LV(I))                                  
      LHRPV(I)=LHRPV(I)/(3600.0*LV(I))                                  
      PAUXEV(I)=PAUXEV(I)*NCARV(I)*1000.                                
      PAUXPV(I) = PAUXPV(I) * 1000.0                                    
C                                                                       
C**** RESISTOR GRID DATA                                                
C-----   INPUT FORM 9D                                                  
  557 WRITE(OUT,248)                                                    
      READ (IN,120) DUMY1,DUMY2,DIACGV(I),DIDCGV(I),ACACCV(I),ACDECV(I),
     1  ARACCV(I), ARDECV(I)                                            
      WRITE (OUT,250) DUMY1,DUMY2                                       
      CALL CHECKR (DUMY1,0.0,2000.,101)                                 
      CALL CHECKR (DUMY2,0.0,2000.,102)                                 
      WRITE (OUT,260) DIACGV(I),DIDCGV(I)                               
      CALL CHECKR (DIACGV(I),0.0,24.,103)                               
      CALL CHECKR (DIDCGV(I),0.0,24.,104)                               
      DIACGV(I)=DIACGV(I)/FTIN                                          
      DIDCGV(I)=DIDCGV(I)/FTIN                                          
      WRITE (OUT,270) ACACCV(I),ACDECV(I)                               
      CALL CHECKR (ACACCV(I),0.0,500.,105)                              
      CALL CHECKR (ACDECV(I),0.0,500.,106)                              
      WRITE (OUT,280) ARACCV(I),ARDECV(I)                               
      CALL CHECKR (ARACCV(I),0.0,500.,107)                              
      CALL CHECKR (ARDECV(I),0.0,500.,108)                              
      READ (IN,120) EMISAG(I),EMISDG(I),DUMY3,DUMY4, TIACCV(I),TIDECV(I)
      WRITE (OUT,290) EMISAG(I),EMISDG(I)                               
      CALL CHECKR (EMISAG(I),0.0,1.0,109)                               
      CALL CHECKR (EMISDG(I),0.0,1.0,110)                               
      WRITE (OUT,300) DUMY3,DUMY4                                       
      CALL CHECKR (DUMY3,0.0,1.0,111)                                   
      CALL CHECKR (DUMY4,0.0,1.0,112)                                   
      IF( ABS(TIACCV(I)) - 0.1 ) 553,554,554                            
  553 TIACCV(I) = TDBAMB                                                
  554 IF( ABS(TIDECV(I)) - 0.1 ) 555,556,556                            
  555 TIDECV(I) = TDBAMB                                                
  556 WRITE(OUT,302)  TIACCV(I),TIDECV(I)                               
      MCPGAV(I)=DUMY1*DUMY3                                             
      MCPGDV(I)=DUMY2*DUMY4                                             
      IF (DUMY1-0.01) 560,560,570                                       
560   MCPGAV(I)=0.0                                                     
      WRITE (OUT,310)                                                   
570   IF (DUMY2-0.01) 580,580,590                                       
580   MCPGDV(I)=0.0                                                     
      WRITE (OUT,320)                                                   
590   CONTINUE                                                          
C                                                                       
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,140) I,TITLE                                          
C                                                                       
C-----   INPUT FORM 9E                                                  
      READ (IN,120) WV(I),MOTORV(I),CORMV(I,1),CORMV(I,2),              
     1   CORMV(I,3),DUMY4                                               
      WRITE (OUT,325) WV(I)                                             
      CALL CHECKR (WV(I),5.0,150.0,56)                                  
      WRITE (OUT,340) MOTORV(I)                                         
      CALL CHECKR (MOTORV(I), 1.0, 10.0, 72)                            
      MOTORV(I)=MOTORV(I)*NPCARV(I)                                     
      WRITE(OUT,345) CORMV(I,1),CORMV(I,2),CORMV(I,3)                   
      CALL CHECKR( CORMV(I,1), 0.5,50.,88)                              
      CALL CHECKR( CORMV(I,3) ,0.001,1.000,89)                          
      CALL CHECKR( CORMV(I,2) ,0.1,500.0,99)                            
      CORMV(I,1)=CORMV(I,1)/TONLB                                       
      CORMV(I,2) = CORMV(I,2) * NCARV(I)                                
      CORMV(I,3)=CORMV(I,3)*FPSMPH/TONLB                                
      WV(I)=WV(I)*TONLB*NCARV(I)                                        
      IF (DUMY4) 620,620,610                                            
610   RRACC(I)=DUMY4                                                    
      GO TO 630                                                         
620   RRACC(I)=RRACC1                                                   
630   WRITE (OUT,350) RRACC(I)                                          
      CALL CHECKR (RRACC(I),0.1,30.0,77)                                
      RRACC(I) = ( 91.2 + RRACC(I) ) / ( 91.2 * GRACC )                 
C                                                                       
C**** ASSIGN INITIAL VALUES TO A NUMBER OF TRAIN VARIABLES              
C                                                                       
C**** ASSIGN VALUE FOR REGENERATIVE BRAKING EFFICIENCY USING THE        
C     INTERNAL DEFINITION, (1.0 - REGENV(I)) = 1.0  (NO REGENERATION)   
      REGENV(I) = 1.0                                                   
C**** ASSIGN VALUES FOR CHOPPER EFFICIENCY USING INTERNAL DEFINITION,   
C     I.E.  (1.0 - EFFICIENCY) = LOSS                                   
      CHPR1V(I) = 0.0                                                   
      CHPR2V(I) = 0.0                                                   
      UCHPRV(I) = 150.0                                                 
C**** ASSIGN VALUE TO FLYWHEEL PARAMETERS                               
      RPSOV(I) = 0.0                                                    
      SPINV(I) = 0.0                                                    
      FLYJV(I) = 0.0                                                    
      FLYOPV(I) = 1.0                                                   
      GO TO (631,631,900), TPOPT                                        
C                                                                       
C     TRAIN PERFORMANCE OPTION ( TPOPT ) = 1 AND 2                      
C                                                                       
C-----   INPUT FORM 9F                                                  
631   READ (IN,360) TITLE1,DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,VOLTMV(I)
      WRITE (OUT,370) TITLE1                                            
      WRITE (OUT,380) DUMY1,DUMY2                                       
      CALL CHECKR (DUMY1,20.0,40.0,59)                                  
      CALL CHECKR (DUMY2,20.0,40.0,59)                                  
      WRITE (OUT,390) DUMY3,DUMY4                                       
      CALL CHECKR (DUMY3,1.0,20.0,60)                                   
      CALL CHECKR (DUMY4,1.0,20.0,60)                                   
      WRITE (OUT,400) DUMY5,DUMY6                                       
      CALL CHECKR (DUMY5,100.,1500.,117)                                
      CALL CHECKR (DUMY6,100.,1500.,117)                                
      VOLTSV(I) = DUMY6                                                 
      WRITE (OUT,401) VOLTMV(I)                                         
      CALL CHECKR (VOLTMV(I),100.0,1000.0,238)                          
C                                                                       
C-----  INPUT FORM 9G                                                   
      READ (IN,120) DUMMY1                                              
      READ (IN,120) DUMMY2                                              
      READ (IN,120) DUMMY3                                              
      READ (IN,120) DUMY7                                               
      WRITE (OUT,410)                                                   
      WRITE (OUT,420) DUMMY1                                            
      IF (DUMMY1(1) .LE. 0.0) CALL EERROR(254)                           
      DO 636 J=1,3                                                      
      IF ((DUMMY1(J+1)-DUMMY1(J)) .LE. 0.0) CALL EERROR(254)             
636   CONTINUE                                                          
      WRITE (OUT,430) DUMMY2                                            
      WRITE (OUT,440) DUMMY3                                            
      NUMY1=DUMY7                                                       
      NOPTV(I)=NUMY1                                                    
      CALL CHECKI (NUMY1,1,2,209)                                       
      IF (NUMY1-2) 632,633,633                                          
632   WRITE (OUT,443)NUMY1
      GO TO 634                                                         
633   WRITE (OUT,444)NUMY1                                         
C **  DUMY1 = (ACTUAL WHEEL D / MOTOR DATA D ) *                        
C               ( MOTOR DATA GEAR RATIO / ACTUAL GEAR RATIO )           
C     VOLTAGE CORRECTION  = ACTUAL VOLTAGE / MOTOR DATA VOLTAGE         
C                         = DUMY6/DUMY5                                 
634   DUMY1=DUMY2*DUMY3/(DUMY1*DUMY4)                                   
C **  ADJUST SPEED AND TRACTIVE EFFORT DATA                             
      DO 640 J=1,4                                                      
      DUMMY1(J)=DUMMY1(J)*DUMY1*MPHFPS*(DUMY6/DUMY5)                    
640   DUMMY2(J)=DUMMY2(J)/DUMY1                                         
      SP1(I)=DUMMY1(1)                                                  
      SP2(I)=DUMMY1(2)                                                  
      TESP1(I)=DUMMY2(1)                                                
C**** CURVE FIT TRACTIVE EFFORT VS SPEED ( IN FPS )  ( FOR SUB. TORQUE )
C *** TE VS SPEED - PART 2                                              
C     CURVE 2 EQUATION    TE = A + B/UTRN                               
C                                                                       
C     CURVE 2 COEFF A = TE1 -(TE1-TE2)*U2/(U2-U1)                       
C     CURVE 2 COEFF B = (TE1-TE2)*U1*U2/(U2-U1)                         
C                                                                       
C---- CHECK IF SPEED DATA POINTS ARE SUFFICIENTLY SPACED                
      IF( ( DUMMY1(2)-DUMMY1(1) ) - 0.1 ) 642,642,645                   
C---- SPEED DATA POINTS ARE TOO CLOSE - USE STRAIGHT LINE               
  642 COATEV(I) = DUMMY2(1)                                             
      COBTEV(I) = 0.0                                                   
      GO TO 647                                                         
C---- COMPUTE CURVE COEFFICIENTS A AND B                                
  645 COATEV(I) = DUMMY2(1) - (DUMMY2(1)-DUMMY2(2))*DUMMY1(2)/          
     1            ( DUMMY1(2) - DUMMY1(1) )                             
      COBTEV(I) = (DUMMY2(1)-DUMMY2(2)) * DUMMY1(1)*DUMMY1(2) /         
     1            ( DUMMY1(2) - DUMMY1(1) )                             
  647 CONTINUE                                                          
C *** TE VS SPEED - PART 3                                              
C     CURVE 3 EQUATION  TE=COTEV(I,1)+COTEV(I,2)/UTRN+COTEV(I,3)/UTRN**2
      DO 650 J=1,3                                                      
      DUMMY4(J)=1.0                                                     
      DUMMY4(J+3)=1.0/DUMMY1(J+1)                                       
      DUMMY4(J+6)=1.0/DUMMY1(J+1)**2                                    
650   DUMMY5(J)=DUMMY2(J+1)                                             
      NOGO=0                                                            
      ITER = 1                                                          
      CALL SIMQ1(DUMMY4,DUMMY5,DUMMY7,3,9,IPIVOT,ROWSIZ,ITER,NOGO)      
      IF (1-NOGO) 660,660,670                                           
660   CALL EERROR (86)                                                   
      COTEV(I,1)=0.0                                                    
      COTEV(I,2)=0.0                                                    
      COTEV(I,3)=0.0                                                    
      GO TO 680                                                         
670   COTEV(I,1) = DUMMY7(1)                                            
      COTEV(I,2) = DUMMY7(2)                                            
      COTEV(I,3) = DUMMY7(3)                                            
C**** CURVE FIT MOTOR CURRENT VS TRAIN SPEED (SUB. AMPERE)              
C     CURVE EQN. - IMOTOR=COAMP(I,1)+COAMP(I,2)/UTRN+COAMP(I,3)/UTRN**2 
680   DO 690 J=1,3                                                      
      DUMMY4(J)=1.0                                                     
      DUMMY4(J+3)=1.0/DUMMY1(J+1)                                       
      DUMMY4(J+6)=1.0/DUMMY1(J+1)**2                                    
690   DUMMY5(J)=DUMMY3(J+1)                                             
      NOGO=0                                                            
      CALL SIMQ1(DUMMY4,DUMMY5,DUMMY7,3,9,IPIVOT,ROWSIZ,ITER,NOGO)      
      IF (1-NOGO) 700,700,705                                           
700   CALL EERROR (87)                                                   
      COAMP(I,1)=0.0                                                    
      COAMP(I,2)=0.0                                                    
      COAMP(I,3)=0.0                                                    
      COAMP(I,4)=0.0                                                    
      GO TO 706                                                         
705   COAMP(I,1) = DUMMY7(1)                                            
      COAMP(I,2) = DUMMY7(2)                                            
      COAMP(I,3) = DUMMY7(3)                                            
      COAMP(I,4)=DUMMY3(1)                                              
C                                                                       
  706 IF( NOPTV(I) - 2 ) 720,708,708                                    
C                                                                       
C**** TRAIN SPEED CONTROL OPTION ( NOPTV(I) ) = 2.0                     
C                                                                       
708   WRITE (OUT,10) CHAR(12)
      WRITE (OUT,140) I,TITLE                                     
C                                                                       
C-----   INPUT FORM 9H                                                  
      READ (IN,120) DUMMY6                                              
      READ (IN,120) CHPR1V(I),UCHPRV(I),CHPR2V(I),REGENV(I),FLYOPV(I)   
      WRITE (OUT,395)                                                   
      WRITE (OUT,441) DUMMY6                                            
      WRITE (OUT,445) CHPR1V(I)                                         
      CALL CHECKR (CHPR1V(I),0.0,100.0,151)                             
C**** EXPRESS IN TERMS OF LOSS, I.E. ( 1.0 - EFFICIENCY ) = LOSS        
      CHPR1V(I) = (1.0 - CHPR1V(I)/100.0)                               
      WRITE (OUT,446) UCHPRV(I)                                         
      UCHPRV(I) = UCHPRV(I)*MPHFPS                                      
      CALL CHECKR (UCHPRV(I),0.0,100.0,210)                             
      WRITE (OUT,447) CHPR2V(I)                                         
      CALL CHECKR (CHPR2V(I),0.0,100.0,151)                             
      CHPR2V(I) = (1.0 - CHPR2V(I)/100.0)                               
      WRITE (OUT,236) REGENV(I)                                         
      CALL CHECKR (REGENV(I),0.0,100.0,151)                             
      REGENV(I) = 1.0 - REGENV(I)/100.0                                 
      NUMY1=FLYOPV(I)                                                   
      IF (NUMY1-2) 710,709,709                                          
C**** FLYWHEELS ARE BEING SIMULATED                                     
709   WRITE (OUT,532)NUMY1
C**** CHECK TPOPT, SINCE FLYWHEELS ARE AVAILABLE WITH IMPLICIT TRAIN    
C     PERFORMANCE ONLY (TPOPT = 1 )                                     
      CALL CHECKI (TPOPT,0,1,231)                                       
      GO TO 711                                                         
C**** FLYWHEEL OPTION IS BYPASSED.                                      
710   WRITE (OUT,533)NUMY1                                                   
  711 CALL CHECKI (NUMY1,1,2,230)                                       
C**** CURVE FIT LINE CURRENT (AMPS/CAR) VS. TRAIN SPEED (FPS)           
      DO 712 J=1,3                                                      
      DUMMY4(J)=1.0                                                     
      DUMMY4(J+3)=1.0/DUMMY1(J+1)                                       
      DUMMY4(J+6)=1.0/DUMMY1(J+1)**2                                    
712   DUMMY5(J)=DUMMY6(J+2)                                             
      NOGO = 0                                                          
      CALL SIMQ1(DUMMY4,DUMMY5,DUMMY7,3,9,IPIVOT,ROWSIZ,ITER,NOGO)      
      IF ( 1 - NOGO ) 713,713,714                                       
713   CALL EERROR ( 208 )                                                
      COAMPL(I,1) = 0.0                                                 
      COAMPL(I,2) = 0.0                                                 
      COAMPL(I,3) = 0.0                                                 
      COAMPL(I,4) = 0.0                                                 
      COAMPL(I,5) = 0.0                                                 
      COAMPL(I,6) = 0.0                                                 
      GO TO 715                                                         
714   COAMPL(I,1) = DUMMY7(1)                                           
      COAMPL(I,2) = DUMMY7(2)                                           
      COAMPL(I,3) = DUMMY7(3)                                           
      COAMPL(I,4) = DUMMY6(3)                                           
      COAMPL(I,5) = DUMMY1(1)                                           
      COAMPL(I,6) = DUMMY6(1)                                           
      SLOPEV(I) = (DUMMY6(2) - DUMMY6(1))/DUMMY1(1)                     
C**** PRINT VERIFICATION OF SPEED - T.E. - MOTOR CURRENT - LINE CURRENT 
C       RELATIONSHIP.                                                   
715   WRITE (OUT,451)                                                   
      J1 = DUMMY1(4)/10.0 * FPSMPH + 4                                  
      DO 716 J = 1,J1                                                   
      DUMMY1(1) = FLOAT(IABS(J-1)) * 5.0                                
      DUMMY2(1) = TORQUE( DUMMY1(1)*MPHFPS,I )                          
      CALL AMPERE(DUMMY2(1),DUMMY2(1),DUMMY1(1)*MPHFPS,I)               
      DUMMY3(1)=IMOTOR                                                  
      DUMMY4(1)=ILINE                                                   
      DUMMY1(2)=DUMMY1(1)+FLOAT(J1)*5.0                                 
      DUMMY2(2)=TORQUE(DUMMY1(2)*MPHFPS,I)                              
      CALL AMPERE(DUMMY2(2),DUMMY2(2),DUMMY1(2)*MPHFPS,I)               
      DUMMY3(2)=IMOTOR                                                  
      DUMMY4(2)=ILINE                                                   
C                                                                       
716   WRITE (OUT,452) (DUMMY1(JA),DUMMY2(JA),DUMMY3(JA),DUMMY4(JA),     
     1 JA = 1,2 )                                                       
      GO TO 731                                                         
C**** PRINT VERIFICATION OF SPEED - T.E. - MOTOR CURRENT RELATIONSHIP   
720   WRITE (OUT,450)                                                   
      J1=DUMMY1(4)/10.0*FPSMPH+4                                        
      DO 730 J=1,J1                                                     
      DUMMY1(1)=FLOAT(IABS(J-1))*5.0                                    
      DUMMY2(1)=TORQUE(DUMMY1(1)*MPHFPS,I)                              
      CALL AMPERE(DUMMY2(1),DUMMY2(1),DUMMY1(1)*MPHFPS,I)               
      DUMMY3(1)=IMOTOR                                                  
      DUMMY1(2)=DUMMY1(1)+FLOAT(J1)*5.0                                 
      DUMMY2(2)=TORQUE(DUMMY1(2)*MPHFPS,I)                              
      CALL AMPERE(DUMMY2(2),DUMMY2(2),DUMMY1(2)*MPHFPS,I)               
      DUMMY3(2)=IMOTOR                                                  
730   WRITE (OUT,460) (DUMMY1(JA),DUMMY2(JA),DUMMY3(JA),JA=1,2)         
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,140) I,TITLE
C                                                                       
C-----   INPUT FORM 9I                                                  
731   READ (IN,120) U2M(I),U3M(I),RE1M(I),RE2M(I),RE3M(I)               
C**** CHECK FOR PRESENCE OF EXTERNAL RESISTANCES                        
      IF ( RE1M(I) + RE2M(I) ) 735,732,735                              
C                                                                       
C**** NO EXTERNAL RESISTANCES                                           
732   IF( NOPTV(I) - 1 ) 735,735,733                                    
733   WRITE (OUT,461) RE3M(I)                                           
      CALL CHECKR ( RE3M(I),0.001,3.0,61 )                              
      SRE1M(I) = 0.0                                                    
      SRE2M(I) = 0.0                                                    
      U2M(I) = 0.0                                                      
      U3M(I) = 0.0                                                      
      GO TO 741                                                         
C**** EXTERNAL RESISTANCES ARE PRESENT                                  
735   WRITE (OUT,470)                                                   
      WRITE (OUT,480) U2M(I),U3M(I)                                     
      CALL CHECKR ( U2M(I),0.0,100.0,62 )                               
      CALL CHECKR ( U3M(I),0.0,100.0,62 )                               
      U2M(I) = U2M(I) * DUMY1 *MPHFPS                                   
      U3M(I) = U3M(I) * DUMY1 * MPHFPS                                  
      IF ( U3M(I) - U2M(I) ) 736,736,740                                
736   CALL EERROR(62)                                                    
740   WRITE (OUT,475) RE1M(I),RE2M(I),RE3M(I)                           
      CALL CHECKR ( RE1M(I),0.001,3.0,61 )                              
      CALL CHECKR ( RE2M(I),0.001,3.0,61 )                              
      CALL CHECKR ( RE3M(I),0.001,3.0,61 )                              
      SRE1M(I) = ( RE1M(I) - RE3M(I) ) / ( -U2M(I) )                    
      SRE2M(I) = ( RE2M(I) - RE3M(I) ) / ( U2M(I) - U3M(I) )            
741   CONTINUE                                                          
C                                                                       
      GO TO (760,900,900), TPOPT                                        
C                                                                       
C     TRAIN PERFORMANCE OPTION ( TPOPT ) = 1                            
C                                                                       
C----   INPUT FORM 9J                                                   
760   READ (IN,120) ACCV(I),DECV(I),DECV1V(I),DUMY1,DUMY2               
      WRITE (OUT,490) ACCV(I)                                           
      CALL CHECKR (ACCV(I),0.50,5.0,74)                                 
      ACCV(I) = ACCV(I) * MPHFPS                                        
      WRITE (OUT,500) DECV(I)                                           
      CALL CHECKR (DECV(I),0.50,5.0,54)                                 
      WRITE (OUT,510) DECV1V(I)                                         
      CALL CHECKR (DECV1V(I),10.0,100.0,118)                            
      WRITE (OUT,520) DUMY1                                             
      CALL CHECKR (DUMY1,0.50,5.0,54)                                   
      WRITE (OUT,530) DUMY2                                             
      CALL CHECKR (DUMY2,DECV1V(I),250.0,119)                           
C                                                                       
C     B + A*V1 = ACC1          B+A*DECV1V(I) = DECV(I)                  
C     B + A*V2 = ACC2          B+A*DUMY2     = DUMY1                    
C                                                                       
C*****IF DECELERATION RATE IS CONSTANT FOR ANY SPEED SET V1 AND V2 TO   
C*****HIGH VELOCITIES TO AVOID SINGULARITIES IN SUBROUTINE TRAIN        
C*****CALCULATIONS                                                      
      IF(ABS(DUMY1-DECV(I))-0.02) 768,769,769                           
  768 DECV1V(I)=150.0                                                   
      DUMY2=155.0                                                       
  769 DUMY1 = DUMY1 * MPHFPS                                            
      DUMY2 = DUMY2 * MPHFPS                                            
      DECV1V(I) = DECV1V(I) * MPHFPS                                    
      DECV(I) = DECV(I) * MPHFPS                                        
      DECAV(I)=(DUMY1-DECV(I))/(DUMY2-DECV1V(I))                        
      DECBV(I)=(DUMY1*DECV1V(I)-DECV(I)*DUMY2)/(DECV1V(I)-DUMY2)        
770   NUMY1=NOPTV(I)                                                    
      GO TO (900,771), NUMY1                                            
771   NUMY1=FLYOPV(I)                                                   
      GO TO (900,772), NUMY1                                            
772   WRITE (OUT,10) CHAR(12)
      WRITE (OUT,140) I,TITLE
C                                                                       
C----    INPUT FORM 9K                                                  
      READ (IN,120) FLYJV(I),WEELSV(I),RPSMNV(I),RPSMXV(I),RPSOV(I)     
      WRITE (OUT,534) FLYJV(I)                                          
      CALL CHECKR (FLYJV(I),50.0,4000.0,232)                            
      NUMY1=WEELSV(I)                                                   
      WRITE (OUT,535) NUMY1                                             
C**** CALCULATE THE EFFECTIVE MOMENT OF INERTIA OF ALL THE FLYWHEELS    
C     ON-BOARD THE TRAIN                                                
      FLYJV(I)=FLYJV(I)* NCARV(I)*WEELSV(I)                             
      CALL CHECKI (NUMY1,1,4,233)                                       
      WRITE (OUT,536) RPSMNV(I)                                         
      CALL CHECKR (RPSMNV(I),0.0,20000.0,234)                           
      WRITE (OUT,537) RPSMXV(I)                                         
      CALL CHECKR (RPSMXV(I),RPSMNV(I),30000.0,235)                     
      WRITE (OUT,538) RPSOV(I)                                          
      CALL CHECKR  (RPSOV(I),RPSMNV(I),RPSMXV(I),236)                   
C**** CHANGE THE UNITS OF THE FLYWHEEL ROTATIONAL SPEEDS FROM RPM TO RPS
      RPSMNV(I)=RPSMNV(I)/60.0                                          
      RPSMXV(I)=RPSMXV(I)/60.0                                          
      RPSOV(I)=RPSOV(I)/60.0                                            
C**** CALCULATE THE INITIAL KINETIC ENERGY OF THE FLYWHEEL              
      SPINV(I) = (2.0*PI*RPSOV(I))**2*FLYJV(I)/(2.0 * GRACC)            
C                                                                       
C----    INPUT FORM 9L                                                  
      READ (IN,121) ETA1V(I),ETA2V(I),ALOSSV(I),BLOSSV(I),CLOSSV(I),    
     1DLOSSV(I),ELOSSV(I)                                               
      WRITE (OUT,539) ETA1V(I)                                          
      CALL CHECKR (ETA1V(I),0.0,100.0,237)                              
      WRITE (OUT,540) ETA2V(I)                                          
      CALL CHECKR (ETA2V(I),0.0,100.0,237)                              
      WRITE (OUT,541) ALOSSV(I),BLOSSV(I),CLOSSV(I)                     
      IF (ALOSSV(I)) 780,781,781                                        
780   CALL EERROR (239)                                                  
781   IF (BLOSSV(I)) 782,783,783                                        
782   CALL EERROR (239)                                                  
783   IF (CLOSSV(I)) 784,785,785                                        
784   CALL EERROR (239)                                                  
785   WRITE (OUT,542) DLOSSV(I)                                         
      CALL CHECKR (DLOSSV(I),0.0,3.0,240)                               
      WRITE (OUT,543) ELOSSV(I)                                         
      CALL CHECKR (ELOSSV(I),0.0,3.0,240)                               
C**** CHANGE POWER CONVERSION EFFICIENCIES FROM PERCENT TO DECIMAL      
      ETA1V(I) = ETA1V(I)/100.0                                         
      ETA2V(I) = ETA2V(I)/100.0                                         
C                                                                       
900   CONTINUE                                                          
      RETURN                                                            
      END                                                               
