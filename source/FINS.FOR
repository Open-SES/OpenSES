      SUBROUTINE FINS( IFT )                                            
C                                                                       
C*****THIS SUBROUTINE READS AND CHECKS THE INPUT DATA PECULIAR TO EACH  
C*****FAN TYPE AND COMPUTES THE COEFFICIENTS OF THE CUBIC POLYNOMIALS   
C*****USED TO APPROXIMATE THE FAN PERFORMANCE CURVES FOR BOTH THE       
C*****OUTFLOW AND INFLOW OPERATING DIRECTIONS.                          
C*****                                                                  
C*****-9.21034037 EQUALS THE NATURAL LOG OF 0.0001                      
C*****                                                                  
C*****                                                                  
C*****ROWTFT IS THE AIR DENSITY GIVEN WITH THE FAN PERFORMANCE          
C*****CURVE DATA (LBS/CUFT)                                             
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'
C                                                                       
C                                                                       
      DIMENSION NUMMY1(9), DUMMY1(8), DUMMY2(8), DUMMY3(16), DUMMY4(4)  
      DIMENSION  DUMMY5(4), IPIVOT(4), ROWSIZ(4)                        
1     FORMAT(A1)
10    FORMAT (A36,4X,4F10.0)                                            
20    FORMAT ('INPUT VERIFICATION FOR FAN TYPE',I2,15X,A36,T112,
     1        'FORM 7A',///)                                                        
30    FORMAT ('BI-DIRECTIONAL FAN PERFORMANCE CHARACTERISTICS')    
40    FORMAT (/,8X,'AIR DENSITY AT WHICH THE FAN PERFORMANCE CURVE WAS M
     1EASURED',T79,F14.5,T96,'LBS/CUFT')                              
50    FORMAT (/,8X,'TIME REQUIRED FOR FAN TO REACH FULL OPERATING SPEED' 
     1,T79,F14.3,T96,'SECONDS')                                       
60    FORMAT (/,8X,'FAN LOWER FLOW LIMIT (POINT OF MOTOR BREAKDOWN TORQU
     1E OR STOPPING)',T79,I14,T96,'CFM')                              
70    FORMAT (/,8X,'FAN UPPER FLOW LIMIT (POINT OF WINDMILLING)',     
     1T79,I14,T96,'CFM',/)                                              
80    FORMAT (8F10.0)                                                   
90    FORMAT (16X,'BI-DIRECTIONAL (OUTFLOW(EXHAUST) AND INFLOW(SUPPLY)) 
     1FAN PERFORMANCE CURVE INFORMATION',T112,'FORM 7B')            
100   FORMAT (34X,'OUTFLOW(EXHAUST) FAN PERFORMANCE CURVE INFORMATION'
     1,T112,'FORM 7B')                                                
110   FORMAT (35X,'INFLOW(SUPPLY) FAN PERFORMANCE CURVE INFORMATION',
     1        T112,'FORM 7B')                                              
120   FORMAT ('FAN PERFORMANCE CURVE DATA POINTS',/)               
130   FORMAT ('FITTED FAN PERFORMANCE CURVE',//)                   
140   FORMAT (8X,'FAN TOTAL PRESSURE ',4F14.2,12X,'IN. WG',/,13X,'VS')                 
150   FORMAT (8X,'VOLUME FLOWRATE',4X,4I14,13X,'CFM'/)                       
160   FORMAT (8X,'FAN TOTAL PRESSURE ',7F9.2,5X,'IN. WG',/,13X,'VS')                 
170   FORMAT (8X,'VOLUME FLOWRATE',4X,7I9,6X,'CFM'/)                       
200   FORMAT (T46,'FAN PERFORMANCE CURVE FIT COEFFICIENTS'/T46,38('-'),
     1///T5,'THE COEFFICIENTS C1, C2, C3, C4 FIT THE FAN PERFORMANCE CUR
     2VE OF THE FOLLOWING EQUATION -'//T11,'C1 * (Q**3) + C2 * (Q**2) +
     3C3 * (Q) + C4 = P'//T5,'WHERE Q IS THE AIRFLOW RATE (CFS) OF INFLO
     4W (EXHAUST) OR OUTFLOW (SUPPLY)'//T11,'P IS THE PRESSURE HEAD IN F
     5T**2/SEC**2'///T41,'C1',18X,'C2',18X,'C3',18X,'C4')             
210   FORMAT (/,T5,'OUTFLOW (EXHAUST)',T29,4E20.7/T5,17('-'))         
220   FORMAT (/,T5,'INFLOW (SUPPLY)  ',T29,4E20.7/T5,15('-'))         
C
C*****SET FAN COEFFICIENTS EQUAL TO ZERO                                
      FN1OFT(IFT)=0.0                                                   
      FN2OFT(IFT)=0.0                                                   
      FN3OFT(IFT)=0.0                                                   
      FN4OFT(IFT)=0.0                                                   
      FN1IFT(IFT)=0.0                                                   
      FN2IFT(IFT)=0.0                                                   
      FN3IFT(IFT)=0.0                                                   
      FN4IFT(IFT)=0.0                                                   
C*****READ AND CHECK FAN NAME, GIVEN AIR DENSITY, RUN UP TIME, AND      
C*****OPERATIONAL CFM LIMITS                                            
      READ (IN,10) TITLE,ROWTFT,TIMFT(IFT),CFMLFT(IFT),CFMHFT(IFT)     
      WRITE (OUT,20) IFT,TITLE                                         
      WRITE (OUT,30)                                                    
      WRITE (OUT,40) ROWTFT                                             
      CALL CHECKR (ROWTFT,0.040,0.085,187)                              
      ROWTFT = ROWTFT / GRACC                                           
      DUMY1=TIMFT(IFT)                                                  
      WRITE (OUT,50) DUMY1                                              
      CALL CHECKR (DUMY1,0.0,300.0,176)                                 
C*****CHECK FOR FAN OF ZERO RUNUP TIME. IF ZERO RUNUP TIME, SET         
C*****TIMFT EQUAL TO MINUS INFINITY, OTHERWISE SET IT SO THE FAN        
C*****REACHES 0.9999 OPERATING PRESSURE AT THE END OF RUNUP TIME        
      IF (DUMY1-0.005) 230,230,240                                      
230   TIMFT(IFT)=-1.0E+09                                               
      GO TO 245                                                         
240   TIMFT(IFT)=-9.21034037/((DUMY1*100.0)**2)                         
245   NUMMY1(1)=CFMLFT(IFT)                                             
      WRITE (OUT,60) NUMMY1(1)                                          
      CALL CHECKI (NUMMY1(1),-100000,0,193)                             
      CFMLFT(IFT)=CFMLFT(IFT)/60.0                                      
      NUMMY1(2)=CFMHFT(IFT)                                             
      WRITE (OUT,70) NUMMY1(2)                                          
      CALL CHECKI (NUMMY1(2),1000,2000000,194)                          
      CFMHFT(IFT)=CFMHFT(IFT)/60.0                                      
C*****COMPUTE INTERVAL IN CFM BETWEEN POINTS OF FITTED FAN              
C*****CURVE VERIFICATION                                                
      IINT=5000                                                          
      IF (NUMMY1(2)-NUMMY1(1)-250000) 270,270,250                       
250   IINT=10000                                                         
      IF (NUMMY1(2)-NUMMY1(1)-1000000) 270,270,260                      
260   IINT=50000                                                         
270   NLOW=(NUMMY1(1)-IINT)/IINT                                          
      NHIGH=NLOW+500                                                    
C*****READ IN PRESSURE VS VOLUME CURVE FOR FAN                          
      READ (IN,80) (DUMMY1(J),DUMMY2(J),J=1,8)                          
      SUM1=0.0                                                          
      SUM2=0.0                                                          
C*****COMPUTE CFM TOTALS FOR EACH DIRECTION OF OPERATION                
      DO 280 J=1,4                                                      
      SUM1=SUM1+DUMMY2(J)                                               
280   SUM2=SUM2+DUMMY2(J+4)                                             
C*****CHECK FOR BOTH CFM TOTALS BEING ZERO OR LESS                      
      IF (SUM1+SUM2) 290,290,300                                        
290   CALL EERROR( 163 )                                                 
      GO TO 660                                                         
C*****CHECK IF FAN DEFINITELY HAS THE SAME PERFORMANCE CURVE IN BOTH    
C*****DIRECTIONS                                                        
300   IF (SUM1*SUM2) 290,310,370                                        
C*****CHECK IF DATA POINTS WERE ENTERED FOR BOTH OUTFLOW(EXHAUST) OR    
C*****INFLOW(SUPPLY)                                                    
310   IF (SUM1) 290,320,340                                             
C*****COPY INFLOW DATA POINTS INTO OUTFLOW DATA POINTS                  
320   DO 330 J=1,4                                                      
      DUMMY1(J)=DUMMY1(J+4)                                             
330   DUMMY2(J)=DUMMY2(J+4)                                             
      GO TO 360                                                         
C*****COPY OUTFLOW DATA POINTS INTO INFLOW DATA POINTS                  
340   DO 350 J=1,4                                                      
      DUMMY1(J+4)=DUMMY1(J)                                             
350   DUMMY2(J+4)=DUMMY2(J)                                             
360   WRITE (OUT,90)                                                    
      GO TO 380                                                         
370   WRITE (OUT,100)                                                   
C*****BEGIN OUTFLOW FAN PERFORMANCE CURVE FIT                           
380   WRITE (OUT,120)                                                   
C*****PRINTOUT TOTAL PRESSURE RISES                                     
      WRITE (OUT,140) (DUMMY1(J),J=1,4)                                 
C*****CHECK TOTAL PRESSURE RISES AND CONVERT THEM TO PROGRAM INTERNAL   
C*****UNITS                                                             
      DO 390 J=1,4                                                      
      CALL CHECKR (DUMMY1(J),-15.0,50.0,68)                             
390   DUMMY4(J)=WGCONS*DUMMY1(J)/ROWTFT                                 
C*****PRINTOUT VOLUME FLOWRATES                                         
      DO 400 J=1,4                                                      
400   NUMMY1(J)=DUMMY2(J)                                               
      WRITE (OUT,150) (NUMMY1(J),J=1,4)                                 
C*****CHECK VOLUME FLOWRATES AND CONVERT THEM TO PROGRAM INTERNAL UNITS 
      DO 410 J=1,4                                                      
      CALL CHECKR (DUMMY2(J),0.0,2000000.0,69)                          
410   DUMMY2(J)=DUMMY2(J)/60.0                                          
      WRITE (OUT,130)                                                   
C*****COMPUTE MATRIX COEFFICIENTS - VOLUME FLOWRATES RAISED TO POWERS   
C*****ZERO THROUGH THREE                                                
      DO 420 J=1,4                                                      
      DUMMY3(J)=DUMMY2(J)**3                                            
      DUMMY3(J+4)=DUMMY2(J)**2                                          
      DUMMY3(J+8)=DUMMY2(J)                                             
420   DUMMY3(J+12)=1.0                                                  
      NOGO=0                                                            
      ITER = 1                                                          
C*****SOLVE SYSTEM OF FOUR EQUATIONS-FOUR UNKNOWNS FOR THE COEFFICIENTS 
C*****OF THE FITTED CUBIC POLYNOMIAL                                    
      CALL SIMQ1(DUMMY3,DUMMY4,DUMMY5,4,16,IPIVOT,ROWSIZ,ITER,NOGO)     
C*****CHECK TO SEE IF SOLUTION CAME OFF ALL RIGHT OR WAS WRONG DUE TO   
C*****THE SAME DATA POINT BEING ENTERED TWICE, ETC.                     
      IF (1-NOGO) 430,430,440                                           
430   CALL EERROR (75)                                                   
      GO TO 500                                                         
C*****STORE COEFFICIENTS OF THE FITTED FAN CURVE                        
440   FN1OFT(IFT) = DUMMY5(1)                                           
      FN2OFT(IFT) = DUMMY5(2)                                           
      FN3OFT(IFT) = DUMMY5(3)                                           
      FN4OFT(IFT) = DUMMY5(4)                                           
C*****PRINTOUT FITTED FAN CURVE WITH AIR DENSITY ADJUSTMENT             
      K=NLOW                                                            
450   DO 460 J=1,7                                                      
      NUMMY1(J)=IINT*K                                                   
      K=K+1                                                             
      DUMY1=FLOAT(NUMMY1(J))/60.0                                       
      DUMMY3(J)=DUMY1*(DUMY1*(DUMY1*FN1OFT(IFT)+FN2OFT(IFT))+FN3OFT(IFT)
     1)+FN4OFT(IFT)                                                     
460   DUMMY3(J)=RHOMAS*DUMMY3(J)/WGCONS
      WRITE (OUT,160) (DUMMY3(J),J=1,7)                                 
      WRITE (OUT,170) (NUMMY1(J),J=1,7)                                 
C*****CHECK FOR PRINTOUT OF MORE THAN 500 POINTS ON FITTED FAN CURVE    
      IF (NHIGH-K) 470,480,480                                          
470   CALL EERROR (85)                                                   
      GO TO 500                                                         
C*****TERMINATE PRINTING OF FITTED FAN CURVE IF                         
C*****FIRST  - THE FAN TOTAL PRESSURE RISE IS NEGATIVE                  
C*****SECOND - POINTS HAVE BEEN PRINTED THROUGHOUT THE ENTRIE FAN       
C*****         OPERATING RANGE                                          
480   IF (DUMMY3(7)) 490,450,450                                        
490   IF (DUMY1-CFMHFT(IFT)) 450,450,500                                
500   IF (SUM1*SUM2) 290,630,510                                        
510   WRITE (OUT,1) CHAR(12)
      WRITE (OUT,110)                                                   
C*****BEGIN INFLOW FAN PERFORMANCE CURVE FIT                            
      WRITE (OUT,120)                                                   
C*****PRINTOUT TOTAL PRESSURE RISES                                     
      WRITE (OUT,140) (DUMMY1(J),J=5,8)                                 
C*****CHECK TOTAL PRESSURE RISES AND CONVERT THEM TO PROGRAM INTERNAL   
C*****UNITS                                                             
      DO 520 J=5,8                                                      
      CALL CHECKR (DUMMY1(J),-15.0,50.0,68)                             
520   DUMMY4(J-4)=WGCONS*DUMMY1(J)/ROWTFT                               
C*****PRINTOUT VOLUME FLOWRATES                                         
      DO 530 J=5,8                                                      
530   NUMMY1(J)=DUMMY2(J)                                               
      WRITE (OUT,150) (NUMMY1(J),J=5,8)                                 
C*****CHECK VOLUME FLOWRATES AND CONVERT THEM TO PROGRAM INTERNAL UNITS 
      DO 540 J=5,8                                                      
      CALL CHECKR (DUMMY2(J),0.0,2000000.0,69)                          
540   DUMMY2(J)=DUMMY2(J)/60.0                                          
      WRITE (OUT,130)                                                   
C*****COMPUTE MATRIX COEFFICIENTS - VOLUME FLOWRATES RAISED TO POWERS   
C*****ZERO THROUGH THREE                                                
      DO 550 J=1,4                                                      
      DUMMY3(J)=DUMMY2(J+4)**3                                          
      DUMMY3(J+4)=DUMMY2(J+4)**2                                        
      DUMMY3(J+8)=DUMMY2(J+4)                                           
550   DUMMY3(J+12)=1.0                                                  
      NOGO=0                                                            
C*****SOLVE SYSTEM OF FOUR EQUATIONS-FOUR UNKNOWNS FOR THE COEFFICIENTS 
C*****OF THE FITTED CUBIC POLYNOMIAL                                    
      CALL SIMQ1(DUMMY3,DUMMY4,DUMMY5,4,16,IPIVOT,ROWSIZ,ITER,NOGO)     
C*****CHECK TO SEE IF SOLUTION CAME OFF ALL RIGHT OR WAS WRONG DUE TO   
C*****THE SAME DATA POINT BEING ENTERED TWICE, ETC.                     
      IF (1-NOGO) 560,560,570                                           
560   CALL EERROR (75)                                                   
      GO TO 640                                                         
C*****STORE COEFFICIENTS OF THE FITTED FAN CUREVE                       
570   FN1IFT(IFT) = DUMMY5(1)                                           
      FN2IFT(IFT) = -DUMMY5(2)                                          
      FN3IFT(IFT) = DUMMY5(3)                                           
      FN4IFT(IFT) = -DUMMY5(4)                                          
C*****PRINTOUT FITTED FAN CURVE WITH AIR DENSITY ADJUSTMENT             
      K=NLOW                                                            
580   DO 590 J=1,7                                                      
      NUMMY1(J)=IINT*K                                                   
      K=K+1                                                             
      DUMY1=-FLOAT(NUMMY1(J))/60.0                                      
      DUMMY3(J)=DUMY1*(DUMY1*(DUMY1*FN1IFT(IFT)+FN2IFT(IFT))+FN3IFT(IFT)
     1)+FN4IFT(IFT)                                                     
590   DUMMY3(J)=-RHOMAS*DUMMY3(J)/WGCONS                                
      WRITE (OUT,160) (DUMMY3(J),J=1,7)                                 
      WRITE (OUT,170) (NUMMY1(J),J=1,7)                                 
C*****CHECK FOR PRINTOUT OF MORE THAN 500 POINTS ON FITTED FAN CURVE    
      IF (NHIGH-K) 600,610,610                                          
600   CALL EERROR (85)                                                   
      GO TO 640                                                         
C*****TERMINATE PRINTING OF FITTED FAN CURVE IF                         
C*****FIRST  - THE FAN TOTAL PRESSURE RISE IS NEGATIVE                  
C*****SECOND - POINTS HAVE BEEN PRINTED THROUGHOUT THE ENTRIE FAN       
C*****         OPERATING RANGE                                          
610   IF (DUMMY3(7)) 620,580,580                                        
620   IF (DUMY1+CFMHFT(IFT)) 640,580,580                                
C*****COMPUTE THE INFLOW FAN CURVE COEFFICIENTS FOR A FAN HAVING        
C*****THE SAME PERFORMANCE CURVE IN BOTH DIRECTIONS                     
630   FN1IFT(IFT)=FN1OFT(IFT)                                           
      FN2IFT(IFT)=-FN2OFT(IFT)                                          
      FN3IFT(IFT)=FN3OFT(IFT)                                           
      FN4IFT(IFT)=-FN4OFT(IFT)                                          
C*****OPTIONAL PRINTOUT OF COEFFICIENTS OF FITTED FAN CURVES            
640   GO TO (660,650,650,650,650,650,660,660), SUPOPT                   
650   WRITE (OUT,1)   CHAR(12)
      WRITE (OUT,200)                                                    
      WRITE (OUT,210) FN1OFT(IFT),FN2OFT(IFT),FN3OFT(IFT),FN4OFT(IFT)   
      WRITE (OUT,220) FN1IFT(IFT),FN2IFT(IFT),FN3IFT(IFT),FN4IFT(IFT)   
660   RETURN                                                            
      END                                                               
