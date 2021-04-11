      SUBROUTINE  RKTHRM                                                
C                                                                       
C     THIS SUBROUTINE USES A MODIFIED RUNGE-KUTTA INTEGRATION           
C     TECHNIQUE TO CALCULATE THE NEW SUBSEGMENT TEMPERATURES AND        
C     HUMIDITIES.                                                       
C     KTSTEP  IS THE VARIABLE WHICH INDICATES WHICH OF THE              
C             FOUR STEPS OF THE INTEGRATION IS TO BE PERFORMED.         
C                                                                       
C      REFERENCES -                                                     
C                                                                       
C       'IMPROVING ROUND-OFF IN RUNGE-KUTTA COMPUTATIONS WITH GILL'S    
C       METHOD',  BY ROBERT J. THOMPSON,  COMMUMICATIONS OF THE         
C       ACCOCIATION FOR COMPUTING MACHINERY, VOL 13, NO 12              
C       DECEMBER 1970, PP 739-740.                                      
C                                                                       
C       'A PROCESS FOR THE STEP-BY-STEP INTEGRATION OF DIFFERENTIAL     
C       EQUATIONS IN A AUTOMATIC DIGITAL COMPUTING MACHINE',            
C       BY S. GILL, PROC. CAMBRIDGE PHILOS. SOC 47  1951, PP 96-108.    
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      REAL  AA(4), BB(4), CC(4)                                         
      DATA  AA/ 0.5, 0.292893219, 1.707106781, 0.166666667 /            
      DATA  BB/ 2.0, 1.0, 1.0, 2.0 /                                    
      DATA  CC/ 0.5, 0.292893219, 1.707106781, 0.5 /                    
C                                                                       
C                                                                       
C---- LOOP OVER LINE AND VENT SHAFT SUBSEGMENTS                         
C                                                                       
      DO  100  ISS = 1, TNSS                                            
C                                                                       
C---- INTEGRATE TEMPERATURES                                            
C                                                                       
      TEMP = AA(KTSTEP) * ( DERTSS(ISS) - BB(KTSTEP)*QERTSS(ISS) )      
      W = TTMPSS(ISS)                                                   
      TTMPSS(ISS) = TTMPSS(ISS) + DELTH * TEMP                          
      TEMP = ( TTMPSS(ISS) - W ) / DELTH                                
      QERTSS(ISS) = QERTSS(ISS) + 3.0 * TEMP - CC(KTSTEP)*DERTSS(ISS)   
C                                                                       
C---- INTEGRATE HUMIDITY                                                
C                                                                       
      TEMP = AA(KTSTEP) * ( DERHSS(ISS) - BB(KTSTEP)*QERHSS(ISS) )      
      W = HTMPSS(ISS)                                                   
      HTMPSS(ISS) = HTMPSS(ISS) + DELTH * TEMP                          
      TEMP = ( HTMPSS(ISS) - W ) / DELTH                                
  100 QERHSS(ISS) = QERHSS(ISS) + 3.0 * TEMP - CC(KTSTEP) * DERHSS(ISS) 
C                                                                       
C     SAVE VALUES IF STEP 2 OR 4 OF INTEGRATION                         
C                                                                       
      GO TO ( 200, 140, 200, 140 ), KTSTEP                              
  140 DO  150  ISS = 1, TNSS                                            
      TDBSS(ISS) = TTMPSS(ISS)                                          
  150 HUMSS(ISS) = HTMPSS(ISS)                                          
C                                                                       
C                                                                       
  200 RETURN                                                            
C                                                                       
      END                                                               
