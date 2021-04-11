      SUBROUTINE WLLTM2(TWALLA,TIMA,TWALLM,TIMM,ICLST)                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'DSHRHS'                                                 
C                                                                       
C                                                                       
      DOUBLE PRECISION  B, E, F                                         
      DOUBLE PRECISION  ZM0XR, ZN0XR, ZM1XR, ZN1XR, ZM0X1, ZN0X1,       
     1                  ZM1X1, ZN1X1, ZM0X2, ZN0X2, ZM1X2, ZN1X2        
C                                                                       
      INCLUDE  'JULIEC'                                                 
C                                                                       
C----                                                                   
      DIMENSION   TWALLA(LMSS), TWALLM(LMSS)                            
C                                                                       
C     LOOP OVER LINE SEGMENTS                                           
C                                                                       
      DO 100 ISEG = 1,NLS                                               
      ILSSL = TABL3(ISEG)                                               
      IF(ICLST-TABL44(ILSSL)) 100,1,100                                 
    1 ILSSH = TABL4(ISEG)                                               
      HYRAD = 2.0*ALS(ISEG)/PERMLS(ISEG)                                
      RPRIM = RPRMLS(ISEG)+HYRAD                                        
      IF (DBTLS(ISEG)) 2,2,5                                            
    2 EFTRNS = 1.                                                       
      EFANNL = 1.                                                       
      GO TO 8                                                           
    5 EFTRNS = .607 + .0039823*DBTLS(ISEG) - .00001337*DBTLS(ISEG)**2   
      EFANNL = (EFTRNS + 1.0)/2.                                        
C                                                                       
C     LOOP OVER SUBSEGMENTS IN LINE SEGMENT                             
C                                                                       
    8 DO 20 ISS = ILSSL,ILSSH                                           
C                                                                       
C     EVALUATE COMPONENT OF SURFACE TEMPERATURE DUE                     
C     TO ELEVATED MEAN AIR TEMPERATURE IN SYSTEM 30 YEARS               
C     AFTER OPERATION HAS BEGUN                                         
C                                                                       
      CALL TRANST(HYRAD,RPRIM,THDFLS(1,ISEG),THDFLS(2,ISEG),            
     1   THCNLS(1,ISEG),THCNLS(2,ISEG),30.,HCNLSS(ISS),                 
     2   TMALSS(ISS),TSNKLS(ISEG),HYRAD,TTRANS)                         
      TWALLA(ISS) = TMALSS(ISS)-(TMALSS(ISS)-TTRANS)*EFTRNS             
C     WRITE(6,10) ISS,TWALLA(ISS)                                       
C  10 FORMAT(I10,' TWALLTRANS=',F10.4)                                  
   20 CONTINUE                                                          
C                                                                       
C     EVALUATE TEMPERARURE COMPONENT DUE TO ANNUAL VARIATION            
C                                                                       
      OMEG = 2.*PI/(365.*24.)                                           
C                                                                       
C     ADJUST MONTH SO JANUARY IS THE COLDEST,JULY IS HOTTEST            
C                                                                       
      DUMY1 = MONTH + 5                                                 
      IF(DUMY1-12.0) 30,25,25                                           
   25 DUMY1 = DUMY1 - 12.0                                              
C                                                                       
C     CONVERT DUMY1 INTO HOURS                                          
C                                                                       
   30 DUMY1 = DUMY1*(365./12.)*24.                                      
C                                                                       
C     COMPUTE WALL TEMPERATURE COMPONENT DUE TO ANNUAL VARIATION        
C                                                                       
      DO 50 ISS = ILSSL,ILSSH                                           
      ISKIP=ISS-ILSSL                                                   
      CALL JULIE(HYRAD,RPRIM,OMEG,THDFLS(1,ISEG),THDFLS(2,ISEG),        
     1   THCNLS(1,ISEG),THCNLS(2,ISEG),HCNLSS(ISS),TAALS(ISEG),         
     2   0.0,0.0,DUMY1,HYRAD,TANN,ISKIP)                                
      TWALLA(ISS) = TWALLA(ISS)+TAALS(ISEG)*COS(OMEG*DUMY1)*(1.-EFANNL) 
     1              + TANN*EFANNL                                       
      TWALLM(ISS) = TWALLA(ISS)                                         
C     WRITE(6,46) ISS,TWALLA(ISS)                                       
C  46 FORMAT(I10,' TWALLANNUAL=',F10.4)                                 
   50 CONTINUE                                                          
C                                                                       
C     COMPUTE WALL TEMPERATURE COMPONENT DUE TO DIURNAL VARIATION       
C                                                                       
      OMEG = 2.*PI/24.                                                  
      DUMY2 = TIMA                                                      
      DUMY3 = TIMM                                                      
      DO 70 ISS = ILSSL,ILSSH                                           
      DIAMP = TSMAX(ISS)-TSMEAN(ISS)                                    
      ISKIP=ISS-ILSSL                                                   
      CALL JULIE(HYRAD,RPRIM,OMEG,THDFLS(1,ISEG),THDFLS(2,ISEG),        
     1   THCNLS(1,ISEG),THCNLS(2,ISEG),HCNLSS(ISS),DIAMP,               
     2   0.0,0.0,DUMY2,HYRAD,THETA1,ISKIP)                              
      CALL JULIE(HYRAD,RPRIM,OMEG,THDFLS(1,ISEG),THDFLS(2,ISEG),        
     1   THCNLS(1,ISEG),THCNLS(2,ISEG),HCNLSS(ISS),DIAMP,               
     2   0.0,0.0,DUMY3,HYRAD,THETA2,ISKIP)                              
      TWALLA(ISS) = TWALLA(ISS)+THETA1                                  
      TWALLM(ISS) = TWALLM(ISS)+THETA2                                  
C     WRITE(6,66) ISS,TWALLA(ISS),TWALLM(ISS)                           
C  66 FORMAT(I10,' TWALLDIURNALMN=',F10.4,' TWALLDIURNALMX=',F10.4)     
   70 CONTINUE                                                          
  100 CONTINUE                                                          
      RETURN                                                            
      END                                                               
