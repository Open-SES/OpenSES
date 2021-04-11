      SUBROUTINE  JULIE(  R, RPRIM, W, AL1, AL2, K1, K2, HA, AA,        
     1            TAVG, TDS, TIM, RADIUS, TWALL, ISKIP )                
C                                                                       
C                                                                       
C      INPUT PARAMETERS                                                 
C      ----- ----------                                                 
C      R     - RADIUS OF TUNNEL SECTION, FT.                            
C      RPRIM - RADIUS OF MATERIAL 1 - MATERIAL 2 BOUNDARY, FT.          
C      W     - ANGULAR VELOCITY OF TEMPERATURE VARIATION, RADIANS/HOUR  
C              ( 2*PI / (365*24))                                       
C      AL1   - THERMAL DIFFUSIVITY OF MATERIAL NEAR TUNNEL              
C              FT**2/HR                                                 
C      AL2   - THERMAL DIFFUSIVITY OF MATERIAL DISTANT FROM TUNNEL      
C              FT**2/HR                                                 
C      K1    - THERMAL CONDUCTIVITY OF MATERIAL NEAR TUNNEL             
C              BTU/HR-FT-DEG F.                                         
C      K2    - THERMAL CONDUCTIVITY OF MATERIAL DISTANT FROM TUNNEL     
C              BTU/HR-FT-DEG F.                                         
C      HA    - ANNUAL AVERAGE HEAT TRANSFER COEFFICIENT AT TUNNEL       
C              WALL SURFACE,  BTU/HR-FT**2-DEG F.                       
C      AA    - AMPLITUDE OF THE ANNUAL TUNNEL AIR TEMPERATURE VARIATION 
C              ABOUT THE MEAN VALUE,  DEG F.                            
C      TAVG  - MEAN ANNUAL TUNNEL AIR TEMPERATURE, DEG F.               
C      TDS   - TEMPERATURE OF DEEP SINK, DEG F.                         
C      TIM   - TIME AT WHICH TEMPERATURE IS TO BE EVALUATED, HOURS      
C      RADIUS- DISTANCE FROM CENTER OF TUNNEL AT WHICH TEMPERATURE      
C              IS TO BE EVALUATED,  FT.                                 
C                                                                       
C      OUTPUT PARAMETER                                                 
C      ------ ---------                                                 
C      TWALL - TEMPERATURE OF MATERIAL AT DISTANCE 'RADIUS' FROM        
C              CENTER OF TUNNEL,  DEG F.                                
C                                                                       
C                                                                       
      REAL  K1, K2                                                      
C                                                                       
      DOUBLE PRECISION  B,E,F,G,RARGM0,RARGN0                           
      DOUBLE PRECISION  Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10                  
      DOUBLE PRECISION  ZI1,ZI2,ZI3,ZI4,ZI5,ZI6,ZI7,ZI8,ZI9,ZI10,       
     1   ZI11,ZI12,ZI13,ZI14,ZI15,ZI16,ZI17                             
      DOUBLE PRECISION  ZR1,ZR2,ZR3,ZR4,ZR5,ZR6,ZR7,ZR8,ZR9,ZR10,       
     1   ZR11,ZR12,ZR13,ZR14,ZR15,ZR16,ZR17                             
      DOUBLE PRECISION  ZMC1,ZMC2,ZMC3,ZMC4,ZMC5,ZMC6,ZMC7,ZMC8,ZMC9    
      DOUBLE PRECISION  ZM0XR,ZN0XR,ZM1XR,ZN1XR,ZM0X1,ZN0X1,            
     1   ZM1X1,ZN1X1,ZM0X2,ZN0X2,ZM1X2,ZN1X2                            
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE   'JULIEC'                                                
C----                                                                   
C                                                                       
      IF (ISKIP) 10,10,20                                               
   10 PIHS = 3.14159265                                                 
      TM = TAVG - TDS                                                   
      XR = R*SQRT(W/AL1)                                                
      X1 = RPRIM*SQRT(W/AL1)                                            
      X2 = RPRIM*SQRT(W/AL2)                                            
      CALL BESSEL(0,XR,ZM0XR,ZN0XR,ATH0XR,APH0XR)                       
      CALL BESSEL(1,XR,ZM1XR,ZN1XR,ATH1XR,APH1XR)                       
      CALL BESSEL(0,X1,ZM0X1,ZN0X1,ATH0X1,APH0X1)                       
      CALL BESSEL(1,X1,ZM1X1,ZN1X1,ATH1X1,APH1X1)                       
      CALL BESSEL(0,X2,ZM0X2,ZN0X2,ATH0X2,APH0X2)                       
      CALL BESSEL(1,X2,ZM1X2,ZN1X2,ATH1X2,APH1X2)                       
      B = K1*SQRT(W/AL1)*ZM1X1                                          
      E = K2*SQRT(W/AL2)*ZN1X2*ZM0X1/ZN0X2                              
      F = K1*SQRT(W/AL1)*ZM1XR                                          
C                                                                       
C **** ENTRY POINT JULIE2 FOR  RAPID EVALUATION OF                      
C **** MULTIPLE SUBSEGMENT CONFIGURATIONS                               
C                                                                       
C                                                                       
   20 G = HA*ZM0XR                                                      
      ZR1 = COS(ATH1X1-PIHS/4.0)                                        
      ZI1 = SIN(ATH1X1-PIHS/4.0)                                        
      ZR2 = COS(APH1X2+ATH0X1-APH0X2+3.0*PIHS/4.0)                      
      ZI2 = SIN(APH1X2+ATH0X1-APH0X2+3.0*PIHS/4.0)                      
      ZR3 = COS(ATH1XR-PIHS/4.0)                                        
      ZI3 = SIN(ATH1XR-PIHS/4.0)                                        
      ZR4 = COS(ATH0XR)                                                 
      ZI4 = SIN(ATH0XR)                                                 
      ZMC1 =DSQRT((B*ZR1+E*ZR2)**2+(B*ZI1+E*ZI2)**2)                    
      ALPH1 =DATAN2((B*ZI1+E*ZI2),(B*ZR1+E*ZR2))                        
      ZMC2 =DSQRT((F*ZR3-G*ZR4)**2+(F*ZI3-G*ZI4)**2)                    
      ALPH2 =DATAN2((F*ZI3-G*ZI4),(F*ZR3-G*ZR4))                        
      Y1 = HA*ZN0XR                                                     
      Y2 = K1*SQRT(W/AL1)*ZN1XR                                         
      Y3 = K1*SQRT(W/AL1)*ZM1XR                                         
      Y4 = HA*ZM0XR                                                     
      Y5 = ZM1X1                                                        
      Y6 = ZN1X1                                                        
      Y7 = ZN1X2                                                        
      Y8 = ZN0X2                                                        
      Y9 = ZM0X1                                                        
      Y10 = ZN0X1                                                       
      ZR5 = COS(APH0XR)                                                 
      ZI5 = SIN(APH0XR)                                                 
      ZR6 = COS(APH1XR+3.0*PIHS/4.0)                                    
      ZI6 = SIN(APH1XR+3.0*PIHS/4.0)                                    
      ZR7 = COS(ATH1XR-PIHS/4.0)                                        
      ZI7 = SIN(ATH1XR-PIHS/4.0)                                        
      ZR8 = COS(ATH0XR)                                                 
      ZI8 = SIN(ATH0XR)                                                 
      ZMC3 =DSQRT((Y1*ZR5+Y2*ZR6)**2+(Y1*ZI5+Y2*ZI6)**2)                
      ALPH3 =DATAN2((Y1*ZI5+Y2*ZI6),(Y1*ZR5+Y2*ZR6))                    
      ZMC4 =DSQRT((Y3*ZR7-Y4*ZR8)**2+(Y3*ZI7-Y4*ZI8)**2)                
      ALPH4 =DATAN2((Y3*ZI7-Y4*ZI8),(Y3*ZR7-Y4*ZR8))                    
      ZR9 = COS(ALPH3-ALPH4+ATH1X1-PIHS/4.0)                            
      ZI9 = SIN(ALPH3-ALPH4+ATH1X1-PIHS/4.0)                            
      ZR10 = COS(APH1X1+3.0*PIHS/4.0)                                   
      ZI10 = SIN(APH1X1+3.0*PIHS/4.0)                                   
      ZR11 = COS(ALPH3-ALPH4+ATH0X1)                                    
      ZI11 = SIN(ALPH3-ALPH4+ATH0X1)                                    
      ZR12 = COS(APH0X1)                                                
      ZI12 = SIN(APH0X1)                                                
      DELT1 = APH1X2-APH0X2+3.0*PIHS/4.0                                
      ZMC5 =DSQRT((ZMC3*Y5*ZR9/ZMC4-Y6*ZR10)**2+(ZMC3*Y5*ZI9/ZMC4       
     1-Y6*ZI10)**2)                                                     
      ALPH5 =DATAN2((ZMC3*Y5*ZI9/ZMC4-Y6*ZI10),(ZMC3*Y5*ZR9/ZMC4        
     1-Y6*ZR10))                                                        
      ZMC6 =DSQRT((ZMC3*Y9*ZR11/ZMC4+Y10*ZR12)**2+(ZMC3*Y9*ZI11/ZMC4    
     1+Y10*ZI12)**2)                                                    
      ALPH6 =DATAN2((ZMC3*Y9*ZI11/ZMC4+Y10*ZI12),(ZMC3*Y9*ZR11/ZMC4     
     1+Y10*ZR12))                                                       
      ZR13 = COS(ALPH5)                                                 
      ZI13 = SIN(ALPH5)                                                 
      ZR14 = COS(DELT1+ALPH6)                                           
      ZI14 = SIN(DELT1+ALPH6)                                           
      ZMC7 =DSQRT((K1*SQRT(W/AL1)*ZMC5*ZR13+K2*SQRT(W/AL2)*Y7*ZMC6      
     1*ZR14/Y8)**2+(K1*SQRT(W/AL1)*ZMC5*ZI13+K2*SQRT(W/AL2)*Y7*ZMC6     
     2*ZI14/Y8)**2)                                                     
      ALPH7 =DATAN2((K1*SQRT(W/AL1)*ZMC5*ZI13+K2*SQRT(W/AL2)*Y7*ZMC6    
     1*ZI14/Y8),(K1*SQRT(W/AL1)*ZMC5*ZR13+K2*SQRT(W/AL2)*Y7*ZMC6        
     2*ZR14/Y8))                                                        
      ZR15 = COS(ALPH1-ALPH2-ALPH7+ALPH3)                               
      ZI15 = SIN(ALPH1-ALPH2-ALPH7+ALPH3)                               
      ZMC8 =DSQRT((ZMC3*ZMC1*ZR15/ZMC7/ZMC2-1.0)**2+(ZMC3*ZMC1*ZI15     
     1/ZMC7/ZMC2)**2)                                                   
      ALPH8 =DATAN2((ZMC3*ZMC1*ZI15/ZMC7/ZMC2),(ZMC3*ZMC1*ZR15          
     1/ZMC7/ZMC2-1.0))                                                  
      TAU1 = ALPH8-ALPH4                                                
      TAU2 = ALPH1-ALPH2-ALPH7                                          
      ZR16 = COS(ALPH8-ALPH4+ATH0X1)                                    
      ZI16 = SIN(ALPH8-ALPH4+ATH0X1)                                    
      ZR17 = COS(ALPH1-ALPH2-ALPH7+APH0X1)                              
      ZI17 = SIN(ALPH1-ALPH2-ALPH7+APH0X1)                              
      ZMC9 =DSQRT((ZMC8*ZM0X1*ZR16/ZMC4+ZMC1*ZN0X1*ZR17/ZMC7/ZMC2)**2   
     1+(ZMC8*ZM0X1*ZI16/ZMC4+ZMC1*ZN0X1*ZI17/ZMC7/ZMC2)**2)             
      ALPH9 =DATAN2((ZMC8*ZM0X1*ZI16/ZMC4+ZMC1*ZN0X1*ZI17/ZMC7/ZMC2),   
     1(ZMC8*ZM0X1*ZR16/ZMC4+ZMC1*ZN0X1*ZR17/ZMC7/ZMC2))                 
      TAU3 = ALPH9-APH0X2                                               
C                                                                       
      IF(RADIUS-RPRIM) 300,300,400                                      
C                                                                       
C     EVALUATE TEMPERATURE IN MATERIAL NEAR TUNNEL  (T1 AREA)           
C                                                                       
  300 RARG = RADIUS*SQRT(W/AL1)                                         
      CALL BESSEL(0,RARG,RARGM0,RARGN0,RARGTH,RARGPH)                   
      T1BAR = HA*ZMC8*RARGM0/ZMC4*(AA*COS(TAU1+RARGTH+W*TIM)            
     1+TM*COS(TAU1+RARGTH))+HA*ZMC1*RARGN0/ZMC7/ZMC2*(AA*COS(TAU2       
     2+RARGPH+W*TIM)+TM*COS(TAU2+RARGPH))                               
      T1 = T1BAR+TDS                                                    
      TWALL = T1                                                        
C     WRITE(6,310) RADIUS,T1                                            
C 310 FORMAT(/'RADIUS =',F12.7,' T1 =',F7.3)                          
      GO TO 490                                                         
C                                                                       
C     EVALUATE TEMPERATURE OF MATERIAL AWAY FROM TUNNEL  (T2 AREA)      
C                                                                       
  400 RARG = RADIUS*SQRT(W/AL2)                                         
      CALL BESSEL(0,RARG,RARGM0,RARGN0,RARGTH,RARGPH)                   
      T2BAR = ZMC9*HA*RARGN0/ZN0X2*(AA*COS(TAU3+RARGPH+W*TIM)+TM*       
     1COS(TAU3+RARGPH))                                                 
      T2 = T2BAR+TDS                                                    
      TWALL = T2                                                        
C     WRITE(6,410) RADIUS,T2                                            
C 410 FORMAT(/'RADIUS =',F12.7,' T2 =',F7.3)                          
  490 CONTINUE                                                          
  500 CONTINUE                                                          
      RETURN                                                            
      END                                                               
