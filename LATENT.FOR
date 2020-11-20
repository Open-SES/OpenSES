      SUBROUTINE LATENT(TD,TW,W)                                        
C                                                                       
C     THIS SUBROUTINE CONVERTS DRY AND WET BULB TEMPERATURES            
C     INTO SPECIFIC HUMIDITY IN LB/LB                                   
C                                                                       
C     TD IS THE DRY BULB TEMPERATURE IN DEG. F.                         
C     TW IS THE WET BULB TWMPERATURE IN DEG. F.                         
C     W IS THE SPECIFIC HUMIDITY IN LBS OF WATER PER LB OF AIR          
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
                                                                        
C                                                                       
                                                                        
      DATA  A,B,C,D,PC,TC/3.2437814,5.86826E-03,1.1702379E-08,          
     1     2.1878462E-03,218.167,647.27 /                               
C***   CHANGE DEG F TO DEG K                                            
      T = 5.0 * (TW - 32.0)/ 9.0 + 273.16                               
      X = TC - T                                                        
      ALPHA = X * (A+B*X + C* X**3) / ( T*(1.0 + D*X ) )                
      ALPHA = -2.3025851 * ALPHA                                        
      PW = PC * EXP( ALPHA)                                             
C*****  CONVERT P TO INCHES HG                                          
      PW=PW*SAINHG                                                      
      PS = PW - (PAMB - PW)*(TD-TW)/(2800. - 1.3*TW)                    
      W = 0.622 * PS / (PAMB - PS)                                      
      RETURN                                                            
      END                                                               
