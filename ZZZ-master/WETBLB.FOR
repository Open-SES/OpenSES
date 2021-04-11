      SUBROUTINE WETBLB( TDB, W, TWB )                                  
C    THIS SUBROUTINE CONVERTS DRY BULB TEMPERATURE AND SPECIFIC HUMIDITY
C             INTO WETBULB TEMPERATURE.  THE ACCURACY OF THE CONVERSION 
C             IS APPROXIMATELY WITHIN ONE HALF OF ONE DEGREE OF THE     
C             ACTUAL VALUE.                                             
C                                                                       
C     TDB = DRY BULB TEMPERATURE (DEG F)                                
C     W = SPECIFIC HUMIDITY (LB OF WATER/LB OF DRY AIR)                 
C     TWB = WET BULB TEMPERATURE (DEG F)                                
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
C                                                                       
      H = .24 * TDB + (1061. + .444 * TDB ) * W                         
      Y = LOG(H)                                                       
      IF (H - 11.758 ) 30,20,20                                         
   20 TWB = 30.9185 - 39.682 * Y + 20.5841 * Y**2 - 1.758 * Y**3        
      GO TO 100                                                         
   30 TWB = .604 + 3.4841 * Y + 1.3601 * Y**2 + .9731 * Y**3            
  100 WBDELT = - ( - .48 * PAMB + 36.71 ) * ( 1. - PAMB / SAINHG )      
      CORR = ( .404 + .00276 * PAMB ) * H / 26.96 - .00425 * PAMB + .385
      TWB = TWB + WBDELT * CORR                                         
      IF( TWB - TDB ) 120,120,110                                       
  110 TWB = TDB                                                         
  120 RETURN                                                            
      END                                                               
