      SUBROUTINE RELHUM( TDB, W, RH )                                   
C                                                                       
C     THIS SUBROUTINE CONVERTS DRY BULB TEMPERATURE AND SPECIFIC        
C     HUMIDITY INTO RELATIVE HUMIDITY.                                  
C                                                                       
C     TDB = DRY BULB TEMPERATURE (DEG F )                               
C     W = SPECIFIC HUMIDITY (LB OF WATER PER LB OF DRY AIR )            
C     RH = RELATIVE HUMIDITY (PERCENT )                                 
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      DATA A,B,C,D,PC,TC / 3.2437814, 5.86826E-03, 1.1702379E-08,       
     1      2.1878462E-03, 218.167, 647.27 /                            
      PATMOS=PAMB/SAINHG                                                
      PV = W*PATMOS/(.622+W)                                            
      T = (TDB - 32. )*5./9. +273.16                                    
      X = TC - T                                                        
      ARG = (X/T)*(A+B*X+C*X**3) / (1.0 + D*X)                          
      PVSAT = PC*10.**(-ARG)                                            
      RH = PV*100./PVSAT                                                
      RETURN                                                            
      END                                                               
