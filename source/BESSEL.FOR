      SUBROUTINE  BESSEL( NU, X, AM, AN, THETA, PHI )                   
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      DOUBLE PRECISION  BER,BEI,KER,KEI,BERX,BEIX,KERX,KEIX             
      DOUBLE PRECISION  AM,AN,DPI,DSQR2,DX                              
                                                                        
C                                                                       
C      THIS SUBROUTINE COMPUTES THE VALUE OF THE MODIFIED BESSEL        
C      FUNCTIONS M, N, THETA AND PHI.  THESE ARE FUNCTIONS OF X AND CAN 
C      BE COMPUTED FOR NU OF 0 OR 1.                                    
C                                                                       
C      THE SERIES EXPANSIONS THAT WERE USED WERE TAKEN FROM             
C          'HANDBOOK OF MATHEMATICAL FUNCTIONS'                         
C          EDITED BY ABRAMOWITZ AND STEGUN, DOVER PUBLICATIONS, 1965    
C                                                                       
C                                                                       
C     CHECK VALUE OF ARGUMENT X -                                       
C         IF LESS THAN 8.0 USE SERIES EXPRESSION  ( ANNUAL )            
C         IF GREATER THAN 8.0 USE POLYNOMIAL      ( DIURNAL )           
C                                                                       
      IF( X - 8.0 ) 100,100,200                                         
C                                                                       
  100 BERX = BER( NU, X )                                               
      BEIX = BEI( NU, X )                                               
      AM = DSQRT( BERX**2 + BEIX**2 )                                   
      THETA = SNGL( DATAN2( BEIX , BERX ) )                             
C                                                                       
      KERX = KER( NU, X ,BERX, BEIX )                                   
      KEIX = KEI( NU, X ,BERX ,BEIX )                                   
      AN = DSQRT( KERX**2 +  KEIX**2 )                                  
      PHI = SNGL( DATAN2( KEIX , KERX ) )                               
      RETURN                                                            
C****                                                                   
C**** POLYNOMIAL ASYMPTOTIC EXPANSIONS FOR LARGE X                      
C****       'HANDBOOK' , PG. 383                                        
C****                                                                   
  200 MU = 4 * NU * NU                                                  
      PI = 3.141592653                                                  
      DPI = 3.1415926535897932385D0                                     
      SQR2 = SQRT( 2.0 )                                                
      DSQR2 = DSQRT( 2.0D0 )                                            
      DX = DBLE( X )                                                    
C                                                                       
      DUMY1=DBLE((1.0+(MU*(MU+14.0)-399.0)/(768.0*X*X))/SQR2)           
      AM=DEXP(DX/DSQR2)/DSQRT(2.0D0*DPI*DX)*DBLE(1.0+((MU-1.0)/(8.0*X))*
     1((MU-1.0)/(32.0*X)-DUMY1))                                        
      AN=DEXP(-DX/DSQR2)*DSQRT(DPI/(2.0D0*DX))*DBLE(1.0+((MU-1.0)/(8.0*X
     1))*((MU-1.0)/(32.0*X)+DUMY1))                                     
C                                                                       
      DUMY1=(1.0-(MU-25.0)/(48.0*X*X))/SQR2                             
      THETA=X/SQR2+(0.5*NU-0.125)*PI+((MU-1.0)/(8.0*X))*(0.5/X+DUMY1)   
      PHI=-X/SQR2-(0.5*NU+0.125)*PI+((MU-1.0)/(8.0*X))*(0.5/X-DUMY1)    
C                                                                       
      RETURN                                                            
      END                                                               
