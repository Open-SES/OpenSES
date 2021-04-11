      DOUBLE PRECISION FUNCTION PSI( N )                                
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      DOUBLE PRECISION GAMMA                                            
                                                                        
      GAMMA = 0.5772156649015328606D0                                   
                                                                        
      PSI = 0.0                                                         
      N1 = N - 1                                                        
      IF( N1 ) 150,150,50                                               
   50 DO 100 J = 1, N1                                                  
  100 PSI = PSI + 1.0 / DBLE( FLOAT( J ) )                              
  150 PSI = PSI - GAMMA                                                 
      RETURN                                                            
      END                                                               
