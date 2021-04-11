      DOUBLE PRECISION FUNCTION KER( NU, X, BERX, BEIX )                
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      DOUBLE PRECISION BERX,BEIX,PSI,FCTORL,DPI,ANG,A,B,C,D             
                                                                        
      DPI = 3.1415926535589793238D0                                     
                                                                        
      IF( NU - 1 ) 20,25,20                                             
   20 A = 0.0                                                           
      GO TO 30                                                          
   25 A = - DSQRT( DBLE( 2.0 )) / ( 2.0 * DBLE( X ))                    
   30 A = A - DLOG( DBLE( X ) / 2.0 ) * BERX                            
      A = A + BEIX * DPI / 4.0                                          
      D = DBLE( X )**2 / 4.0                                            
C**** COMPUTE FIRST TERM IN SERIES ( K = 0 )                            
      B = DCOS(0.75 * DBLE( FLOAT( NU )) * DPI )*(PSI(1)+PSI(NU+1))     
C**** EVALUATE SECOND THRU N'TH TERM                                    
      DO  100  J = 1, 50                                                
      K = J                                                             
      IF( NU ) 60,50,60                                                 
   50 K = J * 2                                                         
   60 ANG = 0.75 * DBLE( FLOAT( NU )) + 0.50 * DBLE( FLOAT( K ))        
      C = DCOS( ANG * DPI) * (PSI(K+1) + PSI(NU+K+1)) * D**K            
     1      / ( FCTORL(K) * FCTORL(NU+K))                               
      B = B + C                                                         
C**** TEST FOR CONVERGENCE                                              
      IF( DABS( C ) - 1.0E-09 ) 105,105,100                             
  100 CONTINUE                                                          
                                                                        
      J = 50                                                            
                                                                        
  105 IF( NU ) 120,110,120                                              
  110 KER = A + B / 2.0                                                 
      GO TO 200                                                         
  120 KER = A + B / 2.0 * ( DBLE( X ) / 2.0 )**NU                       
C 200 WRITE(6,210)  NU,X,KER,J                                          
C 210 FORMAT('KER (',I2,',',F8.3,' ) =',D20.12,5X,I4,                 
C    1    ' ITERATIONS' )                                               
  200 RETURN                                                            
      END                                                               
