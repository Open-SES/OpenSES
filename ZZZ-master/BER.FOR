      DOUBLE PRECISION FUNCTION BER( NU, X )                            
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      DOUBLE PRECISION FCTORL,A,B,C,DPI,ANG                             
      DPI = 3.1415926535589793238D0                                     
      B = DBLE( X )**2 / 4.0                                            
C**** EVALUATE FIRST TERM ( K = 0 )                                     
      A = DCOS( 0.75 * DBLE( FLOAT( NU )) * DPI )                       
C**** EVALUATE SECOND THRU N'TH TERM                                    
      DO  100  J = 1, 50                                                
      K = J                                                             
      IF( NU ) 60,50,60                                                 
   50 K = J * 2                                                         
   60 ANG = 0.75 * DBLE( FLOAT( NU )) +  0.50 * DBLE( FLOAT( K ))       
      C = DCOS( ANG*DPI)*B**K / (FCTORL(K)*FCTORL(NU+K))                
      A = A + C                                                         
C**** TEST FOR CONVERGENCE                                              
      IF( DABS( C ) - 1.0E-09 ) 110,110,100                             
  100 CONTINUE                                                          
C                                                                       
      J = 50                                                            
                                                                        
  110 IF( NU ) 120,120,130                                              
  120 BER =  A                                                          
      GO TO 200                                                         
C**** NU IS 1                                                           
  130 BER = A * ( DBLE( X ) / 2.0 )**NU                                 
C 200 WRITE(6,210) NU,X,BER,J                                           
C 210 FORMAT( ' BER(',I2,',',F8.3,' ) =',D20.12,5X,I4,' ITERATIONS')    
  200 RETURN                                                            
      END                                                               
