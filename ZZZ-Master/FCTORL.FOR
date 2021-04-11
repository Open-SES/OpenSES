      DOUBLE PRECISION FUNCTION FCTORL( N )                             
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      IF( N ) 30,20,10                                                  
   10 FCTORL = 1.0                                                      
      DO  15  I = 1, N                                                  
   15 FCTORL = FCTORL * DBLE( FLOAT( I ) )                              
      RETURN                                                            
   20 FCTORL = 1.0                                                      
      RETURN                                                            
C  30 WRITE(6,35 ) N                                                    
C  35 FORMAT( ' * ERROR IN FCTORL  -  N =', I5 )                        
   30 RETURN                                                            
      END                                                               
