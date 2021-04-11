      FUNCTION  TRSLOP( TAU )                                           
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
                                                                        
      DIMENSION  X(37), Y(37)                                           
                                                                        
      DATA  X/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,2.0,3.0,4.0,      
     1  5.0,6.0,7.0,8.0,9.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,    
     2  90.0,100.,200.,300.,400.,500.,600.,700.,800.,900.,1000. /       
                                                                        
      DATA  Y/5.150,3.941,3.385,3.071,2.829,2.659,2.539,2.442,          
     1    2.345,2.273,1.837,1.644,1.523,1.451,1.378,1.330,1.306,        
     2    1.275,1.233,1.064,0.9913,0.9187,0.8946,0.8704,0.8462,         
     3    0.828,0.812,0.798,0.725,0.677,0.653,0.635,0.621,              
     4    0.604,0.598,0.586,0.570 /                                     
                                                                        
      JLAST = 37                                                        
                                                                        
C**** CHECK IF TAU LIES WITHIN TABLE LIMITS                             
      IF( TAU - X(1) ) 20,20,30                                         
   20 TRSLOP = Y(1)                                                     
      RETURN                                                            
   30 IF( TAU - X(JLAST) ) 50,35,35                                     
   35 TRSLOP = Y(JLAST)                                                 
      RETURN                                                            
C**** SEARCH TABLE                                                      
   50 DO  100  I = 2,JLAST                                              
      IF( TAU - X(I) ) 130,130,100                                      
  100 CONTINUE                                                          
      GO TO 35                                                          
C**** INTERPOLATE                                                       
  130 SLOPE = (Y(I) - Y(I-1)) / (X(I) - X(I-1))                         
      TRSLOP = Y(I-1) + (TAU - X(I-1)) * SLOPE                          
      RETURN                                                            
      END                                                               
