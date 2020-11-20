      FUNCTION  AIR(TEMP,ITABLE)                                        
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
 
C      THIS SUBROUTINE RETURNS ONE OF FOUR TEMP. DEPENDENT PROPERTIES   
C      OF AIR.                                                          
C      ITABLE = 1 - KINEMATIC VISCOSITY ( SQ FT/SEC )                   
C               2 - THERMAL CONDUCTIVITY ( BTU/HR-FT-DEG F )            
C               3 - PRANDTL NUMBER                                      
C               4 - G * BETA / NU**2   ( 1/DEG F-CU FT )                
C                                                                       
      DIMENSION  TABLE(15,4)                                            
                                                                        
      DATA  TABLE/.13E-3,.18E-3,.239E-3,.306E-3,.378E-3,.455E-3,        
     1  .540E-3,.625E-3,.717E-3,.815E-3,.917E-3,1.47E-3,2.14E-3,        
     2  2.80E-3,3.39E-3,                                                
     3  0.0133,0.0154,.0174,.0193,.0212,.0231,.0250,.0268,.0286,        
     4  .0303,.0319,.0400,.0471,.051,.054,                              
     5  0.73,.72,.72,.71,.689,.683,.685,.690,.697,.705,.713,            
     6  .739,.753,.763,.765,                                            
     7  4.2E06,1.76E06,.850E06,.444E06,.258E06,.159E06,.106E06,70.4E03, 
     8  49.8E03,36.0E03,26.5E03,7.45E03,2.84E03,1.41E03,.815E03 /       
                                                                        
      J = ( TEMP / 100. ) +1                                            
      IF( J - 11 ) 100,140,140                                          
C**** TEMP IS IN THE 100 DEG F INTERVAL RANGE ( UP TO 1000 DEG F )      
  100 IF( J ) 120,120,130                                               
  120 AIR = TABLE( 1,ITABLE )                                           
      GO TO 200                                                         
  130 SLOPE = (TABLE(J+1,ITABLE)-TABLE(J,ITABLE))/100.                  
      AIR = TABLE(J,ITABLE) + (TEMP-(J-1)*100.) * SLOPE                 
      GO TO 200                                                         
C**** TEMP IS IN THE 500 DEG F TNTERVAL RANGE ( 1000 TO 3000 DEG F )    
  140 IF( J - 31 ) 160,150,150                                          
  150 AIR = TABLE( 15,ITABLE)                                           
      GO TO 200                                                         
  160 J = (J-1) / 5 + 9                                                 
      SLOPE = (TABLE(J+1,ITABLE) - TABLE(J,ITABLE)) /500.               
      AIR = TABLE(J,ITABLE) + (TEMP-(J-9)*500.)*SLOPE                   
  200 RETURN                                                            
      END                                                               
