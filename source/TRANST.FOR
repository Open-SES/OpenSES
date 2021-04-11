      SUBROUTINE TRANST( R, RPRIM, AL1, AL2, K1, K2, YEAR, HA,          
     1          TAVG, TDS, RADIUS, TWALL )                              
C                                                                       
C      THIS SUBROUTINE COMPUTES THE TRANSIENT TEMPERATURE OF THE        
C      SOIL SURROUNDING THE SUBWAY TUNNEL.                              
C                                                                       
C      INPUT PARAMETERS                                                 
C      ----- ----------                                                 
C                                                                       
C      R     - RADIUS OF TUNNEL SECTION, FT.                            
C      RPRIM - RADIUS OF MATERIAL 1 - MATERIAL 2 BOUNDARY, FT.          
C      AL1   - THERMAL DIFFUSIVITY OF MATERIAL NEAR TUNNEL,             
C              FT**2/HR                                                 
C      AL2   - THERMAL DIFFUSIVITY OF MATERIAL DISTANT FROM TUNNEL,     
C              FT**2/HR                                                 
C      K1    - THERMAL CONDUCTIVITY OF MATERIAL NEAR TUNNEL,            
C              BTU/HR-FT-DEG F.                                         
C      K2    - THERMAL CONDUCTIVITY OF MATERIAL DISTANT FROM TUNNEL,    
C              BTU/HR-FT-DEG F.                                         
C      YEAR  - NUMBER OF YEARS AFTER START OF SUBWAY OPERATION WHEN     
C              TRANSIENT SOIL TEMPERATURE IS TO BE EVALUATED.           
C      HA    - ANNUAL AVERAGE HEAT TRANSFER COEFFICIENT AT TUNNEL       
C              WALL SURFACE, BTU/HR-FT**2-DEG F.                        
C      TAVG  - MEAN ANNUAL TUNNEL AIR TEMPERATURE, DEG F.               
C      TDS   - TEMPERATURE OF DEEP SINK, DEG F.                         
C      RADIUS- DISTANCE FROM CENTER OF TUNNEL AT WHICH TEMPERATURE      
C              IS TO BE EVALUATED, FT.                                  
C                                                                       
C      OUTPUT PARAMETER                                                 
C      ------ ---------                                                 
C                                                                       
C      TWALL - TEMPERATURE OF MATERIAL AT DISTANCE 'RADIUS' FROM        
C              CENTER OF TUNNEL, DEG F.                                 
C                                                                       
C                                                                       
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      REAL  K1, K2                                                      
C                                                                       
      AL1=AL1                                                           
C     DETERMINE IF HOMOGENEOUS MATERIAL OR TWO LAYERED MATERIAL         
C                                                                       
      IF(  ABS( R - RPRIM ) - 0.01 ) 100,200,200                        
C                                                                       
C**** HOMOGENIUS MATERIAL                                               
C                                                                       
  100 TAU2 = AL2 * ( YEAR * 8760. ) / R**2                              
      SLOPE1 = TRSLOP( TAU2 )                                           
      C1 = K2 / HA * SLOPE1 / ( R * 2.302585092 )                       
      TSURF = ( TAVG + TDS*C1 ) / (C1 + 1.0 )                           
      TWALL = (TSURF - TDS) *(1.0 - SLOPE1*LOG10(RADIUS/R)) + TDS      
      RETURN                                                            
C                                                                       
C                                                                       
C     TWO LAYER MATERIAL                                                
C                                                                       
  200 TAU2 = AL2 * (YEAR * 8760. ) / R**2                               
      SLOPE2 = TRSLOP( TAU2 )                                           
      A1 =  SLOPE2 / (RPRIM * 2.302585092 )                             
      B1 = 1.0 - SLOPE2*LOG10(RPRIM / R )                              
      A11 = - K1 / R + HA * LOG( R )                                   
      A21 = HA                                                          
      A31 = HA * TAVG                                                   
      A12 = K2 * A1 / B1 * LOG( RPRIM ) + K1 / RPRIM                   
      A22 = K2 * A1 / B1                                                
      A32 = K2 * A1 / B1 * TDS                                          
      C1 = (A31*A22 - A32*A21) / (A11*A22 - A12*A21)                    
      C2 = ( A31 - C1*A11 ) / A21                                       
C                                                                       
      IF( RADIUS - RPRIM ) 250,250,280                                  
C**** MATERIAL 1                                                        
  250 TWALL = C1 * LOG( RADIUS ) + C2                                  
      RETURN                                                            
C                                                                       
C****** MATERIAL 2                                                      
  280 T2R = ( C1 * LOG( RPRIM ) + C2 - TDS ) / B1 + TDS                
      TWALL = (T2R - TDS) *(1.0 -SLOPE2*LOG10(RADIUS/R)) + TDS         
      RETURN                                                            
      END                                                               
