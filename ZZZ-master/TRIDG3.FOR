      SUBROUTINE   TRIDG3( SUB, DIAG, SUP, B )                          
C                                                                       
C      THIS SUBROUTINE SOLVES AX = B FOR THE VECTOR X                   
C      (WHERE A IS TRIDIAGONAL)                                         
C      THE SYSTEM MUST BE OF ORDER 3                                    
C      SUP = SUPERDIAGONAL OF A                                         
C      DIAG = MAIN DIAGONAL OF A                                        
C      SUB = SUBDIAGONAL OF A                                           
C      B = CONSTANT VECTOR                                              
C      SUP AND DIAG ARE DESTROYED                                       
C      SOLUTION VECTOR IS RETURNED IN B                                 
C                                                                       
C      FROM 'ELEMENTARY NUMERICAL ANALYSIS'  BY S.D.CONTE  PG 184       
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
C                                                                       
      DIMENSION  SUP(2),SUB(2),DIAG(3),B(3)                             
C                                                                       
      SUP(1) = SUP(1) / DIAG(1)                                         
      B(1) = B(1) / DIAG(1)                                             
      DO  10  I = 2,3                                                   
      II = I - 1                                                        
C---- DECOMPOSE A TO FORM A = LU WHERE L IS LOWER                       
C     TRIANGULAR, U IS UPPER TRIANGULAR                                 
      DIAG(I) = DIAG(I) - SUP(II)*SUB(II)                               
      IF( I - 3 ) 5, 10, 10                                             
    5 SUP(I) = SUP(I) / DIAG(I)                                         
C---- COMPUTE Z WHERE LZ = B                                            
   10 B(I) = (B(I) - SUB(II)*B(II))/DIAG(I)                             
C---- COMPUTE X BY BACK SUBSTITUTION WHERE UX = Z                       
      DO  20  K = 1,2                                                   
      I = 3 - K                                                         
   20 B(I) = B(I) - SUP(I)*B(I+1)                                       
      RETURN                                                            
      END                                                               
