      SUBROUTINE SIMQ1 (A,B,X,N,M,IPIVOT,ROWSIZ,ITER,NOGO)              
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
      DIMENSION A(M), B(N), X(N), IPIVOT(N), ROWSIZ(N)                  
      DATA  TOL / 1.0E-30  /                                            
      NM1=N-1                                                           
C*****CHECK IF NUMBER OF EQUATIONS IS ZERO, ONE, OR GREATER THAN ONE    
      IF (NM1) 120,10,20                                                
C*****CHECK IF ONLY ELEMENT IS TOO SMALL                                
10    IF (ABS(A(1)) .LT. TOL) GO TO 120                                 
C*****FIND SOLUTION TO ONE EQUATION WITH ONE UNKNOWN                    
      X(1)=B(1)/A(1)                                                    
      RETURN                                                            
C*****MORE THAN ONE EQUATION.  CHECK IF 'A' MATRIX HAS ALREADY          
C     BEEN DECOMPOSED.                                                  
20    IF (ITER .GT. 1) GO TO 70                                         
C*****BEGIN DECOMPOSITION BY INITIALIZING IPIVOT TO INDICATE ROWS OF A  
C     MATRIX HAVE NOT YET BEEN INTERCHANGED AND BY FINDING THE LARGEST  
C     ELEMENT IN EACH ROW.                                              
      DO 40 I=1,N                                                       
      IPIVOT(I)=I                                                       
      ROWMAX=0.0                                                        
      DO 30 J=1,N                                                       
      L1=I+N*(J-1)                                                      
      IF (ABS(A(L1)).GT.ROWMAX) ROWMAX=ABS(A(L1))                       
30    CONTINUE                                                          
      IF (ROWMAX.LT.TOL) GO TO 120                                      
40    ROWSIZ(I)=ROWMAX                                                  
      DO 60 K=1,NM1                                                     
      J=K                                                               
      KP1=K+1                                                           
      IP=IPIVOT(K)                                                      
      L1=N*(K-1)+IP                                                     
      COLMAX=ABS(A(L1))/ROWSIZ(IP)                                      
      DO 50 I=KP1,N                                                     
      IP=IPIVOT(I)                                                      
      L1=N*(K-1)+IP                                                     
      AWIKOV=ABS(A(L1))/ROWSIZ(IP)                                      
      IF (AWIKOV .LE. COLMAX) GO TO 50                                  
      COLMAX=AWIKOV                                                     
      J=I                                                               
50    CONTINUE                                                          
      IF (ABS(COLMAX) .LT. TOL) GO TO 120                               
      IPK=IPIVOT(J)                                                     
      IPIVOT(J)=IPIVOT(K)                                               
      IPIVOT(K)=IPK                                                     
      L1=N*(K-1)+IPK                                                    
      DUMY1=A(L1)                                                       
      DO 60 I=KP1,N                                                     
      IP=IPIVOT(I)                                                      
      L1=N*(K-1)+IP                                                     
      A(L1)=A(L1)/DUMY1                                                 
      RATIO=-A(L1)                                                      
      DO 60 J=KP1,N                                                     
      L1=N*(J-1)+IP                                                     
      L2=N*(J-1)+IPK                                                    
60    A(L1)=RATIO*A(L2)+A(L1)                                           
      IF (ABS(A(L1)) .LT. TOL) GO TO 120                                
70    IP=IPIVOT(1)                                                      
      X(1)=B(IP)                                                        
      DO 90 K=2,N                                                       
      IP=IPIVOT(K)                                                      
      KM1=K-1                                                           
      L1=IP                                                             
      SUM=0.0                                                           
      DO 80 J=1,KM1                                                     
      L2=L1+N*(J-1)                                                     
80    SUM=SUM+A(L2)*X(J)                                                
90    X(K)=B(IP)-SUM                                                    
      L1=(N*(N-1))+IP                                                   
      X(N)=X(N)/A(L1)                                                   
      K=N                                                               
      DO 110 I=2,N                                                      
      KP1=K                                                             
      K=K-1                                                             
      IP=IPIVOT(K)                                                      
      L1=(N*(N-I))+IP                                                   
      SUM=0.0                                                           
      DO 100 J=KP1,N                                                    
      L1=L1+N                                                           
100   SUM=A(L1)*X(J)+SUM                                                
      L2=N*(K-1)+IP                                                     
110   X(K)=(X(K)-SUM)/A(L2)                                             
      RETURN                                                            
C*****ERROR HAS OCCURRED.  ABANDON SOLUTION.                            
120   NOGO=1                                                            
      RETURN                                                            
      END                                                               
