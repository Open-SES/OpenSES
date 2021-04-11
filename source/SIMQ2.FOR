C     THIS SUBROUTINE SOLVES, BY CHOLESKY DECOMPOSITION, THE LINEAR SET 
C     OF EQUATIONS B*DQDTLP=OMEGLP WHERE THE COEFFICIENT MATRIX B IS    
C     SYMMETRIC, POSITIVE DEFINITE AND OF KNOWN BANDWIDTH. IN GENERAL,  
C     A BANDED MATRIX IS DESCRIBED BY AN UPPER BANDWIDTH (THE NUMBER OF 
C     UPPER DIAGONALS NECESSARY TO INCLUDE ALL THE NON-ZERO ELEMENTS    
C     PLUS ONE - THE PRINCIPAL DIAGONAL) AND A LOWER BANDWIDTH (THE     
C     NUMBER OF LOWER DIAGONALS NECESSARY TO INCLUDE ALL THE NON-ZERO   
C     ELEMENTS PLUS ONE - THE PRINCIPAL DIAGONAL). HOWEVER, A SYMMETRIC 
C     MATRIX NEED ONLY BE DESCRIBED BY THE HALF BANDWIDTH, WHICH IS     
C     EITHER THE UPPER OR LOWER BANDWIDTH AND FORMALLY IS THE NUMBER M  
C     SUCH THAT B(I,J)=0.0 FOR ABS(I-J) GREATER THAN OR EQUAL TO M.     
C     STORAGE SPACE IS CONSERVED BY STORING ONLY THE NECESSARY 'HALF'   
C     (I.E. EITHER THE UPPER OR LOWER TRIANGULAR MATRIX) IN ASCENDING   
C     ORDER IN THE ARRAY BLP EITHER (1) ROWWISE-UPPER HALF OR (2) COLUMN
C     WISE-LOWER HALF - IT MAKES NO DIFFERENCE DUE TO SYMMETRY. THIS    
C     METHOD OF STORAGE REQUIRES THAT EACH ROW OR COLUMN WITH THE       
C     EXCEPTION OF THE LAST ONE BE COMPLETED, WHERE NECESSARY, TO M     
C     ELEMENTS BY INSERTING OR DELETING ZEROS. CONSIDER THE FOLLOWING   
C     COEFFICIENT MATRIX HAVING AN ORDER OF 7 (7 X 7) AND A HALF        
C     BANDWIDTH M=5                                                     
C                                                                       
C     1  2  3  4  5  0  0                   1  2  3  4  5  0  0         
C     2  3  4  5  6  7  0                      3  4  5  6  7  0         
C     3  4  5  6  7  8  9   WHICH HAS THE         5  6  7  8  9         
C     4  5  6  7  8  9 10   UPPER HALF SHOWN         7  8  9 10         
C     5  6  7  8  9 10 11   TO THE RIGHT                9 10 11         
C     0  7  8  9 10 11 12                                 11 12         
C     0  0  9 10 11 12 13                                    13         
C                                                                       
C     IF THE ZEROS OUTSIDE THE BAND ARE DELETED AND THE ROWS ARE        
C     COMPLETED BY INSERTING ZEROS THEN THE FOLLOWING RESULTS           
C                                                                       
C     1  2  3  4  5                                                     
C        3  4  5  6  7                                                  
C           5  6  7  8  9                                               
C              7  8  9 10  0                                            
C                 9 10 11  0  0                                         
C                   11 12  0  0  0                                      
C                      13  0  0  0  0                                   
C                                                                       
C     WHICH ROWWISE IS AS FOLLOWS                                       
C                                                                       
C     1 2 3 4 5 3 4 5 6 7 5 6 7 8 9 7 8 9 10 0 9 10 11 0 0 11 12 0 0 0  
C     13 0 0 0 0                                                        
C                                                                       
C     WHICH IS STORED AS FOLLOWS IN ARRAY BLP                           
C                                                                       
C     1 2 3 4 5 3 4 5 6 7 5 6 7 8 9 7 8 9 10 0 9 10 11 0 0 11 12 0 0 0  
C     13                                                                
C                                                                       
C     SINCE THE LAST 4 ZEROS ARE NOT REQUIRED THE STORAGE AREA FOR A    
C     MATRIX HAVING AN ORDER N AND A HALF BANDWIDTH OF M IS N X M - M   
C     +1 - IN THIS CASE 7 X 5 - 5 + 1 = 31. AN ADVANTAGE OF THIS STORAGE
C     METHOD IS THAT THE BEGINNING ELEMENT OF ROW K, B(K,K), IS STORED  
C     IN BLP(M*(K-1)+1), WHICH HAS AN EASILY COMPUTED SUBSCRIPT. THE    
C     ARRAY BLP BECOMES THE DECOMPOSED FORM OF THE MATRIX B. IF THE     
C     MATRIX B IS FOUND TO BE NOT POSITIVE DEFINITE, THE COMPUTATION    
C     SCHEME IS ABORTED AND AN ERROR MESSAGE IS WRITTEN. IF THE MATRIX  
C     B HAS NOT BEEN CHANGED BETWEEN CALLS TO THIS SUBROUTINE THEN ITS  
C     DECOMPOSED FORM HAS NOT CHANGED EITHER - HENCE FOR THIS CASE THE  
C     DECOMPOSITION PORTION OF THE COMPUTATION IS BYPASSED. IT SHOULD BE
C     NOTED THAT A CORRESPONDING BYPASS IS MADE IN SUBROUTINE QDERIV TO 
C     AVOID DESTROYING THE DECOMPOSED MATRIX STORED IN ARRAY BLP.       
C                                                                       
C     SUBROUTINE REFERENCES (INCLUDING FLOW CHARTS)                     
C                                                                       
C      (1) 'ALGORITHMUS 1 - LINEARES GLEICHUNGS-SYSTEM MIT SYMMETRISCHER
C          POSITIV-DEFINITER BANDMATRIX NACH CHOLESKY-COMPUTING', BY    
C          H. RUTISHAUSER, ARCHIVES FOR ELECTRONIC COMPUTING, VOL. 1    
C          ISS. 1  1966  PAGES 77-78                                    
C                                                                       
C      (2) 'COMPUTER PROGRAMS FOR STRUCTURAL ANALYSIS', BY WILLIAM      
C          WEAVER PUBLISHED BY VAN NOSTRAND  1967  PAGES 56-68          
C                                                                       
      SUBROUTINE SIMQ2                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      DATA WEETOL/1.0E-15/                                              
      DATA BIGTOL/1.0E+15/                                              
      DO 10 ILP=1,NLOOP                                                 
10    DQDTLP(ILP)=OMEGLP(ILP)                                           
      IF (KBND1-1) 40,20,40                                             
20    DO 30 NROW=1,NLOOP                                                
      IF (BLP(NROW)) 30,450,30                                          
30    DQDTLP(NROW)=DQDTLP(NROW)/BLP(NROW)                               
      RETURN                                                            
40    IF (KAERO) 350,50,350                                             
50    DE=0.0                                                            
      MP=KBND1+1                                                        
      KK=2                                                              
      FAC=0.0                                                           
      IF (BLP(1)) 450,450,60                                            
60    DE=BLP(1)                                                         
      BLP(1)=1.0/SQRT(BLP(1))                                           
      BIGL=BLP(1)                                                       
      SML=BLP(1)                                                        
      BLP(2)=BLP(2)*BLP(1)                                              
      TEMP=BLP(MP)-BLP(2)*BLP(2)                                        
      IF (TEMP) 450,450,70                                              
70    BLP(MP)=1.0/SQRT(TEMP)                                            
      DE=DE*TEMP                                                        
      IF (BLP(MP)-BIGL) 90,90,80                                        
80    BIGL=BLP(MP)                                                      
      GO TO 110                                                         
90    IF (SML-BLP(MP)) 110,110,100                                      
100   SML=BLP(MP)                                                       
110   IF (NLOOP-2) 120,340,120                                          
120   MP=MP+KBND1                                                       
      DO 330 J=MP,KBND3,KBND1                                           
      JP=J-KBND2                                                        
      MZC=0                                                             
      IF (KK-KBND1) 130,140,140                                         
130   KK=KK+1                                                           
      II=1                                                              
      JC=1                                                              
      GO TO 150                                                         
140   KK=KK+KBND1                                                       
      II=KK-KBND2                                                       
      JC=KK-KBND2                                                       
150   DO 170 I=KK,JP,KBND2                                              
      IF (BLP(I)) 180,160,180                                           
160   JC=JC+KBND1                                                       
170   MZC=MZC+1                                                         
      ASUM1=0.0                                                         
      GO TO 240                                                         
180   MMZC=KBND2*MZC                                                    
      II=II+MZC                                                         
      KM=KK+MMZC                                                        
      BLP(KM)=BLP(KM)*BLP(JC)                                           
      IF (KM-JP) 190,220,220                                            
190   KJ=KM+KBND2                                                       
      DO 210 I=KJ,JP,KBND2                                              
      ASUM2=0.0                                                         
      IM=I-KBND2                                                        
      II=II+1                                                           
      KI=II+MMZC                                                        
      DO 200 K=KM,IM,KBND2                                              
      ASUM2=ASUM2+BLP(KI)*BLP(K)                                        
200   KI=KI+KBND2                                                       
210   BLP(I)=(BLP(I)-ASUM2)*BLP(KI)                                     
220   ASUM1=0.0                                                         
      DO 230 K=KM,JP,KBND2                                              
230   ASUM1=ASUM1+BLP(K)*BLP(K)                                         
240   S=BLP(J)-ASUM1                                                    
      IF (S) 450,450,250                                                
250   BLP(J)=1./SQRT(S)                                                 
      DE=DE*S                                                           
      IF (DE-WEETOL) 260,260,270                                        
260   DE=DE*BIGTOL                                                      
      GO TO 290                                                         
270   IF (BIGTOL-DE) 280,280,290                                        
280   DE=DE*WEETOL                                                      
290   IF (BLP(J)-BIGL) 310,310,300                                      
300   BIGL=BLP(J)                                                       
      GO TO 330                                                         
310   IF (SML-BLP(J)) 330,320,320                                       
320   SML=BLP(J)                                                        
330   CONTINUE                                                          
340   IF (FAC*BIGL-SML) 350,450,450                                     
350   DQDTLP(1)=DQDTLP(1)*BLP(1)                                        
      KK=1                                                              
      K1=1                                                              
      J=1                                                               
      DO 400 L=2,NLOOP                                                  
      BSUM1=0.0                                                         
      LM=L-1                                                            
      J=J+KBND1                                                         
      IF (KK-KBND1) 360,370,370                                         
360   KK=KK+1                                                           
      GO TO 380                                                         
370   KK=KK+KBND1                                                       
      K1=K1+1                                                           
380   JK=KK                                                             
      DO 390 K=K1,LM                                                    
      BSUM1=BSUM1+BLP(JK)*DQDTLP(K)                                     
390   JK=JK+KBND2                                                       
400   DQDTLP(L)=(DQDTLP(L)-BSUM1)*BLP(J)                                
      DQDTLP(NLOOP)=DQDTLP(NLOOP)*BLP(KBND3)                            
      NMM=KBND3                                                         
      NN=NLOOP-1                                                        
      NDZ=NLOOP                                                         
      DO 440 L=1,NN                                                     
      BSUM2=0.0                                                         
      NL=NLOOP-L                                                        
      NL1=NL+1                                                          
      NMM=NMM-KBND1                                                     
      NJ1=NMM                                                           
      IF (L-KBND1) 420,410,410                                          
410   NDZ=NDZ-1                                                         
420   DO 430 K=NL1,NDZ                                                  
      NJ1=NJ1+1                                                         
430   BSUM2=BSUM2+BLP(NJ1)*DQDTLP(K)                                    
440   DQDTLP(NL)=(DQDTLP(NL)-BSUM2)*BLP(NMM)                            
      RETURN                                                            
450   CALL SIMERR (7)                                                   
      RETURN                                                            
      END                                                               
