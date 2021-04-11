      SUBROUTINE QUARRK                                                 
C                                                                       
C*****THIS SUBROUTINE PERFORMS ONE STEP OF THE RUNGE-KUTTA INTEGRATION  
C*****THE NAME QUARRK COMES FROM 'QUARTER RUNGE-KUTTA'.                 
C                                                                       
C     SUBROUTINE REFERENCES                                             
C                                                                       
C      (1) 'IMPROVING ROUND-OFF IN RUNGE-KUTTA COMPUTATIONS WITH GILL'S 
C          METHOD', BY ROBERT J. THOMPSON, COMMUNICATIONS OF THE        
C          ASSOCIATION FOR COMPUTING MACHINERY, VOL 13, NO 12,          
C          DECEMEBER 1970, PAGES 739-740                                
C                                                                       
C      (2) 'A PROCESS FOR THE STEP-BY-STEP INTEGRATION OF DIFFERENTIAL  
C          EQUATIONS IN AN AUTOMATIC DIGITAL COMPUTING MACHINE', BY     
C          S. GILL, PROC. CAMBRIDGE PHILOS. SOC. 47  1951  PAGES 96-108 
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      DIMENSION AA(4), BB(4), CC(4)                                     
      DATA AA/0.5,0.292893219,1.707106781,0.166666667/                  
      DATA BB/2.0,1.0,1.0,2.0/                                          
      DATA CC/0.5,0.292893219,1.707106781,0.5/                          
      DO 10 ILP=1,NLOOP                                                 
      DUMY1=QLP(ILP)                                                    
      QLP(ILP)=DUMY1+DELAE*AA(KASTEP)*(DQDTLP(ILP)-BB(KASTEP)*QERRLP(ILP
     1))                                                                
      DUMY1=(QLP(ILP)-DUMY1)/DELAE                                      
   10 QERRLP(ILP)=QERRLP(ILP)+3.0*DUMY1-CC(KASTEP)*DQDTLP(ILP)          
      RETURN                                                            
      END                                                               
