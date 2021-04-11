C                                                                       
C*****COMPUTE SECTION FLOWRATES AND SECTION FLOWRATE DERIVATIVES FOR    
C*****EACH SECTION BY SUMMING THE LOOP FLOWRATES AND LOOP FLOWRATE      
C*****DERIVATIVES. THE NAME QSQLP COMES FROM 'QS FROM QLP'.             
      SUBROUTINE QSQLP                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C*****LOOP OVER SECTIONS                                                
      DO 30 ISCT=1,NSEC                                                 
      NLOW=NDSS(ISCT)                                                   
      NHIGH=NDSS(ISCT+1)-1                                              
      SUM1=0.0                                                          
      SUM2=0.0                                                          
C*****LOOP OVER LOOPS THAT PASS THROUGH EACH SECTION, PERFORMING SUMMATI
      DO 10 L=NLOW,NHIGH                                                
      ILP=LKAN(L)                                                       
      IABILP=IABS(ILP)                                                  
      ILP=IABILP/ILP                                                    
      SUM1=SUM1+ILP*DQDTLP(IABILP)                                      
   10 SUM2=SUM2+ILP*QLP(IABILP)                                         
      DQDTS(ISCT)=SUM1                                                  
C*****SAVE FLOWRATES FOR PRINTING - IF POSSIBLY NECESSARY               
      IF(KAERO) 30,20,30                                                
   20 QSAVES(ISCT)=SUM2                                                 
   30 QS(ISCT)=SUM2                                                     
      RETURN                                                            
      END                                                               
