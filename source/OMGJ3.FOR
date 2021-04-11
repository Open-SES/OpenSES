      SUBROUTINE OMGJ3                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      REAL LAMBDA                                                       
      ISCT=TABL17(INODE,1)                                              
      IF (ISCT) 1,1000,2                                                
1     ISCT=IABS(ISCT)                                                   
      V1=ABS(VLOWS(ISCT))                                               
      A1=ALOWS(ISCT)                                                    
      GO TO 3                                                           
2     V1=ABS(VHIGHS(ISCT))                                              
      A1=AHIGHS(ISCT)                                                   
3     ISCT=TABL17(INODE,2)                                              
      IF (ISCT) 4,1000,5                                                
4     ISCT=IABS(ISCT)                                                   
      V2=ABS(VLOWS(ISCT))                                               
      A2=ALOWS(ISCT)                                                    
      GO TO 6                                                           
5     V2=ABS(VHIGHS(ISCT))                                              
      A2=AHIGHS(ISCT)                                                   
6     ISCT=TABL17(INODE,3)                                              
      IF (ISCT) 7,1000,8                                                
7     ISCT=IABS(ISCT)                                                   
      V3=ABS(VLOWS(ISCT))                                               
      A3=ALOWS(ISCT)                                                    
      GO TO 9                                                           
8     V3=ABS(VHIGHS(ISCT))                                              
      A3=AHIGHS(ISCT)                                                   
9     Q1=A1*V1                                                          
      Q2=A2*V2                                                          
      Q3=A3*V3                                                          
      GO TO (100,200,300,400,500,600,700,100),ISITU                     
100   DELP=0.0                                                          
      RETURN                                                            
C                                                                       
C*****FLOW SITUATION 2                                                  
C                                                                       
200   DELV12=V1-V2                                                      
      GO TO (210,240,250),IEQUAT                                        
210   IF(DELV12) 230,230,220                                            
C*****FLOW SITUATION 2A                                                 
220   DELP=-0.5*DELV12*DELV12                                           
      GO TO 1000                                                        
C*****FLOW SITUATION 2B                                                 
230   DELP=0.25*V2*DELV12                                               
      GO TO 1000                                                        
C*****FLOW SITUATIONS 2A + 2B                                           
240   DELP=-0.5*V1*V1                                                   
      GO TO 1000                                                        
250   IF(DELV12) 270,270,260                                            
260   DELP=0.5*V2*(V2-2.0*V1)                                           
      GO TO 1000                                                        
C*****FLOW SITUATION 2B                                                 
270   DELP=-0.25*(V2*DELV12+2.0*V1*V1)                                  
      GO TO 1000                                                        
C                                                                       
C*****FLOW SITUATION 3                                                  
C                                                                       
300   DELV12=V1-V2                                                      
      GO TO (310,340,370),IEQUAT                                        
310   IF(DELV12) 320,330,330                                            
C*****FLOW SITUATION 3A                                                 
320   DELP=0.5*DELV12*DELV12                                            
      GO TO 1000                                                        
C*****FLOW SITUATION 3B                                                 
330   DELP=0.25*V1*DELV12                                               
      GO TO 1000                                                        
340   IF(DELV12) 350,360,360                                            
C*****FLOW SITUATION 3A                                                 
350   DELP=0.5*V1*(V1-2.0*V2)                                           
      GO TO 1000                                                        
C*****FLOW SITUATION 3B                                                 
360   DELP=0.25*(V1*DELV12-2.0*V2*V2)                                   
      GO TO 1000                                                        
C*****FLOW SITUATIONS 3A + 3B                                           
370   DELP=-0.5*V2*V2                                                   
      GO TO 1000                                                        
C                                                                       
C*****FLOW SITUATION 4                                                  
C                                                                       
400   ICND=TABL23(INODE)                                                
      CDHCP1=COAEND(ICND)                                               
      GAMMA1=A1/A3                                                      
      GAMMA2=A2/A3                                                      
      LAMBDA=V2/V1                                                      
      CO1=(LAMBDA*LAMBDA-1.0)/CDHCP1                                    
      CO2=-2.0*CO1                                                      
      CO5=-GAMMA1*GAMMA1                                                
      CO3=CO1 + (LAMBDA*GAMMA2)*(LAMBDA*GAMMA2) + CO5                   
      CO4=-2.0*CO5                                                      
      NCUNT=-10                                                         
      XNEW=Q1/Q3                                                        
403   ALPHA=XNEW                                                        
      NCUNT=NCUNT+1                                                     
      FX=ALPHA*(ALPHA*(ALPHA*(ALPHA*CO1+CO2)+CO3)+CO4)+CO5              
      FXP=ALPHA*(ALPHA*(ALPHA*4.0*CO1+3.0*CO2)+2.0*CO3)+CO4             
      IF(ABS(FXP)-0.001) 406,409,409                                    
406   ALPHA=Q1/Q3                                                       
      GO TO 416                                                         
409   XNEW=ALPHA-FX/FXP                                                 
      IF(ABS(XNEW-ALPHA)-0.01)415,415,412                               
412   IF(NCUNT)403,415,415                                              
415   ALPHA=XNEW                                                        
416   IF(ALPHA-0.97)421,418,418                                         
418   ALPHA=0.97                                                        
      GO TO 430                                                         
421   IF(ALPHA-0.03)424,424,430                                         
424   ALPHA=0.03                                                        
430   GO TO (440,450,460),IEQUAT                                        
440   DELP=0.5*(CDHCP1*((Q2/(1.0-ALPHA))**2-(Q1/ALPHA)**2)/(A3*A3)+V2*V2
     1-V1*V1)                                                           
      GO TO 1000                                                        
450   DELP=(Q1*Q1/ALPHA+Q2*Q2/(1.0-ALPHA)-0.5*CDHCP1*((Q1/ALPHA)**2))/(A
     13*A3)-0.5*(V3*V3+V1*V1)                                           
      GO TO 1000                                                        
460   DELP=(Q1*Q1/ALPHA+Q2*Q2/(1.0-ALPHA)-0.5*CDHCP1*((Q2/(1.0-ALPHA))**
     12))/(A3*A3)-0.5*(V3*V3+V2*V2)                                     
      GO TO 1000                                                        
                                                                        
                                                                        
C                                                                       
C*****FLOW SITUATION 5                                                  
C                                                                       
500   GO TO (510,520,530),IEQUAT                                        
510   DELP=0.5*(V1*V1+V2*V2*(1.0-2.0*A2/A1))                            
      GO TO 1000                                                        
520   DELP=0.5*(Q2*(V1*V1-V2*V2*(2.0*A2/A1-1.0))/Q3+V3*V3)              
      GO TO 1000                                                        
530   DELP=0.5*(Q1*(V2*V2*(2.0*A2/A1-1.0)-V1*V1)/Q3+V3*V3)              
      GO TO 1000                                                        
C                                                                       
C*****FLOW SITUATION 6                                                  
C                                                                       
600   GO TO (610,620,630),IEQUAT                                        
610   DELP=0.5*(V1*V1*(2.0*A1/A2-1.0)-V2*V2)                            
      GO TO 1000                                                        
620   DELP=0.5*(V3*V3-V1*V1)                                            
      GO TO 1000                                                        
630   DELP=0.5*(V3*V3+V2*V2)-A1*V1*V1/A2                                
      GO TO 1000                                                        
C                                                                       
C*****FLOW SITUATION 7                                                  
C                                                                       
700   GO TO (710,720,730),IEQUAT                                        
710   DELP=0.5*(V1*V1+V2*V2*(1.0-2.0*A2/A1))                            
      GO TO 1000                                                        
720   DELP=0.5*(V1*V1+V3*V3)-A2*V2*V2/A1                                
      GO TO 1000                                                        
730   DELP=0.5*(V3*V3-V2*V2)                                            
1000  DELP=(FLOAT(INT(DELP*1000000.0)))/1000000.0                                   
C1000  CONTINUE
      RETURN                                                            
      END                                                               
