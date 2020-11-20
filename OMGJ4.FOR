      SUBROUTINE OMGJ4                                                  
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
9     Q1=V1*A1                                                          
      Q2=V2*A2                                                          
      Q3=V3*A3                                                          
      ICND=TABL23(INODE)                                                
      C1=COAEND(ICND+3)                                                 
      C2=COAEND(ICND+4)                                                 
      C3=COAEND(ICND+5)                                                 
      C4=COAEND(ICND+6)                                                 
      C5=COAEND(ICND+7)                                                 
      C6=COAEND(ICND+8)                                                 
      C7=COAEND(ICND+9)                                                 
      C8=COAEND(ICND+10)                                                
      C9=COAEND(ICND+11)                                                
      C10=COAEND(ICND+12)                                               
      C11=COAEND(ICND+13)                                               
      C12=COAEND(ICND+14)                                               
      C13=COAEND(ICND+15)                                               
      C14=COAEND(ICND+16)                                               
      C15=COAEND(ICND+17)                                               
      C16=COAEND(ICND+18)                                               
      C17=COAEND(ICND+19)                                               
      C18=COAEND(ICND+20)                                               
      GO TO (100,200,300,400,500,600,700,100),ISITU                     
100   DELP=0.0                                                          
      RETURN                                                            
C*****FLOW SITUATION 2                                                  
200   GO TO (210,220,230),IEQUAT                                        
210   DELP=C1*V1*V1+C2*V1*Q2/A1+C3*((Q2/A1)**2)+0.5*V2*V2               
      GO TO 1000                                                        
220   DELP=C4*V1*V1+C5*V1*Q2/A1+C6*((Q2/A1)**2)                         
      GO TO 1000                                                        
230   DELP=(C4-C1)*V1*V1+(C5-C2)*V1*Q2/A1+(C6-C3)*((Q2/A1)**2)-0.5*V2*V2
      GO TO 1000                                                        
C*****FLOW SITUATION 3                                                  
300   GO TO (310,320,330),IEQUAT                                        
310   DELP=C7*V2*V2+C8*V2*Q1/A2+C9*((Q1/A2)**2)-0.5*V1*V1               
      GO TO 1000                                                        
320   DELP=(C7+C10)*V2*V2+(C8+C11)*Q1*V2/A2+(C9+C12)*((Q1/A2)**2)-0.5*V1
     1*V1                                                               
      GO TO 1000                                                        
330   DELP=C10*V2*V2+C11*Q1*V2/A2+C12*((Q1/A2)**2)                      
      GO TO 1000                                                        
C*****FLOW SITUATION 4                                                  
400   CDHCP1=COAEND(ICND)                                               
      BETA1=COAEND(ICND+1)                                              
      BETA2=COAEND(ICND+2)                                              
      GAMMA1=A1/A3                                                      
      GAMMA2=A2/A3                                                      
      LAMBDA=V2/V1                                                      
      CO1=LAMBDA*LAMBDA*BETA2-BETA1                                     
      CO2=-2.0*CO1                                                      
      CO5=-GAMMA1*GAMMA1                                                
      CO3=LAMBDA*LAMBDA*(GAMMA2*GAMMA2+BETA2) + CO5 - BETA1             
      CO4=-2.0*CO5                                                      
      NCUNT=-10                                                         
      XNEW=Q1/Q3                                                        
403   ALPHA=XNEW                                                        
      NCUNT=NCUNT+1                                                     
      FX=ALPHA*(ALPHA*(ALPHA*(CO1*ALPHA+CO2)+CO3)+CO4)+CO5              
      FXP=ALPHA*(ALPHA*(4.0*CO1*ALPHA+3.0*CO2)+2.0*CO3)+CO4             
      IF(ABS(FXP)-0.001)406,409,409                                     
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
440   DELP=0.5*CDHCP1*(((Q2/(1.0-ALPHA))**2)-((Q1/ALPHA)**2))/(A3*A3)-C1
     10*V2*V2+C4*V1*V1                                                  
      GO TO 1000                                                        
450   DELP=C4*V1*V1-0.5*CDHCP1*((Q1/(ALPHA*A3))**2)-0.5*V3*V3+(Q1*Q1/ALP
     1HA+Q2*Q2/(1.0-ALPHA))/(A3*A3)                                     
      GO TO 1000                                                        
460   DELP=C10*V2*V2-0.5*CDHCP1*((Q2/(A3*(1.0-ALPHA)))**2)-0.5*V3*V3+(Q1
     1*Q1/ALPHA+Q2*Q2/(1.0-ALPHA))/(A3*A3)                              
      GO TO 1000                                                        
C*****FLOW SITUATION 5                                                  
500   GO TO (510,520,530),IEQUAT                                        
510   DELP=(C7+C10)*V3*V3+(C8+C11)*Q1*V3/A3+(C9+C12)*((Q1/A3)**2)-0.5*V1
     1*V1                                                               
      GO TO 1000                                                        
520   DELP=C7*V3*V3+C8*V3*Q1/A3+C9*((Q1/A3)**2)-0.5*V1*V1               
      GO TO 1000                                                        
530   DELP=-C10*V3*V3-C11*V3*Q1/A3-C12*((Q1/A3)**2)                     
      GO TO 1000                                                        
C*****FLOW SITUATION 6                                                  
600   GO TO (610,620,630),IEQUAT                                        
610   DELP=C13*V2*V2+C14*Q1*V2/A2+C15*((Q1/A2)**2)-0.5*V1*V1            
      GO TO 1000                                                        
620   DELP=0.5*(V3*V3-V1*V1)                                            
      GO TO 1000                                                        
630   DELP=-C13*V2*V2-C14*Q1*V2/A2-C15*((Q1/A2)**2)+0.5*V3*V3           
      GO TO 1000                                                        
C*****FLOW SITUATION 7                                                  
700   GO TO (710,720,730),IEQUAT                                        
710   DELP=C16*V1*V1+C17*V1*Q2/A1+C18*((Q2/A1)**2)+0.5*V2*V2            
      GO TO 1000                                                        
720   DELP=C16*V1*V1+C17*V1*Q2/A1+C18*((Q2/A1)**2)+0.5*V3*V3            
      GO TO 1000                                                        
730   DELP=0.5*(V3*V3-V2*V2)                                            
1000  DELP=(FLOAT(INT(DELP*1000000.0)))/1000000.0                                   
C1000  CONTINUE
      RETURN                                                            
      END                                                               
