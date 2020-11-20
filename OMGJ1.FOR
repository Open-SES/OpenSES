      SUBROUTINE OMGJ1                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      ICND=TABL23(INODE)                                                
      CDHC=COAEND(ICND)                                                 
      ISCT=TABL17(INODE,1)                                              
      IF(ISCT) 10,10000,20                                              
10    ISCT=IABS(ISCT)                                                   
      V1=ABS(VLOWS(ISCT))                                               
      A1=ALOWS(ISCT)                                                    
      GO TO 30                                                          
20    V1=ABS(VHIGHS(ISCT))                                              
      A1=AHIGHS(ISCT)                                                   
30    ISCT=TABL17(INODE,2)                                              
      IF(ISCT) 40,10000,50                                              
40    ISCT=IABS(ISCT)                                                   
      V2=ABS(VLOWS(ISCT))                                               
      A2=ALOWS(ISCT)                                                    
      GO TO 60                                                          
50    V2=ABS(VHIGHS(ISCT))                                              
      A2=AHIGHS(ISCT)                                                   
60    ISCT=TABL17(INODE,3)                                              
      IF(ISCT) 70,10000,80                                              
70    ISCT=IABS(ISCT)                                                   
      V3=ABS(VLOWS(ISCT))                                               
      A3=ALOWS(ISCT)                                                    
      GO TO 90                                                          
80    V3=ABS(VHIGHS(ISCT))                                              
      A3=AHIGHS(ISCT)                                                   
90    ISCT=TABL17(INODE,4)                                              
      IF(ISCT) 100,10000,110                                            
100   ISCT=IABS(ISCT)                                                   
      V4=ABS(VLOWS(ISCT))                                               
      A4=ALOWS(ISCT)                                                    
      GO TO 120                                                         
110   V4=ABS(VHIGHS(ISCT))                                              
      A4=AHIGHS(ISCT)                                                   
120   Q1=V1*A1                                                          
      Q2=V2*A2                                                          
      Q3=V3*A3                                                          
      Q4=V4*A4                                                          
      GO TO (130,200,300,400,500,600,700,800,900,1000,1100,1200,1300,140
     10,1500,130),ISITU                                                 
130   DELP=0.0                                                          
      RETURN                                                            
C*****FLOW SITUATION 2                                                  
200   GO TO (210,220,230,240,250,260),IEQUAT                            
210   DELP=-0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q1)/(A1*A1)+0.5*(V2*V2-V1*V1) 
      GO TO 10000                                                       
220   DELP=-0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q1)/(A1*A1)+0.5*(V3*V3-V1*V1) 
      GO TO 10000                                                       
230   DELP=-0.5*V1*V1                                                   
      GO TO 10000                                                       
240   DELP=0.5*(V3*V3-V2*V2)                                            
      GO TO 10000                                                       
250   DELP=0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q1)/(A1*A1)-0.5*V2*V2          
      GO TO 10000                                                       
260   DELP=0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q1)/(A1*A1)-0.5*V3*V3          
      GO TO 10000                                                       
C*****FLOW SITUATION 3                                                  
300   GO TO (310,320,330,340,350,360),IEQUAT                            
310   DELP=0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q2)/(A2*A2)+0.5*(V2*V2-V1*V1)  
      GO TO 10000                                                       
320   DELP=0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q2)/(A2*A2)-0.5*V1*V1          
      GO TO 10000                                                       
330   DELP=0.5*(V4*V4-V1*V1)                                            
      GO TO 10000                                                       
340   DELP=-0.5*V2*V2                                                   
      GO TO 10000                                                       
350   DELP=-0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q2)/(A2*A2)+0.5*(V4*V4-V2*V2) 
      GO TO 10000                                                       
360   DELP=-0.53333333*(Q3+Q4)*(Q3+Q4-1.5*Q2)/(A2*A2)+0.5*V4*V4         
      GO TO 10000                                                       
C*****FLOW SITUATION 4                                                  
400   GO TO (410,420,430,440,450,460),IEQUAT                            
410   DELP=0.0                                                          
      GO TO 10000                                                       
420   DELP=0.5*(V2*V2+V4*V4*(1.0-CDHC)-V3*V3-3.0*V1*V1)                 
      GO TO 10000                                                       
430   DELP=-0.5*V4*V4*CDHC-V1*V1                                        
      GO TO 10000                                                       
440   DELP=0.5*(V2*V2+V4*V4*(1.0-CDHC)-V3*V3-3.0*V1*V1)                 
      GO TO 10000                                                       
450   DELP=-0.5*V4*V4*CDHC-V1*V1                                        
      GO TO 10000                                                       
460   DELP=0.5*(V1*V1-V2*V2+V3*V3-V4*V4)                                
      GO TO 10000                                                       
C*****FLOW SITUATION 5                                                  
500   GO TO (510,520,530,540,550,560),IEQUAT                            
510   DELP=0.53333333*(Q1+Q2)*(Q1+Q2-1.5*Q3)/(A3*A3)-0.5*V1*V1          
      GO TO 10000                                                       
520   DELP=0.53333333*(Q1+Q2)*(Q1+Q2-1.5*Q3)/(A3*A3)+0.5*(V3*V3-V1*V1)  
      GO TO 10000                                                       
530   DELP=0.5*(V4*V4-V1*V1)                                            
      GO TO 10000                                                       
540   DELP=0.5*V3*V3                                                    
      GO TO 10000                                                       
550   DELP=-0.53333333*(Q1+Q2)*(Q1+Q2-1.5*Q3)/(A3*A3)+0.5*V4*V4         
      GO TO 10000                                                       
560   DELP=-0.53333333*(Q1+Q2)*(Q1+Q2-1.5*Q3)/(A3*A3)+0.5*(V4*V4-V3*V3) 
      GO TO 10000                                                       
C*****FLOW SITUATION 6                                                  
600   GO TO (610,620,630,640,650,660),IEQUAT                            
610   DELP=-0.0375*Q3*(V2+V3)*(V2+V3)/(Q1+Q3)                           
      GO TO 10000                                                       
620   DELP=0.5*(V3*V3-5.0*V1*V1)                                        
      GO TO 10000                                                       
630   DELP=0.5*(V4*V4+V2*V2)-3.0*V1*V1-(V1-V2)*(V3+V4)-0.0375*Q3*(V2+V3)
     1*(V2+V3)/(Q1+Q3)                                                  
      GO TO 10000                                                       
640   DELP=0.5*(V3*V3-5.0*V1*V1)+0.0375*Q3*(V2+V3)*(V2+V3)/(Q1+Q3)      
      GO TO 10000                                                       
650   DELP=0.5*(V4*V4+V2*V2)-(V1-V2)*(V3+V4)-3.0*V1*V1                  
      GO TO 10000                                                       
660   DELP=0.5*(V4*V4+V2*V2-V3*V3-V1*V1)-(V1-V2)*(V1-V2+2.0*V3)-0.0375*Q
     13*(V2+V3)*(V2+V3)/(Q1+Q3)                                         
      GO TO 10000                                                       
C*****FLOW SITUATION 7                                                  
700   GO TO (710,720,730,740,750,760),IEQUAT                            
710   DELP=0.125*(V2*(V2+2.0*V3)-3.0*V3*V3)                             
      GO TO 10000                                                       
720   DELP=0.125*(V3*(V3+2.0*V2)-3.0*V2*V2)                             
      GO TO 10000                                                       
730   DELP=0.0                                                          
      GO TO 10000                                                       
740   DELP=0.5*(V3*V3-V2*V2)                                            
      GO TO 10000                                                       
750   DELP=0.125*(3.0*V3*V3-V2*(2.0*V3+V2))                             
      GO TO 10000                                                       
760   DELP=0.125*(3.0*V2*V2-V3*(2.0*V2+V3))                             
      GO TO 10000                                                       
C*****FLOW SITUATION 8                                                  
800   GO TO (810,820,830,840,850,860),IEQUAT                            
810   DELP=0.5*(V2*V2-V1*V1)                                            
      GO TO 10000                                                       
820   DELP=0.5*(V3*V3-V1*V1)                                            
      GO TO 10000                                                       
830   DELP=-0.96*(Q4**1.6)*((Q4-Q3)**0.4)/(A4*A4)+0.5*(V4*V4-V1*V1)     
      GO TO 10000                                                       
840   DELP=0.5*(V3*V3-V2*V2)                                            
      GO TO 10000                                                       
850   DELP=-0.96*(Q4**1.6)*((Q4-Q3)**0.4)/(A4*A4)+0.5*(V4*V4-V2*V2)     
      GO TO 10000                                                       
860   DELP=-0.96*(Q4**1.6)*((Q4-Q3)**0.4)/(A4*A4)+0.5*(V4*V4-V3*V3)     
      GO TO 10000                                                       
C*****FLOW SITUATION 9                                                  
900   GO TO (910,920,930,940,950,960),IEQUAT                            
910   DELP=-0.53333333*(Q2+Q1)*(Q2+Q1-1.5*Q4)/(A4*A4)+0.5*V2*V2         
      GO TO 10000                                                       
920   DELP=-0.53333333*(Q2+Q1)*(Q2+Q1-1.5*Q4)/(A4*A4)+0.5*V3*V3         
      GO TO 10000                                                       
930   DELP=0.5*V4*V4                                                    
      GO TO 10000                                                       
940   DELP=0.5*(V3*V3-V2*V2)                                            
      GO TO 10000                                                       
950   DELP=0.53333333*(Q2+Q1)*(Q2+Q1-1.5*Q4)/(A4*A4)+0.5*(V4*V4-V2*V2)  
      GO TO 10000                                                       
960   DELP=0.53333333*(Q2+Q1)*(Q2+Q1-1.5*Q4)/(A4*A4)+0.5*(V4*V4-V3*V3)  
      GO TO 10000                                                       
C*****FLOW SITUATION 10                                                 
1000  GO TO (1010,1020,1030,1040,1050,1060),IEQUAT                      
1010  DELP=0.125*(3.0*V4*V4-V1*(2.0*V4+V1))                             
      GO TO 10000                                                       
1020  DELP=0.125*(3.0*V4*V4-V1*(2.0*V4+V1))                             
      GO TO 10000                                                       
1030  DELP=0.5*(V4*V4-V1*V1)                                            
      GO TO 10000                                                       
1040  DELP=0.0                                                          
      GO TO 10000                                                       
1050  DELP=0.125*(V4*(V4+2.0*V1)-3.0*V1*V1)                             
      GO TO 10000                                                       
1060  DELP=0.125*(V4*(V4+2.0*V1)-3.0*V1*V1)                             
      GO TO 10000                                                       
C*****FLOW SITUATION 11                                                 
1100  GO TO (1110,1120,1130,1140,1150,1160),IEQUAT                      
1110  DELP=0.0375*Q4*(V1+V4)*(V1+V4)/(Q2+Q4)                            
      GO TO 10000                                                       
1120  DELP=0.5*(V3*V3+V1*V1)-3.0*V2*V2-(V2-V1)*(V3+V4)                  
      GO TO 10000                                                       
1130  DELP=0.5*(V4*V4-5.0*V2*V2)+0.0375*Q4*(V1+V4)*(V1+V4)/(Q2+Q4)      
      GO TO 10000                                                       
1140  DELP=0.5*(V3*V3+V1*V1)-3.0*V2*V2-(V2-V1)*(V4+V3)-0.0375*Q4*(V1+V4)
     1*(V1+V4)/(Q2+Q4)                                                  
      GO TO 10000                                                       
1150  DELP=0.5*(V4*V4-5.0*V2*V2)                                        
      GO TO 10000                                                       
1160  DELP=0.5*(V4*V4+V2*V2-V3*V3-V1*V1)+(V2-V1)*(V2-V1+2.0*V4)+0.0375*Q
     14*(V1+V4)*(V1+V4)/(Q2+Q4)                                         
      GO TO 10000                                                       
C*****FLOW SITUATION 12                                                 
1200  GO TO (1210,1220,1230,1240,1250,1260),IEQUAT                      
1210  DELP=0.5*(V2*V2-V1*V1)                                            
      GO TO 10000                                                       
1220  DELP=-0.96*(Q3**1.6)*((Q3-Q4)**0.4)/(A3*A3)+0.5*(V3*V3-V1*V1)     
      GO TO 10000                                                       
1230  DELP=0.5*(V4*V4-V1*V1)                                            
      GO TO 10000                                                       
1240  DELP=-0.96*(Q3**1.6)*((Q3-Q4)**0.4)/(A3*A3)+0.5*(V3*V3-V2*V2)     
      GO TO 10000                                                       
1250  DELP=0.5*(V4*V4-V2*V2)                                            
      GO TO 10000                                                       
1260  DELP=+0.96*(Q3**1.6)*((Q3-Q4)**0.4)/(A3*A3)+0.5*(V4*V4-V3*V3)     
      GO TO 10000                                                       
C*****FLOW SITUATION 13                                                 
1300  GO TO (1310,1320,1330,1340,1350,1360),IEQUAT                      
1310  DELP=0.5*(V3*V3-V4*V4+V1*V1-V2*V2)                                
      GO TO 10000                                                       
1320  DELP=0.5*(3.0*V3*V3-V4*V4-V2*V2*(1.0-CDHC)+V1*V1)                 
      GO TO 10000                                                       
1330  DELP=0.5*(3.0*V3*V3-V4*V4-V2*V2*(1.0-CDHC)+V1*V1)                 
      GO TO 10000                                                       
1340  DELP=V3*V3+0.5*V2*V2*CDHC                                         
      GO TO 10000                                                       
1350  DELP=V3*V3+0.5*V2*V2*CDHC                                         
      GO TO 10000                                                       
1360  DELP=0.0                                                          
      GO TO 10000                                                       
C*****FLOW SITUATION 14                                                 
1400  GO TO (1410,1420,1430,1440,1450,1460),IEQUAT                      
1410  DELP=-0.96*(Q2**1.6)*((Q2-Q1)**0.4)/(A2*A2)+0.5*(V2*V2-V1*V1)     
      GO TO 10000                                                       
1420  DELP=0.5*(V3*V3-V1*V1)                                            
      GO TO 10000                                                       
1430  DELP=0.5*(V4*V4-V1*V1)                                            
      GO TO 10000                                                       
1440  DELP=0.96*(Q2**1.6)*((Q2-Q1)**0.4)/(A2*A2)+0.5*(V3*V3-V2*V2)      
      GO TO 10000                                                       
1450  DELP=0.96*(Q2**1.6)*((Q2-Q1)**0.4)/(A2*A2)+0.5*(V4*V4-V2*V2)      
      GO TO 10000                                                       
1460  DELP=0.5*(V4*V4-V3*V3)                                            
      GO TO 10000                                                       
C*****FLOW SITUATION 15                                                 
1500  GO TO (1510,1520,1530,1540,1550,1560),IEQUAT                      
1510  DELP=0.96*(Q1**1.6)*((Q1-Q2)**0.4)/(A1*A1)+0.5*(V2*V2-V1*V1)      
      GO TO 10000                                                       
1520  DELP=0.96*(Q1**1.6)*((Q1-Q2)**0.4)/(A1*A1)+0.5*(V3*V3-V1*V1)      
      GO TO 10000                                                       
1530  DELP=0.96*(Q1**1.6)*((Q1-Q2)**0.4)/(A1*A1)+0.5*(V4*V4-V1*V1)      
      GO TO 10000                                                       
1540  DELP=0.5*(V3*V3-V2*V2)                                            
      GO TO 10000                                                       
1550  DELP=0.5*(V4*V4-V2*V2)                                            
      GO TO 10000                                                       
1560  DELP=0.5*(V4*V4-V3*V3)                                            
10000 DELP=(FLOAT(INT(DELP*1000000.0)))/1000000.0                                   
C10000 CONTINUE
      RETURN                                                            
      END                                                               
