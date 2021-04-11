      SUBROUTINE OMGJ2                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE' 
C                                                                       
C                                                                       
      ISCT=TABL17(INODE,1)                                              
      IF (ISCT) 1,10000,2                                               
1     ISCT=IABS(ISCT)                                                   
      V1=ABS(VLOWS(ISCT))                                               
      GO TO 3                                                           
2     V1=ABS(VHIGHS(ISCT))                                              
3     ISCT=TABL17(INODE,2)                                              
      IF (ISCT) 4,10000,5                                               
4     ISCT=IABS(ISCT)                                                   
      V2=ABS(VLOWS(ISCT))                                               
      GO TO 6                                                           
5     V2=ABS(VHIGHS(ISCT))                                              
6     ISCT=TABL17(INODE,3)                                              
      IF (ISCT) 7,10000,8                                               
7     ISCT=IABS(ISCT)                                                   
      V3=ABS(VLOWS(ISCT))                                               
      GO TO 9                                                           
8     V3=ABS(VHIGHS(ISCT))                                              
9     GO TO (10,20,30,40,50,60,70,10),ISITU                             
10    DELP=0.0                                                          
      RETURN                                                            
20    DELV12=V1-V2                                                      
      GO TO (100,200,300),IEQUAT                                        
30    DELV21=V2-V1                                                      
      DELV23=V2-V3                                                      
      GO TO 400                                                         
40    DELV23=V2-V3                                                      
      GO TO (700,800,900),IEQUAT                                        
50    DELV32=V3-V2                                                      
      GO TO (1000,1100,1200),IEQUAT                                     
60    DELV12=V1-V2                                                      
      DELV32=V3-V2                                                      
      GO TO 1300                                                        
70    DELV21=V2-V1                                                      
      GO TO (1600,1700,1800),IEQUAT                                     
C*****                                                                  
C*****FLOW SITUATION 2                                                  
C*****                                                                  
100   IF(DELV12) 120,120,110                                            
C*****FLOW SITUATION 2A                                                 
110   DELP=-0.5*DELV12*DELV12                                           
      GO TO 10000                                                       
C*****FLOW SITUATION 2B                                                 
120   DELP=0.25*V2*DELV12                                               
      GO TO 10000                                                       
C*****FLOW SITUATION 2A + 2B                                            
200   DELP=-0.5*V1*V1                                                   
      GO TO 10000                                                       
300   IF(DELV12) 320,320,310                                            
C*****FLOW SITUATION 2A                                                 
310   DELP=0.5*V2*(V2-2.0*V1)                                           
      GO TO 10000                                                       
C*****FLOW SITUATION 2B                                                 
320   DELP=-0.25*(2.0*V1*V1+V2*DELV12)                                  
      GO TO 10000                                                       
C*****                                                                  
C*****FLOW SITUATION 3                                                  
C*****                                                                  
400   IF(DELV21) 420,420,410                                            
410   IF(DELV23) 450,450,430                                            
420   IF(DELV23) 460,460,440                                            
C*****FLOW SITUATION 3A                                                 
430   GO TO (470,480,490),IEQUAT                                        
C*****FLOW SITUATION 3B                                                 
440   GO TO (500,510,520),IEQUAT                                        
C*****FLOW SITUATION 3C                                                 
450   GO TO (530,540,550),IEQUAT                                        
C*****FLOW SITUATION 3D                                                 
460   GO TO (560,570,580),IEQUAT                                        
C*****FLOW SITUATION 3A                                                 
470   DELP=0.5*DELV21*DELV21                                            
      GO TO 10000                                                       
480   DELP=0.5*(V1-V3)*(V1+V3-2.0*V2)                                   
      GO TO 10000                                                       
490   DELP=-0.5*DELV23*DELV23                                           
      GO TO 10000                                                       
C*****FLOW SITUATION 3B                                                 
500   DELP=-0.25*V1*DELV21                                              
      GO TO 10000                                                       
510   DELP=-0.25*V1*DELV21-0.5*DELV23*DELV23                            
      GO TO 10000                                                       
520   DELP=-0.5*DELV23*DELV23                                           
      GO TO 10000                                                       
C*****FLOW SITUATION 3C                                                 
530   DELP=0.5*DELV21*DELV21                                            
      GO TO 10000                                                       
540   DELP=0.25*V3*DELV23+0.5*DELV21*DELV21                             
      GO TO 10000                                                       
550   DELP=0.25*V3*DELV23                                               
      GO TO 10000                                                       
C*****FLOW SITUATION 3D                                                 
560   DELP=-0.25*V1*DELV21                                              
      GO TO 10000                                                       
570   DELP=0.25*(V1-V3)*(V1+V3-V2)                                      
      GO TO 10000                                                       
580   DELP=0.25*V3*DELV23                                               
      GO TO 10000                                                       
C*****                                                                  
C*****FLOW SITUATION 4                                                  
C*****                                                                  
700   IF(DELV23) 720,720,710                                            
C*****FLOW SITUATION 4A                                                 
710   DELP=0.5*(DELV23*DELV23-V1*V1)                                    
      GO TO 10000                                                       
C*****FLOW SITUATION 4B                                                 
720   DELP=-0.25*(2.0*V1*V1+V3*DELV23)                                  
      GO TO 10000                                                       
C*****FLOW SITUATIONS 4A + 4B                                           
800   DELP=-0.5*V1*V1                                                   
      GO TO 10000                                                       
900   IF(DELV23) 920,920,910                                            
C*****FLOW SITUATION 4A                                                 
910   DELP=-0.5*DELV23*DELV23                                           
      GO TO 10000                                                       
920   DELP=0.25*V3*DELV23                                               
      GO TO 10000                                                       
C*****                                                                  
C*****FLOW SITUATION 5                                                  
C*****                                                                  
1000  IF(DELV32) 1020,1020,1010                                         
C*****FLOW SITUATION 5A                                                 
1010  DELP=0.5*V2*(2.0*V3-V2)                                           
      GO TO 10000                                                       
C*****FLOW SITUATION 5B                                                 
1020  DELP=+0.25*(2.0*V3*V3+V2*DELV32)                                  
      GO TO 10000                                                       
C*****FLOW SITUATIONS 5A + 5B                                           
1100  DELP=0.5*V3*V3                                                    
      GO TO 10000                                                       
1200  IF(DELV32) 1220,1220,1210                                         
C*****FLOW SITUATION 5A                                                 
1210  DELP=0.5*DELV32*DELV32                                            
      GO TO 10000                                                       
C*****FLOW SITUATION 5B                                                 
1220  DELP=-0.25*V2*DELV32                                              
      GO TO 10000                                                       
C*****                                                                  
C*****FLOW SITUATION 6                                                  
C*****                                                                  
1300  IF(DELV12) 1320,1320,1310                                         
1310  IF(DELV32) 1350,1350,1330                                         
1320  IF(DELV32) 1360,1360,1340                                         
C*****FLOW SITUATION 6A                                                 
1330  GO TO (1370,1380,1390),IEQUAT                                     
C*****FLOW SITUATION 6B                                                 
1340  GO TO (1400,1410,1420),IEQUAT                                     
C*****FLOW SITUATION 6C                                                 
1350  GO TO (1430,1440,1450),IEQUAT                                     
C*****FLOW SITUATION 6D                                                 
1360  GO TO (1460,1470,1480),IEQUAT                                     
C***** FLOW SITUATION 6A                                                
1370  DELP=-0.5*DELV12*DELV12                                           
      GO TO 10000                                                       
1380  DELP=0.5*(V3-V1)*(V3+V1-2.0*V2)                                   
      GO TO 10000                                                       
1390  DELP=0.5*DELV32*DELV32                                            
      GO TO 10000                                                       
C*****FLOW SITUATION 6B                                                 
1400  DELP=0.25*V2*DELV12                                               
      GO TO 10000                                                       
1410  DELP=0.5*DELV32*DELV32+0.25*V2*DELV12                             
      GO TO 10000                                                       
1420  DELP=0.5*DELV32*DELV32                                            
      GO TO 10000                                                       
C*****FLOW SITUATION 6C                                                 
1430  DELP=-0.5*DELV12*DELV12                                           
      GO TO 10000                                                       
1440  DELP=-0.25*V2*DELV32-0.5*DELV12*DELV12                            
      GO TO 10000                                                       
1450  DELP=-0.25*V2*DELV32                                              
      GO TO 10000                                                       
C*****FLOW SITUATION 6D                                                 
1460  DELP=0.25*V2*DELV12                                               
      GO TO 10000                                                       
1470  DELP=0.25*V2*(V1-V3)                                              
      GO TO 10000                                                       
1480  DELP=-0.25*V2*DELV32                                              
      GO TO 10000                                                       
C*****                                                                  
C*****FLOW SITUATION 7                                                  
C*****                                                                  
1600  IF(DELV21) 1620,1620,1610                                         
C*****FLOW SITUATION 7A                                                 
1610  DELP=0.5*DELV21*DELV21                                            
      GO TO 10000                                                       
C*****FLOW SITUATION 7B                                                 
1620  DELP=-0.25*V1*DELV21                                              
      GO TO 10000                                                       
C*****FLOW SITUATIONS 7A + 7B                                           
1700  DELP=0.5*V3*V3                                                    
      GO TO 10000                                                       
1800  IF(DELV21) 1820,1820,1810                                         
C*****FLOW SITUATION 7A                                                 
1810  DELP=0.5*(V3*V3-DELV21*DELV21)                                    
      GO TO 10000                                                       
C*****FLOW SITUATION 7B                                                 
1820  DELP=0.25*(2.0*V3*V3+V1*DELV21)
10000 DELP=(FLOAT(INT(DELP*1000000.0)))/1000000.0                                   
C10000 CONTINUE
      RETURN                                                            
      END                                                               
