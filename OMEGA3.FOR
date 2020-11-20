      SUBROUTINE OMEGA3                                                 
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
                                                                        
C                                                                       
C                                                                       
C*****LOOP OVER ALL NODES IN SYSTEM                                     
      DO 200 INODE=1,NNODE                                              
C*****COMPUTE RANGE OF LOCATIONS IN ARRAY TABL2 THAT LOOP THROUGH NODE  
C*****DATA IS STORED                                                    
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-3*LMTRRT-2                                   
C*****CHECK IF NODE IS A MULTIPLY-BRANCHED JUNCTION                     
      IF(NHIGH-NLOW) 200,10,10                                          
C*****NODE IS A MULTIPLY BRANCHED JUNCTION. FIND TYPE.                  
10    NDTYPA=TABL2(NHIGH+1)                                             
C*****IF NODE TYPE IS SEVEN THEN NO CALCULATIONS ARE TO BE DONE         
      IF (NDTYPA-7) 20,200,20                                           
C                                                                       
C*****COMPUTATION OF JUNCTION EFFECT ON AIRFLOW FORCING FUNCTION        
C                                                                       
C*****FIND NUMBER OF BRANCHES AT JUNCTION                               
20    NBR=TABL16(INODE)                                                 
C*****FIND FLOW SITUATION AT JUNCTION                                   
      CALL FLOWAY                                                       
C*****LOOP OVER ALL LOOPS PASSING THROUGH NODE                          
      DO 100 ITBL2=NLOW,NHIGH,3                                         
C*****FIND LOOP NUMBER                                                  
      ILOOP=TABL2(ITBL2)                                                
C*****FIND EQUATION SET TO BE USED                                      
      IEQUAT=TABL2(ITBL2+1)                                             
C*****FIND SIGN OF EQUATION SET TO BE USED                              
      SIGN=TABL2(ITBL2+2)                                               
      GO TO (30,40,50,60,70,80,200), NDTYPA                             
C*****AERODYNAMIC JUNCTION TYPE 1 - TUNNEL-TO-TUNNEL CROSSOVER          
30    CALL OMGJ1                                                        
      GO TO 100                                                         
C*****AERODYNAMIC JUNCTION TYPE 2 - DIVIDING WALL TERMINATION           
40    CALL OMGJ2                                                        
      GO TO 100                                                         
C*****AERODYNAMIC JUNCTION TYPE 3 - 'T' JUNCTION                        
50    CALL OMGJ3                                                        
      GO TO 100                                                         
C*****AERODYNAMIC JUNCTION TYPE 4 - ANGLED JUNCTION                     
60    CALL OMGJ4                                                        
      GO TO 100                                                         
C*****AERODYNAMIC JUNCTION TYPE 5 - 'Y' JUNCTION                        
70    CALL OMGJ5                                                        
      GO TO 100                                                         
C*****AERODYNAMIC JUNCTION TYPE 6 - RESERVED - SEE USERS + PROG. MANUALS
80    CALL OMGJ6                                                        
C*****ADD HEAD CHANGE ACROSS NODE TO LOOP FORCING FUNCTION              
100   OMEGLP(ILOOP)=OMEGLP(ILOOP)+SIGN*DELP                             
      IF(KAERO) 200,102,200                                             
C                                                                       
C*****COMPUTATION OF JUNCTION EFFECT ON TRAIN DRAG                      
C*****INITIALIZE LOCATION IN ARRAY TABL2 WHERE TRAIN-AT-NODE DATA       
C*****MAY BE STORED                                                     
102   ITBL2=NHIGH-1                                                     
C*****LOOP OVER MAXIMUM NUMBER OF TRAINS THAT MAY BE AT NODE            
      DO 190 ITRAN=1,LMTRRT                                             
C*****COMPUTE LOCATION IN ARRAY TABL2 WHERE TRAIN-AT-NODE DATA          
C*****MAY BE STORED                                                     
      ITBL2=ITBL2+3                                                     
C*****FIND TRAIN NUMBER                                                 
      NUMV=TABL2(ITBL2)                                                 
C*****IF TRAIN NUMBER IS ZERO, THEN ALL CALCULATIONS FOR ALL            
C*****OF THE TRAINS AT THIS NODE (IF ANY) HAVE BEEN COMPLETED           
      IF (NUMV) 200,200,110                                             
C*****COMPUTE EQUATION SET NUMBER AND SIGN                              
110   ISCTB=TABL2(ITBL2+1)                                              
      ISCTA=TABL2(ITBL2+2)                                              
      CALL EQUAT(ISCTB,ISCTA)                                           
      IEQUAT=ISCTB                                                      
      SIGN=ISCTA                                                        
      GO TO (120,130,140,150,160,170,200),NDTYPA                        
C*****AERODYNAMIC JUNCTION TYPE 1 - TUNNEL-TO-TUNNEL CROSSOVER          
120   CALL OMGJ1                                                        
      GO TO 180                                                         
C*****AERODYNAMIC JUNCTION TYPE 2 - DIVIDING WALL TERMINATION           
130   CALL OMGJ2                                                        
      GO TO 180                                                         
C*****AERODYNAMIC JUNCTION TYPE 3 - 'T' JUNCTION                        
140   CALL OMGJ3                                                        
      GO TO 180                                                         
C*****AERODYNAMIC JUNCTION TYPE 4 - ANGLED JUNCTION                     
150   CALL OMGJ4                                                        
      GO TO 180                                                         
C*****AERODYNAMIC JUNCTION TYPE 5 - 'Y' JUNCTION                        
160   CALL OMGJ5                                                        
      GO TO 180                                                         
C*****AERODYNAMIC JUNCTION TYPE 6 - RESERVED - SEE USERS + PROG. MANUALS
170   CALL OMGJ6                                                        
C*****CLEAR PORTION OF TABL2 USED BY THIS TRAIN AT NODE                 
180   TABL2(ITBL2)=0                                                    
      TABL2(ITBL2+1)=0                                                  
      TABL2(ITBL2+2)=0                                                  
C*****ADD HEAD CHANGE ACROSS NODE TO TRAIN HEAD CHANGE                  
190   DRAGV(NUMV)=DRAGV(NUMV)+SIGN*DELP                                 
200   CONTINUE                                                          
      RETURN                                                            
      END                                                               
