C                                                                       
C*****THIS SUBROUTINE FINDS THE FLOW SITUATION AT A JUNCTION (ISITU).   
C*****AIR FLOW INTO A JUNCTION IS TAKEN TO BE POSITIVE AND AIR FLOW OUT 
C*****OF A JUNCTION IS TAKEN AS NEGATIVE. A BINARY NUMBER IS CODED WITH 
C*****POSITIVE AIR FLOW FOR A BRANCH BEING 1 AND NEGATIVE AIR FLOW BEING
C*****0. THE BRANCHES ARE NUMBERED 1 THROUGH NBR(NUMBER OF BRANCHES AT  
C*****THE JUNCTION) IN THE ORDER THEY APPEAR IN ARRAY TABL17. FOR       
C*****EXAMPLE, A FOUR BRANCHED JUNCTION HAVING POSITIVE FLOW IN BRANCHES
C*****1 AND 3 AND NEGATIVE FLOW IN BRANCHES 2 AND 4 WOULD CODE 0101.    
C*****CONVERTING THIS TO BASE 10 GIVES 5. 1 IS ALSO ADDED TO ISITU SO   
C*****THAT ISITU ALWAYS RANGES FROM 1 TO 2**NBR - NOTE THAT 1 AND 2**NBR
C*****VIOLATE COMPRESSIBILITY. FOR THE EXAMPLE ISITU BECOMES 6. CONSIDER
C*****THE EXAMPLE WHERE THE AIR FLOW IS REVERSED FROM ABOVE. THEN ISITU 
C*****BECOMES 1+BASE 10 OF (1010) EQUALS 1+10 EQUALS 11.                
C                                                                       
      SUBROUTINE FLOWAY                                                 
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C*****INITIALIZE ISITU                                                  
      ISITU=1                                                           
C*****LOOP OVER BRANCHES OF JUNCTION                                    
      DO 50 IBR=1,NBR                                                   
      ISCT=TABL17(INODE,IBR)                                            
      IF(ISCT) 10,20,20                                                 
10    JSCT=IABS(ISCT)                                                   
      V=VLOWS(JSCT)                                                     
      GO TO 30                                                          
20    V=VHIGHS(ISCT)                                                    
30    IF(ISCT*V) 50,50,40                                               
C*****AIR FLOW IS COMING INTO JUNCTION                                  
40    ISITU=ISITU+2**(IBR-1)                                            
50    CONTINUE                                                          
      RETURN                                                            
      END                                                               
