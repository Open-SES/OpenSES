      SUBROUTINE EQUAT(J1,J2)                                           
C*****THIS SUBROUTINE FINDS THE NUMBER OF THE EQUATION SET TO BE USED   
C*****AND THE SIGN TO BE USED WITH IT. AN EQUATION SET CONSISTS OF THE  
C*****EQUATIONS USED IN FINDING THE HEAD CHANGE BETWEEN TWO BRANCHES OF 
C*****THE JUNCTION. THE NUMBER OF EQUATIONS IN EACH SET IS EQUAL TO THE 
C*****NUMBER OF FLOW SITUATIONS THAT MAY OCCUR IN THE JUNCTION - THERE  
C*****BEING ONE EQUATION FOR EACH FLOW SITUATION. THE INPUT TO THE      
C*****SUBROUTINE IS THE 'FROM' BRANCH NUMBER AND THE 'TO' BRANCH NUMBER.
C*****BY EXAMPLE THE NUMBER OF THE EQUATION SET AND THE SIGN ARE DEFINED
C*****AS FOLLOWS                                                        
C*****                    FROM     TO    EQUATION  SIGN                 
C*****                   BRANCH  BRANCH     SET                         
C*****                                    NUMBER                        
C*****3 BRANCH JUNCTION     1       2        1      +1                  
C*****                      1       3        2      +1                  
C*****                      2       3        3      +1                  
C*****                      2       1        1      -1                  
C*****                      3       1        2      -1                  
C*****                      3       2        3      -1                  
C*****4 BRANCH JUNCTION     1       2        1      +1                  
C*****                      1       3        2      +1                  
C*****                      1       4        3      +1                  
C*****                      2       3        4      +1                  
C*****                      2       4        5      +1                  
C*****                      3       4        6      +1                  
C*****                      2       1        1      -1                  
C*****                      3       1        2      -1                  
C*****                      4       1        3      -1                  
C*****                      3       2        4      -1                  
C*****                      4       2        5      -1                  
C*****                      4       3        6      -1                  
C*****5 BRANCH JUNCTION     1       2        1      +1                  
C*****                      1       3        2      +1                  
C*****                      1       4        3      +1                  
C*****                      1       5        4      +1                  
C*****                      2       3        5      +1                  
C*****                      2       4        6      +1                  
C*****                      2       5        7      +1                  
C*****                      3       4        8      +1                  
C*****                      3       5        9      +1                  
C*****                      4       5       10      +1                  
C*****                      2       1        1      -1                  
C*****                      3       1        2      -1                  
C*****                      4       1        3      -1                  
C*****                      5       1        4      -1                  
C*****                      3       2        5      -1                  
C*****                      4       2        6      -1                  
C*****                      5       2        7      -1                  
C*****                      4       3        8      -1                  
C*****                      5       3        9      -1                  
C*****                      5       4       10      -1                  
C*****THIS PROCEDURE CAN BE EXTENDED TO JUNCTIONS WITH ANY NUMBER OF    
C*****BRANCHES. BRANCH NUMBERING, 'FROM' BRANCHES, AND 'TO' BRANCHES    
C*****ARE DESCRIBED IN SUBROUTINES HOOKUP, JINS, AND OMEGA2.            
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C*****FIND BRANCH NUMBERS OF SECTION NUMBERS                            
      DO 20 IBR=1,NBR                                                   
      NUMY1=TABL17(INODE,IBR)                                           
      NUMY1=IABS(NUMY1)                                                 
      IF(NUMY1-J1) 20,10,20                                             
10    J1=IBR                                                            
      GO TO 30                                                          
20    CONTINUE                                                          
      GO TO 140                                                         
30    DO 50 IBR=1,NBR                                                   
      NUMY1=TABL17(INODE,IBR)                                           
      NUMY1=IABS(NUMY1)                                                 
      IF(NUMY1-J2) 50,40,50                                             
40    J2=IBR                                                            
      GO TO 60                                                          
50    CONTINUE                                                          
      GO TO 140                                                         
C*****FIND SIGN                                                         
60    NUMY1=0                                                           
      IF(J2-J1) 70,90,80                                                
70    NUMY1=-1                                                          
      NUMY2=J1                                                          
      J1=J2                                                             
      J2=NUMY2                                                          
      GO TO 90                                                          
80    NUMY1=+1                                                          
C*****FIND EQUATION SET NUMBER                                          
90    NUMY2=J1-1                                                        
      NUMY3=0                                                           
      IF(NUMY2) 120,120,100                                             
100   DO 110 I=1,NUMY2                                                  
110   NUMY3=NUMY3+NBR-I                                                 
120   NUMY3=NUMY3+J2-J1                                                 
      J1=NUMY3                                                          
      J2=NUMY1                                                          
130   RETURN                                                            
C*****CHECK IF PROGRAM IS IN INPUT OR EXECUTION - THIS PROBLEM CAN      
C*****LEGITIMATELY OCCUR DURING EXECUTION DUE TO PRECISION. IN THIS CASE
C*****IT MAY BE IGNORED.                                                
140   IF(NTIME) 160,150,150                                             
150   ISITU=1                                                           
      GO TO 130                                                         
160   WRITE (OUT,180)                                                   
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
      IF (NHIGH-NLOW) 200,200,170                                       
170   WRITE(OUT,190) INODE,TABL15(INODE),(TABL2(ITBL2),ITBL2=NLOW,NHIGH)
180   FORMAT (T16,'IMPOSSIBLE SITUATION - SUBROUTINE EQUAT')        
190   FORMAT (T16,'NODE INTERNAL NUMBER=',I4,' NODE EXTERNAL NUMBER=',  
     1  I4,/,T16,'TABL2 FOR THIS NODE',/,5(30I5,/))                     
200   CALL EXIT (EXITCODE)                                                              
      END                                                               
