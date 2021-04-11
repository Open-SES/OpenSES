      SUBROUTINE JINS                                                   
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
      DIMENSION DUMMY1(8), NUMMY1(8)                                    
      DIMENSION A1(3), A3(3), B1(3), B3(3), C1(3), C3(3)                
      DATA A1/9.36,3.882,3.104/                                         
      DATA A3/0.3763,0.0191,0.3555/                                     
      DATA B1/-17.48,-8.912,-6.008/                                     
      DATA B3/-0.0478,0.367,0.12/                                       
      DATA C1/6.65,4.253,3.071/                                         
      DATA C3/0.7333,0.7273,0.821/                                      
      DATA D1/-2.1283/,D2/-1.3557/,D3/-1.2972/,D4/0.62634/              
      DATA E1/3.0419/,E2/1.1111/,E3/1.9078/,E4/1.7141/                  
      DATA F1/-0.45467/,F2/0.5085/,F3/-0.29865/,F4/0.56938/             
10    FORMAT (8F10.0)                                                    
20    FORMAT (/,'BRANCH NUMBER',I3,T74,'SECTION NUMBER',I5)         
30    FORMAT (/,'JUNCTION ASPECT RATIO',T79,F14.4)                   
40    FORMAT (/,'JUNCTION ANGLE',T79,F14.1,'   DEGREES')             
C*****READ IN THE GENERAL ARRAY OF JUNCTION DATA AND CONVERT ITS FIRST  
C*****NBR LOCATIONS TO INTEGER FORMAT IN ARRAY NUMMY1. THESE FIRST NBR  
C*****LOCATIONS ARE THE NUMBERS OF THE SECTIONS THAT MEET AT THE        
C*****JUNCTION IN THE MANNER REQUIRED BY THE JUNCTION FORM. FOR EXAMPLE,
C*****THE SECOND ENTRY IN ARRAY NUMMY1 IS BRANCH NUMBER TWO, ETC. THE   
C*****THE REMAINING ENTRIES IN ARRAY DUMMY1 ARE ITEMS SUCH AS THE       
C*****ASPECT RATIO AND THE ANGLE OF THE JUNCTION DEPENDING ON THE       
C*****JUNCTION TYPE.                                                    
      READ (IN,10) DUMMY1                                               
      DO 50 IBR=1,NBR                                                   
      NUMY1=DUMMY1(IBR)                                                 
50    NUMMY1(IBR)=TABL14(NUMY1)                                         
C*****WRITE OUT BRANCH NUMBERS AND SECTION NUMBERS                      
      DO 60 IBR=1,NBR                                                   
      NUMY1=DUMMY1(IBR)                                                 
60    WRITE (OUT,20) IBR,NUMY1                                          
C*****CHECK IF ALL SECTION NUMBERS ENTERED ARE THOSE OF SECTIONS MEETING
C*****AT THE NODE AND THAT NONE HAVE BEEN ENTERED MORE THAN ONCE        
C*****FIRST LOOP OVER ALL BRANCHES AT NODE                              
      DO 100 IBR=1,NBR                                                  
C*****INITIALIZE NUMBER OF MATCHES FOUND                                
      NUMY1=0                                                           
C*****SECOND LOOP OVER ALL BRANCHES AT NODE                             
      DO 80 JBR=1,NBR                                                   
C*****CHECK FOR MATCH BETWEEN ENTRIES IN ARRAYS TABL17 AND NUMMY1       
C*****(SECTION NUMBERS OF BRANCHES)                                     
      NUMY2=TABL17(INODE,IBR)                                           
      IF(IABS(NUMY2)-NUMMY1(JBR)) 80,70,80                              
C*****YES, THERE IS A MATCH. INCREASE NUMBER OF MATCHES FOUND BY ONE    
70    NUMY1=NUMY1+1                                                     
80    CONTINUE                                                          
C*****CHECK FOR ONE AND ONLY ONE MATCH FOUND - OTHERWISE ERROR          
      IF(NUMY1-1) 90,100,90                                             
90    CALL EERROR(203)                                                   
      GO TO 330                                                         
100   CONTINUE                                                          
C*****REARRANGE THE ORDER OF THE ENTRIES IN ARRAYS TABL17 AND TABL18    
C*****FOR THIS NODE SUCH THAT IT IS THE SAME AS THE BRANCH NUMBERING.   
C*****FOR EXAMPLE, IF SECTION 29 IS BRANCH 3 OF THE NODE THEN           
C*****TABL17(INODE,3)=29. (SIMILARILY WITH TABL18 ONLY WITH SUBSEGMENT  
C*****NUMBERS)                                                          
C*****LOOP OVER ALL BRANCHES OF NODE, CHECKING FOR AGREEMENT BETWEEN    
C*****ARRAYS TABL17 AND NUMMY1                                          
110   DO 120 IBR=1,NBR                                                  
      NUMY1=TABL17(INODE,IBR)                                           
      NUMY1=IABS(NUMY1)                                                 
      IF(NUMY1-NUMMY1(IBR)) 130,120,130                                 
120   CONTINUE                                                          
C*****SEARCH COMPLETE, ENTRIES IN ARRAYS TABL17 AND TABL18 ARE IN PROPER
C*****ORDER                                                             
      GO TO 160                                                         
C*****ENTRY IBR IN ARRAYS TABL17 AND TABL18 IS NOT IN ORDER. SEARCH     
C*****NBR-IBR ENTRIES IN ARRAY TABL17 FOR PROPER ENTRY                  
130   NUMY2=IBR+1                                                       
      DO 140 JBR=NUMY2,NBR                                              
      IF(NUMY1-NUMMY1(JBR)) 140,150,140                                 
140   CONTINUE                                                          
C*****ENTRY JBR IN ARRAYS TABL17 AND TABL18 IS PROPER ENTRY. EXCHANGE   
C*****ENTRIES IBR AND JBR IN ARRAYS TABL17 AND TABL18 SO THAT NEW ENTRY 
C*****IBR IS IN PROPER ORDER                                            
150   NUMY1=TABL17(INODE,IBR)                                           
      TABL17(INODE,IBR)=TABL17(INODE,JBR)                               
      TABL17(INODE,JBR)=NUMY1                                           
      NUMY1=TABL18(INODE,IBR)                                           
      TABL18(INODE,IBR)=TABL18(INODE,JBR)                               
      TABL18(INODE,JBR)=NUMY1                                           
C*****EXCHANGE OF ENTRIES COMPLETE. CHECK IF ALL ENTRIES ARE NOW IN     
C*****PROPER ORDER                                                      
      GO TO 110                                                         
C*****STORE AERODYNAMIC JUNCTION TYPE                                   
160   NUMY1=TABL1(INODE+1)-3*LMTRRT-1                                   
      TABL2(NUMY1)=NDTYPA                                               
      NOCOEF=0                                                          
      ICND=TABL23(INODE)                                                
C*****COMPUTED GO TO BASED ON JUNCTION TYPE                             
      GO TO (170,180,190,210,240,270,300),NDTYPA                        
C*****                                                                  
C*****TUNNEL-TO-TUNNEL CROSSOVER                                        
C*****                                                                  
C*****DUMY1 IS ASPECT RATIO                                             
170   DUMY1=DUMMY1(NBR+1)                                               
C*****WRITE AND CHECK ASPECT RATIO                                      
      WRITE (OUT,30) DUMY1                                              
      CALL CHECKR (DUMY1,0.1,50.0,206)                                  
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENT RELATED TO ASPECT RATIO  
      CDHC=0.45*(DUMY1**0.35)                                           
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
      NOCOEF=1                                                          
C*****MOVE CONTENTS OF ARRAY COAEND TO HIGHER SUBSCRIPT LOCATIONS       
C*****TO MAKE ROOM FOR NOCOEF ENTRIES BEGINNING AT LOCATION ICND        
      NHIGH=LMCOND-NOCOEF-ICND+1                                        
      DO 175 ISHIFT=1,NHIGH                                             
      JSHIFT=LMCOND-ISHIFT+1                                            
175   COAEND(JSHIFT)=COAEND(JSHIFT-NOCOEF)                              
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
      COAEND(ICND)=CDHC                                                 
      GO TO 300                                                         
C*****                                                                  
C*****DIVIDING WALL TERMINATION                                         
C*****                                                                  
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
180   NOCOEF=0                                                          
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
      GO TO 300                                                         
C*****                                                                  
C*****'T' JUNCTION                                                      
C*****                                                                  
C*****DUMY1 IS ASPECT RATIO                                             
190   DUMY1=DUMMY1(NBR+1)                                               
C*****WRITE AND CHECK ASPECT RATIO                                      
      WRITE (OUT,30) DUMY1                                              
      CALL CHECKR (DUMY1,0.1,30.0,207)                                  
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENT RELATED TO ASPECT RATIO  
      CDHCP1=1.0+0.45*(DUMY1**0.35)                                     
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
      NOCOEF=1                                                          
C*****MOVE CONTENTS OF ARRAY COAEND TO HIGHER SUBSCRIPT LOCATIONS       
C*****TO MAKE ROOM FOR NOCOEF ENTRIES BEGINNING AT LOCATION ICND        
      NHIGH=LMCOND-NOCOEF-ICND+1                                        
      DO 195 ISHIFT=1,NHIGH                                             
      JSHIFT=LMCOND-ISHIFT+1                                            
195   COAEND(JSHIFT)=COAEND(JSHIFT-NOCOEF)                              
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
      COAEND(ICND)=CDHCP1                                               
      GO TO 300                                                         
C*****                                                                  
C*****ANGLED JUNCTION                                                   
C*****                                                                  
C*****DUMY1 IS ASPECT RATIO                                             
210   DUMY1=DUMMY1(NBR+1)                                               
C*****WRITE AND CHECK ASPECT RATIO                                      
      WRITE (OUT,30) DUMY1                                              
      CALL CHECKR (DUMY1,0.1,30.0,207)                                  
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENT RELATED TO ASPECT RATIO  
      CDHCP1=1.0+0.45*(DUMY1**0.35)                                     
C*****DUMY2 IS ANGLE                                                    
      DUMY2=DUMMY1(NBR+2)                                               
C*****WRITE ANGLE AND CHECK TO SEE IF IT'S 10,20, OR 30 DEGREES         
      WRITE (OUT,40) DUMY2                                              
      NUMY1=DUMY2                                                       
      DUMY2=PI*DUMY2/180.0                                              
      DO 220 IDEGA=1,3                                                  
      NUMY2=10*IDEGA                                                    
      IF(NUMY2-NUMY1) 220,230,220                                       
220   CONTINUE                                                          
      CALL EERROR (205)                                                  
      DUMY2=PI/9.0                                                      
      IDEGA=2                                                           
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
230   NOCOEF=21                                                         
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENTS                         
      SEE1=-0.5*(1.0-D1-E1-F1)                                          
      SEE2=-0.5*(2.0*D1+E1)                                             
      SEE3=0.5*D1                                                       
      SEE4=-0.5*(A1(IDEGA)+B1(IDEGA)+C1(IDEGA))*SIN(DUMY2)              
      SEE5=0.5*((2.0*A1(IDEGA)+B1(IDEGA))*SIN(DUMY2))                   
      SEE6=-0.5*A1(IDEGA)*SIN(DUMY2)                                    
      SEE7=0.5*(1.0-D3-E3-F3)                                           
      SEE8=0.5*(2.0*D3+E3)                                              
      SEE9=-0.5*D3                                                      
      SEE10=-0.5*(2.0-SIN(DUMY2))*(A3(IDEGA)+B3(IDEGA)+C3(IDEGA))       
      SEE11=0.5*(2.0-SIN(DUMY2))*(2.0*A3(IDEGA)+B3(IDEGA))              
      SEE12=0.5*A3(IDEGA)*(SIN(DUMY2)-2.0)                              
      SEE13=0.5*(1.0-D4-E4-F4)                                          
      SEE14=0.5*(2.0*D4+E4)                                             
      SEE15=-0.5*D4                                                     
      SEE16=0.5*(D2+E2+F2-1.0)                                          
      SEE17=-0.5*(2.0*D2+E2)                                            
      SEE18=0.5*D2                                                      
      BETA1=-2.0*SEE4/CDHCP1                                            
      BETA2=-2.0*SEE10/CDHCP1                                           
C*****CHECK FOR ARRAY COAEND OVERFLOW                                   
      CALL CHECKI(ICND+20,1,LMCOND,214)                                 
C*****MOVE CONTENTS OF ARRAY COAEND TO HIGHER SUBSCRIPT LOCATIONS       
C*****TO MAKE ROOM FOR NOCOEF ENTRIES BEGINNING AT LOCATION ICND        
      NHIGH=LMCOND-NOCOEF-ICND+1                                        
      DO 235 ISHIFT=1,NHIGH                                             
      JSHIFT=LMCOND-ISHIFT+1                                            
235   COAEND(JSHIFT)=COAEND(JSHIFT-NOCOEF)                              
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
      COAEND(ICND)=CDHCP1                                               
      COAEND(ICND+1)=BETA1                                              
      COAEND(ICND+2)=BETA2                                              
      COAEND(ICND+3)=SEE1                                               
      COAEND(ICND+4)=SEE2                                               
      COAEND(ICND+5)=SEE3                                               
      COAEND(ICND+6)=SEE4                                               
      COAEND(ICND+7)=SEE5                                               
      COAEND(ICND+8)=SEE6                                               
      COAEND(ICND+9)=SEE7                                               
      COAEND(ICND+10)=SEE8                                              
      COAEND(ICND+11)=SEE9                                              
      COAEND(ICND+12)=SEE10                                             
      COAEND(ICND+13)=SEE11                                             
      COAEND(ICND+14)=SEE12                                             
      COAEND(ICND+15)=SEE13                                             
      COAEND(ICND+16)=SEE14                                             
      COAEND(ICND+17)=SEE15                                             
      COAEND(ICND+18)=SEE16                                             
      COAEND(ICND+19)=SEE17                                             
      COAEND(ICND+20)=SEE18                                             
      GO TO 300                                                         
C*****                                                                  
C*****'Y' JUNCTION                                                      
C*****                                                                  
C*****DUMY1 IS ASPECT RATIO                                             
240   DUMY1=DUMMY1(NBR+1)                                               
C*****WRITE AND CHECK ASPECT RATIO                                      
      WRITE (OUT,30) DUMY1                                              
      CALL CHECKR (DUMY1,0.1,30.0,207)                                  
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENT RELATED TO ASPECT RATIO  
      CDHCP1=1.0+0.45*(DUMY1**0.35)                                     
C*****DUMY2 IS ANGLE                                                    
      DUMY2=DUMMY1(NBR+2)                                               
C*****WRITE ANGLE AND CHECK TO SEE IF IT'S 10,20, OR 30 DEGREES         
      WRITE (OUT,40) DUMY2                                              
      NUMY1=DUMY2                                                       
      DUMY2=PI*DUMY2/180.0                                              
      DO 250 IDEGA=1,3                                                  
      NUMY2=10*IDEGA                                                    
      IF(NUMY2-NUMY1) 250,260,250                                       
250   CONTINUE                                                          
      CALL EERROR (205)                                                  
      DUMY2=PI/9.0                                                      
      IDEGA=2                                                           
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
260   NOCOEF=15                                                         
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENTS                         
      SEE1=-0.5*(A1(IDEGA)+B1(IDEGA)+C1(IDEGA))*SIN(DUMY2)              
      SEE2=0.5*(2.0*A1(IDEGA)+B1(IDEGA))*SIN(DUMY2)                     
      SEE3=-0.5*A1(IDEGA)*SIN(DUMY2)                                    
      SEE4=0.5*(1.0-D3-E3-F3)                                           
      SEE5=0.5*(2.0*D3+E3)                                              
      SEE6=-0.5*D3                                                      
      SEE7=0.5*A3(IDEGA)*(SIN(DUMY2)-2.0)                               
      SEE8=-0.5*(A3(IDEGA)+B3(IDEGA)+C3(IDEGA))*(2.0-SIN(DUMY2))        
      SEE9=0.5*(2.0-SIN(DUMY2))*(2.0*A3(IDEGA)+B3(IDEGA))               
      SEE10=0.5*(D2+E2+F2-1.0)                                          
      SEE11=-0.5*(2.0*D2+E2)                                            
      SEE12=0.5*D2                                                      
      BETA1=-2.0*SEE1/CDHCP1                                            
      BETA23=-2.0*SEE8/CDHCP1                                           
C*****CHECK FOR ARRAY COAEND OVERFLOW                                   
      CALL CHECKI(ICND+14,1,LMCOND,214)                                 
C*****MOVE CONTENTS OF ARRAY COAEND TO HIGHER SUBSCRIPT LOCATIONS       
C*****TO MAKE ROOM FOR NOCOEF ENTRIES BEGINNING AT LOCATION ICND        
      NHIGH=LMCOND-NOCOEF-ICND+1                                        
      DO 265 ISHIFT=1,NHIGH                                             
      JSHIFT=LMCOND-ISHIFT+1                                            
265   COAEND(JSHIFT)=COAEND(JSHIFT-NOCOEF)                              
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
      COAEND(ICND)=CDHCP1                                               
      COAEND(ICND+1)=BETA1                                              
      COAEND(ICND+2)=BETA23                                             
      COAEND(ICND+3)=SEE1                                               
      COAEND(ICND+4)=SEE2                                               
      COAEND(ICND+5)=SEE3                                               
      COAEND(ICND+6)=SEE4                                               
      COAEND(ICND+7)=SEE5                                               
      COAEND(ICND+8)=SEE6                                               
      COAEND(ICND+9)=SEE7                                               
      COAEND(ICND+10)=SEE8                                              
      COAEND(ICND+11)=SEE9                                              
      COAEND(ICND+12)=SEE10                                             
      COAEND(ICND+13)=SEE11                                             
      COAEND(ICND+14)=SEE12                                             
      GO TO 300                                                         
C*****                                                                  
C*****AERODYNAMIC JUNCTION TYPE 6 - RESERVED - SEE USERS + PROG. MANUALS
C*****                                                                  
270   CONTINUE                                                          
280   CONTINUE                                                          
C*****FIND NUMBER OF AERODYNAMIC EQUATION COEFFICIENTS TO BE STORED IN  
C*****ARRAY COAEND                                                      
290   NOCOEF=0                                                          
C*****COMPUTE AERODYNAMIC EQUATION COEFFICIENTS                         
C*****STORE AERODYNAMIC EQUATION COEFFICIENTS                           
C*****ADJUST TABL23 FOR NUMBER OF COEFFICIENTS ENTERED IN ARRAY COAEND  
300   NLOW=INODE+1                                                      
      NHIGH=NNODE+1                                                     
      DO 310 KNODE=NLOW,NHIGH                                           
      NUMY1=TABL23(KNODE)+NOCOEF                                        
      CALL CHECKI(NUMY1+1,1,LMCOND,214)                                 
310   TABL23(KNODE)=NUMY1                                               
C*****COMPUTE LOCATIONS OF LOOP THROUGH NODE DATA IN ARRAY TABL2        
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-3*LMTRRT-2                                   
C*****CONVERT 'FROM' AND 'TO' SECTION NUMBERS TO THE EQUATION SET       
C*****NUMBER AND SIGN                                                   
      DO 320 ITBL2=NLOW,NHIGH,3                                         
      ISCTB=TABL2(ITBL2+1)                                              
      ISCTA=TABL2(ITBL2+2)                                              
      CALL EQUAT(ISCTB,ISCTA)                                           
      TABL2(ITBL2+1)=ISCTB                                              
320   TABL2(ITBL2+2)=ISCTA                                              
330   RETURN                                                            
      END                                                               
