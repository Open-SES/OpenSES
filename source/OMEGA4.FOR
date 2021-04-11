      SUBROUTINE OMEGA4 (ILS,ISCT,SUM1,SUM2,NTRLS)                      
C                                                                       
C**** THIS SUBROUTINE CORRECTS THE LINE SEGMENT FORCING FUNCTION (FOR   
C**** SEGMENTS WITHOUT TRAINS) FOR THE EFFECTS OF A FIRE                
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
      Q = QS( ISCT )                                                    
      ABSQ = ABS( Q )                                                   
      ADD3 = 0.0                                                        
C**** ADD THE THROTTLING TERM - COMPUTED IN PINPNT                      
      IF( NUMUL ) 120, 120, 90                                          
90    DO  110  IUL = 1, NUMUL                                           
      ISS = LSSUL(IUL)                                                  
      IF( ILS .NE. TABL7(ISS) ) GO TO 110                               
      SUM2 = SUM2 + THROTL(IUL)                                         
110   CONTINUE                                                          
120   CONTINUE                                                          
C                                                                       
C**** CHECK FOR THE PRESENCE OF TRAINS IN THE LINE SEGMENT              
      IF( NTRNLS(ILS) ) 10,10,130                                       
C**** NO TRAINS PRESENT                                                 
C**** CHECK FOR PRESENCE OF IMPULSE FANS BY BRANCHING ON SEGMENT TYPE   
10    NUMY1 = IABS( TYPLS(ILS) )                                        
      GO TO (50,50,50,50,50,50,50,50,20,20,20,20,20,20,50,50) , NUMY1   
C**** IMPULSE FAN -- CHECK IF FAN IS OPERATING                          
20    IFS = NUMY1 - 8                                                   
      IF( NTIME .LT. TONIFS(IFS) .OR. NTIME .GT. TOFIFS(IFS) ) GO TO 50 
C**** COMPUTE GAIN IN TOTAL HEAD CAUSED BY IMPULSE FAN USING            
C**** 'THRUST' EQUATION                                                 
C     (ASSUMED TO USE OUTSIDE AIR OR BE UPSTREAM FROM FIRE -            
C     HENCE NO DENSITY ADJUSTMENT)                                      
      GAIN = QIFS(IFS)*( VIFS(IFS) - Q/ALS(ILS) )/ ALS(ILS)             
      SUM2 = SUM2 + GAIN                                                
C**** FIND THE LOW AND HIGH SUBSEGMENT IN THIS LINE SEGMENT             
50    ILOWSS = TABL3(ILS)                                               
      IHISS  = TABL4(ILS)                                               
C**** COMPUTE 'STANDARD' REYNOLDS NUMBER AND MULTIPLY BY THE TERM       
C**** ( VISAIR/RHOMAS )                                                 
      RESTD = RNFLS(ILS) * ABSQ * (VISAIR/RHOMAS)                       
C**** COMPUTE THE VISCOUS LOSSES IN EACH LINE SEGMENT BY LOOPING OVER   
C**** THE SUBSEGMENTS.                                                  
      SUBSUM = 0.0                                                      
      DO 80 ISS = ILOWSS,IHISS                                          
C**** COMPUTE THE 'ACTUAL' REYNOLDS NUMBER                              
      TSS = TDBSS(ISS)                                                  
      VISCOS = AIR (TSS,1)                                              
      REACT = RESTD * TSSTAB(ISS)/ VISCOS                               
      RELSS(ISS) = REACT                                                
C**** CHECK FOR TURBULENT OR LAMINAR FLOW REGIME                        
      IF( REACT - 2000.0 ) 60,70,70                                     
60    IF( REACT .LT. 1.0 )  REACT = 1.0                                 
      SUBSUM = SUBSUM - 64.0 * TSSTAB(ISS) / REACT                      
      GO TO 80                                                          
C**** TURBULENT FLOW -- COMPUTE TURBULENT DARCY-WEISBACH FRICTION FACTOR
70    FRICT = 0.0055 *(1.0 + (FFFLS(ILS)+1000000.0/REACT)**0.33333333)  
      SUBSUM = SUBSUM - FRICT * TSSTAB(ISS)                             
80    CONTINUE                                                          
      SUBSUM = SUBSUM * C1LS(ILS) * Q * ABSQ / FLOAT(IHISS-ILOWSS+1)    
      SUM2 = SUM2 + SUBSUM                                              
C---- FIND THE TEMPERATURE CORRECTION FOR THE SUBSEGMENT OR NODE AT     
C---- EITHER END OF THIS SEGMENT (SIMILAR CODE IN PINPNT)               
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      ILSSB = TABL3(ISEGL)                                              
      ILSSF = TABL4(ISEGH)                                              
C --  CHECK BACK (LOW) END                                              
      IF( ILOWSS .NE. ILSSB ) GO TO 122                                 
      NODBB = TABL20(ISCT) / 64                                         
      TADJB = ( TDBTN(NODBB) + ABTEMP ) / ABSAMB                        
      GO TO 124                                                         
122   TADJB = TSSTAB(ILOWSS - 1 )                                       
C --  CHECK FORWARD (HIGH) END                                          
124   IF( IHISS .NE. ILSSF ) GO TO 126                                  
      NODFF = TABL21(ISCT) / 64                                         
      TADJF = ( TDBTN(NODFF) + ABTEMP ) / ABSAMB                        
      GO TO 128                                                         
126   TADJF = TSSTAB( IHISS + 1 )                                       
C**** COMPUTE THE TEMPERATURE-CORRECTED HEAD CHANGE AT THE SEGMENT ENDS 
128   ADD1 = CFPLS(ILS) * TSSTAB(IHISS) + CBPLS(ILS) * TADJB            
      ADD2 = CFNLS(ILS) * TADJF         + CBNLS(ILS) * TSSTAB(ILOWSS)   
      ADD3 = - Q *(ADD1*(Q+ABSQ) - ADD2*(Q-ABSQ))/(4.0 * ALS(ILS)**2)   
      GO TO 140                                                         
C**** AT LEAST ONE TRAIN IS PRESENT IN THE LINE SEGMENT                 
130   NTRLS = 1                                                         
C**** DEDUCT LINE SEGMENT INERTIAL TERM FROM LINE SECTION INERTIAL TERM 
      BS(ISCT) = BS(ISCT) - LLS(ILS)/ALS(ILS)                           
C**** ADJUST THE LINE SECTION VARIABLE FOR HEAD CHANGES AT THE ENDS OF  
C**** THE CURRENT LINE SEGMENT BY DEDUCTING THE 'OLD' TERM AND          
C**** REPLACING IT BY A TEMPERATURE-CORRECTED TERM.                     
140   SUM1 = SUM1 + ADD3 + Q*((CFPLS(ILS)+CBPLS(ILS))*(Q+ABSQ) -        
     1(CFNLS(ILS)+CBNLS(ILS))*(Q-ABSQ))/(4.0 * ALS(ILS)**2)             
      RETURN                                                            
      END                                                               
