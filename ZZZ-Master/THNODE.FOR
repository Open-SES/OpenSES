      SUBROUTINE   THNODE                                               
C                                                                       
C     THIS SUBROUTINE COMPUTES THE TEMPERATURE AND HUMIDITY OF THE AIR  
C     AT EACH NODE.  THIS IS THE TEMPERATURE / HUMIDITY OF THE AIR      
C     WHICH IS FLOWING FROM THE NODE INTO THE SURROUNDING SECTIONS.     
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      REAL   DIAGT(3), SUPT(2), SUBT(2), BT(3)                          
      REAL   DIAGH(3), SUPH(2), SUBH(2), BH(3)                          
      REAL   QSUM(3)                                                    
C                                                                       
C                                                                       
C---- LOOP OVER EACH (AERODYNAMIC) NODE IN SYSTEM                       
      DO  400  NODE = 1, NNODE                                          
      KSCT = TABL16(NODE)                                               
      GO TO ( 400, 120, 200, 150, 150 ), KSCT                           
C                                                                       
C---- TWO SECTIONS ATTACHED TO NODE                                     
  120 ISS = TABL18(NODE,1)                                              
      NUMY1=TABL17(NODE,1)                                              
      ISCT=IABS(NUMY1)                                                  
C---- DETERMINE WHICH END OF SECTION IS CONNECTED TO NODE               
      IF(NUMY1) 130,125,125                                             
C---- FORWARD END OF SECTION IS ATTACHED TO NODE                        
  125 NODTH = TABL21(ISCT) / 64                                         
      IF( FFSS(ISS) ) 135,140,140                                       
C---- BACKWARD END OF SECTION IS ATTACHED TO NODE                       
  130 NODTH = TABL20(ISCT) / 64                                         
      IF( FBSS(ISS) ) 140,140,135                                       
C---- AIR IS ENTERING FROM SUBSEGMENT IN POSITION 2 OF TABL18           
  135 ISS = TABL18(NODE,2)                                              
C---- AIR IS ENTERING FROM SUBSEGMENT IN POSITION 1 OF TABL18           
  140 TDBTN(NODTH) = TTMPSS(ISS)                                        
      HUMTN(NODTH) = HTMPSS(ISS)                                        
      GO TO 400                                                         
C                                                                       
C---- CHECK JUNCTION TYPE IF 4 OR 5 SECTIONS ARE ATTACHED               
  150 NTYP = TYPTND(NODE)                                               
      GO TO ( 200, 300, 300, 400, 400 ), NTYP                           
C                                                                       
C---- MIXING NODE - FIND MIXTURE TEMP/HUMIDITY    **********************
C                                                                       
  200 SUMT = 0.0                                                        
      SUMH = 0.0                                                        
      SUMQ = 0.0                                                        
C---- GET THERMODYNAMIC NODE NUMBER                                     
      ISCT = TABL17(NODE,1)                                             
      IF( ISCT ) 215,210,210                                            
  210 NODTH = TABL21(ISCT) / 64                                         
      GO TO 220                                                         
  215 NODTH = TABL20( - ISCT) / 64                                      
C---- LOOP OVER SECTIONS ATTACHED TO NODE                               
  220 DO  290  J = 1, KSCT                                              
      ISS = TABL18(NODE,J)                                              
C---- DETERMINE WHICH END OF SECTION IS CONNECTED TO NODE               
      IF( TABL17(NODE,J) ) 250,230,230                                  
C---- FORWARD END OF SECTION IS CONNECTED TO NODE                       
  230 IF( FFSS(ISS) ) 290,290,240                                       
  240 SUMT = SUMT + TTMPSS(ISS) * FFSS(ISS)                             
      SUMH = SUMH + HTMPSS(ISS) * FFSS(ISS)                             
      SUMQ = SUMQ + FFSS(ISS)                                           
      GO TO 290                                                         
C---- BACKWARD END OF SECTION IS CONNECTED TO NODE                      
  250 IF( FBSS(ISS) ) 260,290,290                                       
  260 SUMT = SUMT - TTMPSS(ISS) * FBSS(ISS)                             
      SUMH = SUMH - HTMPSS(ISS) * FBSS(ISS)                             
      SUMQ = SUMQ - FBSS(ISS)                                           
  290 CONTINUE                                                          
C                                                                       
C---- COMPUTE AVERAGE TEMPERATURE AND HUMIDITY                          
      IF( SUMQ - 0.01 ) 140,295,295                                     
  295 TDBTN(NODTH) = SUMT / SUMQ                                        
      HUMTN(NODTH) = SUMH / SUMQ                                        
      GO TO 400                                                         
C                                                                       
C---- PREFERED FLOW NODE    ********************************************
C                                                                       
C     THIS NODE IS DESCRIBED BY THREE SEPARATE THERMODYNAMIC NODES      
C                                                                       
C---- INITIALIZE ARRAYS                                                 
  300 DO  310  I = 1,3                                                  
      QSUM(I) = 0.0                                                     
      DIAGT(I) = 0.0                                                    
      DIAGH(I) = 0.0                                                    
      BT(I)    = 0.0                                                    
  310 BH(I)    = 0.0                                                    
      DO  315  I = 1,2                                                  
      SUPT(I) = 0.0                                                     
      SUPH(I) = 0.0                                                     
      SUBT(I) = 0.0                                                     
  315 SUBH(I) = 0.0                                                     
C---- LOOP OVER SECTIONS ATTACHED TO NODE                               
      DO  345  J = 1, KSCT                                              
      NUMY1=TABL17(NODE,J)                                              
      ISCT=IABS(NUMY1)                                                  
      ISIGN=ISCT/NUMY1                                                  
      ISS=TABL18(NODE,J)                                                
      IF( ISIGN ) 325,320,320                                           
C --  FORWARD END OF SECTION IS ATTACHED TO NODE                        
  320 NODTH = TABL21(ISCT) / 64                                         
      Q = FFSS(ISS)                                                     
      GO TO 330                                                         
C --  BACKWARD END OF SECTION IS ATTACHED TO NODE                       
  325 NODTH = TABL20(ISCT) / 64                                         
      Q = - FBSS(ISS)                                                   
  330 I = NODTH - TABL22(NODE)                                          
      IF( Q ) 345,345,340                                               
C --  AIR IS FLOWING INTO NODE FROM SECTION  ( Q IS + )                 
  340 DIAGT(I) = DIAGT(I) + Q                                           
      DIAGH(I) = DIAGT(I)                                               
      BT(I) = BT(I) + Q * TTMPSS(ISS)                                   
      BH(I) = BH(I) + Q * HTMPSS(ISS)                                   
  345 QSUM(I) = QSUM(I) + Q                                             
C     WRITE(OUT,1020) QSUM                                              
C1020 FORMAT(// 'QSUM',3G15.7 )                                        
C                                                                       
C---- COMPUTE INTRA-NODE FLOWS                                          
      IF( QSUM(1) ) 355,359,350                                         
C---- AIR IS FLOWING FROM 1(A) TO 2(B)                                  
  350 SUBT(1) =  - QSUM(1)                                              
      SUBH(1) =  - QSUM(1)                                              
      DIAGT(2) = DIAGT(2) + QSUM(1)                                     
      DIAGH(2) = DIAGT(2)                                               
      GO TO 359                                                         
C---- AIR IS FLOWING FROM 2(B) TO 1(A)                                  
  355 SUPT(1) = QSUM(1)                                                 
      SUPH(1) = QSUM(1)                                                 
      DIAGT(1) = DIAGT(1) - QSUM(1)                                     
      DIAGH(1) = DIAGT(1)                                               
C                                                                       
  359 IF( QSUM(3) ) 365,369,360                                         
C---- AIR IS FLOWING FROM 3(C) TO 2(B)                                  
  360 SUPT(2) = - QSUM(3)                                               
      SUPH(2) = - QSUM(3)                                               
      DIAGT(2) = DIAGT(2) + QSUM(3)                                     
      DIAGH(2) = DIAGT(2)                                               
      GO TO 369                                                         
C---- AIR IS FLOWING FROM 2(B) TO 3(C)                                  
  365 SUBT(2) = QSUM(3)                                                 
      SUBH(2) = QSUM(3)                                                 
      DIAGT(3) = DIAGT(3) - QSUM(3)                                     
      DIAGH(3) = DIAGT(3)                                               
C                                                                       
C---- CHECK FOR ZERO-FLOW AT NODE                                       
  369 IF( DIAGT(1) ) 370,370,372                                        
C***** SET TEMPERATURE AND HUMIDITY OF 1(A) TO ARBITRARY SUBSEGMENT     
  370 DIAGT(1) = 1.                                                     
      DIAGH(1) = 1.                                                     
      SUPT(1)  = 0.                                                     
      SUPH(1)  = 0.                                                     
      ISS = TABL18(NODE,1)                                              
      BT(1)    = TTMPSS(ISS)                                            
      BH(1)    = HTMPSS(ISS)                                            
  372 IF( DIAGT(2) ) 373,373,376                                        
C***** SET TEMPERATURE AND HUMIDITY OF 2(B) EQUAL TO 1(A)               
  373 DIAGT(2) = 1.                                                     
      DIAGH(2) = 1.                                                     
      SUBT(1)  = -1.                                                    
      SUBH(1)  = -1.                                                    
      SUPT(2) = 0.                                                      
      SUPH(2) = 0.                                                      
      BT(2)   = 0.                                                      
      BH(2)   = 0.                                                      
      GO TO 376                                                         
  376 IF( DIAGT(3) ) 378,378,380                                        
C***** SET TEMPERATURE AND HUMIDITY OF 3(C) TO ARBITRARY SUBSEGMENT     
  378 DIAGT(3) = 1.                                                     
      DIAGH(3) = 1.                                                     
      SUBT(2)  = 0.                                                     
      SUBH(2)  = 0.                                                     
      ISS = TABL18(NODE,1)                                              
      BT(3)    = TTMPSS(ISS)                                            
      BH(3)    = HTMPSS(ISS)                                            
C                                                                       
C 380 WRITE(OUT,1000) SUPT,DIAGT,SUBT,BT,BH                             
C1000 FORMAT(/ 'SUP ',2G15.7/'DIAG',3G15.7/'SUB ',2G15.7/            
C    1  'BT  ',3G15.7 / 'BH  ',3G15.7 )                               
C                                                                       
C---- SOLVE MATRIX FOR 3 TEMPERATURES AND 3 HUMIDITIES                  
  380 CALL  TRIDG3( SUBT, DIAGT, SUPT, BT )                             
      CALL  TRIDG3( SUBH, DIAGH, SUPH, BH )                             
C---- STORE NODE TEMPERATURES AND HUMIDITIES                            
      DO  390  I = 1,3                                                  
      NODTH = I + TABL22(NODE)                                          
      TDBTN(NODTH) = BT(I)                                              
  390 HUMTN(NODTH) = BH(I)                                              
C                                                                       
C     WRITE(OUT,1010) BT,BH                                             
C1010 FORMAT(/ 'TEMP',3F15.2 / 'HUM ',3F15.4 )                        
C                                                                       
  400 CONTINUE                                                          
      RETURN                                                            
      END                                                               
