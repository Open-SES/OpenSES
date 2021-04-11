
      SUBROUTINE  THDERV                                                
C                                                                       
C     THIS SUBROUTINE COMPUTES THE TEMPERATURE AND HUMIDITY DERIVATIVES 
C     WITH RESPECT TO TIME FOR EACH LINE AND VENT SHAFT SUBSEGMENT.     
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
C                                                                       
      RHOCON=RHOWHT*HVH2O                                               
C                                                                       
C                                                                       
C---- LOOP OVER SECTIONS                                                
      DO  250  ISCT = 1, NSEC                                           
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
C---- FIND NODE AT BOTH ENDS OF SECTION                                 
      NODTHB = TABL20(ISCT) / 64                                        
      NODTHF = TABL21(ISCT) / 64                                        
C                                                                       
C                                                                       
C---- CHECK IF LINE SEGMENT OR VENT SHAFT SEGMENT                       
      IF( ISEGL ) 110,999,120                                           
C                                                                       
C---- VENTILATION SHAFT SEGMENT                                         
C***** ISSBCL AND ISSBCH ARE SUBSEGMENTS ADJACENT TO BOUNDARY CONDITIONS
  110 ISSBCL = TABL5( - ISEGL )                                         
      ISSL = ISSBCL                                                     
      ISSBCH = TABL6( - ISEGH )                                         
      ISSH = ISSBCH                                                     
      ISEG = ISEGH                                                      
      SURSS = SURVSS( - ISEG )                                          
      KFIROP = -FIROP2                                                  
C---- CHECK VELOCITY-TIME STABILITY CRITERIA                            
      IF (FIROP2 .GT. 0)  GO TO 111                                     
C---- NOT FIRE SIMULATION                                               
      IF(ABS(FBSS(ISSL)*DEL2TH) - VOLSS(ISSL)) 140,140,112              
C---- FIRE SIMULATION                                                   
  111 IF(ABS(FBSS(ISSL)* DEL2TH *TSSTAB(ISSL))-VOLSS(ISSL)) 140,140,112 
C---- VELOCITY TIME STABILITY CRITERIA HAS BEEN EXCEEDED IN VENT SHAFT  
C---- USE 'HOLE-IN-WALL' APPROXIMATION                                  
  112 IF( FBSS(ISSL) ) 114,114,116                                      
C --  AIR IS BLOWING IN NEGATIVE DIRECTION                              
  114 DO  115  ISS = ISSL,ISSH                                          
      TDBSS(ISS)  = TDBTN(NODTHF)                                       
      TTMPSS(ISS) = TDBTN(NODTHF)                                       
      DERTSS(ISS) = 0.0                                                 
      HUMSS(ISS)  = HUMTN(NODTHF)                                       
      HTMPSS(ISS) = HUMTN(NODTHF)                                       
  115 DERHSS(ISS) = 0.0                                                 
      GO TO 250                                                         
C --  AIR IS BLOWING IN POSITIVE DIRECTION                              
  116 DO  117  ISS = ISSL,ISSH                                          
      TDBSS(ISS)  = TDBTN(NODTHB)                                       
      TTMPSS(ISS) = TDBTN(NODTHB)                                       
      DERTSS(ISS) = 0.0                                                 
      HUMSS(ISS)  = HUMTN(NODTHB)                                       
      HTMPSS(ISS) = HUMTN(NODTHB)                                       
  117 DERHSS(ISS) = 0.0                                                 
      GO TO 250                                                         
C                                                                       
C---- LINE SEGMENT                                                      
C                                                                       
  120 ISSBCL = TABL3( ISEGL )                                           
      ISSBCH = TABL4( ISEGH )                                           
      ISEG = ISEGL                                                      
C                                                                       
C---- LOOP OVER LINE SEGMENTS IN SECTION                                
C                                                                       
  130 SURSS = SURLSS(ISEG) * PDRYLS(ISEG)                               
      ISSL = TABL3( ISEG )                                              
      ISSH = TABL4( ISEG )                                              
      KFIROP = FIROP2 * TYPLS(ISEG)                                     
C                                                                       
C---- LOOP OVER SUBSEGMENTS IN SEGMENT                                  
C                                                                       
  140 DO  200  ISS = ISSL, ISSH                                         
C                                                                       
C---- COMPUTE FLOW INTO SUBSEGMENT FROM ADJACENT SUBSEGMENTS            
C---- FLOW ENTERING SUBSEGMENT FROM BACKWARD END OF SUBSEGMENT          
      IF( FBSS(ISS) ) 150,150,155                                       
  150 FFLOW = 0.0                                                       
      HFLOW = 0.0                                                       
      GO TO 170                                                         
  155 IF( ISS - ISSBCL ) 999,160,165                                    
C --  FLOW IS FROM BOUNDARY CONDITIONS                                  
  160 FFLOW = FBSS(ISS) * ( TDBTN(NODTHB) - TTMPSS(ISS) )               
      HFLOW = FBSS(ISS) * ( HUMTN(NODTHB) - HTMPSS(ISS) )               
      GO TO 170                                                         
C---- FLOW IS FROM SUBSEGMENT ISS-1                                     
  165 FFLOW = FBSS(ISS) * ( TTMPSS(ISS-1) - TTMPSS(ISS) )               
      HFLOW = FBSS(ISS) * ( HTMPSS(ISS-1) - HTMPSS(ISS) )               
C                                                                       
  170 IF( FFSS(ISS) ) 175,190,190                                       
C---- FLOW ENTERING SUBSEGMENT FROM FORWARD END OF SUBSEGMENT           
  175 IF( ISS - ISSBCH ) 185,180,999                                    
  180 FFLOW = FFLOW - FFSS(ISS) * ( TDBTN(NODTHF) - TTMPSS(ISS) )       
      HFLOW = HFLOW - FFSS(ISS) * ( HUMTN(NODTHF) - HTMPSS(ISS) )       
      GO TO 190                                                         
C---- FLOW IS FROM SUBSEGMENT ISS+1                                     
  185 FFLOW = FFLOW - FFSS(ISS) * ( TTMPSS(ISS+1) - TTMPSS(ISS) )       
      HFLOW = HFLOW - FFSS(ISS) * ( HTMPSS(ISS+1) - HTMPSS(ISS) )       
C                                                                       
C---- COMPUTE DERIVATIVE                                                
C                                                                       
190   IF (KFIROP .GE. 0)  GO TO 191                                     
C*****FIRE SIMULATION
      DERTSS(ISS) = ((FFLOW * RHOCP - HTRNSS(ISS) * SURSS *             
     1 (TTMPSS(ISS) - TSFSS(ISS))+ SHLTSS(ISS) - QRADSS(ISS))/          
     2 (RHOCP*VOLSS(ISS)))*TSSTAB(ISS)                                  
      DERHSS(ISS)= ((HFLOW+LHLTSS(ISS)/RHOCON)/VOLSS(ISS))*TSSTAB(ISS)  
      GO TO 200                                                         
191   DERTSS(ISS) = ( FFLOW * RHOCP - HTRNSS(ISS) * SURSS *             
     1    (TTMPSS(ISS)-TSFSS(ISS)) + SHLTSS(ISS)) / (RHOCP*VOLSS(ISS))  
      DERHSS(ISS)=  (HFLOW+LHLTSS(ISS)/RHOCON)/VOLSS(ISS)               
200   CONTINUE                                                          
C---- CHECK IF MORE SEGMENTS IN SECTION                                 
      IF( ISEG - ISEGH ) 210,250,250                                    
C---- INCREMENT SEGMENT COUNTER AND LOOP BACK  ( LINE SECTIONS ONLY )   
  210 ISEG = ISEG + 1                                                   
      GO TO 130                                                         
C                                                                       
  250 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C---- IMPOSSIBLE SITUATION    ******************************************
C                                                                       
  999 WRITE(OUT,998)                                                    
  998 FORMAT('IMPOSSIBLE SITUATION - SUBROUTINE DERIV' )               
      CALL EXIT (EXITCODE)                                                              
      END                                                               
