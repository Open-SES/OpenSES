      SUBROUTINE  LOCATE( NUMV, ITYP, IROUTE, RG, RC )                  
C                                                                       
C     THIS SUBROUTINE FINDS THE POSITION OF THE TRAIN WITH RESPECT      
C     TO THE TRACK SECTIONS OF THE ROUTE ON WHICH IT IS OPERATING,      
C     AS WELL AS COMPUTING GRADE RESISTANCE AND CURVE RESISTANCE.       
C     ALSO COMPUTED IS THE TRAIN LOCATION WITH RESPECT TO SEGMENT       
C     BOUNDARIES, THE NUMBER OF TRAINS IN EACH SEGMENT, AND THEIR       
C     DIRECTION OF OPERITION WITH RESPECT TO POSATIVE AIR FLOW IN THAT  
C     SEGMENT.                                                          
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C     INITIALIZATION                                                    
C                                                                       
      DRAGV(NUMV) = 0.0                                                 
      RG= 0.0                                                           
      RC = 0.0                                                          
C                                                                       
      NTS = NTSRT(IROUTE)                                               
C --  JTS IS TRACK SECTION THAT FRONT OF TRAIN WAS IN ON LAST 'LOCATE'  
      JTS = NTSV(NUMV)                                                  
C                                                                       
C                                                                       
C                                                                       
C---- COMPARE TRAIN POSITION WITH RESPECT TO TRACK SECTION              
  100 IF (SNGL( XV(NUMV))-FSTS(JTS,IROUTE)) 130,110,110                 
C---- TRAIN HAS MOVED PAST T.S. BOUNDARY -ADVANCE 'JTS' PAST TRAIN FRONT
  110 JTS = JTS + 1                                                     
C---- CHECK FOR END OF TRACK SECTIONS                                   
      IF( JTS - NTS ) 100,100,120                                       
C---- FRONT OF TRAIN IS PAST LAST T.S.  -  OUTSIDE OF TUNNEL SYSTEM     
  120 JTS = NTS                                                         
      SUM = SNGL( XV(NUMV)) - FSTS(NTS,IROUTE)                          
      IDIR = 0                                                          
C---- DRAG ON FRONT OF TRAIN AND SIDES OF LENGTH 'SUM'                  
      DRAGV(NUMV)=(CDFVOV(ITYP)/2.0+LAMDAV(ITYP)*SUM/                   
     1(8.0*AV(ITYP)))*UV(NUMV)*UV(NUMV)                                 
      ISEG2 = NSEGTS( JTS,IROUTE ) / 128                                
      ISEG = IABS(ISEG2)                                                
      NTSV(NUMV)=JTS                                                    
      IF( ISEG ) 175,175,160                                            
C---- FRONT OF TRAIN IS LOCATED WITHIN TRACK SECTIONS                   
  130 SUM = 0.0                                                         
C---- GET SEGMENT NUMBER AND DIRECTION OF TRAIN MOVEMENT IN SEGMENT     
  140 ISEG2 = NSEGTS(JTS,IROUTE) / 128                                  
      ISEG = IABS(ISEG2)                                                
      NTSV(NUMV)=JTS                                                    
C---- CHECK IF OUTSIDE OF TUNNEL SYSTEM                                 
      IF( ISEG ) 150,150,160                                            
C---- OUTSIDE OF TUNNEL SYSTEM                                          
  150 IDIR = 0                                                          
C---- DRAG ON FRONT OF TRAIN                                            
      DRAGV(NUMV) = CDFVOV(ITYP)*UV(NUMV)*UV(NUMV)/2.0                  
      GO TO 175                                                         
C---- TRAIN IS INSIDE TUNNEL SYSTEM - STORE SEGMENT OF TRAIN FRONT      
  160 IDIR = ISEG2 / ISEG                                               
C---- INCREMENT NUMBER OF TRAINS IN THIS SEGMENT                        
      NTRNLS(ISEG) = NTRNLS(ISEG) + 1                                   
      NTRN = NTRNLS(ISEG)                                               
      NF = 2 * NTRN                                                     
      NB = NF - 1                                                       
C---- CHECK IF GREATER THAN MAXIMUM NUMBER OF TRAINS ALLOWED IN SEGMENT 
C----     LIMITED BY ARRAY SIZE ('TRNNLS','TRNDLS') TO 'LMTRSG' TRAINS  
      IF( NTRN - LMTRSG ) 170,170,800                                   
  170 TRNNLS(ISEG,NTRN) = IDIR * NUMV                                   
  175 TNL1 = SNGL( XV(NUMV)) - FSTS(JTS-1, IROUTE)                      
C                                                                       
C---- CHECK IF ENTIRE TRAIN HAS BEEN PROPORTIONED                       
C                                                                       
  200 IF( SUM + TNL1 - LV(ITYP) ) 210,400,400                           
C---- CONTINUE PROPORTIONING TRAIN OVER TRACK SECTIONS                  
  210 SUM = SUM + TNL1                                                  
      RG = RG + TNL1 * GRDTS(JTS,IROUTE)                                
      IF( RADTS(JTS,IROUTE) ) 225,225,220                               
  220 RC = RC + TNL1 / RADTS(JTS,IROUTE)                                
  225 IF( ISEG ) 227,227,230                                            
C---- COMPUTE DRAG ON TRAIN SIDE OF LENGTH 'TNL1'                       
  227 DRAGV(NUMV)=DRAGV(NUMV)+LAMDAV(ITYP)*TNL1/                        
     1       (8.0*AV(ITYP))*UV(NUMV)*UV(NUMV)                           
C---- DECREMENT TRACK SECTION COUNTER -(WORKING TOWARD REAR OF TRAIN)   
  230 JTS = JTS -1                                                      
C---- CHECK FOR BEGINNING OF TRACK SECTION ARRAY                        
      IF( JTS - 1 ) 360,360,240                                         
C---- CHECK IF SEGMENT HAS CHANGED                                      
  240 NUMY1 = NSEGTS(JTS,IROUTE) / 128                                  
      IF(ISEG-IABS(NUMY1)) 250,350,250                                  
C---- SEGMENT CHANGE                                                    
C --  FRONT OF TRAIN IS PROJECTING INTO SEGMENT OR SEGMENT IS COMPLETELY
C --  FILLED BY THE TRAIN                                               
  250 IF( IDIR ) 270,300,260                                            
C---- DIRECTION IS +                                                    
  260 TRNDLS(ISEG,NF) = SNGL( XV(NUMV)) - FSTS(JTS,IROUTE)              
      TRNDLS(ISEG,NB) = TRNDLS(ISEG,NF) - LV(ITYP)                      
      GO TO 300                                                         
C---- DIRECTION IS -                                                    
  270 TRNDLS(ISEG,NB) = FSTS(JTS,IROUTE) - SNGL( XV(NUMV)) + LLS(ISEG)  
      TRNDLS(ISEG,NF) = TRNDLS(ISEG,NB) + LV(ITYP)                      
C                                                                       
C---- GET NEW SEGMENT NUMBER AND DIRECTION                              
  300 ISEG2 = NSEGTS(JTS,IROUTE) / 128                                  
      ISEG = IABS(ISEG2)                                                
C---- CHECK IF OUTSIDE OF TUNNEL SYSTEM                                 
      IF(ISEG) 305,305,310                                              
C---- TRAIN IS OUTSIDE OF TUNNEL                                        
  305 IDIR = 0                                                          
      GO TO 350                                                         
C---- TRAIN IS INSIDE TUNNEL SYSTEM                                     
  310 IDIR = ISEG2 / ISEG                                               
      NTRNLS(ISEG) = NTRNLS(ISEG) + 1                                   
      NTRN = NTRNLS(ISEG)                                               
      NF = 2 * NTRN                                                     
      NB = NF - 1                                                       
C---- CHECK IF GREATER THAN MAXIMUM NVMER OF TRAINS ALLOWED IN SEGMENT  
C----     LIMITED BY ARRAY SIZE ('TRNNLS','TRNDLS') TO 'LMTRSG' TRAINS  
      IF( NTRN - LMTRSG ) 320,320,800                                   
C---- STORE TRAIN NUMBER WITH SEGMENT                                   
  320 TRNNLS(ISEG,NTRN)= IDIR * NUMV                                    
C---- COMPUTE LENGTH OF TRAIN IN THIS TRACK SECTION                     
  350 TNL1 = FSTS(JTS,IROUTE) - FSTS(JTS-1,IROUTE)                      
      GO TO 200                                                         
C                                                                       
C                                                                       
C---- BEGINNING OF TRACK SECTION ARRAY - REMAINDER OF TRAIN             
C---- HAS NOT ENTERED SYSTEM YET                                        
  360 IF( IDIR ) 380,385,370                                            
C---- DIRECTION IS +                                                    
  370 TRNDLS(ISEG,NF) = SNGL( XV(NUMV)) - FSTS(JTS,IROUTE)              
      TRNDLS(ISEG,NB) = TRNDLS(ISEG,NF) - LV(ITYP)                      
      GO TO 385                                                         
C---- DIRECTION IS -                                                    
  380 TRNDLS(ISEG,NB) = FSTS(JTS,IROUTE) - SNGL( XV(NUMV)) + LLS(ISEG)  
      TRNDLS(ISEG,NF) = TRNDLS(ISEG,NB) + LV(ITYP)                      
  385 TNL1 = LV(ITYP) - SUM                                             
C---- COMPUTE DRAG ON TRAIN SIDE 'TNL1' AND REAR OF TRAIN               
  390 DRAGV(NUMV)=DRAGV(NUMV)+(LAMDAV(ITYP)*TNL1/(8.0*AV(ITYP))+        
     1 CDBVOV(ITYP) /2.0 ) * UV(NUMV)* UV(NUMV)                         
      GO TO 600                                                         
C                                                                       
C                                                                       
C---- REAR OF TRAIN HAS BEEN PASSED - COMPUTE REMAINING PORTION OF TRAIN
  400 TNL1 = LV(ITYP) - SUM                                             
      RG = RG + TNL1 * GRDTS(JTS,IROUTE)                                
      IF( RADTS(JTS,IROUTE) ) 420,420,410                               
  410 RC = RC + TNL1 / RADTS(JTS,IROUTE)                                
  420 IF( ISEG ) 390,390,480                                            
C---- DECREMENT TRACK SECTION COUNTER                                   
  480 JTS = JTS - 1                                                     
C---- CHECK FOR END OF TRACK SECTION ARRAY                              
      IF( JTS - 1  ) 520,520,490                                        
C---- CHECK IF SAME SEGMENT                                             
  490 NUMY1 = NSEGTS(JTS,IROUTE) / 128                                  
      IF(ISEG-IABS(NUMY1)) 520,480,520                                  
C---- END OF SEGMENT HAS BEEN LOCATED                                   
C---- TRAIN IS FULLY CONTAINED WITHIN SEGMENT                           
C---- OR IT IS PROJECTING INTO THIS SEGMENT                             
  520 IF( IDIR ) 550,600,540                                            
C---- DIRECTION IS +                                                    
  540 TRNDLS(ISEG,NF) = SNGL( XV(NUMV)) - FSTS(JTS,IROUTE)              
      TRNDLS(ISEG,NB) = TRNDLS(ISEG,NF) - LV(ITYP)                      
      GO TO 600                                                         
C---- DIRECTION IS -                                                    
  550 TRNDLS(ISEG,NB) = LLS(ISEG) + FSTS(JTS,IROUTE) - SNGL( XV(NUMV))  
      TRNDLS(ISEG,NF) = TRNDLS(ISEG,NB) + LV(ITYP)                      
C---- COMPLETE COMPUTATION OF CURVE AND GRADE RESISTANCE                
  600 RG = RG / LV(ITYP)                                                
      RC = RC * 4580.0 / LV(ITYP)                                       
C                                                                       
      RETURN                                                            
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C---- MORE THAN 'LMTRSG' TRAINS IN LINE SEGMENT                         
C                                                                       
C --  POSITION THIS TRAIN OUTSIDE OF SYSTEM                             
  800 NTRNLS(ISEG) = LMTRSG                                             
      RG = 0.0                                                          
      RC = 0.0                                                          
C---- WRITE ERROR MESSAGE                                               
      WRITE(OUT,810) NUMLS(ISEG),TNUMV(NUMV)                            
  810 FORMAT(/,'SUBROUTINE LOCATE     SEGMENT=',I4,3X,'TRAIN= ',A2 )  
      CALL SIMERR( 4 )                                                  
      RETURN                                                            
C                                                                       
C                                                                       
C                                                                       
      END                                                               
