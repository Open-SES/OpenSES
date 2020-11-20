      SUBROUTINE  PINPNT                                                
C                                                                       
C     THIS SUBROUTINE ANALYZES EACH SUBSEGMENT, COMPUTING THE           
C     COEFFICIENT OF HEAT TRANSFER TO THE WALLS OF THE SUBSEGMENT,      
C     THE RATE OF AIR FLOW INTO THE SUBSEGMENT FROM THE ADJACENT        
C     SUBSEGMENTS OR BOUNDARY CONDITIONS, THE EFFECTIVE VOLUME OF       
C     THE SUBSEGMENT,  RATE OF SENSIBLE AND LATENT HEAT REJECTION       
C     INTO EACH SUBSEGMENT, AND AVERAGE AIR TEMPERATURE, WALL           
C     TEMPERATURE, AND ANNULAR VELOCITY SURROUNDING THE RESISTOR        
C     GRIDS.                                                            
C     THE LOCATION OF THE TRAINS WITH RESPECT TO SEGMENT BOUNDARIES,    
C     WHICH WAS COMPUTED IN SUB. 'LOCATE', IS FURTHER REFINED TO        
C     THE LOCATION WITH RESPECT TO SUBSEGMENT BOUNDARIES.  THE PRESENCE 
C     OF TRAINS IN PART OR ALL OF A SUBSEGMENT AFFECTS THE QUANITITES   
C     COMPUTED FOR THAT SUBSEGMENT.                                     
C                                                                       
C     DLIST(ILOC)  IS A TABLE OF DISTANCES FROM THE BACKWARD (LOW)      
C          END OF THE SEGMENT TO THE REGION BOUNDARIES IN THAT SEGMENT. 
C          THE DISTANCE TO THE BACKWARD END ( END CLOSEST TO THE        
C          BACKWARD END OF THE SEGMENT ) OF THE TRAIN IS STORED IN      
C          THE EVEN NUMBERED ARRAY LOCATIONS (ILOC = 2,4,6...12),       
C          AND THE DISTANCE TO THE FORWARD END ( END CLOSEST TO THE     
C          FORWARD END OF THE SEGMENT ) OF THE TRAIN IS STORED IN THE   
C          ODD NUMBERED ARRAY LOCATIONS (ILOC =3,5,7...13).             
C          THE FIRST ARRAY POSITION (ILOC = 1) CONTAINS THE DISTANCE    
C          TO THE NEXT SUBSEGMENT BOUNDARY.                             
C          THIS INFORMATION IS COMPUTED IN SUBROUTINE 'LOCATE' AND      
C          IS COPIED OUT OF ARRAY 'TRNDLS'.                             
C          THE DIMENSION SIZE OF THIS ARRAY IS EQUAL TO                 
C          (2*'LMTRSG')+1.                                              
C      NLIST  IS THE NUMBER OF VALID ENTRYS IN THE 'DLIST' ARRAY.       
C      ITRST(JTRN)  IS AN ARRAY WHICH INDICATES WHICH TRAINS ARE IN THE 
C          REGION CURRENTLY UNDER CONSIDERATION.                        
C          1 = YES, THIS TRAIN IS IN THE REGION                         
C          0 = NO,  THIS TRAIN IS NOT IN THE REGION                     
C          THE TRAIN INDICATOR IN THIS ARRAY CORRESPONDS TO THE TRAIN   
C          NUMBER WHICH IS STORED IN 'TRNDLS'.                          
C          THE DIMENSION SIZE IS EQUAL TO 'LMTRSG'.                     
C      RL  REGION LENGTH, FEET.                                         
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C    DIMENSION - MAXIMUM NUMBER OF TRAINS IN A SEGMENT  ('LMTRSG')      
      INTEGER   ITRST(LMTRSG)                                                
      REAL  EFUXA(LMTRSG),EFUXP(LMTRSG)                                           
C    DIMENSION - TWICE THE MAXIMUM NUMBER OF TRAINS IN A SEGMENT PLUS   
C    ONE    ((2*'LMTRSG')+1)                                            
      REAL      DLIST(2*LMTRSG+1)                                               
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C**** LINE SEGMENT VARIABLE (LMLSS)                                     
      DIMENSION  FLSS(LMLSS),AFSS(LMLSS), ABSS(LMLSS)                      
      DATA  FLSS / 1200 * 0.0 /                                         
C                                                                       
      RECON = 4.0 * RHOMAS / VISAIR                                     
      HTCON = 0.022 * PR**0.6 * THCON                                   
      QSSCON=RHOMAS*FLBBTU/8.0                                          
C*****IF TRAINS MAY BE OPERATING THEN INITIALIZE ITNLSS TO ALPHAMERIC   
C*****BLANK                                                             
      IF( NOTRAN ) 100,100,40                                           
   40 DO 50 IROUTE=1,LMTRRT                                             
      DO 50 ILSS=1,TNLSS                                                
   50 ITNLSS( ILSS, IROUTE) = ' '                                  
C---- LOOP OVER ALL TRAINS - INITIALIZE RESISTOR GRID ENVIRONMENT       
      IF( NTRAN ) 100,100,55                                            
   55 DO  60  ITRN = 1, NTRAN                                           
      NUMV = IINDEX(ITRN)                                                
      TAAVGV(NUMV) = 0.0                                                
      TWAVGV(NUMV) = 0.0                                                
      ANVV(NUMV) = 0.0                                                  
   60 RGSUMV(NUMV) = 0.0                                                
C---- IF UNDERPLATFORM EXHAUST IS NOT OPERATING SET COEFF. TO 1.0       
      IF( OPTUX ) 65,65,100                                             
   65 DO  70  I=1,LMTRSG                                                
      EFUXA(I) = 1.0                                                    
   70 EFUXP(I) = 1.0                                                    
C                                                                       
C---- LOOP OVER SECTIONS IN SYSTEM   ***********************************
C                                                                       
  100 DO  699  ISCT = 1, NSEC                                           
C---- BRANCH ON SECTION TYPE                                            
      IF( TABL9(ISCT) ) 105,200,200                                     
C                                                                       
C---- VENTILATION SHAFT SECTION                                         
C                                                                       
  105 IVS =  - TABL9(ISCT)                                              
      DVS = 4.0 * AVS(IVS) * LVSS(IVS) / SURVSS(IVS)                    
C                                                                       
C---- CALCULATE CONVECTIVE HEAT TRANSFER COEFFICIENT FOR V.S.           
      IF( REVS(IVS) - 2000.0 ) 140,140,155                              
C---- LAMINAR FLOW                                                      
  140 IF( REVS(IVS) - 1.0 ) 145,150,150                                 
  145 HTRNS = 0.0                                                       
      GO TO 160                                                         
  150 HTRNS = 1.86 *(REVS(IVS)*PR*DVS/LVSS(IVS))**0.33333333*THCON/DVS  
      GO TO 160                                                         
C---- TURBULENT FLOW                                                    
  155 HTRNS = HTCON * REVS(IVS)**0.8 / DVS                              
C---- LOOP OVER EACH SUBSEGMENT IN VENT SHAFT                           
  160 ISSL = TABL5( IVS )                                               
      ISSH = TABL6( IVS )                                               
      TSUM = 0.0                                                        
      DO 163 ISS=ISSL,ISSH                                              
C---- CALCULATE FREE CONVECTION HEAT TRANSFER COEFFICIENT               
      TDELT = ABS(TTMPSS(ISS)-TSFSS(ISS))                               
      IF (TDELT .LT. 0.01)  GO TO 161                                   
C----   0.21/3600.0=0.000058333333                                      
      HFC = 0.000058333333*TDELT**0.33333333                            
C---- USE FORCED CONVECTION OR FREE CONVECTION, WHICHEVER IS GREATER    
      IF (HTRNS .GE. HFC)  GO TO 161                                    
      HTRNSS(ISS) = HFC                                                 
      GO TO 162                                                         
161   HTRNSS(ISS) = HTRNS                                               
162   FBSS(ISS) = QSAVES(ISCT)                                          
      FFSS(ISS) = QSAVES(ISCT)                                          
      VOLSS(ISS) = VOLVSS(IVS)                                          
C                                                                       
C**** CHECK FIRE SIMULATION OPTION -- IF A FIRE IS BEING SIMULATED,     
C**** COMPUTE AND STORE THE AVERAGE SUBSEGMENT TEMPERATURE (STORE AS THE
C**** RATIO OF AVERAGE TEMPERATURE TO AMBIENT TEMPERATURE)              
C**** AND ADJUST THE HEAT TRANSFER COEFFICIENT                          
C                                                                       
      IF (FIROP2 .LE. 0)  GO TO 163                                     
C**** FIRE SIMULATION                                                   
      TSUM = TSUM+TDBSS(ISS)                                            
      THCONA = AIR(TDBSS(ISS),2) / 3600.                                
      HTRNSS(ISS) = HTRNSS(ISS)*THCONA/THCON                            
163   CONTINUE                                                          
C                                                                       
C**** STORE THE AVERAGE TEMPERATURE RATIO IN THE ARRAY LOCATION         
C**** CORRESPONDING TO THE LOWEST SUBSEGMENT WITHIN THE VENT SECTION    
C**** FOR USE IN SUBROUTINE OMEGA1                                      
C                                                                       
      IF( FIROP2 .GT. 0 )                                               
     1TSSTAB(ISSL) = (TSUM/FLOAT(ISSH-ISSL+1)+ABTEMP)/ABSAMB            
C                                                                       
C**** CHECK IF BUOYANCY IS TO BE COMPUTED                               
C                                                                       
      IF (STAKVS(IVS))  699,699,191                                     
C**** BUOYANCY GAIN COMPUTATION                                         
C  ** COMPUTE AVERAGE NODE TEMPERATURES                                 
  191 TTA = 0.0                                                         
      INODE = TABL12(ISCT)                                              
      NBR = TABL16(INODE)                                               
      ISORCE = NBR                                                      
      DO 192 IBR = 1,NBR                                                
      ISS = TABL18(INODE,IBR)                                           
  192 TTA = TTA+TDBSS(ISS)                                              
      INODE = TABL13(ISCT)                                              
      NBR = TABL16(INODE)                                               
      ISORCE = ISORCE + NBR                                             
      DO 193 IBR = 1,NBR                                                
      ISS = TABL18(INODE,IBR)                                           
  193 TTA = TTA+TDBSS(ISS)                                              
      TTA = TTA/FLOAT(ISORCE)                                           
C **  COMPUTE AVERAGE TEMPERATURE IN SHAFT                              
      TTB = 0.0                                                         
      DO 194 ISS = ISSL,ISSH                                            
  194 TTB = TTB+TDBSS(ISS)                                              
      TTB = TTB/FLOAT(ISSH-ISSL+1)                                      
      TEMP = 0.5*(TTA+TTB)+ABTEMP                                       
C**** COMPUTE BUOYANCY GAIN, NOTING THAT STAKVS(IVS) IS THE STACK       
C**** HEIGHT OF THE VENTILATION SHAFT TIMES GRACC                       
      BUOYS(ISCT) = STAKVS(IVS)*(1.0-(TDBAMB+ABTEMP)/TEMP)
      BUOYS(ISCT) = (INT(BUOYS(ISCT)*1000000.))/1000000              
      GO TO 699                                                         
C                                                                       
C---- LINE SECTION                                                      
C                                                                       
C---- LOOP OVER SEGMENTS IN SECTION                                     
C                                                                       
  200 ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
C                                                                       
      DO  698  ILS = ISEGL,ISEGH                                        
C---- CALCULATE AIR VELOCITY IN SEGMENT                                 
      VEL = QSAVES(ISCT) / ALS(ILS)                                     
C---- CHECK VELOCITY-TIME STABILITY CRITERIA                            
      IF( ABS( VEL * DEL2TH ) - LLSS(ILS) ) 230,230,220                 
  220 A = VEL * DEL2TH                                                  
      WRITE(OUT,225)  TABL11(ISCT), NUMLS(ILS),A,LLSS(ILS)              
  225 FORMAT('LINE SEGMENT',I4,' -',I3,10X,2F10.2 )                    
      CALL  SIMERR( 5 )                                                 
      IF (NEW .EQ. 1) GOTO 900                                            
230   DLS = 4.0 * ALS(ILS) / PERMLS(ILS)                                
C---- CALCULATE HEAT TRANSFER COEFFICIENT FOR EMPTY LINE SEGMENT        
      IF( RELS(ILS) .GT. 2000.0 ) GO TO 232                             
C---- LAMINAR FLOW                                                      
      IF( RELS(ILS) .GE. 1.0 ) GO TO 231                                
      HTRNS = 0.0                                                       
      FLS(ILS) = 64.0                                                   
      GO TO 236                                                         
231   HTRNS =1.86*(RELS(ILS)*PR*DLS/LLSS(ILS))**0.333333*THCON/DLS      
      FLS(ILS) = 64.0 / RELS(ILS)                                       
      GO TO 236                                                         
C---- TURBULENT FLOW                                                    
232   HTRNS = HTCON * RELS(ILS)**0.8 / DLS                              
      FLS(ILS)=0.0055*(1.0+(FFFLS(ILS)+1000000.0/RELS(ILS))**0.33333333)
      FSMTH = 0.0055 * (1.0 + (1000000.0/RELS(ILS))**0.33333333)        
      FRFS = FLS(ILS)/FSMTH                                             
C---- IF( RIBBED TUNNEL )                                               
C---- NON-RIBBED TUNNEL                                                 
      IF( FRFS .LT. 3.125 ) GO TO 233                                   
      HTRNS = HTRNS * 2.05                                              
      GO TO 236                                                         
233   HTRNS = HTRNS * FRFS**0.63                                        
C*************************  BYPASS CALCULATION   ***********************
C---- RIBBED TUNNEL                                                     
C 234 IF( FRFS .LT. 6.76 ) GO TO 235                                    
C     HTRNS = HTRNS * 2.60                                              
C     GO TO 236                                                         
C 235 HTRNS = HTRNS * FRFS**0.5                                         
C                                                                       
C*************************                       ***********************
C                                                                       
C --  OBTAIN SUBSEGMENT LIMITS OF SEGMENT                               
236   ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
C --                                                                    
C---- BRANCH ON NUMBER OF TRAINS IN SEGMENT                             
      IF( NTRNLS(ILS) ) 999,237,300                                     
C                                                                       
C---- NO TRAINS IN SEGMENT                                              
C                                                                       
C---- CHECK IF A FIRE IS BEING SIMULATED                                
  237 IF( FIROP2 .GT. 0  .AND.  TYPLS(ILS) .LT. 0 ) GO TO 241           
C                                                                       
C---- NOT FIRE SIMULATION                                               
C                                                                       
C --- LOOP OVER EACH SUBSEGMENT IN LINE SEGMENT                         
      DO 240 ISS = ISSL,ISSH                                            
C---- CALCULATE FREE CONVECTION HEAT TRANSFER COEFFICIENT               
      TDELT = ABS( TTMPSS(ISS) - TSFSS(ISS) )                           
      IF( TDELT .LT. 0.01 ) GO TO 238                                   
C----     0.21 / 3600.0 = 0.000058333333                                
      HFC = 0.000058333333 * TDELT**0.333333                            
C---- USE FORCED CONVECTION OR FREE CONVECTION, WHICHEVER IS GREATER    
      IF( HTRNS .GE. HFC ) GO TO 238                                    
      HTRNSS(ISS) = HFC                                                 
      GO TO 239                                                         
238   HTRNSS(ISS) = HTRNS                                               
239   FBSS(ISS) = QSAVES(ISCT)                                          
      FFSS(ISS) = QSAVES(ISCT)                                          
      VOLSS(ISS) = VOLLSS(ILS)                                          
      AFSS(ISS) = ALS(ILS)                                              
      ABSS(ISS) = ALS(ILS)                                              
      SHLTSS(ISS) = SHLSS(ISS)                                          
      SHUXSS(ISS) = 0.0                                                 
      LHLTSS(ISS) = LHLSS(ISS)                                          
C --  COMPUTE AND STORE THE RATIO OF THE ABSOLUTE SUBSEGMENT AIR        
C --  TEMPERATURE AND ABSOLUTE AMBIENT TEMPERATURE FOR USE IN THIS      
C --  SUBROUTINE, OMEGA1 AND OMEGA4                                     
240   TSSTAB(ISS) = ( TDBSS(ISS) + ABTEMP) / ABSAMB                     
      GO TO 698                                                         
C                                                                       
C**** CALCULATIONS FOR A FIRE SEGMENT ( NO TRAINS ARE PRESENT )         
C                                                                       
C---- LOOP OVER EACH SUBSEGMENT IN LINE SEGMENT                         
  241 DO 248 ISS = ISSL,ISSH                                            
C --  COMPUTE AND STORE THE RATIO OF THE ABSOLUTE SUBSEGMENT AIR        
C --  TEMPERATURE AND ABSOLUTE AMBIENT TEMPERATURE FOR USE IN THIS      
C --  SUBROUTINE, OMEGA1 AND OMEGA4                                     
      TSSTAB(ISS) = ( TDBSS(ISS) + ABTEMP) / ABSAMB                     
      IF( RELSS(ISS) .GT. 2000.0 ) GO TO 243                            
C---- LAMINAR FLOW                                                      
      IF( RELSS(ISS) .GE. 1.0 ) GO TO 242                               
      HTRNS = 0.0                                                       
      FLSS(ISS) = 64.0                                                  
      GO TO 245                                                         
242   THCONA = AIR( TDBSS(ISS),2 ) / 3600.                              
      HTRNS =1.86*(RELSS(ISS)*PR*DLS/LLSS(ILS))**0.333333*THCONA/DLS    
      FLSS(ISS) = 64.0/RELSS(ISS)                                       
      GO TO 245                                                         
C---- TURBULENT FLOW                                                    
243   THCONA = AIR( TDBSS(ISS),2 ) / 3600.                              
      HTRNS = HTCON * RELSS(ISS)**0.8* THCONA/(THCON* DLS)              
      FLSS(ISS)=0.0055*(1.0+(FFFLS(ILS)+1000000.0/RELSS(ISS)) **        
     1  0.33333333)                                                     
      FSMTH = 0.0055 * (1.0 + (1000000.0/RELSS(ISS))**0.33333333)       
      FRFS = FLSS(ISS)/FSMTH                                            
      IF( FRFS .LT. 3.125 ) GO TO 244                                   
      HTRNS = HTRNS * 2.05                                              
      GO TO 245                                                         
244   HTRNS = HTRNS * FRFS ** 0.63                                      
C                                                                       
C---- CALCULATE FREE CONVECTION HEAT TRANSFER COEFFICIENT               
245   TDELT = ABS(TTMPSS(ISS) - TSFSS(ISS) )                            
      IF( TDELT .LT. 0.01 ) GO TO 246                                   
C----     0.21 / 3600.0 = 0.000058333333                                
      HFC = 0.000058333333 * TDELT ** 0.33333333                        
C---- USE FORCED CONVECTION OR FREE CONVECTION , WHICHEVER IS GREATER   
      IF( HTRNS .GE. HFC ) GO TO 246                                    
      HTRNSS(ISS) = HFC                                                 
      GO TO 247                                                         
246   HTRNSS(ISS) = HTRNS                                               
247   FBSS(ISS) = QSAVES(ISCT)                                          
      FFSS(ISS) = QSAVES(ISCT)                                          
      VOLSS(ISS) = VOLLSS(ILS)                                          
      AFSS(ISS) = ALS(ILS)                                              
      ABSS(ISS) = ALS(ILS)                                              
      SHLTSS(ISS)= SHLSS(ISS)                                           
      SHUXSS(ISS)= 0.0                                                  
248   LHLTSS(ISS)= LHLSS(ISS)                                           
      GO TO 698                                                         
C                                                                       
C                                                                       
C---- SEGMENT CONTAINS ONE OR MORE TRAINS                               
C                                                                       
  300 FRFSA = 1.0 + FFFLS(ILS)**.33333333                               
C     IF( RIBBED TUNNEL )                                               
C---- NON-RIBBED TUNNEL                                                 
  301 IF( FRFSA - 3.125 ) 303,302,302                                   
  302 HTCONA = HTCON * 2.05                                             
      GO TO 307                                                         
  303 HTCONA = HTCON * FRFSA**0.63                                      
      GO TO 307                                                         
C---- RIBBED TUNNEL                                                     
C 304 IF( FRFSA - 6.76 ) 306,305,305                                    
C 305 HTCONA = HTCON * 2.60                                             
C     GO TO 307                                                         
C 306 HTCONA = HTCON * FRFSA**0.50                                      
  307 KTRN = NTRNLS(ILS)                                                
C---- TRACKWAY EXHAUST - PROPULSION/BRAKING AND AUXILIARIES/PASSENGERS  
      IF( OPTUX ) 319,319,310                                           
  310 ITYP= IABS(TYPLS(ILS))                                            
      IF ( ITYP - 2 ) 317,311,308                                       
  308 IF( ITYP - 4 ) 311,311,317                                        
C --  TYPE 2+3 - PROP/BRKG HEAT EXHAUST,  TYPE 2+4 - AUX/PASS HT EXH    
  311 DO  316  I = 1, KTRN                                              
      NUMV=TRNNLS(ILS,I)                                                
      NUMV=IABS(NUMV)                                                   
      IF( UV(NUMV) - UMAXUX ) 321,315,315                               
  321 IF( TPOPT - 2 ) 312,322,322                                       
  322 IF( UV(NUMV) - 0.01 ) 314,313,313                                 
C                                                                       
C  -- TRACKWAY EXHAUST EFFECTIVENESS = 0 FOR CONSTANT SPEED MODE        
C  -- ( AS ON EXPRESS TRACK THROUGH STATION )                           
C 312 IF( MODEV(NUMV) -1 ) 314,315,313                                  
C  --  TRACKWAY EXHAUST NOT AFFECTED BY CONSTANT SPEED MODE             
  312 IF( MODEV(NUMV) - 1 ) 314,313,313                                 
C --  EXHAUST OPERATING AT MOVING TRAIN EFFECTIVENESS ( ACCEL OR DECEL )
  313 EFUXA(I) = EFAMUX                                                 
      EFUXP(I) = EFMUX                                                  
      GO TO 3141                                                        
C---- EXHAUST OPERATING AT TRAIN STOPPED EFFECTIVENESS                  
  314 EFUXA(I) = EFASUX                                                 
      EFUXP(I) = EFSUX                                                  
 3141 IF( ITYP - 3 ) 316,3143,3144                                      
C --  TYPE 3 SEGMENT - NO AUX/PASS HEAT EXHAUST                         
 3143 EFUXA(I) = 1.0                                                    
      GO TO 316                                                         
C --  TYPE 4 SEGMENT - NO PROP/BRKG HEAT EXHAUST                        
 3144 EFUXP(I) = 1.0                                                    
      GO TO 316                                                         
C --  EXHAUST NOT OPERATING  ( NOT STATION, TRAIN ABOVE SPEED, OR IN    
C --                           MAINTAINING SPEED MODE )                 
  315 EFUXA(I) = 1.0                                                    
      EFUXP(I) = 1.0                                                    
  316 CONTINUE                                                          
      GO TO 319                                                         
C --  TUNNEL OR SPECIAL SEGMENT - NO UPE OR OTE                         
  317 DO  318  I = 1, KTRN                                              
      EFUXA(I) = 1.0                                                    
  318 EFUXP(I) = 1.0                                                    
  319 ISS = ISSL                                                        
      SSI = 1.0                                                         
      FBSS(ISS) = QSAVES(ISCT)                                          
      ABSS(ISS) = ALS(ILS)                                              
      NUMTR = 0                                                         
      DLAST = 0.0                                                       
C---- COPY TRAIN DISTANCES INTO 'DLIST' AND FIND TRAINS IN FIRST REGION 
      NLIST = KTRN * 2 + 1                                              
      DO  330  I = 2, NLIST, 2                                          
      DLIST(I) = TRNDLS(ILS,I-1)                                        
      JTRN = I / 2                                                      
      IF( DLIST(I) ) 325,325,328                                        
C --  TRAIN IS IN FIRST REGION - SET INDICATOR TO 1 (YES)               
  325 ITRST(JTRN) = 1                                                   
      DLIST(I) = ZAP                                                    
      IDIR=TRNNLS(ILS,JTRN)                                             
      NUMV=IABS(IDIR)                                                   
      IDIR=IDIR/NUMV                                                    
      ITYP = ITYPV(NUMV)                                                
      FBSS(ISS) = FBSS(ISS) - IDIR * UV(NUMV) * AV(ITYP)                
      ABSS(ISS) = ABSS(ISS) - AV(ITYP)                                  
      NUMTR = NUMTR + 1                                                 
      GO TO 330                                                         
C --  TRAIN IS NOT IN FIRST REGION - SET INDICATOR TO 0 (NO)            
  328 ITRST(JTRN) = 0                                                   
  330 DLIST(I+1) = TRNDLS(ILS,I)                                        
C     WRITE(OUT,981) NUMLS(ILS),NUMTR                                   
C 981 FORMAT('SEGMENT',I5,10X,'NUMTR',I5,'  LOOP 330' )               
      DLIST(1) = LLSS(ILS)                                              
C                                                                       
C---- LOOP OVER REGIONS IN SEGMENT                                      
C                                                                       
C---- COMPUTE FREE CONVECTION HEAT TRANSFER COEFFICIENT                 
  350 TDELT = ABS( TTMPSS(ISS) - TSFSS(ISS) )                           
      IF( TDELT - 0.01 ) 360,370,370                                    
  360 HFC = 0.0                                                         
      GO TO 380                                                         
C --       0.21 / 3600. = 0.000058333333                                
  370 HFC = 0.000058333333 * TDELT**0.33333333                          
C---- SUBSEGMENT INITIALIZATION                                         
  380 HTOTAL = 0.0                                                      
      FTOTAL = 0.0                                                      
      VOLSS(ISS) = VOLLSS(ILS)                                          
      SHLTSS(ISS) = SHLSS(ISS)                                          
      SHUXSS(ISS) = 0.0                                                 
      LHLTSS(ISS) = LHLSS(ISS)                                          
C --  COMPUTE AND STORE THE RATIO OF THE ABSOLUTE SUBSEGMENT AIR        
C --  TEMPERATURE AND ABSOLUTE AMBIENT TEMPERATURE FOR USE IN THIS      
C --  SUBROUTINE, OMEGA1 AND OMEGA4                                     
      IF( FIROP2 .GT. 0 )                                               
     1TSSTAB(ISS) = ( TDBSS(ISS) + ABTEMP) / ABSAMB                     
C---- SEARCH LIST FOR LOWEST ENTRY ( FIND END OF REGION )               
  385 ILOC = 1                                                          
      DMIN = DLIST(1)                                                   
      DO  430  J = 2, NLIST                                             
      IF( DMIN - DLIST(J) ) 430,430,420                                 
  420 ILOC = J                                                          
      DMIN = DLIST(J)                                                   
  430 CONTINUE                                                          
  440 RL = DMIN - DLAST                                                 
      IF( NUMTR ) 999,450,470                                           
C                                                                       
C---- NO TRAINS IN REGION - USE EMPTY REGION HEAT TRANSFER COEFFICIENT  
  450 FFSS(ISS) = QSAVES(ISCT)                                          
      AFSS(ISS) = ALS(ILS)                                              
      IF( FIROP2 .GT. 0 .AND. TYPLS(ILS) .LT. 0 ) GO TO 460             
      HTRNSA = HTRNS                                                    
      FLSSA = FLS(ILS)                                                  
      GO TO 530                                                         
C --  COMPUTE ADJUSTED REYNOLDS, HEAT TRANS + FRICTION FOR FIRE SEGMENT 
  460 REA = RNFLS(ILS) * ABS( QSAVES(ISCT) )                            
      DLSA = DLS                                                        
      GO TO 487                                                         
C                                                                       
C---- LOOP OVER ALL TRAINS IN SEGMENT                                   
  470 SUMQ = QSAVES(ISCT)                                               
      SUMP = PERMLS(ILS)                                                
      SUMA = ALS(ILS)                                                   
      DO  480  I = 1, KTRN                                              
      IF( ITRST(I) ) 999,480,475                                        
  475 IDIR=TRNNLS(ILS,I)                                                
      NUMV=IABS(IDIR)                                                   
      IDIR=IDIR/NUMV                                                    
      ITYP=ITYPV(NUMV)                                                  
      SUMQ = SUMQ - IDIR * UV(NUMV) * AV(ITYP)                          
      SUMP = SUMP + PERMV(ITYP)                                         
      SUMA = SUMA - AV(ITYP)                                            
      IROUTE = IROTEV(NUMV)                                             
      ITNLSS(ISS,IROUTE) = TNUMV(NUMV)                                  
      VOLSS(ISS) = VOLSS(ISS) - AV(ITYP) * RL                           
      QTRNA = QAXSV(NUMV) * RL                                          
      QTNLA = QAXLV(NUMV) * RL                                          
      QTRNP = QPRPV(NUMV) * RL                                          
      QTRNAE = QTRNA * EFUXA(I)                                         
      QTRNPE = QTRNP * EFUXP(I)                                         
      SHLTSS(ISS) = SHLTSS(ISS) + QTRNAE + QTRNPE                       
      SHUXSS(ISS) = SHUXSS(ISS) + QTRNA*(1.0-EFUXA(I)) +                
     1                            QTRNP*(1.0-EFUXP(I))                  
      LHLTSS(ISS) = LHLTSS(ISS) + QTNLA                                 
C --  SEGMENT HEAT GAINS SUMMARY                                        
      IF( NOSUMY ) 480,480,477                                          
  477 SPROLS(ILS) = SPROLS(ILS) + QTRNP                                 
      SAXSLS(ILS) = SAXSLS(ILS) + QTRNA                                 
      SAXLLS(ILS) = SAXLLS(ILS) + QTNLA                                 
  480 CONTINUE                                                          
C---- LOOP OVER TRAINS COMPUTING RESISTOR GRID ENVIRONMENT              
      DO  485  I = 1, KTRN                                              
      IF( ITRST(I) ) 485,485,484                                        
  484 IDIR=TRNNLS(ILS,I)                                                
      NUMV=IABS(IDIR)                                                   
      IDIR=IDIR/NUMV                                                    
      ITYP = ITYPV(NUMV)                                                
      TAAVGV(NUMV) = TAAVGV(NUMV) + TDBSS(ISS) * RL                     
      TWAVGV(NUMV) = TWAVGV(NUMV) + TSFSS(ISS) * RL                     
      ANVV(NUMV) = ANVV(NUMV) + ABS( SUMQ / SUMA - UV(NUMV)*IDIR ) * RL 
      RGSUMV(NUMV) = RGSUMV(NUMV) + RL                                  
  485 CONTINUE                                                          
      FFSS(ISS) = SUMQ                                                  
      AFSS(ISS) = SUMA                                                  
C                                                                       
      DLSA = 4.0 * SUMA / SUMP                                          
      REA = ABS( SUMQ * RECON / SUMP )                                  
C---- COMPUTE HEAT TRANSFER COEFFICIENT                                 
  487 THCONR = 1.0                                                      
      IF( FIROP2 .LE. 0  .OR.  TYPLS(ILS) .GE. 0 )  GO TO 486           
      THCONB = AIR( TDBSS(ISS), 2 ) / 3600.                             
      THCONR = THCONB / THCON                                           
      VISCOS = AIR( TDBSS(ISS), 1 )                                     
      REA = REA * TSSTAB(ISS) / VISCOS  * ( VISAIR / RHOMAS )           
  486 IF( REA - 2000.0 ) 490,490,520                                    
C --  LAMINAR FLOW                                                      
  490 IF( REA - 1.0 ) 500,510,510                                       
  500 HTRNSA = 0.0                                                      
      FLSSA = 64.0                                                      
      GO TO 530                                                         
  510 HTRNSA = 1.86*(REA*PR*DLSA/LLSS(ILS))**0.33333333*THCON/DLSA *    
     1     THCONR                                                       
      FLSSA = 64.0 / REA                                                
      GO TO 530                                                         
C --  TURBULENT FLOW                                                    
  520 HTRNSA = HTCONA * REA**0.8 / DLSA  * THCONR                       
      FLSSA = 0.0055*(1.0+(FFFLS(ILS)+ 1000000.0/REA)**0.33333333 )     
C---- COMPARE TO FREE CONVECTION COEFFICIENT AND USE LARGER             
  530 IF( HTRNSA - HFC ) 540,550,550                                    
  540 HTOTAL = HTOTAL + HFC * RL                                        
      GO TO 560                                                         
  550 HTOTAL = HTOTAL + HTRNSA * RL                                     
  560 FTOTAL = FTOTAL + FLSSA * RL                                      
C---- CHECK IF AT REGION BOUNDARY OR SUBSEGMENT BOUNDARY                
      IF( ILOC - 1 ) 999,610,570                                        
C---- REGION BOUNDARY - FORWARD OR BACKWARD END OF A TRAIN              
  570 JTRN = ILOC / 2                                                   
      IF( ILOC - JTRN * 2 ) 580,580,590                                 
C---- BEGINNING OF TRAIN - SET STATUS AND INCREMENT NUMBER OF TRAINS    
  580 ITRST(JTRN) = 1                                                   
      NUMTR = NUMTR + 1                                                 
C     J = ISS - ISSL + 1                                                
C     WRITE(OUT,982)  NUMLS(ILS),J,NUMTR,TRNNLS(ILS,JTRN)               
C 982 FORMAT('SUBSEG',I5,' -',I3,10X,'NUMTR',I5,' BACKWARD END OF TRAIN'
C    1,I5)                                                            
      GO TO 600                                                         
C---- FINISH OF TRAIN - RESET STATUS AND DECREASE NO. OF TRAINS         
  590 ITRST(JTRN) = 0                                                   
      NUMTR = NUMTR - 1                                                 
C     J = ISS - ISSL + 1                                                
C     WRITE(OUT,983)  NUMLS(ILS),J,NUMTR,TRNNLS(ILS,JTRN)               
C 983 FORMAT('SUBSEG',I5,' -',I3,10X,'NUMTR',I5, ' FORWARD END OF TRAIN'
C    1, I5)                                                           
  600 DLAST = DLIST(ILOC)                                               
      DLIST(ILOC) = ZAP                                                 
C---- LOOP BACK AND ANALYZE NEXT REGION                                 
      GO TO 385                                                         
C                                                                       
C---- SUBSEGMENT BOUNDARY WAS ENCOUNTERED                               
C                                                                       
C --  CHECK VELOCITY-TIME STABILITY CRITERIA                            
  610 IF( ABS( FBSS(ISS) * DEL2TH ) - VOLSS(ISS) ) 630,630,620          
  620 A = FBSS(ISS) * DEL2TH                                            
      GO TO 650                                                         
  630 IF( ABS( FFSS(ISS) * DEL2TH ) - VOLSS(ISS) ) 670,670,640          
  640 A = FFSS(ISS) * DEL2TH                                            
  650 JSS = ISS - ISSL + 1                                              
      WRITE(OUT,660)  TABL11(ISCT),NUMLS(ILS),JSS,A,VOLSS(ISS)          
  660 FORMAT('ANNULAR VELOCITY    SUBSEGMENT',2(I3,' -'),I3,10X,2F10.2)
      CALL  SIMERR( 5 )                                                 
      IF (NEW .EQ. 1) GOTO 900                                            
C---- STORE AVERAGE HEAT TRANSFER COEFFICIENT AND FRICTION FACTOR       
  670 HTRNSS(ISS) = HTOTAL / LLSS(ILS)                                  
      FLSS(ISS) = FTOTAL / LLSS(ILS)                                    
C---- CHECK IF LAST SUBSEGMENT IN SEGMENT                               
      IF( ISS - ISSH ) 680,698,698                                      
C---- COMPUTE NEXT SUBSEGMENT BOUNDARY AND LOOP BACK FOR NEXT SUBSEGMENT
  680 SSI = SSI + 1.0                                                   
      DLAST = DLIST(1)                                                  
      DLIST(1) = LLSS(ILS) * SSI                                        
      FBSS(ISS+1) = FFSS(ISS)                                           
      ABSS(ISS+1) = ABSS(ISS)                                           
      ISS = ISS + 1                                                     
      GO TO 350                                                         
C                                                                       
  698 CONTINUE                                                          
C                                                                       
  699 CONTINUE                                                          
C                                                                       
C**** SPECIAL SECTION FOR COMPUTING THE BUOYANT AND THROTTLING EFFECTS  
C**** OF A FIRE                                                         
C                                                                       
      IF (FIROP2 .LE. 0)    GO TO 997                                   
C                                                                       
C**** LOOP OVER ALL SECTIONS IN SYSTEM                                  
      DO 990  ISCT = 1,NSEC                                             
      IF (TABL9(ISCT) .LE. 0)    GO TO 990                              
C                                                                       
C**** LINE SECTION ONLY                                                 
C                                                                       
      BUOYLS = 0.0                                                      
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
C**** COMPUTE THE BUOYANT FORCING FUNCTION BY LOOPING OVER ALL THE      
C**** SEGMENTS IN THE LINE SECTION                                      
      DO 980  ILS = ISEGL,ISEGH                                         
      ILOWSS = TABL3(ILS)                                               
      IHISS  = TABL4(ILS)                                               
C**** LOOP OVER SUBSEGMENTS IN LINE SEGMENT                             
C                                                                       
      BUOYSS = 0.0                                                      
      DO 910 ISS = ILOWSS,IHISS                                         
910   BUOYSS = BUOYSS + (1.0 - 1.0 / TSSTAB(ISS))                       
      BUOYLS = BUOYLS + BUOYSS * STAKLS(ILS)                            
C**** END OF LINE SEGMENT LOOP                                          
980   CONTINUE                                                          
C                                                                       
C**** STORE THE BUOYANT FORCING FUNCTION FOR THE LINE SECTION           
      BUOYS(ISCT) = BUOYLS                                              
      BUOYS(ISCT) = (FLOAT(INT(BUOYS(ISCT)*1000000.0)))/1000000.0              
C**** END OF LINE SECTION LOOP                                          
990   CONTINUE                                                          
997   CONTINUE                                                          
C                                                                       
C                                                                       
C**** SPECIAL SECTION FOR COMPUTING THE RADIANT AND CONVECTIVE HEAT     
C     TRANSFER TO THE TUNNEL WALL DURING A FIRE SIMULATION --           
C     ( THIS COMPUTATION IS DONE FOR FIRE SEGMENTS ONLY )               
C                                                                       
      IF ( FIROP2 * NTWALL .LE. 0 ) GO TO 1050                          
C                                                                       
C**** FIRE SIMULATION                                                   
C                                                                       
C**** LOOP OVER ALL LINE SECTIONS                                       
      DO 1020  ISCT = 1,NSEC                                            
      IF( TABL9(ISCT) .LE. 0 ) GO TO 1020                               
C**** LINE SECTION ONLY                                                 
C                                                                       
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
C**** LOOP OVER SEGMENTS IN THE SECTION                                 
      DO 1015  ILS = ISEGL,ISEGH                                        
      ILOWSS = TABL3(ILS)                                               
      IHISS = TABL4(ILS)                                                
C**** LOOP OVER SUBSEGMENTS IN LINE SEGMENT                             
      IF( TYPLS(ILS) .GE. 0 ) GO TO 1015                                
C                                                                       
C**** FIRE SEGMENT                                                      
C                                                                       
      DO 1010 ISS = ILOWSS,IHISS                                        
      IF ( ABS( TDBSS(ISS) - TSFSS(ISS) ) .GT. 0.1 ) GO TO 1002         
      QRADSS(ISS) = 0.0                                                 
      QWALSS(ISS) = 0.0                                                 
      GO TO 1010                                                        
C                                                                       
1002  IF( TDBSS(ISS) .GT. TSFSS(ISS) ) GO TO 1004                       
      QRADSS(ISS) = 0.0                                                 
      GO TO 1006                                                        
C                                                                       
C --  COMPUTE RADIATION HEAT TRANSFER TO WALL                           
1004  QRADSS(ISS) = (SURLSS(ILS) * STEFAN / 3600.0 ) * EECPG *          
     1      ( (TDBSS(ISS)+ABTEMP)**4 -       (TSFSS(ISS)+ABTEMP)**4)    
C                                                                       
1006  QWALSS(ISS) = QRADSS(ISS)/SURLSS(ILS) + HTRNSS(ISS) * (TDBSS(ISS) 
     1 - TSFSS(ISS) )                                                   
1010  CONTINUE                                                          
1015  CONTINUE                                                          
1020  CONTINUE                                                          
C                                                                       
C**** ADD THE FIRE SOURCE( UNSTEADY HEAT SOURCE ) RADIANT TERM          
C                                                                       
      IF ( NUMUL .LE. 0 ) GO TO 1050                                    
C                                                                       
      DO 1040 IUL = 1,NUMUL                                             
C --  CHECK IF UNSTEADY HEAT LOAD IS IN A FIRE SEGMENT                  
      ISSUL = LSSUL(IUL)                                                
      ILS = TABL7(ISSUL)                                                
      IF( TYPLS(ILS) .GT. 0 ) GO TO 1040                                
C**** IS THE HEAT SOURCE ACTIVE                                         
      IF(NTIME .LE. LTONUL(IUL) .OR. NTIME .GT. LTOFUL(IUL)) GO TO 1040 
C**** HEAT SOURCE (FIRE) IS ACTIVE                                      
      IF(  (TFLMUL(IUL)-ABTEMP) .GT. TSFSS(ISSUL) ) GO TO 1025          
      GLOW = 0.0                                                        
      GO TO 1030                                                        
 1025 GLOW = AFIRUL(IUL)*(TFLMUL(IUL)**4 -(TSFSS(ISSUL)+ABTEMP)**4 )    
1030  QRADSS(ISSUL) = QRADSS(ISSUL) + GLOW                              
      ILSUL = TABL7(ISSUL)                                              
      QWALSS(ISSUL) = QWALSS(ISSUL) + GLOW / SURLSS(ILSUL)              
1040  CONTINUE                                                          
C                                                                       
1050  CONTINUE                                                          
C                                                                       
C---- UNSTEADY HEAT LOADS      *****************************************
C                                                                       
      IF( NUMUL ) 750,750,710                                           
C---- LOOP OVER EACH UNSTEADY HEAT LOAD                                 
  710 DO  749  IUL = 1, NUMUL                                           
      THROTL(IUL) = 0.0                                                 
      IF( NTIME - LTONUL(IUL) ) 749,749,720                             
  720 IF( NTIME - LTOFUL(IUL) ) 730,730,749                             
  730 ISS = LSSUL(IUL)                                                  
      SHLTSS(ISS) = SHLTSS(ISS) + HLSUL(IUL)                            
      LHLTSS(ISS) = LHLTSS(ISS) + HLLUL(IUL)                            
      ILS = TABL7(ISS)                                                  
C --  SEGMENT HEAT GAIN SUMMARY                                         
      IF( NOSUMY ) 735, 735, 731                                        
  731 SULSLS(ILS) = SULSLS(ILS) + HLSUL(IUL)                            
      SULLLS(ILS) = SULLLS(ILS) + HLLUL(IUL)                            
C---- COMPUTE THROTTLING EFFECT OF UNSTEADY HEAT SOURCES (FIRE SIM ONLY)
  735 IF( FIROP2 .LE. 0 ) GO TO 749                                     
C --  BYPASS NON-FIRE SEGMENTS                                          
      IF( TYPLS(ILS) .GE. 0 ) GO TO 749                                 
      ISCT = TABL8(ILS)                                                 
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      ILSSB = TABL3(ISEGL)                                              
      ILSSF = TABL4(ISEGH)                                              
C --  FIND MINIMUM AREA AT EITHER END OF SUBSEGMENT                     
      AMIN = AFSS( ISS )                                                
      IF( ABSS(ISS) .LT. AMIN )  AMIN = ABSS(ISS)                       
C --  CHECK IF AIR IS ENTERING SUBSEGMENT FROM FORWARD END              
      THROT1 = 0.0                                                      
      IF ( FFSS(ISS) ) 736,740,740                                      
  736 IF( ISS .NE. ILSSF ) GO TO 738                                    
C --  AIR IS FLOWING INTO SS FROM NODE                                  
      NODFF = TABL21(ISCT) / 64                                         
      TNODFF = TDBTN(NODFF) + ABTEMP                                    
      THROT1 = ((  TDBSS(ISS) + ABTEMP ) / TNODFF - 1.0 ) *             
     1  ( FFSS(ISS) / AMIN ) **2                                        
      GO TO 740                                                         
C --  AIR IS FLOWING INTO SS I FROM I+1                                 
  738 THROT1 = ( TSSTAB(ISS) / TSSTAB(ISS+1) - 1.0 ) *                  
     1  ( FFSS(ISS) / AMIN ) **2                                        
C --  CHECK IF AIR IS ENTERING SUBSEGMENT FROM BACKWARD END             
  740 THROT2 = 0.0                                                      
      IF( FBSS(ISS) ) 746,746,741                                       
  741 IF( ISS .NE. ILSSB ) GO TO 743                                    
C --  AIR IS FLOWING INTO SS FROM NODE                                  
      NODBB = TABL20(ISCT) / 64                                         
      TNODBB = TDBTN(NODBB) + ABTEMP                                    
      THROT2 = ((  TDBSS(ISS) + ABTEMP ) / TNODBB - 1.0 ) *             
     1  ( FBSS(ISS) / AMIN ) **2                                        
      GO TO 744                                                         
C --  AIR IS FLOWING INTO SS I FROM I-1                                 
  743 THROT2 = ( TSSTAB(ISS) / TSSTAB(ISS-1) - 1.0 ) *                  
     1  ( FBSS(ISS) / AMIN ) **2                                        
C --  IF AIR IS FLOWING INTO SS FROM BOTH ENDS (MAY HAPPEN WHEN TRAIN   
C --  IS EXITING SUBSEGMENT)  SET THROTTLING TO ZERO                    
  744 IF( FFSS(ISS) ) 745,746,746                                       
  745 THROTL(IUL) = 0.0                                                 
      GO TO 749                                                         
C --  STORE THROTTLING EFFECT                                           
  746 THROTL(IUL) = THROT1 - THROT2                                     
  749 CONTINUE                                                          
C                                                                       
C---- VISCOUS HEATING    ***********************************************
C                                                                       
  750 DO 760 ILS = 1,NLS                                                
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      ISCT = TABL8(ILS)                                                 
C                                                                       
C**** CHECK IF A FIRE IS BEING SIMULATED -- IF SO, ADJUST PERTINENT     
C**** VARIABLES                                                         
      IF( FIROP2 .GT. 0  .AND.  TYPLS(ILS) .LT. 0 ) GO TO 752           
C---- NOT FIRE SIMULATION                                               
      QSS = QSSCON*FLS(ILS)*SURLSS(ILS)*ABS(QSAVES(ISCT)/ALS(ILS))**3   
      DO 751 ISS = ISSL,ISSH                                            
751   SHLTSS(ISS) = SHLTSS(ISS) + QSS                                   
      GO TO 755                                                         
C                                                                       
C**** FIRE SIMULATION                                                   
C                                                                       
752   QSSX = QSSCON * SURLSS(ILS) * ABS( QSAVES(ISCT)/ ALS(ILS))**3     
      DO 753 ISS = ISSL,ISSH                                            
      QSS = QSSX * FLSS(ISS) * TSSTAB(ISS)**2                           
753   SHLTSS(ISS) = SHLTSS(ISS) + QSS                                   
C --  SEGMENT HEAT GAIN SUMMARY                                         
  755 IF( NOSUMY ) 760,760,756                                          
  756 SULSLS(ILS) = SULSLS(ILS) + QSS * FLOAT(ISSH-ISSL+1)              
760   CONTINUE                                                          
C                                                                       
C---- EVAPORATION OF MOISTURE FROM WALL SURFACES   *******************  
C                                                                       
      IF( THOPT - 2 ) 900,810,900                                       
  810 DO  880  ILS = 1, NLS                                             
      IF( PDRYLS(ILS) - 99.99 ) 820,820,880                             
  820 ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
C---- SKIP CALCULATIONS OF WET-BULB TEMPERATURE IF PERFORMED IN SUMMARY 
C---- ROUTINE 'MAXMIN'                                                  
      IF( HUMOPT - 2 ) 840,830,840                                      
  830 IF( NOSUMY ) 840,840,860                                          
  840 DO  850  ISS = ISSL, ISSH                                         
  850 CALL WETBLB( TDBSS(ISS), HUMSS(ISS), TWBSS(ISS) )                 
  860 EVAPK = ( 1.0 - PDRYLS(ILS) ) * SURLSS(ILS)                       
      DO  870  ISS = ISSL, ISSH                                         
      EVAPQ = EVAPK * HTRNSS(ISS) * ( TDBSS(ISS) - TWBSS(ISS) )         
C --  SEGMENT HEAT GAIN SUMMARY                                         
      IF( NOSUMY ) 864,864,862                                          
  862 SULSLS(ILS) = SULSLS(ILS) - EVAPQ                                 
      SULLLS(ILS) = SULLLS(ILS) + EVAPQ                                 
  864 SHLTSS(ISS) = SHLTSS(ISS) - EVAPQ                                 
  870 LHLTSS(ISS) = LHLTSS(ISS) + EVAPQ                                 
  880 CONTINUE                                                          
C                                                                       
C                                                                       
C --  INCREMENT NUMBER OF SEGMENT HEAT GAIN SUMMARY POINTS              
  900 PINPT = PINPT + 1                                                 
C                                                                       
      RETURN                                                            
C                                                                       
C***********************************************************************
C                                                                       
C     IMPOSSIBLE SITUATION                                              
C                                                                       
  999 WRITE(OUT,998) CHAR(12)                                                    
  998 FORMAT(A1,'IMPOSSIBLE SITUATION - SUBROUTINE PINPNT' )          
      CALL EXIT (EXITCODE)                                                              
      END                                                               
