      SUBROUTINE   MAXMIN                                               
C                                                                       
C      THIS SUBROUTINE CALCULATES THE AVERAGE, MAXIMUM, AND             
C      MINIMUM VALUE OF TEMPERATURE, HUMIDITY, AND AIR VELOCITY         
C      AND THE TIME AT WHICH THE MAXIMUM AND MINIMUM OCCURS.            
C      IT ALSO KEEPS COUNT OF THE NUMBER OF TIMES THAT                  
C      THE DESIGN CRITERIA, I.E. TEMPERATURES FOR LINE                  
C      SEGMENTS AND OUTFLOW AIR VELOCITY FOR VENT SHAFTS, WAS           
C      EXCEEDED.                                                        
C                                                                       
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
      REAL  SUMQ(3)                                                     
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
      IF( NOSYS ) 400,400,100                                           
C                                                                       
C---- SECTION SUMMARY      *********************************************
C                                                                       
  100 DO  140  ISCT = 1, NSEC                                           
      CALL MMAX( QSAVES(ISCT), QSMX(ISCT), MXQS(ISCT) )                  
      CALL MMIN( QSAVES(ISCT), QSMN(ISCT), MNQS(ISCT) )                  
      IF( QSAVES(ISCT) ) 120,115,110                                    
C---- Q IS POSITIVE                                                     
  110 SUMQPS(ISCT) = SUMQPS(ISCT) + QSAVES(ISCT)                        
  115 NTQPS(ISCT) = NTQPS(ISCT) + 1                                     
      GO TO 130                                                         
C---- Q IS NEGATIVE                                                     
  120 SUMQNS(ISCT) = SUMQNS(ISCT) + QSAVES(ISCT)                        
C---- CHECK IF VENT SHAFT                                               
  130 IF( TABL9(ISCT) ) 131,140,140                                     
C---- VENT SHAFT - CHECK MAX OUTFLOW VELOCITY                           
  131 IVS = - TABL9(ISCT)                                               
      IF( VOMXVS(IVS) * AGVS(IVS) - ABS( QSAVES(ISCT) ) ) 132,140,140   
  132 NODEB = TABL12(ISCT)                                              
      IF( TABL16(NODEB) - 1 ) 134,134,136                               
C---- BACKWARD END OF V.S. IS CONNECTED TO ATMOS. - OUTFLOW IS NEGATIVE 
  134 IF( QSAVES(ISCT) ) 138,140,140                                    
C---- FORWARD END OF V.S. IS CONNECTED TO ATMOS. OR SHAFT IS BETWEEN    
C---- TWO BRANCHED JUNCTIONS - OUTFLOW IS POSITIVE                      
  136 IF( QSAVES(ISCT) ) 140,140,138                                    
  138 EXVOVS(IVS) = EXVOVS(IVS) + 1                                     
  140 CONTINUE                                                          
C                                                                       
C                                                                       
      IF( JRATTH ) 400,400,200                                          
C                                                                       
C---- LINE AND VENT SHAFT SUBSEGMENT SUMMARY  **************************
C                                                                       
  200 DO  250  ISS = 1, TNSS                                            
      CALL MMAX( TDBSS(ISS), TDBMX(ISS), MXTDB(ISS) )                    
      CALL MMIN( TDBSS(ISS), TDBMN(ISS), MNTDB(ISS) )                    
      CALL MMAX( HUMSS(ISS), HRMX(ISS), MXHR(ISS) )                      
      CALL MMIN( HUMSS(ISS), HRMN(ISS), MNHR(ISS) )                      
      STDBSS(ISS) = STDBSS(ISS) + TDBSS(ISS)                            
      SHRSS(ISS) = SHRSS(ISS) + HUMSS(ISS)                              
      HSUMSS(ISS) = HSUMSS(ISS) + HTRNSS(ISS)                           
      HTSMSS(ISS) = HTSMSS(ISS) + HTRNSS(ISS)*TDBSS(ISS)                
      QSUMSS(ISS) = QSUMSS(ISS) + SHLTSS(ISS)                           
      QSMXSS(ISS) = QSMXSS(ISS) + SHUXSS(ISS)                           
      LSUMSS(ISS) = LSUMSS(ISS) + LHLTSS(ISS)                           
      IF( FFSS(ISS) ) 230,230,220                                       
  220 EMTFSS(ISS) = EMTFSS(ISS) + TDBSS(ISS)*FFSS(ISS)                  
      EMHFSS(ISS) = EMHFSS(ISS) + HUMSS(ISS)*FFSS(ISS)                  
      FLOFSS(ISS) = FLOFSS(ISS) + FFSS(ISS)                             
  230 IF( FBSS(ISS) ) 240,250,250                                       
  240 EMTBSS(ISS) = EMTBSS(ISS) - TDBSS(ISS)*FBSS(ISS)                  
      EMHBSS(ISS) = EMHBSS(ISS) - HUMSS(ISS)*FBSS(ISS)                  
      FLOBSS(ISS) = FLOBSS(ISS) - FBSS(ISS)                             
  250 CONTINUE                                                          
      DO  259  ISCT = 1, NSEC                                           
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL ) 252,253,253                                           
C---- VENT SHAFT SECTION                                                
  252 ISEGL = - ISEGL                                                   
      ISSL = TABL5(ISEGL)                                               
      ISSH = TABL6(ISEGL)                                               
      GO TO 254                                                         
C---- LINE SECTION                                                      
  253 ISSL = TABL3(ISEGL)                                               
      ISSH = TABL4(ISEGH)                                               
  254 IF( QSAVES(ISCT) ) 257,255,255                                    
  255 DO  256  ISS = ISSL,ISSH                                          
  256 STDPSS(ISS) = STDPSS(ISS) + TDBSS(ISS)                            
      GO TO 259                                                         
  257 DO  258  ISS = ISSL,ISSH                                          
  258 STDNSS(ISS) = STDNSS(ISS) + TDBSS(ISS)                            
  259 CONTINUE                                                          
C                                                                       
      GO TO ( 280, 260, 270 ), HUMOPT                                   
C---- WET-BULB TEMPERATURE                                              
  260 DO  265  ISS = 1, TNSS                                            
      CALL WETBLB( TDBSS(ISS), HUMSS(ISS), TWBSS(ISS) )                 
      CALL MMAX( TWBSS(ISS), TWBMX(ISS), MXTWB(ISS) )                    
      CALL MMIN( TWBSS(ISS), TWBMN(ISS), MNTWB(ISS) )                    
  265 STWBSS(ISS) = STWBSS(ISS) + TWBSS(ISS)                            
      GO TO 280                                                         
C---- RELATIVE HUMIDITY                                                 
  270 DO  275  ISS = 1, TNSS                                            
      CALL RELHUM( TDBSS(ISS), HUMSS(ISS), TWBSS(ISS) )                 
      CALL MMAX( TWBSS(ISS), TWBMX(ISS), MXTWB(ISS) )                    
      CALL MMIN( TWBSS(ISS), TWBMN(ISS), MNTWB(ISS) )                    
  275 STWBSS(ISS) = STWBSS(ISS) + TWBSS(ISS)                            
C---- DESIGN TEMPERATURE TABULATION                                     
  280 IF( HCOPT ) 309,309,279                                           
  279 DO  300  ILS = 1, NLS                                             
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      ICLUST = TABL44( ISSL )                                           
      IF( TYPCL(ICLUST) - 1 ) 300,276,300                               
  276 IF( DESPER - 1 ) 277,277,291                                      
C                                                                       
C---- MORNING DESIGN PERIOD                                             
  277 IF( WNTSUM ) 285,281,281                                          
C---- SUMMER DESIGN   ( ADD 1 IF ABOVE )                                
  281 DO  283  ILSS = ISSL,ISSH                                         
      DO  282  I = 1, 6                                                 
      IF( TDBSS(ILSS) - DTDMLS(ILS) - WNTSUM*FLOAT(I-3) ) 283,283,282   
  282 EXDTDB(ILSS,I) = EXDTDB(ILSS,I) + 1                               
  283 CONTINUE                                                          
      GO TO 300                                                         
C---- WINTER DESIGN   ( ADD 1 IF BELOW )                                
  285 DO  287  ILSS = ISSL,ISSH                                         
      DO  286  I= 1,6                                                   
      IF( TDBSS(ILSS) - DTDMLS(ILS) - WNTSUM*FLOAT(I-3) ) 286,287,287   
  286 EXDTDB(ILSS,I) = EXDTDB(ILSS,I) + 1                               
  287 CONTINUE                                                          
      GO TO 300                                                         
C                                                                       
C---- EVENING DESIGN PERIOD                                             
  291 IF( WNTSUM ) 295,292,292                                          
C---- SUMMER DESIGN  ( ADD 1 IF ABOVE )                                 
  292 DO  294  ILSS = ISSL,ISSH                                         
      DO  293  I = 1, 6                                                 
      IF( TDBSS(ILSS) - DTDELS(ILS) - WNTSUM*FLOAT(I-3) )294,294,293    
  293 EXDTDB(ILSS,I) = EXDTDB(ILSS,I) + 1                               
  294 CONTINUE                                                          
      GO TO 300                                                         
C---- WINTER DESIGN   ( ADD 1 IF BELOW )                                
  295 DO  297  ILSS = ISSL,ISSH                                         
      DO  296  I = 1, 6                                                 
      IF( TDBSS(ILSS) - DTDELS(ILS) - WNTSUM*FLOAT(I-3))296,297,297     
  296 EXDTDB(ILSS,I) = EXDTDB(ILSS,I) + 1                               
  297 CONTINUE                                                          
  300 CONTINUE                                                          
C                                                                       
C---- NODE SUMMARY    **************************************************
C                                                                       
  309 DO  350  NODE = 1, NNODE                                          
C---- INITIALIZE NODE FLOWS                                             
      DO  310  I = 1,3                                                  
  310 SUMQ(I) = 0.0                                                     
      KSCT = TABL16(NODE)                                               
      DO  340  I = 1, KSCT                                              
      NUMY1=TABL17(NODE,I)                                              
      ISCT=IABS(NUMY1)                                                  
      ISIGN=NUMY1/ISCT                                                  
      ISS=TABL18(NODE,I)                                                
      IF( ISIGN ) 330,320,320                                           
C---- FORWARD END OF SECTION IS CONNECTED TO NODE                       
  320 NODTH = TABL21(ISCT) / 64                                         
      IF( FFSS(ISS) ) 325,328,328                                       
  325 IBRCH = TABL21(ISCT) - NODTH*64                                   
      EMTBTN(NODTH,IBRCH) = EMTBTN(NODTH,IBRCH) - FFSS(ISS)*TDBTN(NODTH)
      EMHBTN(NODTH,IBRCH) = EMHBTN(NODTH,IBRCH) - FFSS(ISS)*HUMTN(NODTH)
      FLOBTN(NODTH,IBRCH) = FLOBTN(NODTH,IBRCH) - FFSS(ISS)             
  328 J = NODTH - TABL22(NODE)                                          
      SUMQ(J) = SUMQ(J) - FFSS(ISS)                                     
      GO TO 340                                                         
C---- BACKWARD END OF SECTION IS CONNECTED TO NODE                      
  330 NODTH = TABL20(ISCT) / 64                                         
      IF( FBSS(ISS) ) 338,338,335                                       
  335 IBRCH = TABL20(ISCT) - NODTH*64                                   
      EMTBTN(NODTH,IBRCH) = EMTBTN(NODTH,IBRCH) + FBSS(ISS)*TDBTN(NODTH)
      EMHBTN(NODTH,IBRCH) = EMHBTN(NODTH,IBRCH) + FBSS(ISS)*HUMTN(NODTH)
      FLOBTN(NODTH,IBRCH) = FLOBTN(NODTH,IBRCH) + FBSS(ISS)             
  338 J = NODTH - TABL22(NODE)                                          
      SUMQ(J) = SUMQ(J) + FBSS(ISS)                                     
  340 CONTINUE                                                          
      ITYPT = TYPTND(NODE)                                              
      GO TO ( 350, 342, 342, 350, 350 ), ITYPT                          
C                                                                       
C***** THERMODYNAMIC TYPE 2 AND 3 NODES (NON-MIXING)                    
C --  SUMQ IS POSITIVE FOR AIR  ENTERING END NODE FROM CENTER NODE      
C---- COMPUTE INTRA-NODE FLOWS                                          
  342 IT22 = TABL22(NODE)                                               
      IF( SUMQ(1) ) 343,343,344                                         
C---- AIR IS FLOWING FROM 1(A) TO 2(B)                                  
  343 EMTBTN(IT22+1,5) = EMTBTN(IT22+1,5) - SUMQ(1)*TDBTN(IT22+1)       
      EMHBTN(IT22+1,5) = EMHBTN(IT22+1,5) - SUMQ(1)*HUMTN(IT22+1)       
      FLOBTN(IT22+1,5) = FLOBTN(IT22+1,5) - SUMQ(1)                     
      GO TO 345                                                         
C---- AIR IS FLOWING FROM 2(B) TO 1(A)                                  
  344 EMTBTN(IT22+2,4) = EMTBTN(IT22+2,4) + SUMQ(1)*TDBTN(IT22+2)       
      EMHBTN(IT22+2,4) = EMHBTN(IT22+2,4) + SUMQ(1)*HUMTN(IT22+2)       
      FLOBTN(IT22+2,4) = FLOBTN(IT22+2,4) + SUMQ(1)                     
C                                                                       
  345 IF( SUMQ(3) ) 346,346,347                                         
C***** AIR IS FLOWING FROM 3(C) TO 2(B)                                 
  346 EMTBTN(IT22+3,5) = EMTBTN(IT22+3,5) - SUMQ(3)*TDBTN(IT22+3)       
      EMHBTN(IT22+3,5) = EMHBTN(IT22+3,5) - SUMQ(3)*HUMTN(IT22+3)       
      FLOBTN(IT22+3,5) = FLOBTN(IT22+3,5) - SUMQ(3)                     
      GO TO 350                                                         
C---- AIR IS FLOWING FROM 2(B) TO 3(C)                                  
  347 EMTBTN(IT22+2,5) = EMTBTN(IT22+2,5) + SUMQ(3)*TDBTN(IT22+2)       
      EMHBTN(IT22+2,5) = EMHBTN(IT22+2,5) + SUMQ(3)*HUMTN(IT22+2)       
      FLOBTN(IT22+2,5) = FLOBTN(IT22+2,5) + SUMQ(3)                     
C                                                                       
  350 CONTINUE                                                          
      DO 355 NODTH = 1,NTNODE                                           
      TAVGTN(NODTH) = TAVGTN(NODTH)+TDBTN(NODTH)                        
      HAVGTN(NODTH) = HAVGTN(NODTH)+HUMTN(NODTH)                        
  355 CONTINUE                                                          
C---- FOR FIRE SIMULATION - COMPUTE SUMMATION OF CONVECTION AND         
C---- RADIATION HEAT TRANSFER TO WALL                                   
      IF( FIROP2 .LE. 0 )  GO TO 369                                    
  360 DO  367  ILS = 1, NLS                                             
      IF( TYPLS(ILS) .GE. 0 ) GO TO 367                                 
      ISSL = TABL3( ILS )                                               
      ISSH = TABL4( ILS )                                               
      SQWL = 0.0                                                        
      SQRD = 0.0                                                        
      DO  365  ISS = ISSL, ISSH                                         
      SQWL = SQWL + QWALSS(ISS)                                         
  365 SQRD = SQRD + QRADSS(ISS)                                         
      SQWLLS(ILS) = SQWLLS(ILS) + SQWL                                  
      SQRDLS(ILS) = SQRDLS(ILS) + SQRD                                  
  367 CONTINUE                                                          
  369 CONTINUE                                                          
C                                                                       
C                                                                       
C     INCREMENT MXMNPT WHICH IS A COUNT OF THE NUMBER OF POINTS WHICH   
C     HAVE BEEN ADDED INTO TOTALS                                       
C                                                                       
  400 MXMNPT = MXMNPT + 1                                               
      RETURN                                                            
      END                                                               
