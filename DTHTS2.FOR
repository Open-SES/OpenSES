      SUBROUTINE DTHTS2                                                 
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'DSHRHS'                                                 
C                                                                       
C                                                                       
  119 FORMAT (///,T4,'SUBSEGMENT',T24,'WALL SURFACE TEMPERATURE',T62,'SU
     1SEGMENT AIR TEMPERATURE',T102,'PREVIOUS AIR TEMPERATURE'/T6,      
     2'NUMBER',T24,'AVERAGE',T41,'MAXIMUM',T62,'AVERAGE',T80,'MAXIMUM',
     3 T102,'AVERAGE',T119,'MAXIMUM'/T4,10('-'),T24,8('-'),T40,8('-'),
     4 T62,8('-'),T80,7('-'),T102,7('-'),T119,7('-')/  T6,'(ISS)',                    
     6 T24,'(TSFALS)',T40,'(TSFMLS)',T62,'(TSMEAN)',T80,'(TSMAX)',T102,
     7 '(TSMN2)',T119,'(TSMX2)'/)                                          
  120 FORMAT (///,T6,'THERMO',T27,'SENSIBLE FLUX RATE',T66,'SENSIBLE FLU
     1X RATE',T105,'SENSIBLE FLUX RATE'/T7,'NODE',T24                   
     2   ,'AVERAGE',T41,'MAXIMUM',T62,'AVERAGE',T80,'MAXIMUM',T102,'AVER
     3AGE',T119,'MAXIMUM'/ T5,8('-'),T24,8('-'),T40,8('-'),T62,8('-'),T8
     40,7('-'),T102,7('-'),T119,7('-')/  T5,'(INODTH)',                 
     5T24,'(TSFALS)',T40,'(TSFMLS)',T62,'(TSMEAN)',T80,'(TSMAX)',T102,'(
     6TSMN2)',T119,'(TSMX2)'/)                                          
  124 FORMAT (T4,I6,T19,F12.3,F16.3,F23.3,F17.3,F22.3,F17.3)            
  150 FORMAT (/T4,10('-'),' THIS IS THE END OF ITERATION NUMBER',I3,1X, 
     15('-'),I4,' DIFFERENCES GREATER THAN 0.3 DEG-F ',27('-')/)        
  175 FORMAT (///T9,'SUBSEGMENT',T38,'HUMIDITY RATIO'/T9,'  NUMBER',T34
     1,'MORNING',T49,'EVENING'/T9,10('-'),T33,8('-'),T49,8('-')/T11,  
     2   '(ISS)',T33,'(HUMMSS)',T49,'(HUMESS)'/)                        
  176 FORMAT (//T11,'THERMO',T36,'HUMIDITY FLUX RATE'/T12,'NODE'    , T3
     14,'MORNING',T49,'EVENING'/T10, 8('-'),T33,8('-'),T49,8('-')/T10,  
     2'(INODTH)',T33,'(HUMMSS)',T49,'(HUMESS)'/)                        
  180 FORMAT (T9,I6,10X,2F16.6)                                        
  185 FORMAT(A1)                                                        
  197 FORMAT(T51,'SES HEAT SINK ANALYSIS'/T51,'--- ---- ---- --------'  
     1 ///)                                                             
  198 FORMAT(/,T55,'ZONE NUMBER',1X,I3//)                               
  200 FORMAT( / ,T18,'SYSTEM',T55,'OFF HOUR AVERAGE',T96,'OFF HOUR AVERA
     1GE'/T15,'PARTITIONING',T55,'AIR TEMPERATURE',T97,'HUMIDITY RATIO'/
     2T59,'(DEG F)',T101,'(LB/LB)'/)                                    
  202 FORMAT( / ,T14,I3,' -',I3,' -',I3,T58,F7.1,T99,F9.5)             
  205 FORMAT( / ,T5,'SYSTEM',T15,'MORNING RUSH HOUR',T35,'EVENING RUSH H
     1OUR',T55,'MORNING RUSH HOUR',T75,'EVENING RUSH HOUR',T94,'MORNING 
     2RUSH HOUR',T113,'EVENING RUSH HOUR'/T2,'PARTITIONING',T15,'WALL SU
     3RFACE TEMP.',T35,'WALL SURFACE TEMP.',T55,'AVERAGE AIR TEMP.',T75,
     4'AVERAGE AIR TEMP.',T96,'HUMIDITY RATIO',T115,'HUMIDITY RATIO'/T20
     5,'(DEG F)',T40,'(DEG F)',T60,'(DEG F)',T80,'(DEG F)',T100,'(LB/LB)
     6',T119,'(LB/LB)')                                                 
  206 FORMAT(/I3,' -',I3,' -',I3,T17,F9.1,T37,F9.1,T57,F9.1,T77,F9.1,
     1T97,F9.5,T116,F9.5)                                            
      DO 1 ISS = 1,TNSS                                                 
      EMTFSS(ISS) = EMTFSS(ISS)/FLOAT(MXMNPT)                           
      EMTBSS(ISS) = EMTBSS(ISS)/FLOAT(MXMNPT)                           
      EMHFSS(ISS) = EMHFSS(ISS)/FLOAT(MXMNPT)                           
      EMHBSS(ISS) = EMHBSS(ISS)/FLOAT(MXMNPT)                           
      FLOFSS(ISS) = FLOFSS(ISS)/FLOAT(MXMNPT)                           
      FLOBSS(ISS) = FLOBSS(ISS)/FLOAT(MXMNPT)                           
      SHRSS(ISS) = SHRSS(ISS)/FLOAT(MXMNPT)                             
      STDBSS(ISS) = STDBSS(ISS)/FLOAT(MXMNPT)                           
      TSMAX(ISS) = STDBSS(ISS)                                          
      TSMEAN(ISS) = TSMAX(ISS)-.8*(TAMBM-TAMBA)                         
      HSUMSS(ISS) = HSUMSS(ISS)/FLOAT(MXMNPT)                           
      HTSMSS(ISS) = HTSMSS(ISS)/FLOAT(MXMNPT)                           
      QSUMSS(ISS) = QSUMSS(ISS)/FLOAT(MXMNPT)                           
      LSUMSS(ISS) = LSUMSS(ISS)/FLOAT(MXMNPT)/HVH2O                     
      TSMN2(ISS) = TSMEAN(ISS)                                          
      TSMX2(ISS) = TSMAX(ISS)                                           
      DUMY1 = MONTH+5                                                   
      TMALSS(ISS) = TSMEAN(ISS)-.5*ANNAMP*COS(2.*PI*DUMY1/12.)          
    1 HCNLSS(ISS)= HSUMSS(ISS)*.5*3600.                                 
      DO 8 NODE = 1,NNODE                                               
      ITYPN = TYPTND(NODE)                                              
      GO TO (4,2,2,4,4), ITYPN                                          
    2 DO 3 I = 1,3                                                      
      NODTH = TABL22(NODE)+I                                            
      TAVGTN(NODTH) = TAVGTN(NODTH)/FLOAT(MXMNPT)                       
      HAVGTN(NODTH) = HAVGTN(NODTH)/FLOAT(MXMNPT)                       
      DO 3 J = 1,5                                                      
      EMTBTN(NODTH,J) = EMTBTN(NODTH,J)/FLOAT(MXMNPT)                   
      FLOBTN(NODTH,J) = FLOBTN(NODTH,J)/FLOAT(MXMNPT)                   
    3 EMHBTN(NODTH,J) = EMHBTN(NODTH,J)/FLOAT(MXMNPT)                   
      GO TO 8                                                           
    4 NODTH = TABL22(NODE)+1                                            
      TAVGTN(NODTH) = TAVGTN(NODTH)/FLOAT(MXMNPT)                       
      HAVGTN(NODTH) = HAVGTN(NODTH)/FLOAT(MXMNPT)                       
      IF(ITYPN-4) 5,7,7                                                 
    5 DO 6 J = 1,5                                                      
      EMTBTN(NODTH,J) = EMTBTN(NODTH,J)/FLOAT(MXMNPT)                   
      FLOBTN(NODTH,J) = FLOBTN(NODTH,J)/FLOAT(MXMNPT)                   
    6 EMHBTN(NODTH,J) = EMHBTN(NODTH,J)/FLOAT(MXMNPT)                   
      GO TO 8                                                           
    7 EMTBTN(NODTH,1) = EMTBTN(NODTH,1)/FLOAT(MXMNPT)                   
      EMHBTN(NODTH,1) = EMHBTN(NODTH,1)/FLOAT(MXMNPT)                   
      FLOBTN(NODTH,1) = FLOBTN(NODTH,1)/FLOAT(MXMNPT)                   
    8 CONTINUE                                                          
      IF (NLS) 410,410,9                                                
    9 DO 400 ISEG=1,NLS                                                 
      ILSSL = TABL3(ISEG)                                               
      ILSSH = TABL4(ISEG)                                               
      TAALS(ISEG) = .5*ANNAMP                                           
      ICLST = TABL44(ILSSL)                                             
      IF(TYPCL(ICLST)-1) 300,310,300                                    
  300 DO 305 ISS = ILSSL, ILSSH                                         
      TSFMLS(ISS) = TSFSS(ISS)                                          
      TSFALS(ISS) = TSFMLS(ISS)-0.6*(TAMBM-TAMBA)                       
  305 CONTINUE                                                          
      GO TO 400                                                         
  310 GO TO (315,325,325),DESPER                                        
  315 DO 320 ISS = ILSSL, ILSSH                                         
      TSFALS(ISS) = TSFSS(ISS)                                          
      TSMEAN(ISS) = DTDMLS(ISEG)                                        
      TSMAX(ISS) = DTDELS(ISEG)                                         
      TSFMLS(ISS) = TSMAX(ISS)-(TSMEAN(ISS)-TSFALS(ISS))                
  320 CONTINUE                                                          
      GO TO 400                                                         
  325 DO 330 ISS = ILSSL, ILSSH                                         
      TSFMLS(ISS) = TSFSS(ISS)                                          
      TSMAX(ISS) = DTDELS(ISEG)                                         
      TSMEAN(ISS) = DTDMLS(ISEG)                                        
      TSFALS(ISS) = TSMEAN(ISS)-(TSMAX(ISS)-TSFMLS(ISS))                
  330 CONTINUE                                                          
  400 CONTINUE                                                          
  410 IF(NVSEC) 445,445,420                                             
  420 DO 440 IVS=1,NVSEC                                                
      ISSL = TABL5(IVS)                                                 
      ISSH = TABL6(IVS)                                                 
      DO  440  IVSS = ISSL, ISSH                                        
      TSFMLS(IVSS) = TSFSS(IVSS)                                        
  440 TSFALS(IVSS) = TSFSS(IVSS) - 0.6 * ( TAMBM - TAMBA )              
  445 DO 450 J=1,NTNODE                                                 
      J2 = J + TNSS                                                     
      TSFALS(J2) = 0.0                                                  
      TSFMLS(J2) = 0.0                                                  
      TSMN2(J2) = 0.0                                                   
  450 TSMX2(J2) = 0.0                                                   
C                                                                       
C     LOOP OVER ZONE CLUSTERS                                           
C                                                                       
      DO 208 ICLST = 1,NZONE                                            
      IF(TYPCL(ICLST)-2) 208,10,208                                     
   10 ICOUNT = 1                                                        
C                                                                       
C     ASSIGN SUBSEGMENTS AND NODES IN THIS CLUSTER TO LOCATIONS IN DTRM 
C     AND CREATE CORRESPONDING TABLES                                   
C                                                                       
C     INITIALIZE TABLES                                                 
C                                                                       
      NEQDTC = 0                                                        
      DO 11 ISS = 1,TNSS                                                
   11 TABL40(ISS) = 0                                                   
      DO 12 NODTH = 1,NTNODE                                            
   12 TABL42(NODTH) = 0                                                 
      IF(NLS) 19,19,13                                                  
C                                                                       
C     LOOP OVER LINE SEGMENTS ASSIGNING THOSE IN THIS CLUSTER TO DTRM   
C                                                                       
   13 DO 18 ILS=1,NLS                                                   
      ISSL = TABL3(ILS)                                                 
C                                                                       
C     CHECK IF SEGMENT IS IN THIS CLUSTER                               
C                                                                       
      IF(TABL44(ISSL)-ICLST) 18,14,18                                   
   14 ISSH = TABL4(ILS)                                                 
      DO 16 ISS = ISSL, ISSH                                            
      NEQDTC = NEQDTC+1                                                 
      TABL40(ISS) = NEQDTC                                              
   16 TABL41(NEQDTC) = ISS                                              
   18 CONTINUE                                                          
   19 IF(NVSEC) 25,25,20                                                
C                                                                       
C     LOOP OVER VENT SHAFTS ASSIGNING THOSE IN THIS CLUSTER TO DTRM     
C                                                                       
   20 DO 24 IVS=1,NVSEC                                                 
      ISSL = TABL5(IVS)                                                 
C                                                                       
C     CHECK IF VENT SHAFT IS IN THIS CLUSTER                            
C                                                                       
      IF(TABL44(ISSL)-ICLST) 24,21,24                                   
   21 ISSH=TABL6(IVS)                                                   
      DO 22 ISS = ISSL, ISSH                                            
      NEQDTC = NEQDTC+1                                                 
      TABL40(ISS) = NEQDTC                                              
   22 TABL41(NEQDTC) = ISS                                              
   24 CONTINUE                                                          
   25 NSSDTC=NEQDTC                                                     
C                                                                       
C     LOOP OVER THERMAL NODES ASSIGNING THOSE IN THIS CLUSTER TO DTRM   
C                                                                       
      DO 36 NODE = 1,NNODE                                              
C                                                                       
C     CHECK IF NODE IS IN THIS CLUSTER                                  
C                                                                       
      IF(TABL45(NODE)-ICLST) 36,26,36                                   
   26 ITYPN = TYPTND(NODE)                                              
      GO TO (28,30,30,28,28), ITYPN                                     
   28 J = 1                                                             
      GO TO 32                                                          
   30 J = 3                                                             
C                                                                       
C     LOOP OVER THERMAL NODES IN NODE                                   
C                                                                       
   32 DO 34 I = 1,J                                                     
      NODTH = TABL22(NODE)+I                                            
      NEQDTC = NEQDTC+1                                                 
      TABL42(NODTH) = NEQDTC                                            
   34 TABL41(NEQDTC) = -NODTH                                           
   36 CONTINUE                                                          
      DO 38 J = 1,NEQDTC                                                
      DO 38 I = 1,NEQDTC                                                
   38 DTRM(I,J) = 0.0                                                   
      DO 39 I = 1,NEQDTC                                                
      FORCE2(I) = 0.                                                    
      FORCE3(I) = 0.                                                    
   39 HTSKFM(I) = 0.0                                                   
      CALL CREAT2(NEQDTC,ICLST)                                         
C                                                                       
C     EVALUATE WALL TEMPERATURE AT MEAN (8:30 A.M.)                     
C     AND MAXIMUM (5:00 P.M.)                                           
C                                                                       
      ITER=1                                                            
      GO TO (41, 41, 220), DESPER                                       
   41 CALL WLLTM2(TSFALS,-6.0,TSFMLS,0.0,ICLST)                         
C                                                                       
C     LOOP OVER VENT SHAFTS                                             
C                                                                       
   42 DO 115 ISCT = 1,NSEC                                              
      NVSEG = TABL9(ISCT)                                               
      IF(NVSEG) 43,43,115                                               
   43 IVSSL = TABL5(-NVSEG)                                             
      IF(TABL44(IVSSL)-ICLST) 115,45,115                                
   45 IVSSH = TABL6(-NVSEG)                                             
      INODB = TABL12(ISCT)                                              
      INODF = TABL13(ISCT)                                              
      IF(TABL16(INODB)-1) 50,50,55                                      
   50 KNODE = INODF                                                     
      KSS = IVSSH                                                       
      GO TO 65                                                          
   55 IF(TABL16(INODF)-1) 60,60,85                                      
   60 KNODE = INODB                                                     
      KSS = IVSSL                                                       
   65 M = TABL16(KNODE)                                                 
      SUMA = 0.                                                         
      SUMM = 0.                                                         
      DO 75 J = 1,M                                                     
      N = TABL18(KNODE,J)                                               
      IF(N-KSS) 70,75,70                                                
   70 SUMA = SUMA+TSMEAN(N)-TSFALS(N)                                   
      SUMM = SUMM+TSMAX(N)-TSFMLS(N)                                    
   75 CONTINUE                                                          
      DUMYA = SUMA/FLOAT(M-1)                                           
      DUMYM = SUMM/FLOAT(M-1)                                           
      DO 80 ISS = IVSSL,IVSSH                                           
      TSFALS(ISS) = TSMEAN(ISS)-DUMYA                                   
   80 TSFMLS(ISS) = TSMAX(ISS)-DUMYM                                    
      GO TO 115                                                         
   85 SUMA = 0.                                                         
      SUMM = 0.                                                         
      KSSL = IVSSL                                                      
      KSSH = IVSSH                                                      
      M = TABL16(INODF)                                                 
      DO 95 J = 1,M                                                     
      N = TABL18(INODF,J)                                               
      IF(N-KSSH) 90,95,90                                               
   90 SUMA = SUMA+TSMEAN(N)-TSFALS(N)                                   
      SUMM = SUMM+TSMAX(N)-TSFMLS(N)                                    
   95 CONTINUE                                                          
      M = TABL16(INODB)                                                 
      DO 105 J = 1,M                                                    
      N = TABL18(INODB,J)                                               
      IF(N-KSSL) 100,105,100                                            
  100 SUMA = SUMA+TSMEAN(N)-TSFALS(N)                                   
      SUMM = SUMM+TSMAX(N)-TSFMLS(N)                                    
  105 CONTINUE                                                          
      M = TABL16(INODB)+TABL16(INODF)                                   
      DUMYA = SUMA/FLOAT(M-2)                                           
      DUMYM = SUMM/FLOAT(M-2)                                           
      DO 110 ISS = IVSSL,IVSSH                                          
      TSFALS(ISS) = TSMEAN(ISS)-DUMYA                                   
      TSFMLS(ISS) = TSMAX(ISS)-DUMYM                                    
  110 CONTINUE                                                          
  115 CONTINUE                                                          
  220 CALL FORCE1(TSFMLS,TSMAX,TAMBM,4,FORCE3,ICLST,NEQDTC,NSSDTC,ITER) 
      ITER=2                                                            
      GO TO (116,116,117), DESPER                                       
  116 CALL FORCE1(TSFALS,TSMEAN,TAMBA,3,FORCE2,ICLST,NEQDTC,NSSDTC,ITER)
C                                                                       
C     CHECK FOR CONVERGENCE                                             
C                                                                       
  117 K = 0                                                             
      GO TO (121,121,118,121,121), HSOPT                                
  118 WRITE (OUT,119)                                                   
  121 IF(NSSDTC) 140,140,122                                            
  122 DO 138 IASS=1,NSSDTC                                              
      ISS = TABL41(IASS)                                                
      GO TO (125,125,123,125,125), HSOPT                                
  123 WRITE(OUT,124) ISS,TSFALS(ISS),TSFMLS(ISS),TSMEAN(ISS),           
     1 TSMAX(ISS),TSMN2(ISS),TSMX2(ISS)                                 
  125 IF(ABS(TSMEAN(ISS)-TSMN2(ISS))-.3) 128,128,126                    
  126 K = K+1                                                           
  128 TSMN2(ISS) = TSMEAN(ISS)                                          
      DUMY1 = MONTH+5                                                   
      TMALSS(ISS) = TSMEAN(ISS)-.5*ANNAMP*COS(2.*PI*DUMY1/12.)          
      IF(ABS(TSMAX(ISS)-TSMX2(ISS))-.3) 132,132,130                     
  130 K = K+1                                                           
  132 TSMX2(ISS) = TSMAX(ISS)                                           
      GO TO (134,136,136), DESPER                                       
C     DESIGN PERIOD - MORNING                                           
  134 TSFSS(ISS) = TSFALS(ISS)                                          
      TDBSS(ISS) = TSMEAN(ISS)                                          
      TTMPSS(ISS) = TSMEAN(ISS)                                         
      GO TO 138                                                         
C     DESIGN PERIOD - EVENING                                           
  136 TSFSS(ISS) = TSFMLS(ISS)                                          
      TDBSS(ISS) = TSMAX(ISS)                                           
      TTMPSS(ISS) = TSMAX(ISS)                                          
  138 CONTINUE                                                          
  140 CONTINUE                                                          
      GO TO (142,142,141,142,142), HSOPT                                
  141 WRITE (OUT,120)                                                   
  142 J = NSSDTC + 1                                                    
      DO 145 II = J,NEQDTC                                              
      NODTH = -TABL41(II)                                               
      I = TNSS+NODTH                                                    
      INODTH = -NODTH                                                   
      GO TO ( 144,144,143,144,144),HSOPT                                
  143 WRITE(OUT,124) INODTH,TSFALS(I),TSFMLS(I),TSMEAN(I),              
     1 TSMAX(I),TSMN2(I),TSMX2(I)                                       
  144 TSMN2(I) = TSMEAN(I)                                              
      TSMX2(I) = TSMAX(I)                                               
  145 CONTINUE                                                          
      GO TO (146,146,162), DESPER                                       
  146 GO TO (158,158,147,158,158), HSOPT                                
  147 WRITE (OUT,150) ICOUNT, K                                         
  158 ICOUNT = ICOUNT+1                                                 
C     IF THE NUMBER OF ITERATIONS REQUIRED FOR THE HEAT SINK ANALYSIS   
C     TO CONVERGE IS GREATER THAN 50, PRINT SIMULATION ERROR 8          
      IF(ICOUNT-50) 160,160,161                                         
  160 IF(K) 162,162,41                                                  
  161 CALL SIMERR (8)                                                   
  162 DO 165 J = 1,NEQDTC                                               
      DO 165 I = 1,NEQDTC                                               
  165 DTRM(I,J) = 0.0                                                   
      DO 170 I = 1,NEQDTC                                               
      HUMMSS(I) = 0.0                                                   
      FORCE2(I) = 0.0                                                   
      FORCE3(I) = 0.0                                                   
  170 HTSKFM(I) = 0.0                                                   
      CALL CREAT3(NEQDTC,ICLST)                                         
      CALL FORCEL(HUMESS,HMAMBE,4,FORCE3,ICLST,NEQDTC,NSSDTC,1)         
      GO TO (171,171,172), DESPER                                       
  171 CALL FORCEL(HUMMSS,HMAMBM,3,FORCE2,ICLST,NEQDTC,NSSDTC,2)         
  172 GO TO (177,177,174,177,177), HSOPT                                
  174 WRITE (OUT,175)                                                   
  177 IF(NSSDTC) 191,191,178                                            
  178 DO 190 IASS = 1,NSSDTC                                            
      ISS = TABL41(IASS)                                                
      GO TO (181,181,179,181,181), HSOPT                                
  179 WRITE(OUT,180) ISS, HUMMSS(ISS), HUMESS(ISS)                      
  181 GO TO (182,184,184), DESPER                                       
C     DESIGN PERIOD - MORNING                                           
  182 HUMSS(ISS) = HUMMSS(ISS)                                          
      HTMPSS(ISS) = HUMMSS(ISS)                                         
      GO TO 190                                                         
C     DESIGN PERIOD - EVENING                                           
  184 HUMSS(ISS) = HUMESS(ISS)                                          
      HTMPSS(ISS) = HUMESS(ISS)                                         
  190 CONTINUE                                                          
  191 CONTINUE                                                          
      J = NSSDTC+1                                                      
      GO TO (196,196,192,196,196), HSOPT                                
  192 WRITE (OUT,176)                                                   
      DO 195 II=J,NEQDTC                                                
C                                                                       
      NODTH = -TABL41(II)                                               
      I = TNSS+NODTH                                                    
      INODTH = -NODTH                                                   
      WRITE(OUT,180) INODTH,HUMMSS(I),HUMESS(I)                         
  195 CONTINUE                                                          
C                                                                       
C     PRINT HEAT SINK OUTPUT                                            
C                                                                       
  196 IF(NSSDTC) 208,208,201                                            
  201 WRITE(OUT,185) CHAR(12)
      WRITE(OUT,197)                                                    
      WRITE(OUT,198) ICLST                                              
      GO TO(204,204,199),DESPER                                         
C     FOR OFF HOUR                                                      
  199 WRITE(OUT,200)                                                    
      DO 203 IASS=1,NSSDTC                                              
      ISS=TABL41(IASS)                                                  
      CALL INTEXT(ISS,ISCTX,ILSX,ISSX)                                  
      WRITE(OUT,202) ISCTX,ILSX,ISSX,TSMAX(ISS),HUMESS(ISS)             
  203 CONTINUE                                                          
      GO TO 208                                                         
C     FOR MORNING OR EVENING RUSH HOUR                                  
  204 WRITE(OUT,205)                                                    
      DO 207 IASS=1,NSSDTC                                              
      ISS=TABL41(IASS)                                                  
      CALL INTEXT(ISS,ISCTX,ILSX,ISSX)                                  
      WRITE(OUT,206) ISCTX,ILSX,ISSX,TSFALS(ISS),TSFMLS(ISS),TSMEAN(ISS)
     1,TSMAX(ISS),HUMMSS(ISS),HUMESS(ISS)                               
  207 CONTINUE                                                          
  208 CONTINUE                                                          
C                                                                       
      GO TO (210,209,209), DESPER                                       
  209 CALL ACEST2( TSMAX, HUMESS, DTDELS, DTHELS , DTWELS ,4)           
      GO TO 215                                                         
  210 CALL ACEST2(TSMEAN,HUMMSS,DTDMLS,DTHMLS,DTWMLS,3)                 
C                                                                       
  215 RETURN                                                            
      END                                                               
