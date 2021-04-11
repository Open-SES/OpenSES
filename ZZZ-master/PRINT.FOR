      SUBROUTINE PRINT                                                  
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
                                                                        
C                                                                       
                                                                        
    9 FORMAT(A1)   
   15 FORMAT(//)
   20 FORMAT(//,'TIME ',F9.2,' SECONDS',T49,I2,                         
     1  ' TRAIN(S) ARE OPERATIONAL')                                    
   26 FORMAT( 10X )                                                     
   31 FORMAT( / ,T7,'R T',T43,'AIR   COEF. OF   TRACTIVE     MOTOR    LI
     1NE    FLYWHEEL     GRID TEMP.     HEAT    HEAT' /                 
     2 'TRAIN T Y  LOCATION  SPEED  ACCELERATION  DRAG  AIR DRAG    EFFO
     3RT     CURRENT  CURRENT   SPEED     ACCEL.   DECEL.  GEN.   REJECT
     4.' /                                                             
     5 ' NO.  E P   (FEET)   (MPH)   (MPH/SEC)   (LBS)', T59,          
     6 '(LBS/MOTOR)  (AMPS)   (AMPS)    (RPM)    (DEG F)  (DEG F)   (BTU
     7/SEC-FT)'/)                                                    
   35 FORMAT( 1X,A2,I4,I2,F10.2, F7.2, F10.2, F10.0, F8.2, F13.0,       
     1  F10.0, F9.0, F10.0, 2F9.1, 2F8.3)                               
  710 FORMAT(T14,'SYSTEM',T29,'SENSIBLE',T44,'LATENT',T62,'AIR',T72,
     1'HUMIDITY',T85,'AIR',T97,'AIR',T105,'TRAIN POSITION-ROUTE NUMBER',
     2/,' LENGTH',T11,'PARTITIONING',T29,'HEAT LOAD',T43,'HEAT LOAD',
     3  T58,'TEMPERATURE',T73,'RATIO',T85,'FLOW',T94,'VELOCITY',        
     4  T104,' 0 0 0 0 0 0 0 0 0 1 1 1 1 1' /                                                
     5  2X,'(FT)',T29,'(BTU/SEC)',T43,'(BTU/SEC)',T60,'(DEG F)',        
     6  T72,'(LB/LB)',T84,'(CFM)',T96,'(FPM)',T104,' 1 2 3 4 5 6 7 8 9 0
     7 1 2 3 4'/)                                                              
  712 FORMAT( I12,2(' -',I3),F14.1,F14.1,F16.2,F12.5,F12.1,F10.1,       
     1  3X,14(A2) )                                                
  713 FORMAT((I12,2(' -',I3),F14.1,F14.1,F16.2,F12.5,25X,14(A2) )) 
  714 FORMAT( I12,2(' -',I3),26X,F18.2,F12.5,F12.1,F10.1 )              
  715 FORMAT((I12,2(' -',I3),26X,F18.2,F12.5 ) )                        
  720 FORMAT(T14,'SYSTEM',T29,'SENSIBLE',T44,'LATENT',T62,'AIR',T71,
     1'WET-BULB',T85,'AIR',T97,'AIR',T105,'TRAIN POSITION-ROUTE NUMBER',          
     2 /,' LENGTH',T11,'PARTITIONING',T29,'HEAT LOAD',T43,'HEAT LOAD',
     3  T58,'TEMPERATURE',T70,'TEMPERATURE',T85,'FLOW',T94,'VELOCITY',  
     4  T104,' 0 0 0 0 0 0 0 0 0 1 1 1 1 1' /                                                
     5  2X,'(FT)',T29,'(BTU/SEC)',T43,'(BTU/SEC)',T60,'(DEG F)',        
     6  T71,'(DEG F)',T84,'(CFM)',T96,'(FPM)',T104,' 1 2 3 4 5 6 7 8 9 0    
     7 1 2 3 4 ',/)                                                             
  722 FORMAT( I12,2(' -',I3),F14.1,F14.1,F16.2,F11.2,F13.1,F10.1,       
     1  3X,14(A2) )                                                
  723 FORMAT((I12,2(' -',I3),F14.1,F14.1,F16.2,F11.2,26X,14(A2) )) 
  724 FORMAT( I12,2(' -',I3),26X,F18.2,F11.2,F13.1,F10.1 )              
  725 FORMAT((I12,2(' -',I3),26X,F18.2,F11.2  ) )                       
  730 FORMAT(T14,'SYSTEM',T29,'SENSIBLE',T44,'LATENT',T62,'AIR',T71,
     1'RELATIVE',T85,'AIR',T97,'AIR',T105,'TRAIN POSITION-ROUTE NUMBER',          
     2 /,' LENGTH',T11,'PARTITIONING',T29,'HEAT LOAD',T43,'HEAT LOAD',
     3  T58,'TEMPERATURE',T71,'HUMIDITY',T85,'FLOW',T94,'VELOCITY',     
     4  T104,' 0 0 0 0 0 0 0 0 0 1 1 1 1 1' /                                                
     5  2X,'(FT)',T29,'(BTU/SEC)',T43,'(BTU/SEC)',T60,'(DEG F)',        
     6  T71,'(PERCENT)',T84,'(CFM)',T96,'(FPM)',T104,' 1 2 3 4 5 6 7 8 9 
     7 0 1 2 3 4',/)                                                            
  732 FORMAT( I12,2(' -',I3),F14.1,F14.1,F16.2,F11.1,F13.1,F10.1,       
     1  3X,14(A2) )                                                
  733 FORMAT((I12,2(' -',I3),F14.1,F14.1,F16.2,F11.1,26X,14(A2) )) 
  734 FORMAT( I12,2(' -',I3),26X,F18.2,F11.1,F13.1,F10.1 )              
  735 FORMAT((I12,2(' -',I3),26X,F18.2,F11.1  ) )                       
  740 FORMAT()                                                       
  750 FORMAT( / ,F7.1,I5,' -',I3,5X,'(TUNNEL)',            T49,A36 /)   
  751 FORMAT( / ,F7.1,I5,' -',I3,5X,'(STATION)',           T49,A36 /)   
  752 FORMAT( / ,F7.1,I5,' -',I3,5X,'(SPECIAL TUNNEL)',    T49,A36 /)   
  753 FORMAT( / ,F7.1,I5,' -',I3,5X,'(VENTILATION SHAFT)', T49,A36 /)   
  754 FORMAT( / ,F7.1,I5,' -',I3,5X,'(STAIRWAY)',          T49,A36 /)   
  755 FORMAT( / ,F7.1,I5,' -',I3,5X,'(SPECIAL VENT SHAFT)',T49,A36,/)   
  770 FORMAT( / ,T4,'SYSTEM',T22,'AIR',5X,'AIR',                        
     1  T38,'TEMPERATURE (DEG F)' /                                     
     2  ,'PARTITIONING',T21,'FLOW',5X,'VEL.',4X,19('-') /            
     3  ,T21,'(CFM)',4X,'(FPM)',3X,'HUMIDITY    (LB/LB)' / )         
  771 FORMAT( / ,T4,'SYSTEM',T22,'AIR',5X,'AIR' /                       
     1  ,'PARTITIONING',T21,'FLOW',5X,'VEL.' /                       
     2  ,T21,'(CFM)',4X,'(FPM)' / )                                  
  775 FORMAT( I5, ' -', I3, F15.1, F9.1, 8F10.1 )                   
  777 FORMAT( T35, 8F10.1 )                                            
  778 FORMAT( T35, 8F10.4 )                                            
  641 FORMAT( / ,T7,'R T', T18,13('-'), ' PROPULSION POWER (W/TRAIN) ', 
     1 13('-'),T73,'HEAT GENERATION(BTU/SEC-TRAIN)',                    
     2 T105,'HEAT REJECTION(BTU/SEC-FT)' /,                             
     3 'TRAIN T Y',T32,'PROPULSION',T46,'REGENERATED',T66,'FROM',   
     4  T77,'ACCEL',T87, 'DECEL', T106,'PROPUL',T118,'AUX', T126,'AUX'/ 
     5 1X,'NO.  E P  MODE  AUXILARIES',2(6X, '3RD RAIL'), T64,'FLYWHEEL'
     6 ,T77,'GRID', T87,'GRID',T98,'MECH', T107,'SENS', T118,'SENS',    
     7  T125,'LATENT' / 4X )                                            
  646 FORMAT(1X,A2,2X,2I2, I4, F13.0, F15.0, F14.0, F16.0, 5F10.3, F9.4)           
  945 FORMAT(//T2,'SEGMENT',T13,'NO. OF TRAINS',T34,'TRAIN NUMBER AND DI
     1STANCE FROM BACKWARD END OF SEGMENT TO BACKWARD AND FORWARD END OF
     2 TRAIN'/T2,7('-'),T13,13('-'),T29,102('-')/T2,'(NUMLS)',T14,'(NTRN
     3LS)',T71,'(TRNNLS AND TRNDLS)'/)                                  
  957 FORMAT (T2,I4,T12,I7,T29,6(I3,2F7.1))                             
   44 FORMAT (131('-'))                                             
   57 FORMAT ( / ,T25,'SECTION PRESSURE CHANGES  ( SECTION NUMBER AND TO
     1TAL PRESSURE CHANGE - IN. WG )'/T25,79('-'),/)                    
   58 FORMAT(  8 ( I6, F9.5 ) )                                         
 1020 FORMAT(///, T12, 'SYSTEM', T29, 'WALL SURFACE',T51, 'HEAT TRANSFER           
     1 TO WALL' ,/, T9,'PARTITIONING', T29, 'TEMPERATURE', T49, 'CONVECT
     2ION',T65,'RADIATION',/,T31,'(DEG F)',T50,'(BTU/HR)',T65,'(BTU/HR)'
     3,/)              
 1050 FORMAT( T8, I3, ' -', I3, ' -', I3, F16.1, F20.1, F15.1 )         
  810 FORMAT (    T35,'INSTANTANEOUS THERMODYNAMIC CHARACTERISTICS FOR E
     1ACH SUBSEGMENT'///T7,'SUBSEGMENT',T23,'ADJUSTED    BACK END   FRON
     2T END  HEAT TRANSFER   SENSIBLE    LATENT      REYNOLDS',T114,'INT
     3EGRATION'/T9 ,'NUMBER',T24,'VOLUME     AIR FLOW   AIR  FLOW   COEF
     4FICIENT   HEAT LOAD   HEAT LOAD',T98,'NUMBER',T112,'TEMP.',T120,  
     5'HUMIDITY'/ T7,10('-'),T23,8('-'),4X,8('-'),3X,9('-'),2X,13('-'), 
     62X,9('-'),3X,9('-'),T97,8('-'),T110,8('-'),2X,8('-')/T23,'(VOLSS)'
     7,T36,'(FBSS)',T47,'(FFSS)',T59,'(HTRNSS)',T73,'(SHLTSS)',T85,'(LHL
     8TSS)',T98,'(RELS)',T110,'(TTMPSS)  (HTMPSS)'/)                    
  840 FORMAT (T5,2(I3,' -'),I3,T21,F10.0,F12.2,F12.2,F13.7,F12.3,F12.3, 
     1E14.5,F11.3,F11.6)                                                
  871 FORMAT (//,  T8,'EXTERNAL',T36, 'DRY BULB'/T8,'  NODE',T34,'TEMPER
     1ATURE',T60,'HUMIDITY' /T8 ,8('-'),T34,11('-'),T60,8('-')/T8 ,'(TAB
     2L15)',T34,'  (TDBTN)',T61,'(HUMTN)')                              
  872 FORMAT (/,T9,I4,T32,F10.2,T57,F11.5)                              
  876 FORMAT (T32,F10.2,T57,F11.5)                                      
884   FORMAT(//T8, 'ISCTX-NUMLS-ISS',T34,'BUOYS(ISCT)',                 
     1T51,'TSSTAB(ISS)',T68,'RELSS(ISS)',T84,'TSFSS(ISS)',T100,         
     2'QWALSS(ISS)',T117,'QRADSS(ISS)',/)                               
885   FORMAT(T8, I3, T32, F12.4 )                                     
886   FORMAT(T8,I3,' -',I3,' -',I3,T51,F10.6,T68,F10.0,T85,F8.2,T101,   
     1  F10.5, T118, F10.2 )                                            
887   FORMAT( // T8, 'IUL', T36 ,'THROTL(IUL)'/)                        
888   FORMAT( T8, I3, T34, F12.4 )                                      

                                                                        
C                                                                       
C---- DETAILED PRINT OF SUBSEGMENT DATA      ***************************
C                                                                       
C---- WRITE PAGE HEADINGS                                               
C
C---- CHECK IF A PAGE BREAK HAS ALREADY BEEN WRITTEN IN
C---- SUBROUTINE SIMLAT (I.E. VARIABLE NEWPAGE=1).  IF 
C---- NOT START A NEW PAGE OTHERWISE RESET VARIABLE 
C---- NEWPAGE EQUAL TO 0.
C    
      IF (NEWPAGE .EQ. 1)THEN
        NEWPAGE=0
      ELSE
        WRITE (OUT,9) CHAR(12)
      END IF
C
C     CHECK IF THERMO IS BEING SIMULATED                                
      IF( JRATTH ) 799, 11, 7                                           
    7 KPRNTH=KPRNTH+1                                                   
      IF(GRABBP(IPRGRP)-KPRNTH)8,11,11                                  
C---- DETAILED PRINT  IABB=1                                            
    8 KPRNTH=0                                                          
      IABB = 1                                                          
      GO TO 12                                                          
C---- ABBREVIATED PRINT   IABB=2                                        
   11 IABB = 2                                                          
   12 TIME=FLOAT(NTIME)/100.0+0.001                                     
      WRITE(OUT,20)TIME,NTRAN                                           
      IF( NTRAN ) 25,25,30                                              
   25 WRITE(OUT,26)                                                     
      GO TO 41                                                          
C                                                                       
C---- PRINT TRAIN INFORMATION                                           
C                                                                       
   30 WRITE(OUT,31)                                                     
      DO 40 I=1,NTRAN                                                   
      NUMV=IINDEX(I)                                                     
      U=UV(NUMV)*FPSMPH                                                 
      AC=DUDTV(NUMV)*FPSMPH                                             
      ITYP=ITYPV(NUMV)                                                  
      IF( ABS(U)-0.1) 37,37,36                                          
   36 CDV = 2.0*DRAGV(NUMV)/(RHOMAS*AV(ITYP)*UV(NUMV)**2)               
      GO TO 38                                                          
   37 CDV = 0.0                                                         
   38 RPM = RPS(NUMV)*60.0                                              
      HETGEN =QAXSV(NUMV)+(QACCV(NUMV)+QDECV(NUMV)+RMHTV(NUMV))/LV(ITYP)
      QTRPF = QPRPV(NUMV) + QAXSV(NUMV)                                 
   40 WRITE(OUT,35) TNUMV(NUMV),IROTEV(NUMV),ITYPV(NUMV),XV(NUMV),U,AC, 
     1  DRAGV(NUMV),CDV,TEV(NUMV),AMPV(NUMV),AMPLV(NUMV),RPM,           
     2  TGACCV(NUMV),TGDECV(NUMV),HETGEN,QTRPF                          
C---- OPTIONAL PRINTOUT OF TRAIN POWER AND HEAT INFO                    
      IF( SUPOPT - 2 ) 649,649,640                                      
  640 WRITE(OUT,641)                                                    
      DO  645  I=1,NTRAN                                                
      NUMV = IINDEX(I)                                                   
  645 WRITE(OUT,646) TNUMV(NUMV),IROTEV(NUMV),ITYPV(NUMV),MODEV(NUMV),  
     1  PAUXV(NUMV),PPROPV(NUMV), PREGNV(NUMV), PFLYV(NUMV),            
     2  QACCV(NUMV),QDECV(NUMV), RMHTV(NUMV), QPRPV(NUMV),              
     3  QAXSV(NUMV), QAXLV(NUMV)                                        
  649 CONTINUE                                                          
C                                                                       
C     DUMP LOCATE ARRAYS, IF SUPPLEMENTARY OUTPUT OPTION IS 3, 4 OR 5   
C                                                                       
      GO TO (41,41,41,940,940,940,41,41), SUPOPT                        
  940 IF( NLS ) 41,41,941                                               
  941 WRITE(OUT,945)                                                    
      DO  960  ILS = 1, NLS                                             
      K = NTRNLS(ILS)                                                   
      IF( K ) 950,950,955                                               
  950 WRITE(OUT,957) NUMLS(ILS),K                                       
      GO TO 960                                                         
  955 WRITE(OUT,957) NUMLS(ILS),K,(TRNNLS(ILS,J),TRNDLS(ILS,2*J-1),     
     1                TRNDLS(ILS,2*J),J=1,K)                            
  960 CONTINUE                                                          
C                                                                       
C                                                                       
   41 IF( NOSYS ) 799,799,42                                            
   42 GO TO ( 43,50 ), IABB                                             
C                                                                       
   43 WRITE(OUT,44)                                                     
C                                                                       
C     OPTIONAL PRINT OF SECTION TOTAL PRESSURE CHANGES                  
C                                                                       
   50 GO TO (59,59,59,52,59,52,59,59), SUPOPT                           
   52 IF (NSEC) 59,59,54                                                
   54 ALTER=RHOMAS/WGCONS                                               
      DO 56 ISCT=1,NSEC                                                 
   56 OMEGS(ISCT)=ALTER*(OMEGS(ISCT)-BS(ISCT)*DQDTS(ISCT))              
      WRITE (OUT,57)                                                    
      WRITE (OUT,58) (TABL11(ISCT),OMEGS(ISCT),ISCT=1,NSEC)             
      WRITE (OUT,15)
   59 GO TO (60,400), IABB                                              
C                                                                       
C********************************************************************   
C                                                                       
C     DETAILED PRINT ROUTINE     ***************************************
C                                                                       
C**** WRITE SUBSEGMENT HEADINGS                                         
   60 GO TO ( 100, 110, 120 ), HUMOPT                                   
C---- HUMIDITY RATIO                                                    
  100 WRITE(OUT,710)                                                    
      GO TO 130                                                         
C---- WET-BULB TEMPERATURE                                              
  110 WRITE(OUT,720)                                                    
      GO TO 130                                                         
C---- RELATIVE HUMIDITY                                                 
  120 WRITE(OUT,730)                                                    
C----                                                                   
C---- LOOP OVER SECTION EXTERNAL I.D. NUMBERS                           
  130 DO  250  ISCTX = 1,LMSCTX                                         
C---- CHECK IF SECTION I.D. NUMBER HAS BEEN USED                        
      IF( TABL14(ISCTX) ) 250,250,135                                   
  135 ISCT = TABL14(ISCTX)                                              
CCC      WRITE(OUT,740)
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL ) 170,250,140                                           
C                                                                       
C---- PRINT LINE SECTION DATA - LOOP OVER LINE SEGMENTS                 
C                                                                       
  140 DO  169  ILS = ISEGL,ISEGH                                        
      ITYP= IABS(TYPLS(ILS))                                            
      GO TO (142,144,144,144,146,146,146,146,146,146,146,146,146,146,   
     1         146,146), ITYP                                           
  142 WRITE(OUT,750)  LLS(ILS), ISCTX, NUMLS(ILS), NAMLS(ILS) 
      GO TO 150                                                         
  144 WRITE(OUT,751)  LLS(ILS), ISCTX, NUMLS(ILS), NAMLS(ILS) 
      GO TO 150                                                         
  146 WRITE(OUT,752)  LLS(ILS), ISCTX, NUMLS(ILS), NAMLS(ILS) 
C                                                                       
  150 Q = QSAVES(ISCT) * 60.0                                           
      V = Q / ALS(ILS)                                                  
      ILSSL = TABL3(ILS)                                                
      ILSSH = TABL4(ILS)                                                
      GO TO ( 155, 160, 165 ),HUMOPT                                    
C---- HUMIDITY RATIO                                                    
  155 J = 1                                                             
      WRITE(OUT,712)ISCTX,NUMLS(ILS),J,SHLTSS(ILSSL),LHLTSS(ILSSL),     
     1  TDBSS(ILSSL),HUMSS(ILSSL),Q,V,(ITNLSS(ILSSL,K),K=1,14)           
      J3 = ILSSH - ILSSL + 1                                            
      J2 = ILSSL - 1                                                    
      IF( J3 - 2 ) 169,157,157                                          
  157 WRITE(OUT,713) (ISCTX,NUMLS(ILS),J,SHLTSS(J2+J),LHLTSS(J2+J),     
     1  TDBSS(J2+J),HUMSS(J2+J),(ITNLSS(J2+J,K),K=1,14),J=2,J3)          
      GO TO 169                                                         
C---- WET-BULB TEMPERATURE                                              
  160 DO  161  ILSS = ILSSL, ILSSH                                      
  161 CALL WETBLB( TDBSS(ILSS), HUMSS(ILSS), TWBSS(ILSS) )              
      J = 1                                                             
      WRITE(OUT,722)ISCTX,NUMLS(ILS),J,SHLTSS(ILSSL),LHLTSS(ILSSL),     
     1  TDBSS(ILSSL),TWBSS(ILSSL),Q,V,(ITNLSS(ILSSL,K),K=1,14)           
      J3 = ILSSH - ILSSL + 1                                            
      J2 = ILSSL - 1                                                    
      IF( J3 - 2 ) 169,163,163                                          
  163 WRITE(OUT,723) (ISCTX,NUMLS(ILS),J,SHLTSS(J2+J),LHLTSS(J2+J),     
     1  TDBSS(J2+J),TWBSS(J2+J),(ITNLSS(J2+J,K),K=1,14),J=2,J3)          
      GO TO 169                                                         
C---- RELATIVE HUMIDITY                                                 
  165 DO  166  ILSS = ILSSL,ILSSH                                       
  166 CALL RELHUM( TDBSS(ILSS), HUMSS(ILSS), TWBSS(ILSS) )              
      J = 1                                                             
C---- NOTE THAT ONLY THE FIRST FOURTEEN ROUTES ARE SHOWN ON THIS DETAIL
C---- PRINT (I.E. ITNLSS (ILSSL,K) WITH K GOING FROM 1 TO 14.
      WRITE(OUT,732)ISCTX,NUMLS(ILS),J,SHLTSS(ILSSL),LHLTSS(ILSSL),     
     1  TDBSS(ILSSL),TWBSS(ILSSL),Q,V,(ITNLSS(ILSSL,K),K=1,14)           
      J3 = ILSSH - ILSSL + 1                                            
      J2 = ILSSL - 1                                                    
      IF( J3 - 2 ) 169,168,168                                          
  168 WRITE(OUT,733) (ISCTX,NUMLS(ILS),J,SHLTSS(J2+J),LHLTSS(J2+J),     
     1  TDBSS(J2+J),TWBSS(J2+J),(ITNLSS(J2+J,K),K=1,14),J=2,J3)          
  169 CONTINUE                                                          
      GO TO 250                                                         
C                                                                       
C---- PRINT VENTILATION SHAFT DATA                                      
C                                                                       
  170 IVS = IABS( ISEGL )                                               
      ITYP = TYPVS(IVS)                                                 
      GO TO (172,173,174,174,174,174,174,174,174,174), ITYP             
  172 WRITE(OUT,753)  LVS(IVS),ISCTX,NUMVS(IVS),NAMVS(IVS)    
      GO TO 175                                                         
  173 WRITE(OUT,754)  LVS(IVS),ISCTX,NUMVS(IVS),NAMVS(IVS)    
      GO TO 175                                                         
  174 WRITE(OUT,755)  LVS(IVS),ISCTX,NUMVS(IVS),NAMVS(IVS)    
  175 Q = QSAVES(ISCT) * 60.0                                           
      V = Q / AVS(IVS)                                                  
      IVSSL = TABL5(IVS)                                                
      IVSSH = TABL6(IVS)                                                
      GO TO ( 180, 185, 190 ),HUMOPT                                    
C---- HUMIDITY RATIO                                                    
  180 J = 1                                                             
      WRITE(OUT,714) ISCTX,NUMVS(IVS),J,TDBSS(IVSSL),HUMSS(IVSSL),Q,V   
      J3 = IVSSH - IVSSL + 1                                            
      J2 = IVSSL - 1                                                    
      IF( J3 - 2 ) 250,182,182                                          
  182 WRITE(OUT,715) (ISCTX,NUMVS(IVS),J,TDBSS(J2+J),HUMSS(J2+J),J=2,J3)
      GO TO 250                                                         
C---- WET-BULB TEMPERATURE                                              
  185 DO  186  IVSS = IVSSL, IVSSH                                      
  186 CALL WETBLB( TDBSS(IVSS), HUMSS(IVSS), TWBSS(IVSS) )              
      J = 1                                                             
      WRITE(OUT,724) ISCTX,NUMVS(IVS),J,TDBSS(IVSSL),TWBSS(IVSSL),Q,V   
      J3 = IVSSH - IVSSL + 1                                            
      J2 = IVSSL - 1                                                    
      IF( J3 - 2 ) 250,188,188                                          
  188 WRITE(OUT,725) (ISCTX,NUMVS(IVS),J,TDBSS(J2+J),TWBSS(J2+J),J=2,J3)
      GO TO 250                                                         
C---- RELATIVE HUMIDITY                                                 
  190 DO  191  IVSS = IVSSL, IVSSH                                      
  191 CALL RELHUM( TDBSS(IVSS), HUMSS(IVSS), TWBSS(IVSS) )              
      J = 1                                                             
      WRITE(OUT,734) ISCTX,NUMVS(IVS),J,TDBSS(IVSSL),TWBSS(IVSSL),Q,V   
      J3 = IVSSH - IVSSL + 1                                            
      J2 = IVSSL - 1                                                    
      IF( J3 - 2 ) 250,193,193                                          
  193 WRITE(OUT,735) (ISCTX,NUMVS(IVS),J,TDBSS(J2+J),TWBSS(J2+J),J=2,J3)
  250 CONTINUE                                                          
C                                                                       
C---- PRINT FIRE SIMULATION RESULTS FOR FIRE SEGMENTS                   
C                                                                       
 1000 IF( FIROP2 ) 1080,1080,1010                                       
 1010 WRITE( OUT,1020)                                                  
C --  LOOP OVER SECTIONS AND SEGMENTS, PRINTING ONLY FOR FIRE SEGMENTS  
      DO  1075  ISCTX = 1, LMSCTX                                       
      IF( TABL14(ISCTX) .LE. 0 ) GO TO 1075                             
      ISCT = TABL14( ISCTX )                                            
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL .LE. 0 )  GO TO 1075                                    
      DO  1070  ILS = ISEGL, ISEGH                                      
      IF( TYPLS(ILS) .GE. 0 ) GO TO 1070                                
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      DO  1040  ISS = ISSL, ISSH                                        
      J = ISS - ISSL + 1                                                
      QCONV = (QWALSS(ISS)*SURLSS(ILS) - QRADSS(ISS))*3600.0            
      QRAD = QRADSS(ISS) * 3600.0                                       
 1040 WRITE( OUT,1050)  ISCTX,NUMLS(ILS), J,TSFSS(ISS), QCONV, QRAD     
C                                                                       
 1070 CONTINUE                                                          
 1075 CONTINUE                                                          
C                                                                       
C     CHECK IF SUPPLEMENTARY OUTPUT WAS REQUESTED                       
C                                                                       
C                                                                       
 1080 GO TO ( 799,799,799,799,800,800,799,799 ),SUPOPT                  
C                                                                       
  799 RETURN                                                            
C                                                                       
C                                                                       
C     DUMP THERMO ARRAYS, IF SUPPLEMENTARY OUTPUT OPTION IS 4 OR 5      
C                                                                       
  800 WRITE(OUT,  9) CHAR(12)
      WRITE(OUT,810)                                                   
      DO  870  ISCTX = 1, LMSCTX                                        
      IF( TABL14(ISCTX) ) 870,870,820                                   
  820 WRITE(OUT,740)                                                    
      ISCT = TABL14(ISCTX)                                              
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL ) 850,850,830                                           
  830 DO  845  ILS = ISEGL, ISEGH                                       
      ILSSL = TABL3(ILS)                                                
      ILSSH = TABL4(ILS)                                                
  835 DO  845  ISS = ILSSL, ILSSH                                       
      J = ISS - ILSSL + 1                                               
      WRITE(OUT,840) ISCTX,NUMLS(ILS),J,VOLSS(ISS),FBSS(ISS),           
     1   FFSS(ISS),HTRNSS(ISS),SHLTSS(ISS),LHLTSS(ISS),RELS(ILS),       
     2   TTMPSS(ISS),HTMPSS(ISS)                                        
C                                                                       
  845 CONTINUE                                                          
      GO TO 870                                                         
  850 IVS = - ISEGL                                                     
      IVSSL = TABL5(IVS)                                                
      IVSSH = TABL6(IVS)                                                
      DO  860  ISS = IVSSL,IVSSH                                        
      J = ISS - IVSSL + 1                                               
      WRITE(OUT,840) ISCTX,NUMVS(IVS),J,VOLSS(ISS),FBSS(ISS),           
     1   FFSS(ISS),HTRNSS(ISS),SHLTSS(ISS),LHLTSS(ISS),REVS(IVS),       
     2   TTMPSS(ISS),HTMPSS(ISS)                                        
  860 CONTINUE                                                          
  870 CONTINUE                                                          
C                                                                       
C --  PRINT FIRE SIMULATION INFORMATION                                 
      IF ( FIROP2 ) 920,920,873                                         
  873 WRITE (OUT,871)                                                   
      DO  880  NODE = 1, NNODE                                          
      ITYPT = TYPTND(NODE)                                              
      NODTH = TABL22(NODE) + 1                                          
      WRITE(OUT,872) TABL15(NODE),TDBTN(NODTH),HUMTN(NODTH)             
      GO TO (880,875,875,880,880),ITYPT                                 
  875 WRITE(OUT,876) (TDBTN(NODTH+J),HUMTN(NODTH+J),J=1,2)              
  880 CONTINUE                                                          
      WRITE (OUT,884)                                                   
      DO 900 ISCTX=1,LMSCTX                                             
      IF( TABL14(ISCTX) .LE. 0 ) GO TO 900                              
      ISCT = TABL14(ISCTX)                                              
C                                                                       
C-----WRITE OUT BUOYANCY IN SECTIONS                                    
      WRITE (OUT,885)  ISCTX, BUOYS(ISCT)                               
C                                                                       
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL .LE. 0 ) GO TO 900                                      
      DO  889  ILS = ISEGL, ISEGH                                       
      ILSSL = TABL3(ILS)                                                
      ILSSH = TABL4(ILS)                                                
      DO  889  ISS = ILSSL, ILSSH                                       
      J = ISS - ILSSL + 1                                               
      WRITE(OUT,886) ISCTX, NUMLS(ILS), J, TSSTAB(ISS), RELSS(ISS),     
     1 TSFSS(ISS), QWALSS(ISS), QRADSS(ISS)                             
889   CONTINUE                                                          
900   CONTINUE                                                          
C                                                                       
C-----WRITE OUT UNSTEADY STATE HEAT SOURCE AND THROTTLING EFFECT        
      IF( NUMUL .LE. 0 ) GO TO 910                                      
901   WRITE (OUT,887)                                                   
      DO 905 IUL = 1, NUMUL                                             
905   WRITE(OUT,888)  IUL, THROTL(IUL)                                  
910   CONTINUE                                                          
C                                                                       
920   RETURN                                                            
C                                                                       
C********************************************************************   
C                                                                       
C                                                                       
C     ABBREVIATED PRINT ROUTINE     ************************************
C                                                                       
  400 IF( JRATTH ) 402,402,401                                          
  401 WRITE(OUT,770)                                                    
      GO TO 403                                                         
  402 WRITE(OUT,771)                                                    
C                                                                       
C---- LOOP OVER SECTION EXTERNAL I.D. NUMBERS                           
  403 DO  490  ISCTX = 1, LMSCTX                                        
C---- CHECK IF SECTION I.D. NUMBER HAS BEEN USED                        
      IF( TABL14(ISCTX) ) 490,490,405                                   
  405 ISCT = TABL14(ISCTX)                                              
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      IF( ISEGL ) 430,490,410                                           
C                                                                       
C---- PRINT LINE SECTION DATA - LOOP OVER LINE SEGMENTS                 
C                                                                       
  410 Q = QSAVES(ISCT) * 60.0                                           
      DO  425  ILS = ISEGL, ISEGH                                       
      V = Q / ALS(ILS)                                                  
      IF( JRATTH ) 412,412,414                                          
  412 WRITE(OUT,775)  ISCTX,NUMLS(ILS),Q,V                              
      GO TO 425                                                         
  414 ILSSL = TABL3(ILS)                                                
      ILSSH = TABL4(ILS)                                                
      J = 1                                                             
  415 I2 = ILSSH                                                        
      IF( I2 - ILSSL - 7 ) 420,420,417                                  
  417 I2 = ILSSL + 7                                                    
  420 GO TO ( 421, 422 ), J                                             
  421 WRITE(OUT,775) ISCTX,NUMLS(ILS),Q,V,(TDBSS(ILSS),ILSS=ILSSL,I2)   
      GO TO 423                                                         
  422 WRITE(OUT,777) (TDBSS(ILSS),ILSS=ILSSL,I2)                        
  423 WRITE(OUT,778) (HUMSS(ILSS),ILSS=ILSSL,I2)                        
      ILSSL = I2 + 1                                                    
      J = 2                                                             
      IF( ILSSH - ILSSL ) 425,415,415                                   
  425 CONTINUE                                                          
      GO TO 490                                                         
C                                                                       
C---- PRINT VENTILATION SHAFT DATA                                      
C                                                                       
  430 IVS = IABS(ISEGL)                                                 
      Q = QSAVES(ISCT) * 60.0                                           
      V = Q / AVS(IVS)                                                  
      IF( JRATTH ) 432,432,434                                          
  432 WRITE(OUT,775)  ISCTX,NUMVS(IVS),Q,V                              
      GO TO 490                                                         
  434 IVSSL = TABL5(IVS)                                                
      IVSSH = TABL6(IVS)                                                
      J = 1                                                             
  435 I2 = IVSSH                                                        
      IF( I2 - IVSSL - 7 ) 440,440,437                                  
  437 I2 = IVSSL + 7                                                    
  440 GO TO ( 441, 442 ),J                                              
  441 WRITE(OUT,775) ISCTX,NUMVS(IVS),Q,V,(TDBSS(IVSS),IVSS=IVSSL,I2)   
      GO TO 443                                                         
  442 WRITE(OUT,777) (TDBSS(IVSS),IVSS=IVSSL,I2)                        
  443 WRITE(OUT,778) (HUMSS(IVSS),IVSS=IVSSL,I2)                        
      IVSSL = I2 + 1                                                    
      J = 2                                                             
      IF( IVSSH - IVSSL ) 490,435,435                                   
  490 CONTINUE                                                          
C                                                                       
      GO TO 1000                                                        
C                                                                       
      END                                                               
