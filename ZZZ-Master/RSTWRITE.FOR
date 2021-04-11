      SUBROUTINE RSTWRITE                                           
C                                                                       
C *** THIS SUBROUTINE WRITES USER-SPECIFIED INFORMATION AT THE END OF 
C *** THE OUTPUT FILE.  THE SAME INFORMATION IS ALSO WRITTEN TO AN
C *** INITIALIZATION FILE THAT CAN BE USED TO INITIALIZE THE SYSTEM
C *** IN A SUBSEQUENT SIMULATION.  THE LEVEL OF INFORMATION THAT IS
C *** WRITTEN DEPENDS UPON THE VALUE OF THE FILE INITIALIZATION
C *** OPTION (RSTROPT).  THESE LEVELS ARE AS FOLLOWS:                    
C                                                                       
C     RSTROPT = 1  AERO                                                  
C                                                                       
C             = 2  TRAINS                                                
C                                                                       
C             = 3  THERMO                                                
C                                                                       
C             = 4  AERO AND TRAINS                                       
C                                                                       
C             = 5  AERO, TRAINS, AND THERMO                              
C                                                                       
C             = 6  UNDEFINED                                             
C                                                                       
C             = 7  UNDEFINED                                             
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C     
      CALL OPENWRST                                                                  
      GO TO (10,20,30,40,50,60,70), RSTWOPT                              
   10 WRITE(OUT,410) RSTWOPT                                             
      GO TO 80                                                          
   20 WRITE(OUT,420) RSTWOPT                                             
      GO TO 80                                                          
   30 WRITE(OUT,430) RSTWOPT                                             
      GO TO 80                                                          
   40 WRITE(OUT,440) RSTWOPT                                             
      GO TO 80                                                          
   50 WRITE(OUT,450) RSTWOPT                                             
      GO TO 80                                                          
   60 WRITE(OUT,460) RSTWOPT                                             
      GO TO 80                                                          
   70 WRITE(OUT,470) RSTWOPT                                             
   80 WRITE(RST,520) DATETIME, HEADER
      WRITE(RST,480) RSTWOPT                                           
      GO TO (100,200,300,100,100,100,100), RSTWOPT                       
C                                                                       
C---- AERODYNAMIC INITIALIZATION DATA                                   
C                                                                       
  100 WRITE(OUT,530)                                                    
      WRITE(OUT,540) NSEC                                               
      WRITE(RST,550) NSEC                                             
      WRITE(OUT,560)                                                    
      DO 120 ISCT = 1, NSEC                                             
      ISCTE = TABL11(ISCT)                                              
      DUMY1 = QSAVES(ISCT)*60.0                                         
      WRITE(OUT,570) ISCTE,ISCT,DUMY1                                   
  120 WRITE(RST,580) ISCTE,DUMY1                                      
      IF(RSTWOPT-4) 360,200,200                                          
C                                                                       
C---- TRAIN INITIALIZATION DATA                                         
C                                                                       
  200 WRITE(OUT,600)                                                    
      WRITE(OUT,610) NTRAN                                              
      WRITE(RST,550) NTRAN                                            
      IF(NTRAN) 230,230,203                                             
  203 WRITE(OUT,620)                                                    
      DO 220 ITRAN = 1, NTRAN                                           
      NUMV = IINDEX(ITRAN)                                               
      DUMY1 = UV(NUMV)*FPSMPH                                           
      IF(MODEV(NUMV)) 205,205,210
  205 IF (TSTRTV(NUMV) .GE. IZAP)THEN
        DUMY2=-1.0
	ELSE
        DUMY2 = FLOAT(TSTRTV(NUMV)-NTIME)/100.
	END IF
      DUMY3 = 0.                                                        
      DUMY4 = RPS(NUMV)*60.0                                            
      GO TO 215                                                         
  210 DUMY3 = DUDTV(NUMV)*FPSMPH                                        
      DUMY2 = 0.                                                        
      DUMY4 = RPS(NUMV)*60.0                                            
  215 WRITE(OUT,630) XV(NUMV),DUMY1,IROTEV(NUMV),ITYPV(NUMV),TGACCV(NUMV
     1),TGDECV(NUMV),MODEV(NUMV),DUMY2,DUMY3,NTSV2(NUMV),DUMY4          
  220 WRITE(RST,635) XV(NUMV),DUMY1,IROTEV(NUMV),ITYPV(NUMV),TGACCV(NU
     1MV),TGDECV(NUMV),MODEV(NUMV),DUMY2,DUMY3,NTSV2(NUMV),DUMY4        
	IF (DUMY2 .LT. 0)  WRITE (OUT,636)
  230 IF(RSTWOPT-5) 360,300,300                                          
C                                                                       
C---- THERMODYNAMIC INITIALIZATION DATA                                 
C                                                                       
  300 WRITE(OUT,640)                                                    
      WRITE(OUT,650) TNLSS                                              
      WRITE(RST,550) TNLSS                                            
      IF(TNLSS) 330,330,310                                             
  310 WRITE(OUT,660)                                                    
      DO 320 ILSS = 1, TNLSS                                            
      CALL INTEXT(ILSS,ISCTX,ILSX,ISSX)                                 
      DUMY1 = SACLSS(ILSS)*3600.                                        
      DUMY2 = LACLSS(ILSS)*3600.                                        
      WRITE(OUT,670) ISCTX,ILSX,ISSX,ILSS,TDBSS(ILSS),HUMSS(ILSS),TSFSS(
     1ILSS),DUMY1,DUMY2                                                 
  320 WRITE(RST,680) ISCTX,ILSX,ISSX,TDBSS(ILSS),HUMSS(ILSS),TSFSS(ILS
     1S),DUMY1,DUMY2                                                    
  330 ITNVSS = TNSS - TNLSS                                             
      WRITE(OUT,690) ITNVSS                                             
      WRITE(RST,550) ITNVSS                                           
      IF(TNSS-TNLSS) 360,360,340                                        
  340 IVSS1 = TNLSS + 1                                                 
      DO 350 IVSS = IVSS1, TNSS                                         
      CALL INTEXT(IVSS,ISCTX,IVSX,ISSX)                                 
      WRITE(OUT,710) ISCTX,IVSX,ISSX,IVSS,TDBSS(IVSS),HUMSS(IVSS),TSFSS(
     1IVSS)                                                             
  350 WRITE(RST,720) ISCTX,IVSX,ISSX,TDBSS(IVSS),HUMSS(IVSS),TSFSS(IVS
     1S)                                                                
  360 CONTINUE                                                          
      RETURN                                                            
C                                                                       
  400 FORMAT(A1)
  410 FORMAT('RESTART FILE OPTION =',I2,T28,'INITIALIZE AERO')        
  420 FORMAT('RESTART FILE OPTION =',I2,T28,'INITIALIZE TRAINS')      
  430 FORMAT('RESTART FILE OPTION =',I2,T28,'INITIALIZE THERMO')      
  440 FORMAT('RESTART FILE OPTION =',I2,T28,'INITIALIZE AERO AND TRAIN')
  450 FORMAT('RESTART FILE OPTION =',I2,T28,'INITIALIZE AERO, TRAINS, AN
     1D THERMO')                                                      
  460 FORMAT('RESTART FILE OPTION =',I2,T28,'UNDEFINED')              
  470 FORMAT('RESTART FILE OPTION =',I2,T28,'UNDEFINED')              
  480 FORMAT(I5)                                                        
  520 FORMAT(A20,2X,A110)                                            
  530 FORMAT(/,'AERODYNAMIC INITIALIZATION DATA',/,
     1         '----------- -------------- ----')                                                
  540 FORMAT(/,'TOTAL NUMBER OF SECTIONS = ',I3)                      
  550 FORMAT(I5)                                                        
  560 FORMAT(/,'EXTERNAL INTERNAL',5X,'AIR FLOW RATE',/,2X,'SECT',5X, 
     1'SECT',8X,'(FT**3/MIN)',/)                                        
  570 FORMAT(2X,I3,6X,I3,4X,F15.1)                                      
  580 FORMAT(I5,F15.1)                                                  
  600 FORMAT(/,'TRAIN INITIALIZATION DATA',/,
     1         '----- -------------- ----')                                                            
  610 FORMAT(/,'TOTAL NUMBER OF TRAINS OPERATING =',I3)               
  620 FORMAT(/1X,'DISTANCE  SPEED  ROUTE  TYPE  ACC GRID TEMP  DEC GRID 
     1TEMP  MODE  REMAINING DWELL  ACCELERATION RATE',6X,'FLYWHEEL',/,  
     2'  (FEET)   (MPH)',T33,'(DEGREE F)',5X,'(DEGREE F)',T73,'(SEC)'   
     3,T89,'(MPH/SEC)',T110,'(RPM)',/)                                  
  630 FORMAT(F9.2,F7.2,I5,T27,I2,T34,F6.1,T49,F6.1,T63,I2,T70,F8.2,T90,
     1F6.3,I10,F10.1)                                                    
  635 FORMAT(2F10.2,2I5,2F10.2,I5,F10.2,F10.3,I5,/,F10.2)               
  636 FORMAT (/,'A NEGATIVE VALUE FOR THE REMAINING DWELL TIME INDICATES
     1 THAT THE TRAIN IS TO REMAIN IN THE INITIALIZED LOCATION',/,'FOR T   
     2HE DURATION OF THE SIMULATION.')
  640 FORMAT(/,'THERMODYNAMIC INITIALIZATION DATA',/,
     1         '------------- -------------- ----')                                            
  650 FORMAT(/,'TOTAL NUMBER OF LINE SUBSEGMENTS =',T35,I4)           
  660 FORMAT(/,T3,'EXTERNAL   INTERNAL  DRY BULB TEMP  HUMIDITY  WALL SU
     1RF TEMP   A/C SENSIBLE    A/C LATENT',/                         
     2,'SECT SEG SUB',3X,'SUB',6X,'(DEGREE F)',4X,'(LB/LB)',5X,'(DEGREE 
     3F)',8X,'BTU/HR',9X,'BTU/HR',/)                                   
  670 FORMAT(I3,'-',I3,'-',I3,4X,I3,8X,F6.1,6X,F7.4,7X,F6.1,8X,F9.0,6X,F
     19.0)                                                              
  680 FORMAT(3I5,F8.2,F9.5,F8.2,2F10.0)                                 
  690 FORMAT(/,'TOTAL NUMBER OF VENT SUBSEGMENTS =',I4,/)             
  710 FORMAT(I3,'-',I3,'-',I3,4X,I3,8X,F6.1,6X,F7.4,7X,F6.1)            
  720 FORMAT(3I5,F8.2,F9.5,F8.2)                                        
      END                                                               
