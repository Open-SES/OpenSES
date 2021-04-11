      SUBROUTINE  SIMLAT( ISIMOP )                                      
C                                                                       
C     THIS SUBROUTINE CONTROLS THE SES SIMULATION.                      
C     IT IS CALLED AFTER ALL THE INPUT HAS BEEN READ AND VERIFIED.      
C                                                                       
C      ISIMOP = 1 - BEGIN NEW SIMULATION                                
C      ISIMOP = 2 - RE-START SIMULATION IN PROGRESS                     
C                                                                       
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
      INCLUDE  'HEATUC'                                                 
C                                                                       
C                                                                       
   60 FORMAT (A1)
   61 FORMAT(A1,16(/), 44X,'END OF SIMULATION' )                       
   65 FORMAT( /// T29,'THIS SIMULATION HAS BEEN TERMINATED BECAUSE',/   
     1 T29,'ALL OF THE USER-SPECIFIED PRINT GROUPS HAVE BEEN COMPLETED')
   70 FORMAT ('INTEGRATION STEP SIZE'/                           
     15X,'AERODYNAMIC    RATIO =',I2,5X,                                
     2 'INTEGRATION INCREMENT ( TWO CYCLES ) =',F5.2,' SEC'/            
     35X,'THERMODYNAMIC  RATIO =',I2,5X,                                
     4 'INTEGRATION INCREMENT ( TWO CYCLES ) =',F5.2,' SEC' )           
  998 FORMAT( A1,/,'IMPOSSIBLE SITUATION - SUBROUTINE SIMLAT')          
C                                                                       
C                                                                       
C---- BRANCH ON OPTION                                                  
      GO TO ( 100, 554 ), ISIMOP                                        
C                                                                       
C---- INITIALIZE 'NTIME' WHICH IS THE 'SYSTEM CLOCK'                    
  100 NTIME = 0                                                         
C---- INITIALIZE PRINT CONTROL VARIABLES                                
      IPRGRP = 1                                                        
      NMPRGP = 0                                                        
      PRTIME = GINTPR( IPRGRP )                                         
C---- BEGIN NEW PAGE AND WRITE AERO AND THERMO INTEGRATION
C---- INCREMENTS (SET IN INPUT)       
      WRITE (OUT,60) CHAR(12)
      NEWPAGE=1
      WRITE (OUT,70) GRATAE(IPRGRP),DELAE,GRATTH(IPRGRP),DELTH          
C---- INITIALIZE SUMMARY TOTALS                                         
      IF(  NOSUMY ) 115,115,110                                         
  110 CALL SUMARY( 1 )                                                  
  115 CONTINUE                                                          
C                                                                       
C     FIRST STEP OF AERODYNAMIC INTEGRATION                             
C                                                                       
      IF( JRATAE * NOSYS ) 140,140,130                                  
  130 KASTEP = 1                                                        
      KAERO=0                                                           
      CALL  QDERIV                                                      
      CALL  QUARRK                                                      
      CALL  QSQLP                                                       
      AETIME = JRATAE                                                   
      GO TO 150                                                         
  140 AETIME = 2000000000                                               
      JRATAE = 0                                                        
C                                                                       
C     FIRST STEP OF THERMODYNAMIC INTEGRATION                           
C                                                                       
  150 IF (THOPT*JRATTH*NOSYS) 170,170,160                               
  160 KTSTEP = 1                                                        
      CALL  PINPNT                                                      
      FIROP2 = FIROPT * THOPT * JRATTH                                  
      KTWALL = 0                                                        
      IF( FIROP2 * NTWALL .LE. 0 ) GO TO 165                            
      CALL HEATUP( DEL2TH,1 )                                           
165   CALL THNODE                                                       
      CALL  THDERV                                                      
      CALL  RKTHRM                                                      
      THTIME = JRATTH                                                   
      GO TO 200                                                         
  170 THTIME = 2000000000                                               
      JRATTH = 0                                                        
      FIROP2 = 0                                                        
C                                                                       
C***********************************************************************
C                                                                       
C**** MAIN LOOP - CYCLE THROUGH INCREMENTING 'SYSTEM CLOCK' BY 'NDELT'  
C                 EACH CYCLE UNTIL 'NTIME' IS EQUAL TO 'MAXTIM'.        
C                                                                       
  200 NTIME = NTIME + NDELT                                             
C                                                                       
C**** RESISTOR GRID CALCULATIONS ***************************************
C                                                                       
      IF( THTIME - NTIME ) 201,201,209                                  
  201 IF( NTRAN ) 209,209,202                                           
  202 CALL  GRID                                                        
C                                                                       
C****     TRAIN PERFORMANCE SUBPROGRAM       ***************************
C                                                                       
  209 IF( NOTRAN ) 999,242,220                                          
  220 CALL TRAIN                                                        
      IF( NTIME - TRTIME ) 242,241,241                                  
  241 TRTIME = TRTIME + JRATTR                                          
  242 CONTINUE                                                          
C                                                                       
C---- AERODYNAMIC INTEGRATION       ************************************
C                                                                       
      IF( AETIME - NTIME ) 300,300,400                                  
  300 KASTEP = KASTEP + 1                                               
C---- STEP 2 OR 4 OF RUNGE-KUTTA                                        
      KAERO=0                                                           
      CALL  QDERIV                                                      
      CALL  QUARRK                                                      
      CALL  QSQLP                                                       
      IF( 4 - KASTEP ) 310,310,320                                      
  310 KASTEP = 0                                                        
  320 KASTEP = KASTEP + 1                                               
C---- STEP 1 OR 3 OF RUNGE-KUTTA                                        
      KAERO=1                                                           
      CALL  QDERIV                                                      
      CALL  QUARRK                                                      
      CALL  QSQLP                                                       
      AETIME = AETIME + JRATAE                                          
C                                                                       
C---- IF THERMO IS NOT RUNNING PERFORM SUMMARY HERE                     
  330 IF( JRATTH ) 400,340,400                                          
  340 IF( NOSUMY ) 500,500,350                                          
  350 CALL  MAXMIN                                                      
      GO TO 500                                                         
C                                                                       
C---- THERMODYNAMIC INTEGRATION     ************************************
C                                                                       
  400 IF( THTIME - NTIME ) 410,410,500                                  
  410 IF( NOSYS ) 455,455,430                                           
C                                                                       
  430 CALL PINPNT                                                       
      IF( FIROP2 .LE. 0 ) GO TO 435                                     
      KTWALL = KTWALL + 1                                               
      IF( KTWALL .NE. NTWALL ) GO TO 435                                
      CALL HEATUP( TIEMPO,2 )                                           
      KTWALL = 0                                                        
435   KTSTEP = KTSTEP + 1                                               
C---- STEP 2 OR 4 OF RUNGE-KUTTA                                        
      CALL  THNODE                                                      
      CALL  THDERV                                                      
      CALL  RKTHRM                                                      
      IF( 4 - KTSTEP ) 440,440,450                                      
  440 KTSTEP = 0                                                        
  450 KTSTEP = KTSTEP + 1                                               
C---- STEP 1 OR 3 OF RUNGE-KUTTA                                        
      CALL  THNODE                                                      
      CALL  THDERV                                                      
      CALL  RKTHRM                                                      
C                                                                       
  455 THTIME = THTIME + JRATTH                                          
C---- PERFORM SUMMARY TABULATION IF REQUESTED                           
  460 IF( NOSUMY ) 500,500,465                                          
  465 CALL  MAXMIN                                                      
C                                                                       
C****     PRINT AND SUMMARY ROUTINE      *****************************  
C                                                                       
  500 IF( PRTIME - NTIME ) 510,510,600                                  
C---- TIME FOR ANOTHER PRINT                                            
  510 IF( NPRGR(IPRGRP) ) 520,520,515                                   
  515 CALL PRINT                                                        
C---- INCREMENT NUMBER OF PRINTS FOR THIS GROUP - IS GROUP EXHAUSTED    
  520 NMPRGP = NMPRGP + 1                                               
      NUMY1=NPRGR(IPRGRP)                                               
      IF(NMPRGP-IABS(NUMY1)) 530,540,540                                
  530 PRTIME = PRTIME + GINTPR(IPRGRP)                                  
      GO TO 600                                                         
C---- PRINT GROUP IS EXHAUSTED - PERFORM SUMMARY AND A/C EST. IF REQ'D  
  540 IF( NOSUMY ) 549,549,543                                          
  543 IF( GRSUM(IPRGRP) - 2 ) 547,545,545                               
C---- PRINT SUMMARY OF OPTION IS 2, 3 OR 4                              
  545 CALL SUMARY( 2 )                                                  
C                                                                       
C---- IF SUMMARY OPTION 4 - RETURN TO 'DSES' TO PERFORM ENVIRON. EST.   
C                                                                       
      IF( GRSUM( IPRGRP ) - 4 ) 547,546,546                             
C                                                                       
  546 RETURN                                                            
C                                                                       
C---- RE-START SIMULATION ALREADY IN PROGRESS --------------------------
C                                                                       
554   KTWALL = 0                                                        
C---- INITIALIZE SUMMARY IF REQUIRED                                    
  547 ISUMOP = GRSUM( IPRGRP ) + 1                                      
      GO TO ( 549, 548, 549, 548, 551 ), ISUMOP                         
C---- SHOW DETAIL PRINT OF RE-START CONDITIONS                          
  551 KPRNTH = 1000000                                                  
      CALL PRINT                                                        
C---- INITIALIZE SUMMARY TOTALS ( OPTION 1, 3, 4 )                      
  548 CALL SUMARY( 1 )                                                  
C---- INCREMENT GROUP COUNTER - HAVE ALL GROUPS BEEN EXHAUSTED          
  549 IPRGRP = IPRGRP + 1                                               
      NMPRGP = 0                                                        
      KPRNTH = 0                                                        
      IF( IPRGRP - NPGRP ) 550,550,560                                  
  550 PRTIME = PRTIME + GINTPR(IPRGRP)                                  
C---- ADJUST AERO AND THERMO INTEGRATION INCREMENTS                     
      JRATAE = GRATAE(IPRGRP) * NDELT                                   
      IF (JRATAE * NOSYS) 553,553,552                                   
  553 AETIME=2000000000                                                 
      JRATAE=0                                                          
  552 DELAE = FLOAT(JRATAE)/50.0                                        
      JRATTH = GRATTH(IPRGRP) * NDELT                                   
      DELTH = FLOAT(JRATTH)/50.0                                        
      DEL2TH = DELTH/2.0                                                
      TIEMPO = DEL2TH * FLOAT(NTWALL)                                   
      FIROP2 = FIROPT * THOPT * JRATTH                                  
      IF (THOPT*JRATTH*NOSYS) 555,555,556                               
  555 THTIME=2000000000                                                 
      JRATTH=0                                                          
      FIROP2 = 0                                                        
C---- BEGIN NEW PAGE 
  556 WRITE (OUT,60) CHAR(12)
      NEWPAGE=1
      WRITE (OUT,70) GRATAE(IPRGRP),DELAE,GRATTH(IPRGRP),DELTH          
      GO TO 600                                                         
C---- NO MORE PRINTS - TERMINATE SIMULATION                             
  560 WRITE(OUT,61)CHAR(12)                                                     
C --  DON'T WRITE MESSAGE IF MAXTIM HAS BEEN REACHED                    
      IF( NTIME - MAXTIM ) 565,710,710                                  
  565 WRITE(OUT,65)                                                     
  570 GO TO 710                                                         
  600 CONTINUE                                                          
C                                                                       
C-------- CALLS TO SYSTEMS SERVICE SUBROUTINES DVCHK AND OVERFL --------
C --      THESE ROUTINES CHECK FOR DIVISION BY ZERO AND                 
C --      FLOATING POINT OVERFLOW.                                      
C --      FOR COMPUTERS WITHOUT THESE SYSTEMS SERVICE ROUTINES PLACE    
C --      A 'C' IN COLUMN 1 FOR THE NEXT 6 LINES.                  
C     CALL DVCHK( I )                                                   
C     GO TO ( 610, 620 ), I                                             
C 610 CALL SIMERR( 2 )                                                  
C 620 CALL OVERFL( I )                                                  
C     GO TO ( 625,650, 650 ),I                                          
C 625 CALL SIMERR( 3 )                                                  
C-------- END OF AREA TO PUT 'C' IN COLUMN 1                ------------
C                                                                       
C**** CHECK IF MAXIMUM SIMULATION TIME HAS BEEN REACHED  ***************
C                                                                       
  650 IF( NTIME - MAXTIM ) 670,700,700                                  
C---- IF FATAL ERROR HAS NOT OCCURED LOOP BACK FOR ANOTHER CYCLE        
  670 IF (NEW .EQ. 0)GOTO 200
      GOTO 990
C                                                                       
C****    -END OF SIMULATION   ***************************************** 
C                                                                       
  700 WRITE(OUT,61)CHAR(12)                                                     
  710 CONTINUE                                                          
C---- SET NEW=2 TO INDICATE SUCCESSFUL COMPLETION OF SIMULATION         
  740 NEW=2                                                             
      RETURN                                                            
C                                                                       
C***********************************************************************
C                                                                       
C     FATAL ERROR HAS OCCURRED                                      
C                                                                       
  990 WRITE(OUT,61)CHAR(12)                                                   
      RETURN                                                            
C                                                                       
C---- IMPOSSIBLE SITUATION                                              
C                                                                       
  999 WRITE(OUT,998)CHAR(12)                                                    
      CALL EXIT (EXITCODE)                                                              
      END                                                               
