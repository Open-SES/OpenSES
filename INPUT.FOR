      SUBROUTINE INPUT                                                  
C                                                                       
C     THIS SUBROUTINE CONTROLS THE READING,CHECKING, AND INPUT          
C     VERIFICATION OF ALL THE INPUT DATA. SOME DATA IS READ BY THIS     
C     SUBROUTINE, AND SOME IS READ BY OTHER SUBROUTINES WHICH READ      
C     CERTAIN ITEMS OF DATA. THESE ARE -                                
C              LSINS  - LINE SEGMENT INPUT SUBROUTINE                   
C              VSINS  - VENTILATION SHAFT INPUT SUBROUTINE              
C              TRINS  - TRAIN ROUTE INPUT SUBROUTINE                    
C              GARAGE - TRAIN 'TYPE' INPUT SUBROUTINE                   
C                                                                       
C     OTHER SUBROUTINES WHICH ARE USED ARE -                            
C              CHECKI - CHECKS INTEGER VARIABLE BETWEEN TWO LIMITS      
C              CHECKR - CHECKS REAL VARIABLE BETWEEN TWO LIMITS         
C              ERROR  - WRITES ERROR MESSAGES                           
C                                                                       
C                                                                       
C
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C
C     MAXTIT: MAXIMUM NUMBER OF TITLE LINES
      PARAMETER (MAXTIT=20)
      DIMENSION NUMMY1(18)                             
      DIMENSION DUMMY1(8), NUMMY2(8),NUMMY3(8),NUMMY4(8),NUMMY5(8)   
      CHARACTER*80 TITLINES(MAXTIT), DISPSTR
5     FORMAT (80(' '))                                
10    FORMAT (A1)   
20    FORMAT (A80)                                                  
30    FORMAT (8F10.0)                                                   
40    FORMAT (A36,4X,4F10.0)
45    FORMAT (116(' '))                                            
46    FORMAT (122(' '))
50    FORMAT (T34,
     1    '     UNITED STATES DEPARTMENT OF TRANSPORTATION     ',/,T34,
     1    '           FEDERAL TRANSIT ADMINISTRATION           ',/,T34,
     2    'JOHN A. VOLPE NATIONAL TRANSPORTATION SYSTEMS CENTER',
     32(/),T48,5('X'),3X,7('X'),3X,5('X'),/,T47,'X',T56
     4,'X',T65,'X',/,T48,5('X'),T56,5('X'),T66,5('X'),/,T53,'X',T56,'X'
     5,T71,'X',/,T53,'X',T56,'X',T71,'X',/,T47,6('X'),T56,7('X'),T65,6('
     6X'),//,T45,'SUBWAY ENVIRONMENT SIMULATION',2(/),T51,
     7'SIMULATION OF',/)                 
60    FORMAT (25X,A80,/)
70    FORMAT (25X,A80)
80    FORMAT (36X,'DESIGN TIME',I5,' HRS',3X,A9,I6)            
90    FORMAT (36X,'DESIGN TIME 0',I3,' HRS',3X,A9,I6)            
100   FORMAT (T94,'DEVELOPED BY -',/,T96,'PARSONS, BRINCKERHOFF,',/,T96, 
     1'QUADE AND DOUGLAS, INC.',/,T96,'NEW YORK CITY, NEW YORK',/,T96, 
     2'VERSION ',A8)                                     
110   FORMAT (/ T44,'INPUT VERIFICATION OF GENERAL DATA')             
120   FORMAT (/ T112,'FORM 1C'/T29,'TRAIN PERFORMANCE OPTION',T79,I5,T89
     1,'BYPASS')                                                      
130   FORMAT (/ T112,'FORM 1C'/T29,'TRAIN PERFORMANCE OPTION',T79,I5,T89
     1,'IMPLICIT')                                                    
140   FORMAT (/ T112,'FORM 1C'/T29,'TRAIN PERFORMANCE OPTION',T79,I5,T89
     1,'EXPLICIT (HEAT REJ. COMPUTED)')                               
150   FORMAT (/ T112,'FORM 1C'/T29,'TRAIN PERFORMANCE OPTION',T79,I5,T89
     1,'EXPLICIT (HEAT REJ. INPUT)')                                  
160   FORMAT (/ T29,'TEMPERATURE / HUMIDITY SIMULATION OPTION',T79,I5,
     1T89,'BYPASS')                                                     
170   FORMAT (/ T29,'TEMPERATURE / HUMIDITY SIMULATION OPTION',T79,I5,
     1T89,'YES')                                                        
180   FORMAT (/ T29,'TEMPERATURE / HUMIDITY SIMULATION OPTION',T79,I5,
     1T89,'YES - EVAPORATION')                                          
190   FORMAT (/ T29,'HUMIDITY DISPLAY OPTION',T79,I5,T89,'HUMIDITY RATIO
     1')                                                              
200   FORMAT (/ T29,'HUMIDITY DISPLAY OPTION',T79,I5,T89,'WET-BULB TEMPE
     1RATURE')                                                        
210   FORMAT (/ T29,'HUMIDITY DISPLAY OPTION',T79,I5,T89,'RELATIVE HUMID
     1ITY')                                                           
220   FORMAT (/ T29,'ENVIRONMENTAL CONTROL LOAD EVALUATION OPTION',T79,
     1I5,T89,'BYPASS')                                                 
230   FORMAT (/ T29,'ENVIRONMENTAL CONTROL LOAD EVALUATION OPTION',T79,
     1I5,T89,'YES')                                                    
240   FORMAT (/ T29,'ENVIRONMENTAL CONTROL LOAD EVALUATION OPTION',T79,
     1I5,T89,'OFF-HOUR')                                               
250   FORMAT (/ T29,'HEAT SINK SUMMARY PRINT OPTION',T79,I5,T89,
     1'BYPASS')                                                               
260   FORMAT (/ T29,'HEAT SINK SUMMARY PRINT OPTION',T79,I5,T89,'YES')
270   FORMAT (/ T29,'SUPPLEMENTARY OUTPUT OPTION',T79,I5)             
280   FORMAT (/ T29,'ALLOWABLE SIMULATION ERRORS',T79,I5)             
290   FORMAT (/ T29,'ALLOWABLE INPUT ERRORS',T79,I5)                  
300   FORMAT (/ T29,'ALLOWABLE INPUT ERRORS',T79,I5,T89,'NO SIMULATION')
310   FORMAT (// T29,'NUMBER OF LINE SEGMENTS',T79,I5,T112,'FORM 1D') 
320   FORMAT (/ T29,'TOTAL NUMBER OF SECTIONS',T79,I5)                
330   FORMAT (/ T29,'NUMBER OF VENTILATION SHAFT SECTIONS',T79,I5)    
340   FORMAT (/ T29,'NUMBER OF NODES',T79,I5)                         
350   FORMAT (/ T29,'NUMBER OF BRANCHED JUNCTIONS',T79,I5)            
370   FORMAT (/ T29,'NUMBER OF UNSTEADY HEAT SOURCES',T79,I5)         
380   FORMAT (/ T29,'NUMBER OF FAN TYPES',T79,I5)                     
390   FORMAT (// T29,'NUMBER OF TRAIN ROUTES',T79,I5,T112,'FORM 1E')   
400   FORMAT (/ T29,'NUMBER OF TRAIN TYPES',T79,I5)                   
410   FORMAT (/ T29,'NUMBER OF ENVIRONMENTAL CONTROL ZONES',T79,I5)   
430   FORMAT (/ T29,'FAN STOPPING/WINDMILLING OPTION',T79,I5,5X,         
     1              'SIMULATION TERMINATION')                         
440   FORMAT (/ T29,'FAN STOPPING/WINDMILLING OPTION',T79,I5,5X,         
     1              'FAN SHUTDOWN ONLY')    
450   FORMAT (/ T29,'FAN STOPPING/WINDMILLING OPTION',T79,I5,5X,
     1              'MEANINGLESS') 
460   FORMAT (/ T29,'NUMBER OF TRAINS IN OPERATION AT INITIALIZATION',
     1T79,I5)                                                           
461   FORMAT (/ T29,'NUMBER OF IMPULSE FAN TYPES',T79,I5)             
462   FORMAT (/ T29,'INITIALIZATION FILE WRITING OPTION',T79,I5)     
463   FORMAT (/ T29,'INITIALIZATION FILE READING OPTION',T79,I5)      
470   FORMAT (/ T29,'AN ENTRY OF ZERO (0) HAS BEEN MADE FOR EACH OF THE
     1FOLLOWING:'// T32,'NUMBER OF LINE SEGMENTS',// T32,'NUMBER OF VENT
     2ILATION SHAFTS',// T32,'NUMBER OF SECTIONS',// T32,'NUMBER OF NODE
     3S',// T32,'NUMBER OF BRANCHED JUNCTIONS.',// T29,'THIS INDICATES A
     4N AT GRADE TRAIN PERFORMANCE SIMULATION ONLY',/)                                          
480   FORMAT (// T50,'DESIGN HOUR WEATHER DATA')                      
490   FORMAT (/ T50,'DAILY WEATHER DATA')                             
500   FORMAT (/ 'MORNING AMBIENT AIR DRY-BULB TEMPERATURE',T79,F14.1,
     1T96,'DEG F')                                                      
510   FORMAT (/ 'EVENING OR OFF HOUR AMBIENT AIR DRY-BULB TEMPERATURE',
     1T79,F14.1,T96,'DEG F')                                          
520   FORMAT (/ T50,'ANNUAL WEATHER DATA')                            
530   FORMAT (/ 'AMPLITUDE OF ANNUAL TEMPERATURE FLUCTUATION',T79,
     1F14.1,T96,'DEG F'/)                                                  
540   FORMAT (/ 'MORNING AMBIENT AIR WET-BULB TEMPERATURE',T79,F14.1,
     1T96,'DEG F',/ )                                                  
550   FORMAT (/ 'EVENING OR OFF HOUR AMBIENT AIR WET-BULB TEMPERATURE',
     1T79,F14.1,T96,'DEG F',/ )                                       
560   FORMAT (/ 'AMBIENT BAROMETRIC PRESSURE',T79,F14.2,T96,'IN. HG')
570   FORMAT (/ 'AMBIENT AIR DRY-BULB TEMPERATURE',T79,F14.1,T96, 
     1 'DEG F',T112,'FORM 1F')                                               
580   FORMAT (/ 'AMBIENT AIR WET-BULB TEMPERATURE',T79,F14.1,T96,
     1'DEG F')                                                              
590   FORMAT (/ T3,'AMBIENT AIR DENSITY',T79,F14.5,T96,'LB/CU FT')    
600   FORMAT (T3,'AMBIENT HUMIDITY RATIO',T79,F14.5,T96,
     1 'LB/LB OF DRY AIR')                                                           
610   FORMAT (T3,'AMBIENT RELATIVE HUMIDITY',T79,F14.0,T96,'PERCENT')
620   FORMAT (/ 'PERCENT OF HEAT FROM TRAIN PROPULSION/BRAKING SYSTEM',
     1/ 'CAPTURED BY THE TRACKWAY EXHAUST SYSTEM WHEN TRAIN IS STOPPED
     2',T79,F14.1,T96,'PERCENT')                                     
622   FORMAT (/ 'PERCENT OF HEAT FROM TRAIN PROPULSION/BRAKING SYSTEM',
     1/,'CAPTURED BY THE TRACKWAY EXHAUST SYSTEM WHEN TRAIN IS MOVING',
     2T79,F14.1,T96,'PERCENT')                                      
630   FORMAT (/ 'PERCENT OF SENSIBLE HEAT FROM TRAIN AUXILIARIES AND PAS
     1SENGERS'/,'CAPTURED BY THE TRACKWAY EXHAUST SYSTEM WHEN TRAIN IS S
     2TOPPED ' ,T79,F14.1,T96,'PERCENT')                         
632   FORMAT (/ 'PERCENT OF SENSIBLE HEAT FROM TRAIN AUXILIARIES AND PAS
     1SENGERS'/,'CAPTURED BY THE TRACKWAY EXHAUST SYSTEM WHEN TRAIN IS M
     2OVING '  ,T79,F14.1,T96,'PERCENT')                         
640   FORMAT (/ 'MAXIMUM TRAIN SPEED AT WHICH THE TRACKWAY EXHAUST SYSTE
     1M OPERATES',T79,F14.1,T96,'MPH')                               
642   FORMAT (/ 'FIRE SIMULATION OPTION',T79,I14,T96,'BYPASS')       
643   FORMAT (/ 'FIRE SIMULATION OPTION',T79,I14,T96,'YES')          
644   FORMAT (/ 'FIRE SIMULATION OPTION',T79,I14)                    
645   FORMAT (/ 'EFFECTIVE EMMISSIVITY OF COMBUSTION PRODUCTS', T79, 
     1 F14.3 )                                                          
650   FORMAT (/ T52,'SYSTEM GEOMETRY'//T26,'SECTION',T41,'STARTING',T5
     17,'ENDING',T72,'NUMBER',T86,'INITIAL'/T22,'IDENTIFICATION',T43,'NO
     2DE',T58,'NODE',T74,'OF',T86,'AIR FLOW'/T26,'NUMBER',T42,'NUMBER',T
     357,'NUMBER',T71,'SEGMENTS',T87,'(CFM)')                           
660   FORMAT (/ 'LINE SECTIONS',T112,'FORM 2A')                      
670   FORMAT (/ 'VENTILATION SHAFT SECTIONS',T112,'FORM 2B')         
680   FORMAT (/ T17,I13,I16,2I15,F17.0)                               
690   FORMAT (/ T17,I13,I16,I15,15X,F17.0)                            
700   FORMAT (T19,I5,' NODES WERE DESCRIBED')                       
710   FORMAT (T19,I5,' SEGMENTS IN THIS SYSTEM')                    
720   FORMAT (22X,'INTERNAL-EXTERNAL SECTION-NODE REFERENCE TABLES'//
     1/T4,'INTERNAL',T29,'EXTERNAL',T54,'BACKWARD',T80,' FORWARD'/T4,'SE
     2CTION',T29,'SECTION',T54,'AERO NODE',T80,'AERO NODE'/T4,8('-'),T29
     3,8('-'),T54,9('-'),T80,9('-')/T29,'(TABL11)',T54,'(TABL12)',T80,'(
     4TABL13)',/)                                                       
730   FORMAT (T4,I5,T29,I5,T54,I5,T80,I5)                               
740   FORMAT (////T4,'INTERNAL',T29,'EXTERNAL',T50,'NO. OF SECTIONS',T76
     1,'INTERNAL SECTIONS'  /T4,'  NODE',T29,'  NODE',T50,'ADJACENT TO N
     2ODE',T76,'ADJACENT TO NODE*'/T4,8('-'),T29,8('-'),T50,16('-'),T76,
     317('-')/T29,'(TABL15)',T54,'(TABL16)',T80,'(TABL17)',/)           
750   FORMAT (T4,I5,T29,I5,T54,I5,T74,4I5)                              
755   FORMAT(/T20,'* NOTE - ORDER OF ENTRIES MAY BE CHANGED BY JINS')   
760   FORMAT (////T4,'EXTERNAL',T29,'INTERNAL',T54,'INTERNAL'/T4,'SECTIO
     1N',T29,'SECTION',T54,'  NODE'/T4,'OR NODE',T29,'(TABL14)',T54,'(TA
     2BL19) '/T4,8('-'),T29,8('-'),T54,8('-'),/)                        
770   FORMAT (35X,'NODE NUMBER',I4)                                 
780   FORMAT (30X,'POOR INITIALIZATION')                            
790   FORMAT (////T53,'FLOW INITIALIZATION CHECK'///4X,'INTERNAL SECTION
     1 AIRFLOW RATE (CFM)'/4X,35('-'))                                  
800   FORMAT (/ 5X,10F12.1)                                           
805   FORMAT (///4X,'LOOP AIRFLOW RATE (CFS)'/4X,23('-'))               
810   FORMAT (/ 5X,10F12.3)                                           
820   FORMAT (/ 'AVERAGE PATRON WEIGHT',T79,F14.0,T96,'LBS',T112,'FORM 1
     1G')                                                            
830   FORMAT (/// 'INPUT VERIFICATION OF FIRE/UNSTEADY HEAT SOURCE NUMBE
     1R',I3,3X,A36,T112,'FORM 4',/)                                       
840   FORMAT (/ 'LOCATION OF SOURCE  ( LINE SECTION - SEGMENT - SUBSEGME
     1NT )',T80,2(I3,' -'),I3)                                       
850   FORMAT (/ 'SENSIBLE HEAT RATE',T79,F14.0,T96,'BTU/HR')         
860   FORMAT (/ 'LATENT HEAT RATE',T79,F14.0,T96,'BTU/HR')           
870   FORMAT (/ 'SIMULATION TIME AFTER WHICH SOURCE BECOMES ACTIVE',T79
     1,F14.0,T96,'SECONDS')                                           
880   FORMAT (/ 'SIMULATION TIME AFTER WHICH SOURCE BECOMES INACTIVE',
     1T79,F14.0,T96,'SECONDS')                                         
881   FORMAT (/ 'FIRE SOURCE EFFECTIVE FLAME TEMPERATURE',T79,F14.1, 
     1T96,'DEG F')                                                      
882   FORMAT (/ 'FIRE SOURCE EFFECTIVE AREA FOR RADIATION',T79,F14.1,
     1T96,'SQ FT')                                                      
883   FORMAT (/ T26,'** FIRE/UNSTEADY STATE HEAT SOURCE NUMBERS=',I4,I8)   
890   FORMAT (T37,'SUBSEGMENTS ADJACENT TO THE NODE'////T4,'INTERNAL',
     1T24,'EXTERNAL',T44,'NO. OF SECTIONS',T72,'   SUBSEGMENTS ADJACENT 
     2TO NODE'/T4,'  NODE',T24,'  NODE',T44,'ADJACENT TO NODE',T69,
     3'(ORDER OF ENTRIES MAY BE CHANGED BY JINS)'/T4,8('-'),T24,  
     48('-'),T44,16('-'),T69,41('-'),/T24,'(TABL15)',T48,'(TABL16)',T85,
     5'(TABL18)',//)                                                    
900   FORMAT(/T4,I5,T24,I5,T48,I5,T72,5I7)                              
910   FORMAT (' ')                                                    
920   FORMAT (/ T45,'INPUT VERIFICATION OF NODE DATA')                
930   FORMAT (// 'NODE NUMBER',T79,I14,T112,'FORM 6A')               
940   FORMAT (/A21,T79,I14)                     
950   FORMAT (/A21,T79,I14,3X,'STRAIGHT-THROUGH JUNCTION OR PORTAL')            
960   FORMAT (/A21,T79,I14,3X,'TUNNEL-TO-TUNNEL CROSSOVER',
     1        /95X,'JUNCTION')    
970   FORMAT (/A21,T79,I14,3X,'DIVIDING WALL TERMINATION',     
     1        /95X,'JUNCTION')    
980   FORMAT (/A21,T79,I14,3X,'''T'' JUNCTION')                                 
990   FORMAT (/A21,T79,I14,3X,'ANGLED JUNCTION')                                
1000  FORMAT (/A21,T79,I14,3X,'''Y'' JUNCTION')                                 
1010  FORMAT (/A21,T79,I14,3X,'RESERVED - SEE USERS + PROG.',
     1        /95X,'MANUALS')   
1020  FORMAT (/A21,T79,I14,3X,'ZERO TOTAL PRESSURE CHANGE',
     1        /95X,'JUNCTION')    
1030  FORMAT (/'NODE THERMODYNAMIC TYPE',T79,I14,T96,'(MIXING NODE)')                                                                 
1040  FORMAT (/'NODE THERMODYNAMIC TYPE',T79,I14,T96,'(NON-MIXING NODE)'
     1)                                                             
1050  FORMAT (/ 'NODE THERMODYNAMIC TYPE',T79,I14,T96,'(TEMP/HUMID B.C.)
     1')                                                             
1060  FORMAT (/ 'DRY-BULB TEMPERATURE AT BOUNDARY',T79,F14.1,T96,'DEG F'
     1,T112,'FORM 6B')                                               
1070  FORMAT (/ 'WET-BULB TEMPERATURE AT BOUNDARY',T79,F14.1,T96,'DEG F'
     1)                                                              
1080  FORMAT (/ T5,'HUMIDITY RATIO AT BOUNDARY',T79,F14.5,T96,'LB/LB OF 
     1DRY AIR')                                                       
1090  FORMAT (/ 'MORNING DRY-BULB BOUNDARY CONDITION TEMPERATURE',T79,
     1F14.1,T96,'DEG F')                                               
1100  FORMAT (/ 'MORNING WET-BULB BOUNDARY CONDITION TEMPERATURE',T79,
     1F14.1,T96,'DEG F')                                               
1110  FORMAT (/ T3,'BOUNDARY CONDITION HUMIDITY RATIO',T79,F14.5,T96,
     1'LB/LB OF DRY AIR')                                                
1120  FORMAT (/ 'EVENING OR OFF HOUR DRY-BULB BOUNDARY CONDITION TEMPERA
     1TURE',T79,F14.1,T96,'DEG F')                                   
1130  FORMAT (/ 'EVENING OR OFF HOUR WET-BULB BOUNDARY CONDITION TEMPERA
     1TURE',T79,F14.1,T96,'DEG F')                                   
1140  FORMAT (/ 'SECTIONS CONNECTED TO THERMAL SUBNODE ''A''',T112,'FORM
     1 6C',T72,3I7)                                                  
1150  FORMAT (/ 'SECTIONS CONNECTED TO THERMAL SUBNODE ''B''',T72,I7)
1160  FORMAT (/ 'SECTIONS CONNECTED TO THERMAL SUBNODE ''C''',T72,3I7)
1170  FORMAT (/ 20X,'NODE',I4)                                        
1180  FORMAT (T37,'DUMP OF TABLES RELATED TO AERO AND THERMO PHENOMENA'
     1////,T71,'(ORDER OF ENTRIES MAY HAVE BEEN CHANGED BY JINS)'/T4,
     2'INTERNAL',T24,'EXTERNAL',T44,'NO. OF SECTIONS',T72,'INTERNAL SECT
     3IONS',T103,'SUBSEGMENTS'/T4,'  NODE',T24,'  NODE',T44,'ADJACENT TO
     4 NODE',T72,'ADJACENT TO NODE',T101,'ADJACENT TO NODE'/T4,8('-'),  
     5T24,8('-'),T44,16('-'),T72,17('-'),T101,16('-')/T24,'(TABL15)',T48
     6,'(TABL16)',T76,'(TABL17)',T105,'(TABL18)',//)                    
1190  FORMAT (////T4,'INTERNAL',T24,'EXTERNAL'        ,T55,'(BACKWARD TH
     1ERMO SUBNODE)'        ,T98,'(FORWARD THERMO SUBNODE)'/T4,'SECTION'
     2,T24,'SECTION'        ,T58,'NUMBER       BRANCH'        ,T101,'NUM
     3BER      BRANCH'/T4,8('-'),T24,8('-')        ,T44,8('-'),T55,11('-
     4'),T69,11('-')        ,T87,8('-'),T98 ,11('-'),T111,11('-')/T24,'(
     5TABL11)'        ,T44,'(TABL20)'        ,T87,'(TABL21)'/)          
1200  FORMAT (T4,I5,T24,I5,T48,I5,T69,5I5)                              
1205  FORMAT (T99,5I5)                                                 
1210  FORMAT (///,'FIRST DUMP OF TABL1')                           
1220  FORMAT (//,25I5)                                                  
1230  FORMAT (///,'FIRST DUMP OF TABL2')                           
1240  FORMAT (///,'COMBINED DUMP OF TABL1 AND TABL2')              
1250  FORMAT (/ 2I4,5X,28I4)                                          
1260  FORMAT (///,'FIRST DUMP OF TABL23')                          
1270  FORMAT (///,'FIRST DUMP OF COAEND')                          
1280  FORMAT (/ 8G15.5,/)                                            
1290  FORMAT (///,'COMBINED DUMP OF TABL23 AND COAEND')            
1300  FORMAT (/ 2I5,8G15.5,/,(10X, 8G15.5,/))                        
1312  FORMAT (///,T39,'INPUT VERIFICATION OF IMPULSE FAN DATA')     
1313  FORMAT (///,T24,'IMPULSE FAN TYPE',T83,I9,T112,'FORM 7C',/,T24,
     1'FOR LINE SEGMENT TYPE',T83,I9)                                  
1314  FORMAT (/ T24,'IMPULSE FAN VOLUME FLOW RATE',T77,F9.1,3X,'CFM') 
1315  FORMAT (/ T24,'IMPULSE FAN PRESSURE EFFICIENCY',/,T24,'(RATIO OF A
     1CTUAL/THEORETICAL PRESSURE RISE)',T77,F9.2)                     
1316  FORMAT (/ T24,'IMPULSE FAN NOZZLE DISCHARGE VELOCITY',T77,F9.1,3X,
     1'FPM')                                                          
1317  FORMAT (/ T24,'SIMULATION TIME AFTER WHICH IMPULSE FAN SWITCHES ON
     1',T77,F9.1,3X,'SEC')                                            
1318  FORMAT (/ T24,'SIMULATION TIME AFTER WHICH IMPULSE FAN SWITCHES OF
     1F',T77,F9.1,3X,'SEC')                                           
1320  FORMAT (/T4,I5,T24,I5,T44,I6,T58,I4,T71,I4,T87,I6,T100,I4,T113,I4)  
1337  FORMAT (// T36,'INPUT VERIFICATION OF TRAIN INITIALIZATION DATA',
     1T112,'FORM 10')                                                  
1340  FORMAT (// 'NUMBER OF TRAINS IN OPERATION AT INITIALIZATION',I5)
1350  FORMAT (/ T72,'ACCELERATION',T87,'DECELERATION',T105,'REMAINING'
     1/'TRAIN',T15,'LOCATION',T31,'TRAIN',T45,'ROUTE',T61,'TRAIN',T76
     2,'GRID',T91,'GRID',T107,'DWELL'/'NUMBER',T15,'ON ROUTE',T31,'SPEED
     3',T45,'NUMBER',T61,'TYPE',T72,'TEMPERATURE',T87,'TEMPERATURE',T107
     4, 'TIME',T118,'COASTING'/                                      
     5           T16,'(FEET)',T31,'(MPH)',T74,'(DEG F)',T89,'(DEG F)',T1
     605,'(SECONDS)',//)                                                
1351  FORMAT (/,'A NEGATIVE VALUE FOR THE REMAINING DWELL TIME INDICATES
     1',' THAT THE TRAIN IS TO REMAIN IN THE INITIALIZED LOCATION',/,
     2  'FOR THE DURATION OF THE SIMULATION.')
1360  FORMAT (/ 2X,A2,F17.2,F14.2,I12,I16,F16.2,F15.2,F17.1)          
1361  FORMAT (/ 2X,A2,F17.2,F14.2,I12,I16,F16.2,F15.2,F16.1,T121,'YES')                                               
1362  FORMAT (/ 2X,A2,F17.2,F14.2,I12,I16,F16.2,F15.2,F16.1,T121,'NO')
1363  FORMAT (T29,'COASTING INDICATOR =',I10)                       
1370  FORMAT (// T24,'INPUT VERIFICATION OF ENVIRONMENTAL CONTROL ZONE
     1 DATA')                                                           
1380  FORMAT (// 1X,43('-'),8X,'ZONE NUMBER',I3,8X,47('-'))           
1390  FORMAT (/ 'ZONE TYPE',T79,I14,T96,'CONTROLLED',T112,'FORM 11A')
1400  FORMAT (/ 'ZONE TYPE',T79,I14,T96,'UNCONTROLLED',T112,'FORM 11A')
1410  FORMAT (/ 'ZONE TYPE',T79,I14,T96,'NON-INERTIA',T112,'FORM 11A')
1420  FORMAT (/ 'NUMBER OF LINE SEGMENTS AND VENTILATION SHAFTS IN ZONE'
     1,T79,I14)                                                      
1430  FORMAT (/ 'MORNING RUSH HOUR DESIGN CONDITIONS',T41,'DRY-BULB TEMP
     1ERATURE',T79,F14.1,T96,'DEG F')                                
1440  FORMAT (/ T41,'WET-BULB TEMPERATURE',T79,F14.1,T96,'DEG F'/T41,'HU
     1MIDITY RATIO',6X,T79,F14.5,T96,'LB/LB OF DRY AIR'/,T41,
     2'RELATIVE HUMIDITY',T79,F14.1,T96,'PERCENT')                  
1450  FORMAT (/ 'EVENING RUSH OR OFF HOUR DESIGN ',T41,'DRY-BULB TEMPERA
     1TURE',T79,F14.1,T96,'DEG F'/'CONDITIONS')                    
1460  FORMAT (T41,'WET-BULB TEMPERATURE',T79,F14.1,T96,'DEG F'/T41,
     1'HUMIDITY RATIO',6X,T79,F14.5,T96,'LB/LB OF DRY AIR'/ ,T41,
     2'RELATIVE HUMIDITY',T79,F14.1,T96,'PERCENT')                  
1470  FORMAT (/ 'LINE SEGMENTS AND VENTILATION SHAFTS IN ZONE   ( SECTIO
     1N - SEGMENT )')
1480  FORMAT (I5, ' -', I3)                                                     
1484  FORMAT (/ A80, T112,'FORM 11B')                    
1485  FORMAT (/ 10X,'REQUIRED DTRM MATRIX SIZE IS',I5,', AVAILABLE SIZE 
     1IS', I5 )                                     
1490  FORMAT (5X,'SECTION - SEGMENT',I5,' -',I3)                        
1500  FORMAT (5X,'NODE',I5)                                             
1510  FORMAT (// T35,'INPUT VERIFICATION OF CONTROL GROUP INFORMATION'
     1,T112,'FORM 12')                                                  
1520  FORMAT (// 'TEMPERATURE TABULATION INCREMENT',T79,F14.1,T96,'DEG 
     1F')                                                             
1530  FORMAT (/ 'NUMBER OF CONTROL GROUPS',T79,I14)                  
1540  FORMAT (// T3,'GROUP',T13,'NUMBER OF',T25,'INTERVAL',T37,'NUMBER O
     1F',T55,'SUMMARY OPTION',T90,'NUMBER OF',T105,'NUMBER OF',T122,'TIM
     2E OF'/T3,'NUMBER',T13,'INTERVALS',T26,'LENGTH',T36,'ABBREVIATED',
     3T89,'CYCLES PER',T105,'CYCLES PER',T121,'LAST PRINT'/T36,'PRINTS P
     4ER',T89,'AERODYNAMIC',T104,'THERMODYNAMIC',T122,'IN GROUP'/T36,'DE
     5TAIL PRINT',T89,'EVALUATION',T105,'EVALUATION',T123,'(SEC)',//)   
 5541 FORMAT(/ '  THE WALL TEMPERATURE MODEL AND THE RADIATION HEAT TRAN
     1SFER TO THE WALL HAVE BEEN SUPRESSED',/,                       
     2'BY ENTERING ZERO FOR THE NUMBER OF THERMODYNAMIC CYCLES PER WALL 
     3TEMPERATURE EVALUATION')                                      
1550  FORMAT (/ I6,T13,I5,T22,F10.2,T35,I7,T49,I3,' - NEITHER SUMMARY NO
     1R INITIALIZATION',T90,I5,T106,I5,T119,F10.2)                    
1560  FORMAT (/ I6,T13,I5,T22,F10.2,T35,I7,T49,I3,' - INITIALIZE ONLY',
     1T90,I5,T106,I5,T119,F10.2)                                       
1570  FORMAT (/ I6,T13,I5,T22,F10.2,T35,I7,T49,I3,' - SUMMARY ONLY',T91,
     1I5,T106,I5,T119,F10.2)                                          
1580  FORMAT (/ I6,T13,I5,T22,F10.2,T35,I7,T49,I3,' - SUMMARY AND INITIA
     1LIZE',T90,I5,T106,I5,T119,F10.2)                                
1590  FORMAT (/ I6,T13,I5,T22,F10.2,T35,I7,T49,I3,' - SUMMARY, ENVIRON.  
     1EVAL., INITIALIZE',T90,I5,T106,I5,T119,F10.2)                   
1600  FORMAT (// 51X,'PROGRAM CONTROLS',T112,'FORM 13'/// 'TIME INCREMEN
     1T PER CYCLE',T79,F14.2,T96,'SECONDS'// 'MAXIMUM SIMULATION TIME',
     2T79,F14.2,T96,'SECONDS'// 'NUMBER OF CYCLES PER COMPLETE TRAIN EVA
     3LUATION',T79,I14,// 'NUMBER OF THERMODYNAMIC CYCLES PER WALL TEMPE
     4RATURE EVALUATION',T79,I14)                         
1610  FORMAT ('IMPOSSIBLE SITUATION - SUBROUTINE INPUT ')          
1612  FORMAT (/ 15X, '***** NOTE- OPTION',I2,' INITIALIZATION FILE HAS B
     1EEN READ *****')                                           
1614  FORMAT (/ 15X,'PRINT GROUP =',I2)                               
1616  FORMAT (A254)
1618  FORMAT (I5)
C                                                                       
C***** INITIALIZATION - STAGE 1 ****************************************
C                                                                       
C*****NTIME IS SET EQUAL TO -1 SO THAT SUBROUTINES EQUAT AND QSQLP CAN  
C*****TELL WHETHER THEY ARE BEING CALLED DURING INPUT OR EXECUTION      
      NTIME=-1                                                          
      TNLSS=0                                                           
      LIFE=0                                                            
C --  NUMBER OF LINES PER PRINTED PAGE                                  
      LINES=60                                                          
C --  INITIALIZE EXTERNAL I.D. NUMBER ARRAYS                            
      DO 1620 ILS=1,LMLSEG                                              
1620  NUMLS(ILS)=0                                                      
      DO 1630 IVS=1,LMVSEG                                              
1630  NUMVS(IVS)=0                                                      
      DO 1640 ISECT=1,LMSECT                                            
1640  TABL11(ISECT)=0                                                   
      DO 1650 NODE=1,LMNODE                                             
1650  TABL15(NODE)=0                                                    
C --  INITIALIZE NUMBER OF TRAINS IN LINE SEGMENT                       
      DO 1660 ILS=1,LMLSEG                                              
1660  NTRNLS(ILS)=0                                                     
C---- INITIALIZE ARRAYS TSSTAB, FLS                                     
      DO 1661 I=1, LMSS                                                 
1661  TSSTAB(I) = 1.0                                                   
      DO  1663  ILS = 1, LMLSEG                                         
1663  FLS(ILS) = 0.0                                                    
C                                                                       
C
C
C     GENERAL DATA                     FORM 1    ***********************
C                                                                       
C***** INPUT FORMS 1A AND 1B                                            
C**** BEGIN PROCESSING SERIES 1 INPUT FORMS.  SET EXITCODE TO 1
      EXITCODE=1
C  
C**** BEGIN READING TITLE LINES UNTIL VALID NUMERIC INPUT IS FOUND IN 
C**** COLUMNS 1 THROUGH 80 OR THE MAXIMUM NUMBER OF TITLE LINES (MAXTIT)
C**** IS EXCEEDED.
      DO 1665 I=1, MAXTIT
        READ (IN,20) TITLE
        IF (LEN_TRIM(TITLE) .EQ. 0)GOTO 1664
        READ (TITLE,FMT=30,ERR=1664)DUMY1, DUMY2, DUMY3, DUMY4,
     1                              DUMY5, DUMY6, DUMY7, DUMY8
C**** FOUND VALID NUMERIC INPUT.  STORE NUMBER OF TITLE LINES IN NUMY2
        NUMY2=I-1
        GOTO 1662
1664  TITLINES(I)=TITLE
1665  CONTINUE
C**** SETUP TITLE LINE SPACING.  IF THE NUMBER OF TITLE LINES ENTERED
C**** IS LESS THAN 10 USE FORMAT LABEL #60 (DOUBLE LINE SPACING)
C**** OTHERWISE USE FORMAT LABEL #70 (SINGLE LINE SPACING)
1662  WRITE (OUT,50)
      IF (NUMY2 .GT. 10)GOTO 1667
      NUMY4=2*NUMY2+23
      DO 1666 I= 1,NUMY2
        WRITE(OUT,60)TITLINES(I)
1666  CONTINUE
      GOTO 1669
1667  NUMY4=NUMY2+23
      DO 1668 I= 1,NUMY2  
        WRITE(OUT,70)TITLINES(I)
1668  CONTINUE
C**** DEVELOP THE HEADER AND FOOTER WHICH WILL BE PRINTED ON 
C**** EVERY PAGE OF THE FORMATED OUTPUT FILE.
C**** INITIALIZE HEADER AND FOOTER
1669  WRITE (HEADER,45)
      WRITE (FOOTER,46)
      TITLE=ADJUSTL(TITLINES(1))
	NUMY1=LEN_TRIM(TITLE)
	NUMY2=(80-NUMY1)/2.0
      HEADER(2:24)='SES VER '//SESVER
      HEADER((26+NUMY2):(25+NUMY2+NUMY1))=TITLE(1:NUMY1)
	HEADER(112:116)='PAGE:'
	NUMY1=LEN_TRIM(INPFILE)
      IF (NUMY1 .GT. 60) THEN
        FOOTER(8:28)='FILE: '//INPFILE(1:12)//'...'
        FOOTER(29:73)=INPFILE((NUMY1-54):NUMY1)
      ELSE
	  FOOTER(8:(NUMY1+13))='FILE: '//INPFILE(1:NUMY1)
      ENDIF
      FOOTER(86:122)='SIMULATION TIME: '//DATETIME
C****
      IHOUR=DUMY1                                                       
      MINUTE=(DUMY1-FLOAT(IHOUR))*100.0+0.2                             
      MONTH=DUMY2                                                       
      NUMY3=DUMY3                                                       
      CALL CHECKI (IHOUR,0,24,32)
C**** SET DESIGN HOUR TO DEFAULT (17) IF THE VALUE ENTERED IS EITHER
C**** LESS THAN ZERO OR GREATER THAN 24.
      IF (IHOUR .LT. 0 .OR. IHOUR .GT. 24)IHOUR=17                                       
      CALL CHECKI (MINUTE,0,59,36)                                       
C**** SET MINUTES PORTION OF THE DESIGN HOUR TO DEFAULT (0) IF THE VALUE
C**** ENTERED IS EITHER LESS THAN ZERO OR GREATER THAN 59.
      IF (MINUTE .LT. 0 .AND. MINUTE .GT. 59)MINUTE=0
      CALL CHECKI (MONTH,1,12,33)                                       
C**** SET DESIGN MONTH TO DEFAULT (7) IF THE VALUE ENTERED IS EITHER
C**** LESS THAN ONE OR GREATER THAN 12.
      IF (MONTH .LT. 1 .OR. MONTH .GT. 12)MONTH=7
      IF (NEW .EQ. 1) GOTO 5640                                            
      NUMY1=IHOUR*100.0+MINUTE                                          
      HOUR=FLOAT(IHOUR)+MINUTE/60.0                                     
      IF (NUMY1-1000) 1690,1680,1680                                    
1680  WRITE (OUT,80) NUMY1,MONTHS(MONTH),NUMY3                
      GO TO 1700                                                        
1690  WRITE (OUT,90) NUMY1,MONTHS(MONTH),NUMY3                
1700  IF (IHOUR-12) 1710,1720,1720                                      
1710  DESPER=1                                                          
      GO TO 1730                                                        
1720  DESPER=2                                                          
1730  DO 1732 I=NUMY4,50
        WRITE (OUT,910)
1732  CONTINUE
      WRITE (OUT,100) SESVER
C                                                                       
C                                                                       
C---- FORM 1C                                                           
C                                                                       
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,110)                                                   
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7,DUMY8      
C --  TRAIN PERFORMANCE OPTION (0,1,2,3)                                
      TPOPT=DUMY1                                                       
      IF (TPOPT) 1750,1750,1740                                         
1740  IF (TPOPT-2) 1760,1770,1780                                       
1750  WRITE (OUT,120) TPOPT                                             
      GO TO 1790                                                        
1760  WRITE (OUT,130) TPOPT                                             
      GO TO 1790                                                        
1770  WRITE (OUT,140) TPOPT                                             
      GO TO 1790                                                        
1780  WRITE (OUT,150) TPOPT                                             
1790  CALL CHECKI (TPOPT,0,3,17)
C --  TEMPERATURE / HUMIDITY SIMULATION OPTION (0,1,2)                  
      THOPT=DUMY2                                                       
      IF (THOPT-1) 1800,1810,1820                                       
1800  WRITE (OUT,160) THOPT                                             
      GO TO 1830                                                        
1810  WRITE (OUT,170) THOPT                                             
      GO TO 1830                                                        
1820  WRITE (OUT,180) THOPT                                             
1830  CALL CHECKI (THOPT,0,2,126)                                       
C --  HUMIDITY DISPLAY OPTION (1,2,3)                                   
      HUMOPT=1                                                          
      HCOPT=0                                                           
      HSOPT = 1                                                         
      IF (THOPT) 5650,1960,1840                                         
1840  HUMOPT=DUMY3                                                      
      IF (HUMOPT-2) 1850,1860,1870                                      
1850  WRITE (OUT,190) HUMOPT                                            
      GO TO 1880                                                        
1860  WRITE (OUT,200) HUMOPT                                            
      GO TO 1880                                                        
1870  WRITE (OUT,210) HUMOPT                                            
1880  CALL CHECKI (HUMOPT,1,3,94)
C**** SET HUMIDITY DISPLAY OPTION TO DEFAULT (1) IF THE VALUE ENTERED
C**** IS NEITHER 1, 2 OR 3.
      IF (HUMOPT .LT. 1 .OR. HUMOPT .GT. 3)HUMOPT=1
C --  ENVIRONMENTAL CONTROL LOAD EVALUATION (0,1,2)                     
      HCOPT=DUMY4                                                       
      IF (HCOPT-1) 1890,1900,1910                                       
1890  WRITE (OUT,220) HCOPT                                             
      GO TO 1920                                                        
1900  WRITE (OUT,230) HCOPT                                             
      GO TO 1920                                                        
1910  WRITE (OUT,240) HCOPT                                             
      DESPER=3                                                          
1920  CALL CHECKI (HCOPT,0,2,126)                                       
C --  HEAT SINK OUTPUT OPTION (0,1,2)                                   
      HSOPT=DUMY5                                                       
      IF (HSOPT) 1940,1930,1940                                         
1930  WRITE (OUT,250) HSOPT                                             
      GO TO 1950                                                        
1940  WRITE (OUT,260) HSOPT                                             
1950  CALL CHECKI (HSOPT,0,2,126)                                       
      HSOPT=HSOPT+1                                                     
1960  SUPOPT=DUMY6                                                      
      WRITE (OUT,270) SUPOPT                                            
C --  SUPPLEMENTARY OUTPUT OPTION (0,1,2,3,4,5)                         
      CALL CHECKI (SUPOPT,0,5,152)                                      
C**** SET SUPPLEMENTARY OUTPUT OPTION TO DEFAULT (0) IF THE VALUE
C**** ENTERED IN LESS THAN 0 OR GREATER THAN 5.
      IF (SUPOPT .LT. 0 .OR. SUPOPT .GT. 5)SUPOPT=0
      SUPOPT=SUPOPT+1                                                   
C --  ALLOWABLE SIMULATION ERRORS                                       
      NSIMER=DUMY7                                                      
      WRITE (OUT,280) NSIMER                                            
      CALL CHECKI (NSIMER,0,50,186)                                     
C --  ALLOWABLE INPUT ERRORS ( DEATH OPTION ) (-1,0,+N)                 
      DEATH=DUMY8                                                       
      IF (DEATH) 1980,1970,1970                                         
1970  WRITE (OUT,290) DEATH                                             
      GO TO 1990                                                        
1980  WRITE (OUT,300) DEATH                                             
1990  CONTINUE                                                          
C --  CHECK IF FATAL ERROR HAS OCCURRED                                 
      IF (NEW .EQ. 1) GOTO 5640                                            
C                                                                       
C---- FORM 1D                                                           
C                                                                       
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7,DUMY8      
C*****DUMY6 WAS THE ENTRY FOR THE NUMBER OF PORTALS WHICH HAS BEEN
C*****ELIMINATED IN VERSION 4.0.  THE READ STATEMENT, HOWEVER, WAS
C*****NOT CHANGED IN ORDER TO MAINTAIN COMPATIBILITY WITH INPUT 
C*****FILES FROM EARLIER SES VERSIONS.
      NLS=DUMY1                                                         
      NSEC=DUMY2                                                        
      NVSEC=DUMY3                                                       
      NLSEC=NSEC-NVSEC                                                  
      NLSCP1=NLSEC+1                                                    
      NNODE=DUMY4                                                       
      NBRJCT=DUMY5                                                      
      NOSYS=NLS+NVSEC+NSEC+NNODE+NBRJCT                          
      IF (NOSYS) 2020,2010,2020                                         
2010  WRITE (OUT,470)                                                   
      NUMUL=0                                                           
      NFNTYP=0                                                          
      NLSCP1=0                                                          
      GO TO 2030                                                        
2020  NOSYS=1                                                           
C --  NUMBER OF LINE SEGMENTS                                           
      WRITE (OUT,310) NLS                                               
      CALL CHECKI (NLS,1,LMLSEG,1)                                      
C --  NUMBER OF SECTIONS                                                
      WRITE (OUT,320) NSEC                                              
      CALL CHECKI (NSEC,1,LMSECT,127)                                   
C --  NUMBER OF VENTILATION SHAFTS                                      
      WRITE (OUT,330) NVSEC                                             
      CALL CHECKI (NVSEC,0,LMVSEG,2)                                    
C --  NUMBER OF NODES                                                   
      WRITE (OUT,340) NNODE                                             
      CALL CHECKI (NNODE,2,NSEC+1,128)                                  
C --  NUMBER OF BRANCHED JUNCTIONS                                      
      WRITE (OUT,350) NBRJCT                                            
      CALL CHECKI (NBRJCT,0,NNODE,129)                                  
C --  NUMBER OF UNSTEADY HEAT SOURCES                                   
      NUMUL=DUMY7                                                       
      WRITE (OUT,370) NUMUL                                             
      CALL CHECKI (NUMUL,0,LMUL,132)                                    
C --  NUMBER OF FAN TYPES                                               
      NFNTYP=DUMY8                                                      
      WRITE (OUT,380) NFNTYP                                            
      CALL CHECKI (NFNTYP,0,LMFNTP,133)                                 
C                                                                       
C---- FORM 1E                                                           
C                                                                       
2030  READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7,DUMY8      
C --  NUMBER OF TRAIN ROUTES                                            
      NTRRTE=DUMY1                                                      
      WRITE (OUT,390) NTRRTE                                            
      IF (TPOPT) 5650,2040,2050                                         
2040  CALL CHECKI (NTRRTE,0,0,134)                                      
      GO TO 2060                                                        
2050  CALL CHECKI (NTRRTE,1,LMTRRT,135)                                 
C --  NUMBER OF TRAIN TYPES                                             
2060  NTRTYP=DUMY2                                                      
      WRITE (OUT,400) NTRTYP                                            
      IF (TPOPT) 5650,2070,2080                                         
2070  CALL CHECKI (NTRTYP,0,0,136)                                      
      GO TO 2090                                                        
2080  CALL CHECKI (NTRTYP,1,LMTRTP,137)                                 
2090  NOTRAN=TPOPT*NTRRTE*NTRTYP                                        
      IF (NOTRAN) 2110,2110,2100                                        
2100  NOTRAN=1                                                          
C --  EITHER TRAINS OR SYSTEM SHOULD BE SIMULATED                       
2110  IF (NOSYS+NOTRAN) 2120,2120,2130                                  
2120  CALL EERROR (73)                                                   
      GO TO 5640                                                        
C --  ENVIRONMENTAL CONTROL ZONES                                       
2130  NZONE =DUMY3                                                      
      WRITE (OUT,410) NZONE                                             
      CALL CHECKI (NZONE ,0,LMCLST,100)                                 
C**** VERSION 4.1 UPGRADES
C     THE NUMBER OF ENVIRONMENT CONTROL ZONES SHOULD NOT BE ZERO
C     WHEN FORM 1C ENVIRONMENTAL CONTROL LOAD EVALUATION OPTION IS EQUAL TO ONE OR TWO.
      IF (HCOPT .GT. 0.0) GOTO 1031
      GOTO 1032
1031  IF (NZONE .EQ. 0.0) CALL EERROR (264)
C
C**** FAN STOPPING/WINDMILLING OPTION
1032  FSCFT=DUMY4                                                       
      IF (FSCFT .EQ. 1)THEN
C**** SIMULATION TERMINATION        
        WRITE (OUT,430)FSCFT
      ELSE IF (FSCFT .EQ. 2) THEN
C**** FAN SHUTDOWN        
        WRITE (OUT,440)FSCFT
      ELSE
C**** INVALID ENTRY.  PRINT FSCFT AS ENTERED AND SET 
C**** FAN STOPPING/WINDMILLING OPTION TO DEFAULT VALUE OF ONE (1)       
        WRITE (OUT,430)                                                   
        CALL EERROR (11)                                                   
        FSCFT=1
      END IF
      NTRAN=DUMY5                                                       
      IF (NTRTYP*NTRRTE) 5650,2190,2200                                 
2190  NTRAN=0                                                           
2200  WRITE (OUT,460) NTRAN                                             
      CALL CHECKI (NTRAN,0,LMTRAN,141)                                  
C---- CHECK FOR FATAL ERROR                                             
      IF (NEW .EQ. 1) GOTO 5640                                            
C---- NUMBER OF IMPULSE FAN TYPES                                       
      NIFTYP=DUMY6                                                      
      WRITE (OUT,461) NIFTYP                                            
      CALL CHECKI (NIFTYP,0,6,216)                                      
C---- RESTART FILE WRITING OPTION                                      
      RSTWOPT=DUMY7                                                      
      WRITE (OUT,462) RSTWOPT                                            
      CALL CHECKI (RSTWOPT,0,5,217)                                      
      RSTROPT=DUMY8                                                      
      WRITE (OUT,463) RSTROPT                                            
      CALL CHECKI (RSTROPT,0,5,217)                                      
C---- CHECK FOR FATAL ERROR                                             
      IF (NEW .EQ. 1) GOTO 5640                                            
C                                                                       
C---- FORM 1F                                                           
C                                                                       
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,110)                                                   
      WRITE (OUT,480)                                                   
      READ (IN,30) TDBAMB,TWBAMB,PAMB,TAMBA,TWAMBM,TAMBM,TWAMBE,ANNAMP  
      WRITE (OUT,570) TDBAMB                                            
      CALL CHECKR (TDBAMB,-50.0,140.0,7)                                
      WRITE (OUT,580) TWBAMB                                            
      CALL CHECKR (TWBAMB,-50.0,TDBAMB,8)                               
      WRITE (OUT,560) PAMB                                              
      CALL CHECKR (PAMB,20.0,32.0,5)                                    
C*****COMPUTE AMBIENT DRY BULB TEMPERATURE IN DEGREES RANKINE           
      ABSAMB = TDBAMB+ABTEMP                                            
C*****COMPUTE AMBIENT AIR WEIGHT DENSITY                                
      RHOWHT=(PAMB*SAPSI*FTIN*FTIN)/(SAINHG*RDRYAR*(TDBAMB+ABTEMP))     
      WRITE (OUT,590) RHOWHT                                            
C*****COMPUTE AMBIENT AIR MASS DENSITY                                  
      RHOMAS=RHOWHT/GRACC                                               
C*****COMPUTE KINEMATIC VISCOSITY OF AMBIENT AIR                        
      NUAIR=VISAIR/RHOMAS                                               
C*****COMPUTE VOLUMETRIC SPECIFIC HEAT OF AMBIENT AIR                   
      RHOCP=RHOMAS*CP                                                   
C*****COMPUTE AMBIENT AIR SPECIFIC HUMIDITY                             
      CALL LATENT (TDBAMB,TWBAMB,HUMAMB)                                
      WRITE (OUT,600) HUMAMB                                            
      CALL RELHUM (TDBAMB,HUMAMB,RHAMB)                                 
      WRITE (OUT,610) RHAMB                                             
      WRITE (OUT,490)                                                   
      WRITE (OUT,500) TAMBA                                             
      CALL CHECKR (TAMBA,-50.0,140.0,7)                                 
      WRITE (OUT,540) TWAMBM                                            
      CALL CHECKR (TWAMBM,-50.0,TAMBA,8)                                
      CALL LATENT (TAMBA,TWAMBM,HMAMBM)                                 
      WRITE (OUT,600) HMAMBM                                            
      CALL  RELHUM (TAMBA,HMAMBM,DUMY1)                                 
      WRITE (OUT,610) DUMY1                                             
      WRITE (OUT,510) TAMBM                                             
      CALL CHECKR (TAMBM,-50.0,140.0,7)                                 
      WRITE (OUT,550) TWAMBE                                            
      CALL CHECKR (TWAMBE,-50.0,TAMBM,8)                                
      CALL LATENT (TAMBM,TWAMBE,HMAMBE)                                 
      WRITE (OUT,600) HMAMBE                                            
      CALL  RELHUM (TAMBM,HMAMBE,DUMY2)                                 
      WRITE (OUT,610) DUMY2                                             
      WRITE (OUT,520)                                                   
      WRITE (OUT,530) ANNAMP                                            
      CALL CHECKR (ANNAMP,0.0,50.0,192)                                 
C                                                                       
C---- FORM 1G                                                           
C                                                                       
      READ (IN,30) PATWHT, EFSUX, EFMUX, EFASUX, EFAMUX, UMAXUX,        
     1  DUMY1, EECPG                                                    
      WRITE (OUT,820) PATWHT                                            
      CALL CHECKR (PATWHT,50.0, 200.0, 4 )                              
C---- UNDERPLATFORM EXHAUST                                             
      WRITE (OUT,620) EFSUX                                             
      CALL CHECKR (EFSUX,0.0,100.0,151)                                 
      WRITE (OUT,622) EFMUX                                             
      CALL CHECKR (EFMUX,0.0,100.0,151)                                 
      WRITE (OUT,630) EFASUX                                            
      CALL CHECKR (EFASUX,0.0,100.0,151)                                
      WRITE (OUT,632) EFAMUX                                            
      CALL CHECKR (EFAMUX,0.0,100.0,151)                                
      WRITE (OUT,640) UMAXUX                                            
      CALL CHECKR (UMAXUX,0.0,150.0,145)                                
      FIROPT = DUMY1                                                    
      IF (FIROPT .NE. 0)  GO TO 2211                                    
      WRITE (OUT,642)  FIROPT                                           
      GO TO 2214                                                        
2211  IF (FIROPT .NE. 1)  GO TO 2212                                    
      WRITE (OUT,643)  FIROPT                                           
      WRITE (OUT,645)  EECPG                                            
      CALL CHECKR ( EECPG, 0.0, 1.0, 262 )                              
      GO TO 2213                                                        
2212  WRITE (OUT,644)  FIROPT                                           
      CALL EERROR (247)                                                  
      GO TO 5640                                                        
C**** CHECK IF THERMO IS BEING SIMULATED - IF NOT, TERMINATE PROCESSING 
C     OF INPUT DATA                                                     
2213  IF (THOPT .LE. 0)  CALL EERROR(253)                                
C**** CHECK IF THERE IS ANY UNSTEADY-STATE HEAT LOAD.                   
      IF (NUMUL .LT. 1)  CALL EERROR(257)                                
      IF (NEW .EQ. 1) GOTO 5640                                            
2214  FIROP2 = FIROPT                                                   
      IF (EFSUX+EFMUX+EFASUX+EFAMUX-0.01) 2220,2230,2230                
C --  NO UNDERPLATFORM EXHAUST                                          
2220  OPTUX=0                                                           
      GO TO 2248                                                        
C --  UNDERPLATFORM EXHAUST                                             
2230  OPTUX=1                                                           
      EFSUX=1.0-EFSUX/100.0                                             
      EFMUX=1.0-EFMUX/100.0                                             
      EFASUX = 1.0 - EFASUX / 100.0                                     
      EFAMUX = 1.0 - EFAMUX / 100.0                                     
      UMAXUX=UMAXUX*MPHFPS                                              
C
C********************************************************************
C**** BEGIN PROCESSING SERIES 2 INPUT FORMS.  SET EXITCODE TO 2
 2248 EXITCODE=2
C********************************************************************   
C                                                                       
C     SYSTEM GEOMETRY                  FORM 2    ***********************
C                                                                       
C***** INPUT FORMS 2A AND 2B                                            
C                                                                       
      IF (NOSYS) 4240,4240,2250                                         
2250  WRITE (OUT,10) CHAR(12)
      WRITE (OUT,650)                                                   
C---- INITIALIZATION                                                    
      DO 2260 ISCTX=1,LMSCTX                                            
2260  TABL14(ISCTX)=0                                                   
      DO 2270 NODEX=1,LMNODX                                            
2270  TABL19(NODEX)=0                                                   
      DO 2280 NODE=1,LMNODE                                             
2280  TABL16(NODE)=0                                                    
      ISEG=0                                                            
      IVS=0                                                             
      NODSEQ=0                                                          
      I1=1                                                              
      WRITE (OUT,660)                                                   
C                                                                       
C---- LOOP OVER SECTIONS, READING DATA DESCRIBING EACH SECTION        
      DO 2515 ISCT=1,NSEC                                               
      IF (ISCT-NLSEC-1) 2300,2290,2300                                  
2290  WRITE (OUT,670)                                                   
      I1=2                                                              
2300  READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5                        
      ISCTX=DUMY1                                                       
      NUMY2=DUMY2                                                       
      NUMY3=DUMY3                                                       
      GO TO (2310,2330), I1                                             
C---- LINE SECTION                                                      
2310  ISEGL=ISEG+1                                                      
      TABL9(ISCT)=ISEGL                                                 
      ISEG=ISEG+INT(DUMY4)                                             
      NUMY4=INT(DUMY4)                                                 
      TABL10(ISCT)=ISEG                                                 
      DO 2320 ILS=ISEGL,ISEG                                            
2320  TABL8(ILS)=ISCT                                                   
      QS(ISCT)=DUMY5                                                    
      WRITE (OUT,680) ISCTX,NUMY2,NUMY3,NUMY4,QS(ISCT)                  
      CALL CHECKI (NUMY4,1,NLS,160)                                     
      GO TO 2340                                                        
C---- VENTILATION SHAFT SECTION                                         
2330  QS(ISCT)=DUMY4                                                    
C---- STORE NEGATIVE VENT SHAFT NUMBER                                  
      IVS=IVS-1                                                         
      TABL9(ISCT)=IVS                                                   
      TABL10(ISCT)=IVS                                                  
      WRITE (OUT,690) ISCTX,NUMY2,NUMY3,QS(ISCT)                        
2340  CALL CHECKR (DUMY5,-10000000.0,+10000000.0,174)                   
      QS(ISCT)=QS(ISCT)/60.0                                            
      QSAVES(ISCT)=QS(ISCT)                                             
      CALL CHECKI (ISCTX,1,LMSCTX,153)                                  
      CALL CHECKI (NUMY2,1,LMNODX,154)                                  
      CALL CHECKI (NUMY3,1,LMNODX,154)                                  
      IF (NUMY2-NUMY3) 2360,2350,2360                                   
2350  CALL EERROR (155)                                                  
2360  IF (NEW .EQ. 1) GOTO 5640                                       
C---- CHECK IF SECTION I.D. NUMBER HAS BEEN USED PREVIOUSLY             
      I2=ISCT-1                                                         
      IF (I2) 2410,2410,2380                                            
2380  DO 2400 I=1,I2                                                    
      IF (TABL11(I)-ISCTX) 2400,2390,2400                               
2390  CALL EERROR (156)                                                  
      GO TO 5640                                                        
2400  CONTINUE                                                          
2410  TABL14(ISCTX)=ISCT                                                
      TABL11(ISCT)=ISCTX                                                
C---- LOCATE STARTING NODE IN TABLE OF NODES                            
      IF (NODSEQ) 2440,2440,2420                                        
2420  DO 2430 I=1,NODSEQ                                                
      IF (TABL15(I)-NUMY2) 2430,2450,2430                               
2430  CONTINUE                                                          
C  -- CREATE NEW NODE                                                   
2440  NODSEQ=NODSEQ+1                                                   
      TABL15(NODSEQ)=NUMY2                                              
      TABL19(NUMY2)=NODSEQ                                              
C---- LOCATE ENDING NODE IN TABLE OF NODES                              
2450  DO 2460 I=1,NODSEQ                                                
      IF (TABL15(I)-NUMY3) 2460,2470,2460                               
2460  CONTINUE                                                          
C  -- CREATE NEW NODE                                                   
      NODSEQ=NODSEQ+1                                                   
      TABL15(NODSEQ)=NUMY3                                              
      TABL19(NUMY3)=NODSEQ                                              
2470  NODEL=TABL19(NUMY2)                                               
      NODEH=TABL19(NUMY3)                                               
C---- STORE STARTING AND ENDING NODE NUMBERS WITH SECTION               
      TABL12(ISCT)=NODEL                                                
      TABL13(ISCT)=NODEH                                                
C---- ADD SECTION TO THOSE CONNECTED TO NODE ( STARTING AND ENDING )    
      TABL16(NODEL)=TABL16(NODEL)+1                                     
      NUMY4=TABL16(NODEL)                                               
      IF (NUMY4-LMSCND) 2480,2480,2500                                  
C  -- SECTION STARTS AT THIS NODE - STORE SECTION NUMBER AS NEGATIVE    
2480  TABL17(NODEL,NUMY4)=-ISCT                                         
      TABL16(NODEH)=TABL16(NODEH)+1                                     
      NUMY5=TABL16(NODEH)                                               
      IF (NUMY5-LMSCND) 2490,2490,2500                                  
C  -- SECTION ENDS AT THIS NODE - STORE SECTION NUMBER AS POSITIVE      
2490  TABL17(NODEH,NUMY5)=ISCT                                          
      GO TO 2510                                                        
C---- MORE THAN 'LMSCND' CONNECTED TO THIS NODE                         
2500  CALL EERROR (157)                                                  
      GO TO 5640                                                        
C---- INITIALIZE BUOYANCY IN SECTIONS                                   
2510  BUOYS(ISCT) = 0.0                                                 
2515  CONTINUE                                                          
C                                                                       
C---- CHECK IF NUMBER OF NODES FOUND AGREES WITH NUMBER INPUT           
      IF (NODSEQ-NNODE) 2520,2530,2520                                  
2520  WRITE (OUT,700) NODSEQ                                            
      CALL EERROR (158)                                                  
C---- CHECK IF NUMBER OF LINE SEGMENTS AGREES WITH INPUT                
2530  IF (ISEG-NLS) 2540,2550,2540                                      
2540  WRITE (OUT,710) ISEG                                              
      CALL EERROR (161)                                                  
2550  IF (NEW .EQ. 1) GOTO 5640                                            
C                                                                       
C---- DUMP GEOMETRY TABLES                                              
      GO TO (2650,2570,2570,2570,2570,2570,2650,2650), SUPOPT           
2570  WRITE (OUT,10) CHAR(12)
      WRITE (OUT,720)                                                   
      DO 2580 I=1,NSEC                                                  
2580  WRITE (OUT,730) I,TABL11(I),TABL12(I),TABL13(I)                   
      WRITE (OUT,740)                                                   
      DO 2610 I=1,NNODE                                                 
      J=TABL16(I)                                                       
      IF (J) 2590,2590,2600                                             
2590  WRITE (OUT,750) I,TABL15(I),TABL16(I)                             
      GO TO 2610                                                        
2600  WRITE (OUT,750) I,TABL15(I),TABL16(I),(TABL17(I,K),K=1,J)         
2610  CONTINUE                                                          
      WRITE (OUT,755)                                                   
      WRITE (OUT,760)                                                   
      DO 2640 I=1,LMNODX                                                
      IF (TABL14(I)) 2630,2620,2630                                     
2620  IF (TABL19(I)) 2630,2640,2630                                     
2630  WRITE (OUT,730) I,TABL14(I),TABL19(I)                             
2640  CONTINUE                                                          
C                                                                       
C---- DETERMINE FLOW LOOPS THROUGH SYSTEM                               
C                                                                       
2650  CALL HOOKUP                                                       
C*****IF AN UNHAPPY HOOKUP HAS OCCURRED, ABORT INPUT PHASE OF THIS      
C*****SIMULATION                                                        
      IF (NEW .EQ. 1) GOTO 5640                                            
C                                                                       
C*****CHECK INITIAL AIRFLOW CONDITIONS INPUT BY USER                    
C                                                                       
C*****LOOP OVER ALL NODES IN SYSTEM                                     
      DO 2710 INODE=1,NNODE                                             
C*****FIND NUMBER OF SECTIONS(BRANCHES) THAT MEET AT THE NODE           
      NBR=TABL16(INODE)                                                 
C*****CHECK IF NUMBER OF SECTIONS MEETING AT THE NODE IS GREATER THAN   
C*****ONE OR NOT                                                        
      IF (NBR-1) 2710,2710,2670                                         
C*****FOR NODES HAVING MORE THAN ONE SECTION MEETING AT THEM COMPUTE    
C*****FLOW ENTERING NODE                                                
2670  DUMY1=0.0                                                         
      DO 2680 IBR=1,NBR                                                 
      ISCT=TABL17(INODE,IBR)                                            
      IISCT=IABS(ISCT)                                                  
2680  DUMY1=DUMY1+ISCT*QS(IISCT)/IISCT                                  
C*****IF THE NET FLOW ENTERING THE NODE IS ESSENTIALLY NON-ZERO THEN THE
C*****INPUT HAS VIOLATED CONTINUITY                                     
      IF (ABS(DUMY1)-0.1) 2710,2690,2690                                
2690  CALL EERROR (175)                                                  
      WRITE (OUT,770) TABL15(INODE)                                     
C*****AS A DEFAULT, SET INITIAL FLOWS EQUAL TO ZERO                     
      DO 2700 ILOOP=1,NLOOP                                             
      QERRLP(ILOOP)=0.0                                                 
      DQDTLP(ILOOP)=0.0                                                 
2700  QLP(ILOOP)=0.0                                                    
      GO TO 2890                                                        
2710  CONTINUE                                                          
C                                                                       
C*****INITIALIZATION OF LOOP FLOWS FROM SECTION FLOWS                   
C                                                                       
C*****FIRST, INITIALIZE LOOP FLOWS TO 1000000 CUBIC FEET PER SECOND AND 
C*****SET THEIR TIME DERIVATIVES AND AERO ACCUMULATED ERRORS TO ZERO    
      DO 2720 ILOOP=1,NLOOP                                             
      QERRLP(ILOOP)=0.0                                                 
      DQDTLP(ILOOP)=0.0                                                 
2720  QLP(ILOOP)=1000000.0                                              
C*****FOR ALL SECTIONS HAVING ONLY ONE LOOP PASSING THROUGH THEM SET THE
C*****INITIAL LOOP FLOW EQUAL TO THE INITIAL SECTION FLOW, SIGNING THE  
C*****LOOP FLOW IN ACCORDANCE WITH THE DIRECTION OF THE LOOP THROUGH THE
C*****SECTION                                                           
      DO 2740 ISCT=1,NSEC                                               
      NUMY1=NDSS(ISCT)                                                  
      NUMY2=NDSS(ISCT+1)-1                                              
      IF (NUMY1-NUMY2) 2740,2730,2740                                   
2730  ILP1=LKAN(NUMY1)                                                  
      IILP1=IABS(ILP1)                                                  
      QLP(IILP1)=ILP1*QS(ISCT)/IILP1                                    
2740  CONTINUE                                                          
C*****BEGINNING OF ITERATIVE TYPE PROCEDURE TO INITIALIZE LOOP FLOWS NOT
C*****YET INITIALIZED                                                   
C*****SET ITERATION COUNTER EQUAL TO ZERO                               
      NUMY3=0                                                           
C*****CHECK IF ALL LOOP FLOWS HAVE BEEN INITIALIZED                     
2750  DO 2760 ILOOP=1,NLOOP                                             
      IF (999999.0-ABS(QLP(ILOOP))) 2770,2760,2760                      
2760  CONTINUE                                                          
C*****LOOP FLOW INITIALIZATION COMPLETE                                 
      GO TO 2890                                                        
C*****CHECK FOR FAILURE OF ITERATION PROCEDURE, ASSUMING THAT IT        
C*****NEVER REQUIRES MORE THAN KBND1+1 ITERATIONS                       
2770  IF (KBND1+1-NUMY3) 2780,2790,2790                                 
2780  WRITE (OUT,780)                                                   
      GO TO 5650                                                        
C*****BEGINNING OF NEXT ITERATION - INCREMENT ITERATION COUNTER BY ONE  
2790  NUMY3=NUMY3+1                                                     
C*****LOOP OVER ALL SECTIONS IN SYSTEM                                  
      DO 2880 ISCT=1,NSEC                                               
      NUMY1=NDSS(ISCT)                                                  
      NUMY2=NDSS(ISCT+1)-1                                              
C*****IF SECTION HAS ONLY ONE LOOP PASSING THROUGH IT, THEN THAT LOOP   
C*****FLOW HAS BEEN INITIALIZED AND NO FURTHER INFORMATION MAY BE       
C*****OBTAINED FROM THIS INITIAL SECTION FLOW                           
      IF (NUMY1-NUMY2) 2800,2880,5650                                   
C*****IF MORE THAN ONE LOOP PASSES THROUGH THIS SECTION, LOOP OVER      
C*****LOOPS PASSING THROUGH IT, SEARCHING FOR ONE WHOSE FLOW HAS        
C*****NOT YET BEEN INITIALIZED                                          
2800  DO 2810 NUMY4=NUMY1,NUMY2                                         
      ILP1=LKAN(NUMY4)                                                  
      IILP1=IABS(ILP1)                                                  
      IF (999999.0-ABS(QLP(IILP1))) 2820,2810,2810                      
2810  CONTINUE                                                          
C*****LOOP FLOWS HAVE BEEN INITIALIZED FOR ALL LOOPS PASSING THROUGH    
C*****THIS SECTION, THUS, NO FURTHER INFORMATION MAY BE OBTAINED FROM   
C*****ITS INITIAL FLOW                                                  
      GO TO 2880                                                        
C*****AT LEAST ONE LOOP PASSING THROUGH THIS SECTION HAS NOT BEEN       
2820  NUMY4=NUMY4+1                                                     
      IF (NUMY4-NUMY2) 2830,2830,2850                                   
C*****LOOP OVER REMAINING LOOPS PASSING THROUGH THIS SECTION (IF ANY)   
C*****CHECKING TO SEE IF ANY OF THEM HAVE NOT YET BEEN INITIALIZED      
2830  DO 2840 NUMY5=NUMY4,NUMY2                                         
      NUMY6=LKAN(NUMY5)                                                 
      IILP2=IABS(NUMY6)                                                 
C*****IF ANOTHER UNINITIALIZED LOOP PASSES THROUGH THIS SECTION THEN    
C*****NO FURTHER USE CAN BE MADE OF THIS SECTION INITIAL FLOW AT        
C*****THIS TIME                                                         
      IF (999999.0-ABS(QLP(IILP2))) 2880,2880,2840                      
2840  CONTINUE                                                          
C*****INITIALIZE THE ONE UNINITIALIZED LOOP PASSING THROUGH THIS SECTION
C*****BY TAKING THE SECTION INITIAL FLOW AND SUBTRACTING THE OTHER LOOP 
C*****FLOWS FROM IT                                                     
2850  QLP(IILP1)=ILP1*QS(ISCT)/IILP1                                    
      DO 2870 NUMY4=NUMY1,NUMY2                                         
      ILP2=LKAN(NUMY4)                                                  
      IILP2=IABS(ILP2)                                                  
      IF (IILP1-IILP2) 2860,2870,2860                                   
2860  QLP(IILP1)=QLP(IILP1)-ILP2*ILP1*QLP(IILP2)/(IILP2*IILP1)          
2870  CONTINUE                                                          
2880  CONTINUE                                                          
C*****END OF THIS ITERATION                                             
      GO TO 2750                                                        
C*****INITIALIZATION OF LOOP FLOWS COMPLETE. RECOMPUTE SECTION FLOWS    
C*****FROM LOOP FLOWS TO ELIMINATE ANY MINOR DISCREPANCIES (ONE CFM OR  
C*****LESS) IN CONTINUITY AT THE NODES. THIS ALLOWS THE INPUT OF        
C*****SECTION FLOWS TO THE NEAREST CFM.                                 
2890  KAERO=0                                                           
      CALL QSQLP                                                        
C*****OPTIONAL PRINTOUT OF INITIALIZED LOOP FLOWS AND RECOMPUTED SECTION
C*****FLOWS                                                             
      GO TO (2920,2900,2900,2900,2900,2900,2920,2920), SUPOPT           
2900  WRITE (OUT,790)                                                   
      DO 2910 ISCT=1,NSEC                                               
2910  BLP(ISCT)=QS(ISCT)*60.0                                           
      WRITE (OUT,800) (BLP(ISCT),ISCT=1,NSEC)                           
      WRITE (OUT,805)                                                   
      WRITE (OUT,810) (QLP(ILP),ILP=1,NLOOP)                            
C
C********************************************************************
C**** BEGIN PROCESSING SERIES 3 INPUT FORMS.  SET EXITCODE TO 3
 2920 EXITCODE=3
C********************************************************************   
C                                                                       
C
C     LINE SEGMENTS                    FORM 3    ***********************
C                                                                       
      IF (NLS) 5650,2970,2930                                           
C---- INITIALIZE COUNTER FOR FIRE SEGMENTS                              
2930  ITALLY =0                                                         
      DO 2940 ILS=1,NLS                                                 
C---- LINE SEGMENT INPUT SUBROUTINE ( LSINS )                           
      WRITE (OUT,10) CHAR(12)
      CALL LSINS (ILS)                                                  
      IF (NEW .EQ. 1)  GO TO 5640                                       
C---- COUNT THE NUMBER OF FIRE SEGMENTS. (THE TYPE OF A LINE SEGMENT    
C     ON FIRE HAS ALREADY BEEN CHANGED IN LSINS TO A NEGATIVE.)         
      IF (TYPLS(ILS) .LT. 0)  ITALLY=ITALLY+1                           
2940  CONTINUE                                                          
      IF (FIROPT .EQ. 0)    GO TO 2945                                  
      IF (ITALLY .GT. 0)    GO TO 2945                                  
C---- THERE IS NO FIRE SEGMENT WHILE A FIRE IS BEING SIMULATED          
      CALL EERROR (248)                                                  
      GO TO 5640                                                        
2945  CONTINUE                                                          
      IF (NLSEC) 2970,2970,2950                                         
2950  DO 2960 ISCT=1,NLSEC                                              
      NLOW=TABL9(ISCT)                                                  
      NHIGH=TABL10(ISCT)                                                
      BCS(ISCT)=0.0                                                     
      CEPS(ISCT)=0.0                                                    
      CENS(ISCT)=0.0                                                    
      DO 2960 ILS=NLOW,NHIGH                                            
      CEPS(ISCT)=CEPS(ISCT)+(CBPLS(ILS)+CFPLS(ILS))/(4.0*ALS(ILS)**2)   
      CENS(ISCT)=CENS(ISCT)+(CBNLS(ILS)+CFNLS(ILS))/(4.0*ALS(ILS)**2)   
2960  BCS(ISCT)=BCS(ISCT)+LLS(ILS)/ALS(ILS)                             
C
C********************************************************************
C     BEGIN PROCESSING SERIES 4 FORMS.  SET EXITCODE TO 4
 2970 EXITCODE=4
C********************************************************************
C
C     FIRE/UNSTEADY HEAT SOURCES         FORM 4   **********************
C                                                                       
      IF (NUMUL) 5650,3070,2980                                         
2980  DO 3060 IUL=1,NUMUL                                               
C---- PRINT THREE PER PAGE                                              
      IF ((IUL-1)-(((IUL-1)/3)*3)) 3000,2990,3000                       
2990  WRITE (OUT,10) CHAR(12)
3000  READ (IN,40) TITLE,DUMY1,DUMY2                        
      WRITE (OUT,830) IUL,TITLE                             
      ISEGX=DUMY1                                                       
      ILSS=DUMY2                                                        
C---- FIND INTERNAL SEGMENT NUMBER                                      
      DO 3010 ISEG=1,LMLSEG                                             
      IF (ISEGX-NUMLS(ISEG)) 3010,3020,3010                             
3010  CONTINUE                                                          
      ISCTX=0                                                           
      WRITE (OUT,840) ISCTX,ISEGX,ILSS                                  
      CALL EERROR (138)                                                  
      ISEG=1                                                            
      GO TO 3030                                                        
3020  ISCT=TABL8(ISEG)                                                  
      WRITE (OUT,840) TABL11(ISCT),ISEGX,ILSS                           
3030  NUMY1=TABL4(ISEG)-TABL3(ISEG)+1                                   
      CALL CHECKI (ILSS,1,NUMY1,139)                                    
      LSSUL(IUL)=TABL3(ISEG)+ILSS-1                                     
      READ(IN,30) DUMY1,DUMY2,DUMY3,DUMY4,TFLMUL(IUL),AFIRUL(IUL)       
      WRITE (OUT,850) DUMY1                                             
      WRITE (OUT,860) DUMY2                                             
      HLSUL(IUL)=DUMY1/3600.0                                           
      HLLUL(IUL)=DUMY2/3600.0                                           
      WRITE (OUT,870) DUMY3                                             
      WRITE (OUT,880) DUMY4                                             
      IF (DUMY4-DUMY3) 3040,3050,3050                                   
3040  CALL EERROR (140)                                                  
3050  LTONUL(IUL)=(DUMY3*100.0)+1                                       
      IF( LTONUL(IUL) .EQ. 1 ) LTONUL(IUL) = -1                         
      LTOFUL(IUL)=DUMY4*100.0                                           
C**** IF A FIRE IS BEING SIMULATED, WRITE OUT AND CHECK THE INPUT DATA  
      IF( FIROPT .LE. 0  .OR.  TYPLS(ISEG) .GT. 0 ) GO TO 3060          
      WRITE(OUT,881)  TFLMUL(IUL)                                       
      CALL CHECKR (TFLMUL(IUL),500.0,2000.0,249)                        
C**** STORE TEMPERATURE IN DEGREES RANKINE                              
      TFLMUL(IUL) = TFLMUL(IUL) + ABTEMP                                
      WRITE(OUT,882)  AFIRUL(IUL)                                       
      CALL CHECKR (AFIRUL(IUL),0.0,SURLSS(ISEG),250)                    
      AFIRUL(IUL) = AFIRUL(IUL)*STEFAN/3600.0                           
      ATXX4 = AFIRUL(IUL) * TFLMUL(IUL) ** 4                            
      IF( ATXX4 .GE. HLSUL(IUL) )  CALL EERROR(262)                      
3060  CONTINUE                                                          
C---- INITIALIZE THROTTLING EFFECT OF FIRE/UNSTEADY HEAT SOURCES             
      DO 3061 I=1, LMUL                                                 
3061  THROTL(I) = 0.0                                                   
C                                                                       
C---- CHECK IF TWO FIRE/UNSTEADY STATE HEAT SOURCES ARE IN ONE LINE          
C     SUBSEGMENT WHILE THEIR ACTIVE TIMES ARE OVERLAPPED.               
      IF (FIROPT .EQ. 0)  GO TO 3068                                    
      IF (NUMUL .EQ. 1)   GO TO 3068                                    
      DO 3066 IUL1=1, NUMUL                                             
      ISS1 = LSSUL(IUL1)                                                
      IULA = IUL1 + 1                                                   
C---- CHECK IF IULA REACHES END OF THE LOOP                             
      IF (IULA .GT. NUMUL)  GO TO 3066                                  
      DO 3064 IUL2=IULA, NUMUL                                          
C---- CHECK IF TWO FIRE/UNSTEADY HEAT SOURCES ARE IN ONE SUBSEGMENT          
      ISS2 = LSSUL(IUL2)                                                
      IF (ISS1 .NE. ISS2)  GO TO 3064                                   
C---- TWO FIRE/UNSTEADY STATE HEAT SOURCES ARE IN ONE LINE SUBSEGMENT,       
C     CHECK IF THEIR ACTIVE TIMES ARE OVERLAPPED                        
      IF (LTONUL(IUL1) .LT. LTOFUL(IUL2))  GO TO 3064                   
      IF (LTONUL(IUL2) .LT. LTOFUL(IUL1))  GO TO 3064                   
      WRITE (OUT,883)  IUL1, IUL2                                       
      CALL EERROR(260)                                                   
3064  CONTINUE                                                          
3066  CONTINUE                                                          
3068  CONTINUE
C                                                         
C********************************************************************
C     BEGIN PROCESSING SERIES 5 FORMS.  SET EXITCODE TO 5
 3070 EXITCODE=5
C********************************************************************
C                                                                       
C      VENTILATION SHAFTS               FORM 5     *********************
C                                                                       
C---- SET TOTAL NUMBER OF SUBSEGMENTS                                   
      TNSS=TNLSS                                                        
C                                                                       
C---- READ VENT SHAFT DATA                                              
      IF (NVSEC) 5650,3100,3080                                         
3080  DO 3090 IVS=1,NVSEC                                               
C---- VENTILATION SHAFT INPUT SUBROUTINE ( VSINS )                      
      WRITE (OUT,10) CHAR(12)
      CALL VSINS (IVS)                                                  
3090  CONTINUE                                                          
C                                                                       
C---- CHECK IF TOTAL SUBSEGMENTS HAS BEEN EXCEEDED                      
3100  CALL CHECKI (TNSS,1,LMSS,43)                                      
      IF (NEW .EQ. 1) GOTO 5640                                            
C                                                                       
C---- COMPLETE 'TABL18'  (SUBSEGMENTS CONNECTED TO NODE)                
      IF (NNODE) 5650,3200,3120                                         
C---- LOOP OVER NODES                                                   
3120  DO 3190 NODE=1,NNODE                                              
C---- LOOP OVER SECTIONS ATTACHED TO NODE                               
      JSEC=TABL16(NODE)                                                 
      DO 3190 KSEC=1,JSEC                                               
      ISCT=TABL17(NODE,KSEC)                                            
      IF (ISCT) 3160,5650,3130                                          
C---- SECTION ENDS (FORWARD END) AT THIS NODE                           
3130  ISEG=TABL10(ISCT)                                                 
      IF (ISEG) 3150,5650,3140                                          
C---- LINE SEGMENT                                                      
3140  ISS=TABL4(ISEG)                                                   
      GO TO 3190                                                        
C---- VENTILATION SHAFT SEGMENT                                         
3150  ISS=TABL6(-ISEG)                                                  
      GO TO 3190                                                        
C---- SECTION BEGINS (BACKWARD END) AT THIS NODE                        
3160  ISEG=TABL9(-ISCT)                                                 
      IF (ISEG) 3180,5650,3170                                          
C---- LINE SEGMENT                                                      
3170  ISS=TABL3(ISEG)                                                   
      GO TO 3190                                                        
C---- VENTILATION SHAFT SEGMENT                                         
3180  ISS=TABL5(-ISEG)                                                  
3190  TABL18(NODE,KSEC)=ISS                                             
3200  CONTINUE                                                          
C                                                                       
C---- DUMP GEOMETRY TABLE - SUBSEGMENTS ADJACENT TO NODES               
      IF ( SUPOPT - 1 ) 3230,3230,3210                                  
3210  WRITE (OUT,10)CHAR(12)
      WRITE (OUT,890)                                                   
      DO 3220 I=1,NNODE                                                 
      J=TABL16(I)                                                       
3220  WRITE (OUT,900) I,TABL15(I),TABL16(I),(TABL18(I,K),K=1,J)         
C
C********************************************************************
C     BEGIN PROCESSING SERIES 6 FORMS.  SET EXITCODE TO 6
 3230 EXITCODE=6
C********************************************************************
C                                                                       
C***** NODE DATA ********************* FORM 6 **************************
C                                                                       
C*****INITIALIZE NUMBER OF THERMODYNAMIC SUBNODES, PAGE CONTROL,        
C*****THERMODYNAMIC NODE TYPE, AND TABL23                               
      NTNODE=0                                                          
      KLINES=0                                                          
      DO 3240 INODE=1,LMNODE                                            
      TABL23(INODE)=1                                                   
3240  TYPTND(INODE)=0                                                   
      TABL23(LMNODE+1)=1                                                
C*****LOOP OVER NNODE NODES                                             
      DO 4060 I=1,NNODE                                                 
C*****READ NODE EXTERNAL NUMBER, NODE AERODYNAMIC(JUNCTION) TYPE, AND   
C*****NODE THERMODYNAMIC TYPE          FORM 6A                          
      READ (IN,30) DUMY1,DUMY2,DUMY3                                    
      NODEX=DUMY1                                                       
      NDTYPA=DUMY2                                                      
      ITYPT=DUMY3                                                       
C*****START NEW PAGE IF PAGE CONTROL EQUALS ZERO                        
      IF (KLINES) 3260,3250,3260
3250  WRITE (OUT,10) CHAR(12)      
      WRITE (OUT,920)                                                   
      GO TO 3270                                                        
3260  WRITE (OUT,910)                                                   
C*****CHANGE PAGE CONTROL FROM ZERO TO ONE OR FROM ONE TO ZERO - CAUSES 
C*****NEW PAGE EVERY OTHER NODE                                         
3270  KLINES=1-KLINES                                                   
C*****WRITE EXTERNAL NODE NUMBER AND CHECK IT AGAINST EXTERNAL  NODE    
C*****NUMBER LIMITS                                                     
      WRITE (OUT,930) NODEX                                             
      CALL CHECKI (NODEX,1,LMNODX,154)                                  
      IF (NEW .EQ. 1) GOTO 5640                                            
C*****COMPUTE INTERNAL NODE NUMBER FROM EXTERNAL NODE NUMBER            
      INODE=TABL19(NODEX)                                               
C*****IF COMPUTED INTERNAL NODE NUMBER IS ZERO THEN A WRONG EXTERNAL    
C*****NODE NUMBER HAS BEEN ENTERED - I.E., ONE THAT HAS NOT BEEN        
C*****PREVIOUSLY DEFINED BY GEOMETRY INPUT                              
      IF (INODE) 3290,3290,3300                                         
3290  CALL EERROR (165)                                                  
      GO TO 4060                                                        
C*****WRITE OUT NODE AERODYNAMIC(JUNCTION) TYPE AND CHECK FOR EXISTANCE 
3300  TITLE='NODE AERODYNAMIC TYPE'                                                           
      IF (NDTYPA*(7-NDTYPA)) 3310,3320,3320                             
3310  WRITE (OUT,940)TITLE,NDTYPA
      CALL EERROR (177)                                                  
      GO TO 4060                                                        
C*****FIND NUMBER OF BRANCHES AT THE NODE                               
3320  NBR=TABL16(INODE)                                                 
C*****COMPUTED GO TO FOR EACH NODE AERODYNAMIC(JUNCTION) TYPE           
      NUMY1=NDTYPA+1                                                    
      GO TO (3330,3350,3360,3370,3380,3390,3400,3410), NUMY1            
3330  WRITE (OUT,950)TITLE,NDTYPA
C*****CHECK IF NODE IS INDEED A PORTAL OR STRAIGHT-THROUGH NODE         
      IF (2-NBR) 3340,3440,3440                                         
3340  CALL EERROR (177)                                                  
      GO TO 4060                                                        
C*****WRITE OUT NAME OF NODE AERODYNAMIC(JUNCTION) TYPE AND COMPUTE     
C*****THE NUMBER OF BRANCHES THAT IT REQUIRES (FOR EACH JUNCTION TYPE)  
C*****AERODYNAMIC JUNCTION TYPE 1 - TUNNEL-TO-TUNNEL CROSSOVER          
3350  WRITE (OUT,960)TITLE, NDTYPA
      NBRREQ=4                                                          
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 2 - DIVIDING WALL TERMINATION           
3360  WRITE (OUT,970)TITLE, NDTYPA                                                     
      NBRREQ=3                                                          
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 3 - 'T' JUNCTION                        
3370  WRITE (OUT,980)TITLE, NDTYPA                                                     
      NBRREQ=3                                                          
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 4 - ANGLED JUNCTION                     
3380  WRITE (OUT,990)TITLE, NDTYPA                                                    
      NBRREQ=3                                                          
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 5 - 'Y' JUNCTION                        
3390  WRITE (OUT,1000)TITLE, NDTYPA                                                    
      NBRREQ=3                                                          
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 6 - RESERVED - SEE USERS + PROG. MANUALS
3400  WRITE (OUT,1010)TITLE, NDTYPA                                                    
      NBRREQ=NBR                                                        
      GO TO 3420                                                        
C*****AERODYNAMIC JUNCTION TYPE 7 - ZERO TOTAL PRESSURE CHANGE          
3410  WRITE (OUT,1020)TITLE, NDTYPA                                                    
      NBRREQ=NBR                                                        
C*****CHECK IF THE NUMBER OF BRANCHES REQUIRED BY THE NODE              
C*****AERODYNAMIC(JUNCTION) TYPE AND THE NUMBER THAT IT HAS             
C*****ARE THE SAME                                                      
3420  IF (NBRREQ-NBR) 3430,3440,3430                                    
3430  CALL EERROR (177)                                                  
      GO TO 4060                                                        
C*****WRITING AND CHECKING OF THERMODYNAMIC NODE TYPE                   
3440  IF (ITYPT) 3470,3480,3450                                         
3450  IF (ITYPT-3) 3460,3460,3470                                       
3460  IF (ITYPT-2) 3490,3500,3510                                       
3470  WRITE (OUT,1030) ITYPT                                            
      CALL EERROR (178)                                                  
      ITYPT=1                                                           
      GO TO 3520                                                        
3480  ITYPT=1                                                           
3490  WRITE (OUT,1030) ITYPT                                            
      GO TO 3520                                                        
3500  WRITE (OUT,1040) ITYPT                                            
      GO TO 3520                                                        
3510  WRITE (OUT,1050) ITYPT                                            
3520  CONTINUE                                                          
C*****INPUT FOR AERODYNAMIC NODES THAT ARE MULTIPLY-BRANCHED JUNCTIONS  
      GO TO (3540,3530,3530,3530,3530,3530,3540,3540,3530), NUMY1       
3530  CALL JINS                                                         
C                                                                       
C---- NODE THERMODYNAMIC INFORMATION                                    
C                                                                       
3540  NTNODE=NTNODE+1                                                   
      CALL CHECKI (NTNODE,1,LMTHND,179)                                 
      TABL22(INODE)=NTNODE-1                                            
      KSCT=TABL16(INODE)                                                
      IF (ITYPT-2) 3640,3710,3550                                       
C                                                                       
C---- FORM 6B                                                           
C                                                                       
3550  READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6                  
C                                                                       
C---- THERMO EXT. TYPE 3 NODE -  TEMP/HUMID B.C.                        
      IF (ABS(DUMY1+DUMY2)-0.01) 3560,3560,3570                         
C---- AMBIENT CONDITIONS                                                
3560  TDBTN(NTNODE)=TDBAMB                                              
      HUMTN(NTNODE)=HUMAMB                                              
      WRITE (OUT,1060) TDBTN(NTNODE)                                    
      WRITE (OUT,1070) TWBAMB                                           
      WRITE (OUT,1080) HUMTN(NTNODE)                                    
      TYPTND(INODE)=5                                                   
      GO TO 3575                                                        
C---- USER-SUPPLIED CONDITIONS                                          
3570  TDBTN(NTNODE)=DUMY1                                               
      WRITE (OUT,1060) TDBTN(NTNODE)                                    
      CALL CHECKR (DUMY1,-50.0,140.0,7)                                 
      WRITE (OUT,1070) DUMY2                                            
      CALL CHECKR (DUMY2,-50.0,DUMY1,8)                                 
      CALL LATENT (DUMY1,DUMY2,HUMTN(NTNODE))                           
      WRITE (OUT,1080) HUMTN(NTNODE)                                    
      TYPTND(INODE)=4                                                   
C --  CHECK THAT NODE IS PORTAL OR OPENING TO ATMOSPHERE                
3575  IF (TABL16(INODE)-1) 5650,3590,3580                               
3580  CALL EERROR (166)                                                  
C---- THERMO TYPE 3 NODES WITH HEAT SINK OPTION NO. 1                   
C --  STORE MORNING AND EVENING B.C. TEMP AND SES B.C. TEMP IN 'EMTBTN' 
C --  AMD HUMIDITY RATIOS IN 'EMHBTN'                                   
3590  IF (HCOPT) 3670,3670,3600                                         
3600  IF (ABS(DUMY3+DUMY4+DUMY5+DUMY6)-0.1) 3610,3610,3620              
C---- AMBIENT CONDITIONS                                                
3610  EMTBTN(NTNODE,3)=TAMBA                                            
      EMTBTN(NTNODE,4)=TAMBM                                            
      EMHBTN(NTNODE,3)=HMAMBM                                           
      EMHBTN(NTNODE,4)=HMAMBE                                           
      DUMY4=TWAMBM                                                      
      DUMY6=TWAMBE                                                      
      GO TO 3630                                                        
C---- USER SUPPLIED CONDITIONS                                          
3620  EMTBTN(NTNODE,3)=DUMY3                                            
      CALL LATENT (DUMY3,DUMY4,EMHBTN(NTNODE,3))                        
      EMTBTN(NTNODE,4)=DUMY5                                            
      CALL LATENT (DUMY5,DUMY6,EMHBTN(NTNODE,4))                        
3630  EMTBTN(NTNODE,5)=TDBTN(NTNODE)                                    
      EMHBTN(NTNODE,5)=HUMTN(NTNODE)                                    
      WRITE (OUT,1090) EMTBTN(NTNODE,3)                                 
      CALL CHECKR (EMTBTN(NTNODE,3),-50.0,140.0,7)                      
      WRITE (OUT,1100) DUMY4                                            
      CALL CHECKR (DUMY4,-50.0,EMTBTN(NTNODE,3),8)                      
      WRITE (OUT,1110) EMHBTN(NTNODE,3)                                 
      WRITE (OUT,1120) EMTBTN(NTNODE,4)                                 
      CALL CHECKR (EMTBTN(NTNODE,4),-50.0,140.0,7)                      
      WRITE (OUT,1130) DUMY6                                            
      CALL CHECKR (DUMY6,-50.0,EMTBTN(NTNODE,4),8)                      
      WRITE (OUT,1110) EMHBTN(NTNODE,4)                                 
      GO TO 3670                                                        
C                                                                       
C---- THERMO EXT. TYPE 1 NODE - MIXING NODE                             
3640  IF (KSCT-2) 3650,3660,3660                                        
3650  CALL EERROR (184)                                                  
C --  THERMO INT. TYPE 1 - MIXING NODE WITH 2 OR MORE BRANCHES          
3660  TYPTND(INODE)=1                                                   
C---- LOOP OVER SECTIONS ATTACHED TO NODE, STORING THERMO NODE AND      
C---- BRANCH WITH SECTION                                               
3670  DO 3700 J=1,KSCT                                                  
      NUMY1=TABL17(INODE,J)                                             
      ISCT=IABS(NUMY1)                                                  
      ISIGN=ISCT/NUMY1                                                  
      IF (ISIGN) 3680,3690,3690                                         
C --  BACKWARD END IS ATTACHED TO NODE                                  
3680  TABL20(ISCT)=NTNODE*64+J                                          
      GO TO 3700                                                        
C --  FORWARD END IS ATTACHED TO NODE                                   
3690  TABL21(ISCT)=NTNODE*64+J                                          
3700  CONTINUE                                                          
      GO TO 4060                                                        
C                                                                       
C---- THERMO EXT. TYPE 2 NODE - NON-MIXING NODE          FORM 6A        
3710  READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7            
C---- CHECK THAT 4 OR 5 SECTIONS ARE CONNECTED TO NODE                  
      IF (KSCT-4) 3720,3730,3730                                        
3720  CALL EERROR (198)                                                  
C---- THERMAL NODE 'A'                                                  
3730  NUMMY1(1)=DUMY1                                                   
      NUMMY1(2)=DUMY2                                                   
      NUMMY1(3)=DUMY3                                                   
      IF (NUMMY1(2)) 3750,3740,3750                                     
3740  K=1                                                               
      GO TO 3780                                                        
3750  IF (NUMMY1(3)) 3770,3760,3770                                     
3760  K=2                                                               
      GO TO 3780                                                        
3770  K=3                                                               
3780  WRITE (OUT,1140) (NUMMY1(J),J=1,K)                                
      NBRCHT=0                                                          
      NTNODE=NTNODE+2                                                   
      CALL CHECKI (NTNODE,1,LMTHND,179)                                 
      DO 3840 J=1,K                                                     
      ISCTX=NUMMY1(J)                                                   
      CALL CHECKI (ISCTX,1,LMSCTX,153)                                  
      IF (NEW .EQ. 1) GOTO 5640                                            
      ISCT=TABL14(ISCTX)                                                
C --  CHECK IF SECTION IS ATTACHED TO NODE                              
      DO 3800 J1=1,KSCT                                                 
      NUMY1=TABL17(INODE,J1)                                            
      IF(ISCT-IABS(NUMY1)) 3800,3810,3800                               
3800  CONTINUE                                                          
      CALL EERROR (181)                                                  
      GO TO 3840                                                        
3810  ISIGN=TABL17(INODE,J1)/ISCT                                       
      IF (ISIGN) 3820,3830,3830                                         
C --  BACKWARD END OF SECTION IS ATTACHED TO NODE                       
3820  TABL20(ISCT)=(TABL22(INODE)+1)*64+J                               
      GO TO 3840                                                        
C --  FORWARD END OF SECTION IS ATTACHED TO NODE                        
3830  TABL21(ISCT)=(TABL22(INODE)+1)*64+J                               
3840  NBRCHT=NBRCHT+1                                                   
C---- THERMAL NODE 'B'                                                  
      ISCTX=DUMY4                                                       
      IF (ISCTX) 3860,3850,3860                                         
3850  WRITE (OUT,1150)                                                  
      TYPTND(INODE)=2                                                   
      GO TO 3930                                                        
3860  WRITE (OUT,1150) ISCTX                                            
      TYPTND(INODE)=3                                                   
      CALL CHECKI (ISCTX,1,LMSCTX,153)                                  
      IF (NEW .EQ. 1) GOTO 5640                                            
      ISCT=TABL14(ISCTX)                                                
C --  CHECK IF SECTION IS ATTACHED TO NODE                              
      DO 3880 J1=1,KSCT                                                 
      NUMY1=TABL17(INODE,J1)                                            
      IF (ISCT-IABS(NUMY1)) 3880,3890,3880                              
3880  CONTINUE                                                          
      CALL EERROR (181)                                                  
      GO TO 3930                                                        
3890  ISIGN=TABL17(INODE,J1)/ISCT                                       
      IF (ISIGN) 3900,3910,3910                                         
C --  BACKWARD END OF SECTION IS ATTACHED TO NODE                       
3900  TABL20(ISCT)=(TABL22(INODE)+2)*64+1                               
      GO TO 3920                                                        
C --  FORWARD END OF SECTION IS ATTACHED TO NODE                        
3910  TABL21(ISCT)=(TABL22(INODE)+2)*64+1                               
3920  NBRCHT=NBRCHT+1                                                   
C---- THERMAL NODE 'C'                                                  
3930  NUMMY1(5)=DUMY5                                                   
      NUMMY1(6)=DUMY6                                                   
      NUMMY1(7)=DUMY7                                                   
      IF (NUMMY1(6)) 3950,3940,3950                                     
3940  K=1                                                               
      GO TO 3980                                                        
3950  IF (NUMMY1(7)) 3970,3960,3970                                     
3960  K=2                                                               
      GO TO 3980                                                        
3970  K=3                                                               
3980  K2=4+K                                                            
      WRITE (OUT,1160) (NUMMY1(K1),K1=5,K2)                             
      DO 4040 J=1,K                                                     
      ISCTX=NUMMY1(J+4)                                                 
      CALL CHECKI (ISCTX,1,LMSCTX,153)                                  
      IF (NEW .EQ. 1) GOTO 5640                                            
      ISCT=TABL14(ISCTX)                                                
C --  CHECK IF SECTION IS ATTACHED TO NODE                              
      DO 4000 J1=1,KSCT                                                 
      NUMY1=TABL17(INODE,J1)                                            
      IF (ISCT-IABS(NUMY1)) 4000,4010,4000                              
4000  CONTINUE                                                          
      CALL EERROR (181)                                                  
      GO TO 4040                                                        
4010  ISIGN=TABL17(INODE,J1)/ISCT                                       
      IF (ISIGN) 4020,4030,4030                                         
C --  BACKWARD END OF SECTION IS ATTACHED TO NODE                       
4020  TABL20(ISCT)=(TABL22(INODE)+3)*64+J                               
      GO TO 4040                                                        
C --  FORWARD END OF SECTION IS ATTACHED TO NODE                        
4030  TABL21(ISCT)=(TABL22(INODE)+3)*64+J                               
4040  NBRCHT=NBRCHT+1                                                   
      IF (NBRCHT-KSCT) 4050,4060,4050                                   
4050  CALL EERROR (182)                                                  
4060  CONTINUE                                                          
C---- CHECK THAT ALL NODES HAVE BEEN DEFINED                            
      DO 4080 INODE=1,NNODE                                             
      IF (TYPTND(INODE)) 4070,4070,4080                                 
4070  WRITE (OUT,1170) TABL15(INODE)                                    
      CALL EERROR (183)                                                  
4080  CONTINUE                                                          
      GO TO (4190,4090,4090,4090,4090,4090,4190,4190), SUPOPT           
C*****DUMP OF TABLES RELATED TO AERO NODE PHENOMENA                     
4090  WRITE(OUT,10)CHAR(12)      
      WRITE (OUT,1180)                                                  
C*****DUMP TABL16 AND TABL17                                            
      DO 4100 INODE=1,NNODE                                             
      NBR=TABL16(INODE)                                                 
      WRITE (OUT,1200) INODE,TABL15(INODE),NBR,(TABL17(INODE,IX),IX=1,NB
     1R)                                                                
4100  WRITE (OUT,1205) (TABL18(INODE,IX),IX=1,NBR)                      
      WRITE (OUT,1190)                                                  
      DO 4104 ISCT=1,NSEC                                               
      INOTH  = TABL20(ISCT) / 64                                        
      IBRCH  = TABL20(ISCT) -  INOTH*64                                 
      NNOTH  = TABL21(ISCT) / 64                                        
      NBRCH  = TABL21(ISCT) -  NNOTH*64                                 
4104  WRITE (OUT,1320)  ISCT,TABL11(ISCT),TABL20(ISCT), INOTH, IBRCH,   
     1                  TABL21(ISCT), NNOTH, NBRCH                      
C*****FIRST DUMP OF ARRAY TABL1                                         
      NHIGHS=NNODE+1                                                    
      WRITE (OUT,1210)                                                  
      WRITE (OUT,1220) (TABL1(IX),IX=1,NHIGHS)                          
C*****FIRST DUMP OF ARRAY TABL2                                         
      WRITE (OUT,1230)                                                  
      NHIGHS=TABL1(NHIGHS)                                              
      WRITE (OUT,1220) (TABL2(IX),IX=1,NHIGHS)                          
C*****COMBINED DUMP OF TABL1 AND TABL2                                  
      WRITE (OUT,1240)                                                  
      DO 4130 INODE=1,NNODE                                             
      NLOWS=TABL1(INODE)                                                
      NHIGHS=TABL1(INODE+1)-1                                           
      IF (NHIGHS-NLOWS) 4110,4120,4120                                  
4110  WRITE (OUT,1250) NLOWS,NHIGHS                                     
      GO TO 4130                                                        
4120  WRITE (OUT,1250) NLOWS,NHIGHS,(TABL2(IX),IX=NLOWS,NHIGHS)         
4130  CONTINUE                                                          
C*****FIRST DUMP OF TABL23                                              
      WRITE (OUT,1260)                                                  
      NHIGHS=NNODE+1                                                    
      WRITE (OUT,1220) (TABL23(IX),IX=1,NHIGHS)                         
C*****FIRST DUMP OF COAEND                                              
      WRITE (OUT,1270)                                                  
      NHIGHS=TABL23(NHIGHS)                                             
      WRITE (OUT,1280) (COAEND(IX),IX=1,NHIGHS)                         
C*****COMBINED DUMP OF TABL23 AND COAEND                                
      WRITE (OUT,1290)                                                  
      DO 4160 INODE=1,NNODE                                             
      NLOWS=TABL23(INODE)                                               
      NHIGHS=TABL23(INODE+1)-1                                          
      IF (NHIGHS-NLOWS) 4140,4150,4150                                  
4140  WRITE (OUT,1300) NLOWS,NHIGHS                                     
      GO TO 4160                                                        
4150  WRITE (OUT,1300) NLOWS,NHIGHS,(COAEND(IX),IX=NLOWS,NHIGHS)        
4160  CONTINUE                                                          
C*****DUMP OF TABLES RELATED TO THERMO NODE PHENOMENA                   
4190  IF (NEW .EQ. 1) GOTO 5640
C
C********************************************************************
C     BEGIN PROCESSING SERIES 7 FORMS.  SET EXITCODE TO 7
      EXITCODE=7
C********************************************************************   
C
C     FAN OPERATING CHARACTERISTICS    FORM 7  *************************
C                                                                       
      IF (NFNTYP) 5650,4206,4202                                        
4202  DO 4204 IFT=1,NFNTYP                                              
      WRITE (OUT,10) CHAR(12)
C*****INPUT SUBROUTINE FOR ORDINARY FANS  ( FINS )                      
      CALL FINS (IFT)                                                   
4204  CONTINUE
C------------------------------ FORM 7C --------------------------------
4206  IF (NIFTYP) 5650,4212,4208                                        
4208  WRITE (OUT,10) CHAR(12)       
      WRITE (OUT,1312)                                                  
C*****READ, CHECK AND STORE IMPULSE FAN DATA                            
      DO 4210 IFS=1,NIFTYP                                              
      ITYPLS=IFS + 8                                                    
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5                        
      WRITE (OUT,1313) IFS,ITYPLS                                       
      WRITE (OUT,1314) DUMY1                                            
      WRITE (OUT,1315) DUMY2                                            
      WRITE (OUT,1316) DUMY3                                            
      WRITE (OUT,1317) DUMY4                                            
      WRITE (OUT,1318) DUMY5                                            
      CALL CHECKR (DUMY1,0.,1000000.,218)                               
      CALL CHECKR (DUMY2,0.1,1.0,219)                                   
      CALL CHECKR (ABS(DUMY3),0.,10000.,220)                            
      CALL CHECKR (DUMY4,0.,10000.,221)                                 
      CALL CHECKR ((DUMY5-DUMY4),0.,10000.,221)                         
      QIFS(IFS)=DUMY1*DUMY2/60.                                         
      VIFS(IFS)=DUMY3/60.                                               
      TONIFS(IFS)=DUMY4*100.+1.0                                        
4210  TOFIFS(IFS)=DUMY5*100.0                                           
4212  CONTINUE                                                          
C
C********************************************************************
C     BEGIN PROCESSING SERIES 8 FORMS.  SET EXITCODE TO 8
 4240 EXITCODE=8
C********************************************************************   
C
C     TRAIN ROUTE DESCRIPTION          FORM 8    ***********************
C                                                                       
      IF (NTRRTE) 5650,4270,4250                                        
4250  DO 4260 IROUTE=1,NTRRTE                                           
      WRITE (OUT,10) CHAR(12)
      CALL TRINS (IROUTE)                                               
C                                                                       
C---- DUMP TRACK SECTION ARRAYS                                         
C     WRITE (OUT,2530)                                                  
C2530 FORMAT (1H1,11X,'FSTS',5X,'RADTS',5X,'GRDTS',5X,'UMXTS',5X,'DWLTS'
C    1,4X,'NPETTS',4X,'NSEGTS'//)                                       
C     NTS=NTSRT(IROUTE)                                                 
C     WRITE (OUT,2540) (ITS,FSTS(ITS,IROUTE),RADTS(ITS,IROUTE),GRDTS(ITS
C    1,IROUTE),UMXTS(ITS,IROUTE),DWLTS(ITS,IROUTE),NPETTS(ITS,IROUTE),NS
C    2EGTS(ITS,IROUTE),ITS=1,NTS)                                       
C2540 FORMAT (I6,F10.2,F10.0,F10.5,F10.2,3I10)                          
C                                                                       
      IF (NEW .EQ. 1) GOTO 5640                                            
4260  CONTINUE                                                          
C
C********************************************************************
C     BEGIN PROCESSING SERIES 9 FORMS.  SET EXITCODE TO 9
 4270 EXITCODE=9
C********************************************************************   
C
C     TRAIN PHYSICAL CHARACTERISTICS    FORM 9    **********************
C                                                                       
      IF (NTRTYP) 5650,4640,4280                                        
4280  DO 4290 ITYP=1,NTRTYP                                             
      WRITE (OUT,10) CHAR(12)
      CALL GARAGE (ITYP)                                                
      IF (NEW .EQ. 1) GOTO 5640                                            
4290  CONTINUE                                                          
C
C********************************************************************
C     BEGIN PROCESSING SERIES 10 FORMS.  SET EXITCODE TO 10
      EXITCODE=10
C********************************************************************   
C
C     TRAIN INITIALIZATION DATA         FORM 10  ***********************
C                                                                       
C --  INITIALIZE TRAIN INDEX ARRAY                                      
      DO 4300 I=1,LMTRAN                                                
4300  IINDEX(I)=I                                                        
C --  INITIALIZE THE NUMBER OF THE LAST TRAIN DISPATCHED INTO THE SYSTEM
      NUMBTR=0                                                          
      IF (NTRAN) 4640,4640,4310                                         
4310  WRITE (OUT,10) CHAR(12)       
      WRITE (OUT,1337)                                                  
      WRITE (OUT,1340) NTRAN                                            
C                                                                       
C---- READ LOCATION, SPEED, ROUTE AND TYPE OF TRAINS IN OPERATION       
C---- AT BEGINING OF SIMULATION                                         
      WRITE (OUT,1350)                                                  
      DO 4630 ITRAN=1,NTRAN                                             
      NUMV=IINDEX(ITRAN)                                                 
      READ (IN,30) XV(NUMV),UV(NUMV),DUMY1,DUMY2,TGACCV(NUMV),TGDECV(NUM
     1V),DUMY4,COAST                                                    
      KCOAST=COAST                                                      
      UV(NUMV)=UV(NUMV)*88./60.                                         
      RPS(NUMV) = 0.0                                                   
      IROTEV(NUMV)=DUMY1                                                
      IROUTE=IROTEV(NUMV)                                               
      ITYPV(NUMV)=DUMY2                                                 
      ITYP=ITYPV(NUMV)                                                  
      NUMBTR=NUMBTR+1                                                   
      CALL ALCHAR (NUMBTR,TNUMV(NUMV))                                  
      GO TO (4420,4320,4320), TPOPT                                     
C---- LOCATE TRAIN ON SPEED-TIME PROFILE         TPOPT =2,3             
4320  NEXP=NEXPDP(IROUTE)                                               
C --  INTEGRATE SPEED-TIME PROFILE UNTIL TRAIN IS LOCATED               
      DUMY1=ORIGRT(IROUTE)                                              
      DO 4340 I=2,NEXP                                                  
      DUMY2=DUMY1+(SPEEDV(I,IROUTE)+SPEEDV(I-1,IROUTE))*(TIMEV(I,IROUTE)
     1-TIMEV(I-1,IROUTE))/(2.*100.0)                                    
      IF (SNGL( XV(NUMV))-DUMY2) 4350,4350,4330                         
4330  DUMY1=DUMY2                                                       
4340  CONTINUE                                                          
C---- TRAIN IS NOT LOCATED WITHIN LIMITS OF SPEED-TIME PROFILE          
C --  THIS WILL CAUSE ERROR 144 TO BE WRITTEN                           
      TLPSV(NUMV)=0                                                     
      MODEV(NUMV)=1                                                     
      DPV(NUMV)=1                                                       
      XLASTV(NUMV)=SNGL( XV(NUMV))                                      
      DUDTV(NUMV)=0.0                                                   
      DGRDQV(NUMV)=0.0                                                  
      GO TO 4460                                                        
C---- TRAIN IS LOCATED BETWEEN POINTS I-1 AND I                         
4350  MODEV(NUMV)=1                                                     
      DPV(NUMV)=I-1                                                     
      XLASTV(NUMV)=DUMY1                                                
      DTIME=(TIMEV(I,IROUTE)-TIMEV(I-1,IROUTE))/100.0                   
      DUDTV(NUMV)=(SPEEDV(I,IROUTE)-SPEEDV(I-1,IROUTE))/DTIME           
      DGRDQV(NUMV)=(QGRIDV(I,IROUTE)-QGRIDV(I-1,IROUTE))/DTIME          
      IF (ABS(DUDTV(NUMV))-0.005) 4370,4370,4360                        
4360  DUMY3=(-SPEEDV(I-1,IROUTE)+SQRT(SPEEDV(I-1,IROUTE)**2-2.0*DUDTV(NU
     1MV)*(DUMY1-SNGL( XV(NUMV)))))/DUDTV(NUMV)                         
      GO TO 4380                                                        
4370  DUMY3=(SNGL( XV(NUMV))-DUMY1)/SPEEDV(I-1,IROUTE)                  
4380  TLPSV(NUMV)=TIMEV(I-1,IROUTE)+DUMY3*100.0                         
      UV(NUMV)=SPEEDV(I-1,IROUTE)+DUDTV(NUMV)*(TLPSV(NUMV)-TIMEV(I-1,IRO
     1UTE))/100.0                                                       
      QGRID=QGRIDV(I-1,IROUTE)+DGRDQV(NUMV)*(TLPSV(NUMV)-TIMEV(I-1,IROUT
     1E))/100.0                                                         
      IF (QGRID) 4400,4390,4390                                         
4390  QACCV(NUMV)=QGRID                                                 
      QDECV(NUMV)=0.0                                                   
      GO TO 4410                                                        
4400  QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=-QGRID                                                
4410  WPATV(NUMV)=NPETTS(1,IROUTE)*PATWHT                               
C                                              TPOPT = 1,2,3            
4420  U=UV(NUMV)*60./88.                                                
      ZERO=0.0                                                          
      IF (ABS(TGACCV(NUMV))-0.1) 4430,4440,4440                         
4430  TGACCV(NUMV)=TDBAMB                                               
4440  IF (ABS(TGDECV(NUMV))-0.1) 4450,4460,4460                         
4450  TGDECV(NUMV)=TDBAMB                                               
4460  IF (TPOPT .NE. 1)  GO TO 4490                                     
C                                                                       
C---- TPOPT=1, CHECK IF TRAIN IS MOVING                                 
      IF (U .LT. 0.01)  GO TO 4490                                      
C                                                                       
C---- TPOPT = 1 AND TRAIN IS MOVING, CHECK IF IT IS COASTING            
      IF (KCOAST .NE. 1)  GO TO 4480                                    
      WRITE (OUT,1361) TNUMV(NUMV),XV(NUMV),U,IROTEV(NUMV),ITYPV(NUMV), 
     1                 TGACCV(NUMV),TGDECV(NUMV),ZERO                   
      GO TO 4500                                                        
4480  WRITE (OUT,1362) TNUMV(NUMV),XV(NUMV),U,IROTEV(NUMV),ITYPV(NUMV), 
     1                 TGACCV(NUMV),TGDECV(NUMV),ZERO                   
C                                                                       
C---- CHECK IF COASTING INDICATOR IS NEITHER ONE NOR ZERO.              
      IF (KCOAST .EQ. 0)  GO TO 4500                                    
      WRITE (OUT,1363)  KCOAST                                          
      CALL EERROR (258)                                                  
      KCOAST = 0                                                        
      GO TO 4500                                                        
C                                                                       
C---- TPOPT= 2, 3  OR TRAIN IS NOT MOVING.  RESET KCOAST TO ZERO.       
4490  KCOAST = 0                                                        
      WRITE (OUT,1360) TNUMV(NUMV),XV(NUMV),U,IROTEV(NUMV),ITYPV(NUMV), 
     1                 TGACCV(NUMV),TGDECV(NUMV),DUMY4                  
4500  NUMY1=IROTEV(NUMV)                                                
      CALL CHECKI (NUMY1,1,NTRRTE,142)                                  
      NUMY1=ITYPV(NUMV)                                                 
      CALL CHECKI (NUMY1,1,NTRTYP,143)                                  
      NTS=NTSRT(IROUTE)                                                 
      IF (SNGL( XV(NUMV))-ORIGRT(IROUTE)) 4510,4520,4520                
4510  CALL EERROR (18)                                                   
4520  CALL CHECKR (SNGL( XV(NUMV)),FSTS(1,IROUTE),FSTS(NTS,IROUTE),144) 
      CALL CHECKR (UV(NUMV),0.0,150.0,145)                              
C                                                                       
C---- INITIALIZE TRAIN PARAMETERS                                       
      GO TO (4530,4600,4600), TPOPT                                     
C --  COMPUTE NUMBER OF PERSONS ABOARD TRAIN                            
4530  ITOT=0                                                            
      DO 4540 ITS=1,NTS                                                 
      IF (SNGL( XV(NUMV))-FSTS(ITS,IROUTE)) 4550,4540,4540              
4540  ITOT=ITOT+NPETTS(ITS,IROUTE)                                      
4550  WPATV(NUMV)=ITOT*PATWHT                                           
      RSISTV(NUMV)=CORMV(ITYP,2)+(CORMV(ITYP,1)+CORMV(ITYP,3)*UV(NUMV))*
     1(WV(ITYP)+WPATV(NUMV))                                            
      TEV(NUMV)=TORQUE(UV(NUMV),ITYP)                                   
      CALL AMPERE(TEV(NUMV),TEV(NUMV),UV(NUMV),ITYP)                    
      AMPV(NUMV) = IMOTOR                                               
      AMPLV(NUMV) = ILINE                                               
      DUDTV(NUMV)=(TEV(NUMV)*MOTORV(ITYP)-RSISTV(NUMV))/(WV(ITYP)*RRACC(
     1ITYP)+WPATV(NUMV)/GRACC)                                          
      IF (DUDTV(NUMV)-ACCV(ITYP)) 4570,4570,4560                        
4560  DUDTV(NUMV)=ACCV(ITYP)                                            
4570  TLPSV(NUMV)=NDELT                                                 
      MODEV(NUMV)=2                                                     
      QACCV(NUMV)=0.0                                                   
      QDECV(NUMV)=0.0                                                   
C---- CHECK IF TRAIN IS STOPPED WITH DWELL TIME REMAINING               
      IF(UV(NUMV)-0.01) 4580,4591,4591
C---- IF A NEGATIVE VALUE HAS BEEN ENTERED FOR THE REMAINING
C---- DWELL TIME, THE TRAIN IS TO REMAIN IN THE INITIALIZED
C---- LOCATION FOR THE DURATION OF THE SIMULATION.  INTERNALLY
C---- THE DWELL TIME IS SET EQUAL TO IZAP.
4580  IF (DUMY4 .LT. 0.0)THEN
        TSTRTV(NUMV)=IZAP
        GOTO 4590
      END IF
      IF (DUMY4-0.01) 4610,4590,4590                                    
C---- TRAIN IS DWELLING IN STATION                                      
4590  RSISTV(NUMV)=0.0                                                  
      DUDTV(NUMV)=0.0                                                   
      MODEV(NUMV)=0                                                     
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      IF(DUMY4 .GT. 0.0)TSTRTV(NUMV)=DUMY4*100.0
C---- IF A NEGATIVE DWELL TIME IS ENTERED PRINT MESSAGE OTHERWISE
C---- CHECK IF DWELL TIME IS WITHIN LIMITS
      IF (DUMY4 .LT. 0.0)THEN
        WRITE (OUT,1351)
      ELSE
        CALL CHECKR (DUMY4,0.0,900.0,50)                                  
      END IF
      GO TO 4620                                                        
C---- CHECK IF TRAIN IS COASTING                                        
 4591 IF(KCOAST-1)4610,4592,4610                                        
C---- INITIALIZE TRAIN AS COASTING (MODE=5)                             
 4592 MODEV(NUMV)=5                                                     
      TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      GO TO 4610                                                        
4600  TEV(NUMV)=0.0                                                     
      AMPV(NUMV)=0.0                                                    
      AMPLV(NUMV)=0.0                                                   
      WPATV(NUMV) = 0.0                                                 
4610  TSTRTV(NUMV)=0                                                    
4620  NTSV(NUMV)=2                                                      
      NTSV2(NUMV)=2                                                     
      QERDCV(NUMV)=0.0                                                  
      QERACV(NUMV)=0.0                                                  
      DRAGV(NUMV)=0.0                                                   
      RMHTV(NUMV)=0.0                                                   
C---- CHECK IF FLYWHEELS ARE BEING SIMULATED                            
      IF (FLYOPV(ITYP) - 2.0) 4629,4621,4629                            
4621  IF (UV(NUMV) - 0.01) 4622,4622,4623                               
C                                                                       
C---- SET THE FLYWHEEL ROTATIONAL SPEED EQUAL TO THE MAXIMUM VALUE      
4622  RPS(NUMV) = RPSMXV(ITYP)                                          
      GO TO 4624                                                        
C---- SET THE FLYWHEEL ROTATIONAL SPEED EQUAL TO THE MINIMUM VALUE      
4623  RPS(NUMV) = RPSMNV(ITYP)                                          
C---- CALCULATE THE CORRESPONDING KINETIC ENERGY STORED IN THE FLYWHEEL 
4624  FKENEW(NUMV) = (2.0*PI*RPS(NUMV))**2*FLYJV(ITYP)/(GRACC*2.0)      
      POW1(NUMV) = 0.0                                                  
4629  PPROPV(NUMV) = 0.0                                                
      PREGNV(NUMV) = 0.0                                                
      PAUXV(NUMV) = PAUXEV(ITYP)+PAUXPV(ITYP)*WPATV(NUMV)/PATWHT        
      PFLYV(NUMV) = 0.0                                                 
      QPRPV(NUMV) = 0.0                                                 
      QAXSV(NUMV) = SHREV(ITYP)+SHRPV(ITYP)*WPATV(NUMV)/PATWHT          
      QAXLV(NUMV) = LHREV(ITYP)+LHRPV(ITYP)*WPATV(NUMV)/PATWHT          
4630  CALL LOCATE (NUMV,ITYP,IROUTE,RG,RC)                              
C                                                                       
4640  CONTINUE                                                          
C
C********************************************************************
C     BEGIN PROCESSING SERIES 11 FORMS.  SET EXITCODE TO 11
      EXITCODE=11
C********************************************************************   
C
C**** ENVIRONMENTAL CONTROL ZONE DATA             FORM 11   *********** 
C                                                                       
      IF (NZONE -1) 5350,4650,4650                                      
4650  WRITE (OUT,10) CHAR(12)
      WRITE (OUT,1370)                                                  
C---- ZONE INITIALIZATION                                               
      DO 4660 ISS=1,TNSS                                                
4660  TABL44(ISS)=0                                                     
      DO 4670 NODE=1,NNODE                                              
4670  TABL45(NODE)=0                                                    
C                                                                       
C---- LOOP OVER ZONES                                                   
      DO 5150 IZONE=1,NZONE                                             
      WRITE (OUT,1380) IZONE                                            
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6                  
      TYPCL(IZONE)=DUMY1                                                
      IF (TYPCL(IZONE)-1) 4720,4690,4680                                
4680  IF (TYPCL(IZONE)-3) 4700,4710,4720                                
4690  WRITE (OUT,1390) TYPCL(IZONE)                                     
      GO TO 4730                                                        
4700  WRITE (OUT,1400) TYPCL(IZONE)                                     
      GO TO 4730                                                        
4710  WRITE (OUT,1410) TYPCL(IZONE)                                     
      GO TO 4730                                                        
4720  WRITE (OUT,1400) TYPCL(IZONE)                                     
      TYPCL(IZONE)=2                                                    
      CALL EERROR (185)                                                  
4730  JSEG=DUMY2                                                        
      IF (NZONE -1) 5350,4740,4750                                      
4740  JSEG=NLS+NVSEC                                                    
4750  WRITE (OUT,1420) JSEG                                             
      CALL CHECKI (JSEG,1,NLS+NVSEC,204)                                
      NEQDTC=0                                                          
      IF (TYPCL(IZONE)-2) 4760,4770,4770                                
4760  WRITE (OUT,1430) DUMY3                                            
      CALL CHECKR (DUMY3,40.0,100.0,26)                                 
      CALL LATENT (DUMY3,DUMY4,DUMY7)                                   
      CALL  RELHUM (DUMY3,DUMY7,DUMY8)                                  
      WRITE (OUT,1440) DUMY4,DUMY7,DUMY8                                
      CALL CHECKR (DUMY4,40.0,DUMY3,27)                                 
      WRITE (OUT,1450) DUMY5                                            
      CALL CHECKR (DUMY5,40.0,100.0,26)                                 
      CALL LATENT (DUMY5,DUMY6,DUMY9)                                   
      CALL  RELHUM (DUMY5,DUMY9,DUMY10)                                 
      WRITE (OUT,1460) DUMY6,DUMY9,DUMY10                               
      CALL CHECKR (DUMY6,40.0,DUMY5,27)                                 
      GO TO 4780                                                        
4770  DUMY3=0.0                                                         
      DUMY4=0.0                                                         
      DUMY5=0.0                                                         
      DUMY6=0.0                                                         
      DUMY7=0.0                                                         
      DUMY8=0.0                                                         
4780  IF (NZONE -1) 5350,5310,4790                                      
4790  WRITE (OUT,1470)                                                  
C                                                                       
C---- LOOP OVER SEGMENTS IN ZONE                                        
      DO 5140 ISEG=1,JSEG,8                                             
      READ (IN,30) DUMMY1                                               
      J=1                                                               
      DO 4870 I=1,8                                                     
      NUMMY3(I)=DUMMY1(I)                                               
      IF (NUMMY3(I)) 4800,4880,4800                                     
4800  J=I                                                               
C---- FIND SEGMENT OR VENT SHAFT                                        
      DO 4810 ILS=1,NLS                                                 
      IF (NUMLS(ILS)-NUMMY3(I)) 4810,4850,4810                          
4810  CONTINUE                                                          
      IF (NVSEC) 5650,4840,4820                                         
4820  DO 4830 IVS=1,NVSEC                                               
      IF (NUMVS(IVS)-NUMMY3(I)) 4830,4860,4830                          
4830  CONTINUE                                                          
C---- NO LINE SEGMENT OR VENT SHAFT WITH THIS I.D.                      
4840  NUMMY2(I)=0                                                       
      NUMMY4(I)=0                                                       
      NUMMY5(I)=0                                                       
      GO TO 4870                                                        
C---- LINE SEGMENT                                                      
4850  ISCT=TABL8(ILS)                                                   
      NUMMY2(I)=TABL11(ISCT)                                            
      NUMMY4(I)=ISCT                                                    
      NUMMY5(I)=ILS                                                     
      GO TO 4870                                                        
C---- VENTILATION SHAFT                                                 
4860  ISCT=NLSEC+IVS                                                    
      NUMMY2(I)=TABL11(ISCT)                                            
      NUMMY4(I)=ISCT                                                    
      NUMMY5(I)=IVS                                                     
4870  CONTINUE
C**** INITIALIZE VARIABLE DISPSTR
4880  WRITE(DISPSTR,5)
C**** STORE SEGMENT ID NUMBERS IN VARIABLE DISPSTR FOR LATER PRINTING
	DO 4885 I=1,J
	  JJ=1+10*(I-1)
	  KK=JJ+9
	  WRITE(DISPSTR(JJ:KK),1480)NUMMY2(I),NUMMY3(I)
4885  CONTINUE
      WRITE (OUT,1484)DISPSTR                      
C                                                                       
C---- PLACE LINE SEGMENT OR VENT SHAFT IN ZONE                          
      DO 5130 I=1,J                                                     
      ISCT=NUMMY4(I)                                                    
      IF (ISCT) 4890,4890,4900                                          
4890  CALL EERROR (95)                                                   
      GO TO 5130                                                        
4900  ISEGL=TABL9(ISCT)                                                 
      ISEGH=TABL10(ISCT)                                                
      IF (ISEGL) 4930,5650,4910                                         
C---- LINE SEGMENT                                                      
4910  ILS=NUMMY5(I)                                                     
      ISSL=TABL3(ILS)                                                   
      ISSH=TABL4(ILS)                                                   
      ISEGA=ILS                                                         
      DTDMLS(ILS)=DUMY3                                                 
      DTWMLS(ILS)=DUMY4                                                 
      DTHMLS(ILS)=DUMY7                                                 
      DTDELS(ILS)=DUMY5                                                 
      DTWELS(ILS)=DUMY6                                                 
      DTHELS(ILS)=DUMY9                                                 
C---- LINE SEGMENT MUST NOT BE IN TYPE 3 ZONE                           
      IF (TYPCL(IZONE)-2) 4950,4950,4920                                
4920  CALL EERROR (195)                                                  
      GO TO 4950                                                        
C---- VENT SHAFT                                                        
4930  IVS=NUMMY5(I)                                                     
      ISSL=TABL5(IVS)                                                   
      ISSH=TABL6(IVS)                                                   
      ISEGA=-IVS                                                        
C---- VENT SHAFT MUST NOT BE IN TYPE 1 ZONE                             
      IF (TYPCL(IZONE)-2) 4940,4950,4950                                
4940  WRITE (OUT,1490) NUMMY2(I),NUMMY3(I)                              
      CALL EERROR (44)                                                   
C---- CHECK IF SEGMENT IS IN ANY OTHER ZONE                             
4950  IF (TABL44(ISSL)) 5650,4970,4960                                  
4960  WRITE (OUT,1490) NUMMY2(I),NUMMY3(I)                              
      CALL EERROR (96)                                                   
      GO TO 5130                                                        
4970  DO 4980 ISS=ISSL,ISSH                                             
4980  TABL44(ISS)=IZONE                                                 
      NEQDTC=NEQDTC+ISSH-ISSL+1                                         
C---- INCLUDE ADJACENT NODES IN TYPE 2 ZONES                            
      IF (TYPCL(IZONE)-2) 5130,4990,5130                                
C---- CHECK NODE AT BACKWARD END OF SECTION                             
4990  IF (ISEGA-ISEGL) 5060,5000,5060                                   
5000  NODE=TABL12(ISCT)                                                 
C---- CHECK IF NODE HAS BEEN PUT INTO ANOTHER ZONE                      
      IF (TABL45(NODE)) 5650,5030,5010                                  
5010  IF (TABL45(NODE)-IZONE) 5020,5060,5020                            
5020  WRITE (OUT,1500) TABL15(NODE)                                     
      CALL EERROR (97)                                                   
      GO TO 5060                                                        
5030  TABL45(NODE)=IZONE                                                
      ITYPT=TYPTND(NODE)                                                
      GO TO (5050,5040,5040,5050,5050), ITYPT                           
5040  NEQDTC=NEQDTC+2                                                   
5050  NEQDTC=NEQDTC+1                                                   
C---- CHECK IF NODE IS AT END OF SECTION                                
5060  IF (ISEGA-ISEGH) 5130,5070,5130                                   
5070  NODE=TABL13(ISCT)                                                 
C --  CHECK IF NODE HAS BEEN PUT INTO ANOTHER ZONE                      
      IF (TABL45(NODE)) 5650,5100,5080                                  
5080  IF (TABL45(NODE)-IZONE) 5090,5130,5090                            
5090  WRITE (OUT,1500) TABL15(NODE)                                     
      CALL EERROR (97)                                                   
      GO TO 5130                                                        
5100  TABL45(NODE)=IZONE                                                
      ITYPT=TYPTND(NODE)                                                
      GO TO (5120,5110,5110,5120,5120), ITYPT                           
5110  NEQDTC=NEQDTC+2                                                   
5120  NEQDTC=NEQDTC+1                                                   
5130  CONTINUE                                                          
C                                                                       
5140  CONTINUE                                                          
      IF( SUPOPT - 1 ) 5142,5142,5144                                   
 5142 IF( NEQDTC - LMEQRM ) 5150, 5150, 5144                            
 5144 WRITE(OUT,1485) NEQDTC, LMEQRM                                    
      CALL CHECKI (NEQDTC,0,LMEQRM,168)                                 
C                                                                       
5150  CONTINUE                                                          
C                                                                       
C---- CHECK IF ALL LINE SEGMENTS AND VENT SHAFTS ARE IN ZONES           
      DO 5170 ILS=1,NLS                                                 
      ISSL=TABL3(ILS)                                                   
      IF (TABL44(ISSL)) 5160,5160,5170                                  
5160  ISCT=TABL8(ILS)                                                   
      WRITE (OUT,1490) TABL11(ISCT),NUMLS(ILS)                          
      CALL EERROR (98)                                                   
5170  CONTINUE                                                          
      IF (NVSEC) 5650,5210,5180                                         
5180  DO 5200 IVS=1,NVSEC                                               
      ISSL=TABL5(IVS)                                                   
      IF (TABL44(ISSL)) 5650,5190,5200                                  
5190  ISCT=NLSEC+IVS                                                    
      WRITE (OUT,1490) TABL11(ISCT),NUMVS(IVS)                          
      CALL EERROR (98)                                                   
5200  CONTINUE                                                          
C---- CHECK IF ALL NODES ARE IN ZONES                                   
5210  DO 5300 NODE=1,NNODE                                              
      IF (TABL45(NODE)) 5650,5220,5300                                  
C --  CHECK IF PORTAL WITH TYPE 1 OR 3 ZONE ATTACHED                    
5220  IF (TABL16(NODE)-1) 5650,5230,5260                                
C --  PORTAL NODE                                                       
5230  ISS=TABL18(NODE,1)                                                
      IZONE=TABL44(ISS)                                                 
      IF (TYPCL(IZONE)-2) 5240,5250,5240                                
C---- NODE IS PORTAL OR OPENING TO ATMOS. ADJACENT TO TYPE 1 OR 3 ZONE  
5240  TABL45(NODE)=IZONE                                                
      GO TO 5300                                                        
C --  NODE IS ADJACENT TO TYPE 2 ZONE                                   
5250  WRITE (OUT,1500) TABL15(NODE)                                     
      CALL EERROR (114)                                                  
      GO TO 5300                                                        
C---- NODE IS NOT PORTAL - INTERNAL TO SYSTEM                           
C --  CHECK THAT IT IS SURROUNDED BY ONLY CONTROLLED ZONES AND EITHER   
C --  PUT NODE IN SAME ZONE IF SURROUNDED BY SAME ZONE OR PLACE NODE    
C --  IN A NEWLY CREATED UNCONTROLLED ZONE IF SURROUNDED BY DIFFERENT   
C --  CONTROLLED ZONES                                                  
5260  JSEC=TABL16(NODE)                                                 
      ISS=TABL18(NODE,1)                                                
      IZONE=TABL44(ISS)                                                 
      NUMCON=1                                                          
      IF (TYPCL(IZONE)-1) 5290,5270,5290                                
C                                                                       
 5270 DO 5280 J=1,JSEC                                                  
      ISS=TABL18(NODE,J)                                                
      IZONEJ=TABL44(ISS)                                                
      IF(TYPCL(IZONEJ)-1) 5290,5272,5290                                
 5272 IF(IZONE-IZONEJ) 5274,5280,5274                                   
 5274 NUMCON=NUMCON+1                                                   
 5280 CONTINUE                                                          
      IF(NUMCON-1) 5284,5282,5284                                       
C---- ASSIGN NODE TO SURROUNDING CONTROLLED ZONE                        
 5282 TABL45(NODE)=IZONE                                                
      GO TO 5300                                                        
C --  CREATE NEW UNCONTROLLED ZONE AND ASSIGN NODE TO IT                
 5284 NZONE =NZONE +1                                                   
      TYPCL(NZONE )=2                                                   
      TABL45(NODE)=NZONE                                                
      CALL CHECKI(NZONE ,0,LMCLST,100)                                  
      GO TO 5300                                                        
C---- NODE IS NOT SURROUNDED BY CONTROLLED ZONES ON ALL SIDES           
5290  WRITE (OUT,1500) TABL15(NODE)                                     
      CALL EERROR (191)                                                  
C                                                                       
5300  CONTINUE                                                          
      GO TO 5340                                                        
C                                                                       
C---- ONE ZONE - PUT ENTIRE SYSTEM INTO ZONE NO. 1                      
5310  CONTINUE                                                          
C---- VENT SHAFT MUST NOT BE IN TYPE 1 ZONE
      IF (TYPCL(1) .EQ. 1 .AND. NVSEC .NE. 0)THEN
        CALL EERROR (44)
C---- LINE SEGMENT MUST NOT BE IN TYPE 3 ZONE
      ELSE IF (TYPCL(1) .EQ. 3 .AND. NLS .NE. 0)THEN
        CALL EERROR (195)      
C---- STORE MORNING AND EVENING RUSH HOUR DESIGN CONDITIONS
      ELSE
        DO 5315 ILS=1,NLS
          DTDMLS(ILS)=DUMY3                                                 
          DTWMLS(ILS)=DUMY4                                                 
          DTHMLS(ILS)=DUMY7                                                 
          DTDELS(ILS)=DUMY5                                                 
          DTWELS(ILS)=DUMY6                                                 
          DTHELS(ILS)=DUMY9                                                 
5315    CONTINUE
      END IF
      DO 5320 ISS=1,TNSS                                                
5320  TABL44(ISS)=1                                                     
      DO 5330 NODE=1,NNODE                                              
5330  TABL45(NODE)=1                                                    
      IF( SUPOPT - 1 ) 5332, 5332, 5334                                 
 5332 IF( TNSS+NTNODE - LMEQRM ) 5336,5336,5334                         
 5334 I = TNSS + NTNODE                                                 
      WRITE(OUT,1485) I, LMEQRM                                         
 5336 CALL CHECKI( TNSS+NTNODE, 0, LMEQRM, 168 )                        
5340  IF (NEW .EQ. 1) GOTO 5640
C
C********************************************************************
C     BEGIN PROCESSING SERIES 12 FORMS.  SET EXITCODE TO 12
 5350 EXITCODE=12
C********************************************************************   
C
C                                                                       
C**** READ PRINT CONTROL DATA           FORM 12    *********************
C                                                                       
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,1510)                                                  
      READ (IN,30) WNTSUM,DUMY2                                         
      WRITE (OUT,1520) WNTSUM                                           
      CALL CHECKR (WNTSUM,-10.0,10.0,130)                                
      NPGRP=DUMY2                                                       
      WRITE (OUT,1530) NPGRP                                            
      CALL CHECKI (NPGRP,1,LMPRGP,79)                                   
      IF (NEW .EQ. 1) GOTO 5640                                            
      WRITE (OUT,1540)                                                  
      TOTAL=0.0                                                         
      NOSUMY=0                                                          
      DO 5460 I=1,NPGRP                                                 
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6                  
      NPRGR(I)=DUMY1                                                    
      GINTPR(I)=INT((DUMY2+0.009)*100.0)                               
      GRABBP(I)=DUMY3                                                   
      GRSUM(I)=DUMY4                                                    
      NOSUMY=NOSUMY+DUMY4                                               
      GRATAE(I)=DUMY5                                                   
      GRATTH(I)=DUMY6                                                   
      TOTAL=TOTAL+ABS(DUMY1)*DUMY2                                      
      IF ((GRSUM(I))*(4-GRSUM(I))) 5380,5370,5370                       
5370  NUMY1=GRSUM(I)+1                                                  
      GO TO (5380,5390,5400,5410,5420), NUMY1                           
5380  WRITE (OUT,1550) I,NPRGR(I),DUMY2,GRABBP(I),GRSUM(I),GRATAE(I),   
     1GRATTH(I),TOTAL                                                   
      GRSUM(I)=0                                                        
      GO TO 5430                                                        
5390  WRITE (OUT,1560) I,NPRGR(I),DUMY2,GRABBP(I),GRSUM(I),GRATAE(I),   
     1GRATTH(I),TOTAL                                                   
      GO TO 5430                                                        
5400  WRITE (OUT,1570) I,NPRGR(I),DUMY2,GRABBP(I),GRSUM(I),GRATAE(I),   
     1GRATTH(I),TOTAL                                                   
      GO TO 5430                                                        
5410  WRITE (OUT,1580) I,NPRGR(I),DUMY2,GRABBP(I),GRSUM(I),GRATAE(I),   
     1GRATTH(I),TOTAL                                                   
      GO TO 5430                                                        
5420  WRITE (OUT,1590) I,NPRGR(I),DUMY2,GRABBP(I),GRSUM(I),GRATAE(I),   
     1GRATTH(I),TOTAL                                                   
C---- CHECK IF THE ENVIRONMENTAL CONTROL LOAD EVALUATION OPTION IS 0    
C     WHEN SUMMARY OPTION IS 4.  IF YES -- CALL FATAL ERROR             
      IF (HCOPT .EQ. 0) CALL EERROR(246)                                 
C---- CHECK IF FIRE SIMULATION OPTION IS 1 -- IF YES, CALL FATAL ERROR  
      IF (FIROPT .EQ. 1)  CALL EERROR (259)                              
5430  NUMY2=NPRGR(I)                                                    
      CALL CHECKI (NUMY2,-1,+1000,80)                                   
      IF(NUMY2) 5450,5440,5450                                          
5440  CALL EERROR (80)                                                   
5450  CALL CHECKI (IABS(GINTPR(I)),10,360000,81)                        
      NUMY3=GRABBP(I)                                                   
      CALL CHECKI (NUMY3,0,NUMY2,82)                                    
      NUMY4=GRSUM(I)                                                    
      CALL CHECKI (NUMY4,0,4,83)                                        
      CALL CHECKI (GRATAE(I),0,100,147)                                 
      CALL CHECKI (GRATTH(I),0,100,148)                                 
5460  CONTINUE                                                          
      IF (NOSYS) 5640,5480,5470                                         
5470  IF (NOSUMY) 5480,5480,5490                                        
5480  NOSUMY=0                                                          
      GO TO 5492                                                        
5490  NOSUMY=1                                                          
5492  CONTINUE                                                          
      IF (NPGRP .EQ. 1)  GO TO 5500                                     
      ISUMAE=0                                                          
      ISUMTH=0                                                          
      DO 5496 I=2,NPGRP                                                 
      IF (GRATAE(I) .NE. 0)  GO TO 5494                                 
      ISUMAE=ISUMAE+1                                                   
5494  IF (GRATTH(I) .NE. 0)  GO TO 5496                                 
      ISUMTH=ISUMTH+1                                                   
5496  CONTINUE                                                          
      IF (GRATAE(1) .NE. 0)  GO TO 5497                                 
      IF (ISUMAE .NE. (NPGRP-1))  CALL EERROR (229)                      
      GO TO 5498                                                        
5497  IF (ISUMAE .GT. 0)  CALL EERROR (229)                              
5498  IF (GRATTH(1) .NE. 0)  GO TO 5499                                 
      IF (ISUMTH .NE. (NPGRP-1))  CALL EERROR (229)                      
      GO TO 5500                                                        
5499  IF (ISUMTH .GT. 0)  CALL EERROR (229)                              
5500  CONTINUE                                                          
C---- CHECK IF GRATAE AND GRATTH ARE ALLOWED TO CHANGE                  
C     IF SUMMARY OPTION IS 0 (WHEN NOSUMY=0), 1, 3, 4 ----- YES         
C     IF SUMMARY OPTION IS 0 (WHEN NOSUMY=1), 2 ----------- NO          
      IF (NOSUMY .EQ. 0)  GO TO 5508                                    
      IF (NPGRP .EQ. 1)  GO TO 5508                                     
      DO 5506 I=2, NPGRP                                                
      NUMY1 = GRSUM(I-1) + 1                                            
      GO TO (5502,5506,5502,5506,5506), NUMY1                           
5502  IF (GRATAE(I) .NE. GRATAE(I-1))  GO TO 5504                       
      IF (GRATTH(I) .NE. GRATTH(I-1))  GO TO 5504                       
      GO TO 5506                                                        
5504  WRITE (OUT,1614)  I                                               
      CALL EERROR (228)                                                  
5506  CONTINUE                                                          
      IF (NEW .EQ. 1) GOTO 5640                                            
5508  CONTINUE                                                          
C
C********************************************************************
C     BEGIN PROCESSING SERIES 13 FORMS.  SET EXITCODE TO 13
      EXITCODE=13
C********************************************************************   
C
C************** PROGRAM CONTROLS ***** FORM 13 *************************
C                                                                       
      WRITE (OUT,10) CHAR(12)       
      READ (IN,30) DUMY1,DUMY2,DUMY3,DUMY4                              
      NDELT=DUMY1                                                       
      MAXTIM=DUMY2*100.0                                                
      JRATTR=DUMY3                                                      
      NTWALL = DUMY4                                                    
      DUMY1=DUMY1/100.0                                                 
      WRITE (OUT,1600) DUMY1,DUMY2,JRATTR,NTWALL                        
C---- CHECK IF NDELT OR MAXTIM ARE ZERO OR NEGATIVE                     
      IF (NDELT*MAXTIM) 5510,5510,5520                                  
5510  CALL EERROR (113)                                                  
      GO TO 5640                                                        
C---- IF TRAIN PERFORMANCE OPTION IS 1, JRATTR SHOULD NOT BE ZERO       
5520  IF (TPOPT-1) 5540,5530,5540                                       
5530  CALL CHECKI (JRATTR,1,100,146)                                    
5540  CALL CHECKI (MAXTIM,0,INT(TOTAL*100.0)+1,84)                     
      DELT=FLOAT(NDELT)/100.0                                           
      DELTR=FLOAT(JRATTR*NDELT)/100.0                                   
      JRATTR=JRATTR*NDELT                                               
C---- INITIALIZE AERO AND THERMO INTEGRATION INCREMENTS                 
      JRATAE = GRATAE(1) * NDELT                                        
      DELAE = FLOAT(JRATAE)/50.0                                        
      JRATTH = GRATTH(1) * NDELT                                        
      DELTH = FLOAT(JRATTH)/50.0                                        
      DEL2TH = DELTH/2.0                                                
      TIEMPO = DEL2TH * FLOAT(NTWALL)                                   
      FIROP2 = FIROPT * THOPT * JRATTH                                  
      IF( NTWALL .LT. 0 )  CALL EERROR(261)                              
      IF( FIROP2 .LE. 0 ) GO TO 5542                                    
      IF( NTWALL .EQ. 0 )  WRITE(OUT,5541)                              
C
C********************************************************************
C     BEGIN PROCESSING SERIES 14 FORMS.  SET EXITCODE TO 14
5542      EXITCODE=14
C********************************************************************   
C
C---- READ INITIALIZATION DATA FILE                                     
C                                                                       
      IF (RSTROPT .EQ. 0 )  GO TO 5545                                   
      WRITE (OUT,10) CHAR(12)
C---- CHECK WHETHER THE INPUT FILE CONTAINS JUST THE NAME OF THE 
C---- RESTART FILE OR THE ENTIRE RESTART FILE HAS BEEN ATTACHED TO
C---- THE INPUT FILE (COMPATIBILITY WITH OLDER SES VERSIONS)
      READ (IN,1616)RESTART
      READ(UNIT=IN,FMT=20,ERR=5543)TITLE
      IF (LEN_TRIM(TITLE) .EQ. 0)GOTO 5543
      READ (TITLE,FMT=1618,ERR=5543)NUMY1
C---- RESTART FILE IS ATTACHED TO THE INPUT FILE ( RST=IN )
      RST=IN
C---- CALL SUBROUTINE RSTREAD
5543  CALL RSTREAD
      CLOSE (RST)
C---- RESET VALUE OF RST TO 7
      RST=7                                      
      WRITE (OUT,1612) RSTROPT                                           
      IF (NEW .EQ. 1) GOTO 5640                                            
C
C********************************************************************
C     BEGIN DETAIL PRINT OF INITIAL CONDITIONS.  SET EXITCODE TO 15
5545  EXITCODE=15
C********************************************************************   
C
      CONTINUE                                                          
C                                                                       
C**** INITIALIZATION  -  STAGE 2     ***********************************
C                                                                       
      NTIME=0                                                           
      TRTIME=NDELT                                                      
      KPRNTH=1000000                                                    
      IPRGRP=1                                                          
      IF (THOPT*JRATTH) 5560,5560,5550                                  
5550  CALL PINPNT                                                       
      GO TO 5570                                                        
5560  JRATTH=0                                                          
      FIROP2 = 0                                                        
5570  CONTINUE                                                          
C                                                                       
C     PRINT INITIAL CONDITIONS               ***************************
C                                                                       
5610  IF (THOPT) 5650,5620,5630                                         
5620  WRITE (OUT,10) CHAR(12)
5630  CALL PRINT
C
C********************************************************************
C     INPUT VERIFICATION COMPLETED SUCCESSFULLY.  SET EXITCODE TO 99.
      EXITCODE=99
C********************************************************************   
C
5640  RETURN                                                            
C                                                                       
C*****IMPOSSIBLE SITUATION                                              
C                                                                       
5650  WRITE(OUT,10) CHAR(12)
      WRITE (OUT,1610)                                                  
C                                                                       
C                                                                       
      CALL EXIT (EXITCODE)                                                              
      END                                                               
