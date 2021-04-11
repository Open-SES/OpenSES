      SUBROUTINE ACEST2 (TAIR,HRATIO,DTDXLS,DTHXLS,DTWXLS,NBRNCH)       
C                                                                       
C     THIS SUBROUTINE COMPUTES AN AIR CONDITIONING LOAD ESTIMATE        
C     FOR EACH SUBSEGMENT LOCATED IN A TEMPERATURE CONTROLLED ZONE      
C                                                                       
C DTDXLS(ISEG) IS THE DESIGN DRY BULB TEMPERATURE IN DEG F FOR A        
C        CONTROLLED ZONE. IT IS STORED FOR EACH LINE SEGMENT ISEG       
C        IN THE ZONE.                                                   
C                                                                       
C DTHXLS(ISEG) IS THE DESIGN SPECIFIC HUMIDITY IN DEG F FOR A           
C        CONTROLLED ZONE. IT IS STORED FOR EACH LINE SEGMENT ISEG       
C        IN THE ZONE.                                                   
C                                                                       
C DTWXLS(ISEG) IS THE DESIGN WET BULB TEMPERATURE IN DEG F FOR A        
C        CONTROLLED ZONE. IT IS STORED FOR EACH LINE SEGMENT ISEG       
C        IN THE ZONE.                                                   
C                                                                       
C HRATIO(IZZ) HAS TWO MEANINGS                                          
C        1) FOR VALUES OF IZZ FROM 1 TO TNSS, HRATIO(IZZ) IS THE AVERAGE
C           SUBSEGMENT AIR SPECIFIC HUMIDITY RATIO FOR SUBSEGEMNT IZZ   
C           THAT HAS BEEN COMPUTED BY THE 'DTRH' AS PART OF THE HEAT    
C           SINK - AIR CONDITIONING ITERATION. IT IS USED ONLY FOR      
C           SUBSEGMENTS IN UNCONTROLLED ZONES SINCE THE SUBSEGMENTS IN  
C           CONTROLLED ZONES ARE ASSUMED TO HAVE AVERAGE SPECIFIC       
C           HUMIDITY RATIOS EQUAL TO DESIGN.                            
C        2) FOR VALUES OF IZZ FROM TNSS+1 TO TNSS+NTNODE, HRATIO(IZZ) IS
C           THE AVERAGE MOISTURE LEAVING THERMODYNAMIC NODE IZZ-TNSS (OR
C           NODE+TNSS) IN (FT**3)/SEC THAT HAS BEEN COMPUTED BY THE     
C           'DTRH' AS PART OF THE HEAT SINK - AIR CONDITIONING ITERATION
C           IT IS ONLY USED FOR NODES IN UNCONTROLLED ZONES SINCE ALL   
C           OTHER NODES ARE AT DESIGN SPECIFIC HUMIDITY (THOSE INTERIOR 
C           TO ZONES) OR ARE AT BOUNDARY SPECIFIC HUMIDITIES.           
C                                                                       
C TAIR(IZZ) HAS TWO MEANINGS                                            
C        1) FOR VALUES OF IZZ FROM 1 TO TNSS, TAIR(IZZ) IS THE AVERAGE  
C           SUBSEGMENT AIR TEMPERATURE FOR SUBSEGEMNT IZZ IN DEG F THAT 
C           HAS BEEN COMPUTED BY THE 'DTRM' AS PART OF THE HEAT SINK    
C           - AIR CONDITIONING ITERATION. IT IS USED ONLY FOR           
C           SUBSEGMENTS IN UNCONTROLLED ZONES SINCE THE SUBSEGMENTS IN  
C           CONTROLLED ZONES ARE ASSUMED TO HAVE AVERAGE TEMPERATURES   
C           EQUAL TO DESIGN.                                            
C        2) FOR VALUES OF IZZ FROM TNSS+1 TO TNSS+NTNODE, TAIR(IZZ) IS  
C           THE AVERAGE ENERGY LEAVING THERMODYNAMIC NODE IZZ-TNSS (OR  
C           NODE+TNSS) IN (FT**3)*(DEG F)/SEC THAT HAS BEEN COMPUTED BY 
C           THE 'DTRM' AS PART OF THE HEAT SINK - AIR CONDITIONING      
C           ITERATION. IT IS ONLY USED FOR NODES IN UNCONTROLLED ZONES  
C           SINCE ALL OTHER NODES ARE AT DESIGN TEMPERATURE (THOSE      
C           INTERIOR TO ZONES) OR ARE AT BOUNDARY TEMPERATURES.         
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'DSHRHS'                                                 
C                                                                       
C                                                                       
      DIMENSION  TAIR(LMSSTN), HRATIO(LMSSTN)                           
      DIMENSION  DTHXLS(LMLSEG), DTWXLS(LMLSEG), DTDXLS(LMLSEG)         
C                                                                       
   10 FORMAT(A1)    
   20 FORMAT (T45,'ENVIRONMENTAL CONTROL SYSTEM LOAD ESTIMATES')    
   30 FORMAT (/,T39,'AVERAGED SUBSEGMENT HEAT GAINS(+) OR LOSSES(-), ',
     1              'BTU/HR')                                                           
   40 FORMAT (/,T47,'THE DESIGN PERIOD IS MORNING RUSH HOUR')         
   50 FORMAT (/,T47,'THE DESIGN PERIOD IS EVENING RUSH HOUR')         
   60 FORMAT (/,T37,'THE DESIGN PERIOD IS NEITHER MORNING NOR ',
     1              'EVENING RUSH HOUR')                                                       
   70 FORMAT (/,'----------------  ZONE NUMBER',I3,'  -----  DESIGN ',
     1          'CONDITION  -----',F6.1,' DEG F DRY BULB  -----',F6.1,
     2          ' DEG F WET BULB  ----------------')                                          
   80 FORMAT(/,T4,'SYSTEM', T17,'TRAINS AND', T115,'TOTAL' ,/,       
     1 'PARTITIONING', T16,'MISCELLANEOUS', T36,'STEADY-STATE',      
     2 T52,'HEAT SINK', T68, 'AIR FLOW', T82,'ENVIRONMENTAL CONTROL',   
     3 T107,'ENVIRONMENTAL CONTROL' ,/, T112,'REQUIREMENT' )                                            
   90 FORMAT (/,T15,'SENSIBLE',T25,'LATENT',T33,'SENSIBLE',T43,'LATENT
     1',T52,'SENSIBLE',T63,3('SENSIBLE    LATENT  '),T125,'TOTAL',/)    
110   FORMAT(/,'SENSIBLE',11X,5G14.7,//,'LATENT',13X,5G14.7)       
  120 FORMAT (/,I3,' -',I3,' -',I2,4I9,2I11,5I10,I11)                 
  130 FORMAT (T15,4('-------  '),T53,'-------',T64,6('-------   '),
     1'--------')                                                        
  140 FORMAT (T2,'ZONE TOTAL ',4I9,2I11,5I10,I11)                   
  200 FORMAT ('IMPOSSIBLE SITUATION CREATED IN AIR CONDITIONING ESTIMA
     1TE SUBROUTINE')                                                
C                                                                       
C*****LOOP OVER ALL ZONES IN SYSTEM COMPUTING AND PRINTING TOTALS FOR   
C*****EACH CONTROLLED ZONE ONLY                                         
C                                                                       
      DO 1040 ICLST=1,NZONE                                             
C*****CHECK IF ZONE IS A CONTROLLED ZONE - IF NOT GO TO END OF LOOP     
      IF (TYPCL(ICLST)-1) 1040,210,1040                                 
C*****ZONE IS A CONTROLLED ZONE - WRITE PAGE HEADINGS                   
  210 IPRINT=0                                                          
      WRITE (OUT,10) CHAR(12)
      WRITE (OUT,20)                                                    
      WRITE (OUT,30)                                                    
C*****WRITE DESIGN PERIOD - MORNING, EVENING OR OTHER                   
      GO TO (220,230,240), DESPER                                       
  220 WRITE (OUT,40)                                                    
      GO TO 250                                                         
  230 WRITE (OUT,50)                                                    
      GO TO 250                                                         
  240 WRITE (OUT,60)                                                    
C*****INITIALIZE TO ZERO ZONE TOTALS THAT WILL BE COMPUTED AND PRINTED  
  250 NUMY11=0                                                          
      NUMY12=0                                                          
      NUMY13=0                                                          
      NUMY14=0                                                          
      NUMY15=0                                                          
      NUMY16=0                                                          
      NUMY17=0                                                          
      NUMY18=0                                                          
      NUMY19=0                                                          
      NUMY23=0                                                          
      NUMY24=0                                                          
      NUMY25=0                                                          
C                                                                       
C*****LOOP OVER ALL LINE SECTIONS IN SYSTEM                             
C                                                                       
      DO 855 ISCT=1,NLSEC                                               
C*****FIND LOWEST NUMBERED LINE SEGMENT IN SECTION                      
      ISEGL=TABL9(ISCT)                                                 
C*****FIND HIGHEST NUMBERED LINE SEGMENT IN SECTION                     
      ISEGH=TABL10(ISCT)                                                
C*****LOOP OVER LINE SEGMENTS IN LINE SECTION                           
      DO 850 ISEG=ISEGL,ISEGH                                           
C*****FIND LOWEST NUMBERED SUBSEGMENT IN THIS LINE SEGMENT              
      ISSL=TABL3(ISEG)                                                  
C*****FIND ZONE NUMBER THAT SUBSEGMENT IS IN                            
      ISSCL=TABL44(ISSL)                                                
C*****CHECK IF SUBSEGMENT (AND THEREFORE SEGMENT) IS IN THIS ZONE       
      IF (ICLST-ISSCL) 850,270,850                                      
C*****THIS SEGMENT IS IN THIS ZONE                                      
C*****FIND HIGHEST NUMBERED SUBSEGMENT IN LINE SEGMENT                  
  270 ISSH=TABL4(ISEG)                                                  
C*****FIND THERMODYNAMIC NODE NUMBER OF NODE AT BACK END OF SECTION     
      NODTHB=TABL20(ISCT)/64                                            
C*****FIND NODE BRANCH NUMBER THRU WHICH CONNECTION TO SECTION IS MADE  
      IBRCHB=TABL20(ISCT)-NODTHB*64                                     
C*****FIND THERMODYNAMIC NODE NUMBER OF NODE AT FORWARD END OF SECTION  
      NODTHF=TABL21(ISCT)/64                                            
C*****FIND NODE BRANCH NUMBER THRU WHICH CONNECTION TO SECTION IS MADE  
      IBRCHF=TABL21(ISCT)-NODTHF*64                                     
C*****FIND LOWEST NUMBERED SUBSEGMENT IN THIS LINE SECTION              
      ISSBCL=TABL3(ISEGL)                                               
C*****FIND HIGHEST NUMBERED SUBSEGMENT IN THIS LINE SECTION             
      ISSBCH=TABL4(ISEGH)                                               
C*****PRINT ZONE NUMBER AND ZONE DRY BULB AND WET BULB DESIGN           
C*****TEMPERATURES IF THEY HAVE NOT BEEN PRINTED                        
      IF (IPRINT) 280,280,290                                           
  280 WRITE (OUT,70) ICLST,DTDXLS(ISEG),DTWXLS(ISEG)                    
      WRITE (OUT,80)                                                    
      WRITE (OUT,90)                                                    
      IPRINT=1                                                          
C*****LOOP OVER SUBSEGMENTS IN LINE SEGMENT                             
  290 DO 840 ISS=ISSL,ISSH                                              
C*****FIND CURRENT VALUE OF SENSIBLE A.C. ESTIMATE IN BTU/HR FOR        
C*****SUBSEGMENT                                                        
      DUMY1=SACLSS(ISS)*3600.0                                          
      NUMY1=DUMY1+0.1                                                   
C*****FIND CURRENT VALUE OF LATENT A.C. ESTIMATE IN BTU/HR FOR          
C*****SUBSEGMENT                                                        
      DUMY2=LACLSS(ISS)*3600.0                                          
      NUMY2=DUMY2+0.1                                                   
C*****FIND SENSIBLE STEADY-STATE HEAT GAIN IN BTU/HR EXCLUDING A.C.     
      DUMY3=SHLSS(ISS)*3600.0-DUMY1                                     
      NUMY3=DUMY3+0.1                                                   
C*****FIND LATENT STEADY-STATE HEAT GAIN IN BTU/HR EXCLUDING A.C.       
      DUMY4=LHLSS(ISS)*3600.0-DUMY2                                     
      NUMY4=DUMY4+0.1                                                   
C*****FIND SENSIBLE HEAT GAIN IN BTU/HR CAUSED BY TRAINS, VISCOUS       
C*****HEATING, AND OTHER UNSTEADY HEAT SOURCES                          
      DUMY5=QSUMSS(ISS)*3600.0-DUMY1-DUMY3                              
      NUMY5=DUMY5                                                       
C*****FIND LATENT HEAT GAIN IN BTU/HR CAUSED BY TRAINS AND OTHER        
C*****UNSTEADY SOURCES                                                  
      DUMY6=LSUMSS(ISS)*3600.0*HVH2O-DUMY2-DUMY4                        
      NUMY6=DUMY6                                                       
C*****COMPUTE THE SENSIBLE HEAT GAIN FROM THE HEAT SINK IN BTU/HR       
      DUMY9=HSUMSS(ISS)*SURLSS(ISEG)*(TSFSS(ISS)-DTDXLS(ISEG))*3600.0   
      NUMY9=DUMY9                                                       
C*****SET A.C. ESTIMATES FOR SUBSEGMENT TO ZERO                         
      SACLSS(ISS)=0.                                                    
      LACLSS(ISS)=0.                                                    
C                                                                       
C*****CONVECTIVE LOAD AT 'BACK' OF SUBSEGMENT                           
C                                                                       
C*****CHECK IF SUBSEGMENT IS FIRST IN SECTION                           
      IF (ISS-ISSBCL) 1050,300,510                                      
C*****SUBSEGMENT IS FIRST IN SECTION                                    
C*****FIND AVERAGE FLOW THAT ENTERED BACK OF SUBSEGMENT - (FT**3)/SEC   
  300 FLOBK=FLOBTN(NODTHB,IBRCHB)-FLOBSS(ISS)                           
C*****FIND THE NODE NUMBER AT THE BACK END OF THE SECTION               
      NODB=TABL12(ISCT)                                                 
C*****CHECK IF NODE AT BACK END OF SECTION IS IN THIS CONTROLLED ZONE   
      IF (TABL45(NODB)-ISSCL) 350,310,350                               
C*****NODE IS IN THIS CONTROLLED ZONE - CHECK IF IT'S A BOUNDARY NODE   
  310 IF (TYPTND(NODB)-4) 340,320,320                                   
C*****NODE IS A BOUNDARY NODE - COMPUTE THE AVERAGE SUBSEGMENT          
C*****CONVECTIVE SENSIBLE HEAT GAIN IN (FT**3)*(DEG F)/SEC ADJUSTING FOR
C*****THE DIFFERENCE BETWEEN THE 'HEAT SINK' NODE TEMPERATURES AND THE  
C*****'SES' NODE TEMPERATURES                                           
  320 CVBKS1=EMTBTN(NODTHB,IBRCHB)*EMTBTN(NODTHB,NBRNCH)/EMTBTN(NODTHB,5
     1)                                                                 
C*****COMPUTE THE AVERAGE SUBSEGMENT SENSIBLE HEAT LOSS IN (FT**3)*     
C*****(DEG F)/SEC ADJUSTING FOR THE DIFFERENCE BETWEEN THE SEGMENT      
C*****(ZONE) DESIGN TEMPERATURE AND THE SUBSEGMENT ACTUAL AVERAGE       
C*****TEMPERATURE                                                       
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****COMPUTE THE AVERAGE SUBSEGMENT CONVECTIVE LATENT HEAT GAIN IN     
C*****(FT**3)/SEC ADJUSTING FOR THE DIFFERENCE BETWEEN THE 'HEAT SINK'  
C*****NODE SPECIFIC HUMIDITIES AND THE 'SES' NODE SPECIFIC HUMIDITIES   
      CVBKL1=EMHBTN(NODTHB,IBRCHB)*EMHBTN(NODTHB,NBRNCH)/EMHBTN(NODTHB,5
     1)                                                                 
C*****COMPUTE THE AVERAGE SUBSEGMENT LATENT HEAT LOSS IN (FT**3)/SEC    
C*****ADJUSTING FOR THE DIFFERENCE BETWEEN THE SEGMENT(ZONE) DESIGN     
C*****SPECIFIC HUMIDITY AND THE SUBSEGMENT ACTUAL AVERAGE               
C*****SPECIFIC HUMIDITY                                                 
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
      GO TO 550                                                         
C*****NODE IS INTERIOR TO THE  SYSTEM AND THEREFORE INTERIOR TO THE ZONE
C*****COMPUTE THE AVERAGE SUBSEGMENT CONVECTIVE SENSIBLE HEAT GAIN IN   
C*****(FT**3)*(DEG F)/SEC ADJUSTING FOR THE DIFFERENCE BETWEEN THE      
C*****SEGMENT(ZONE) DESIGN TEMPERATURE AND THE NODE ACTUAL AVERAGE      
C*****TEMPERATURE                                                       
  340 CVBKS1=EMTBTN(NODTHB,IBRCHB)*DTDXLS(ISEG)/TAVGTN(NODTHB)          
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****COMPUTE THE AVERAGE SUBSEGMENT CONVECTIVE LATENT HEAT GAIN IN     
C*****(FT**3)/SEC ADJUSTING FOR THE DIFFERENCE BETWEEN THE SEGMENT(ZONE)
C*****DESIGN SPECIFIC HUMIDITY AND THE NODE ACTUAL AVERAGE SPECIFIC     
C*****HUMIDITY                                                          
      CVBKL1=EMHBTN(NODTHB,IBRCHB)*DTHXLS(ISEG)/HAVGTN(NODTHB)          
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
      GO TO 550                                                         
C*****NODE IS NOT IN ANY CONTROLLED ZONE AND CANNOT BE A BOUNDARY NODE -
C*****THEREFORE IT MUST BE A THERMODYNAMIC INTERNAL TYPE NODE 1,2, OR 3 
C*****COMPUTE THE AVERAGE 'ENERGY' AND 'MOISTURE' LEAVING THE           
C*****THERMODYNAMIC NODE OVER THE SUMMARY INTERVAL                      
  350 SUMT1=0.                                                          
      SUMH1=0.                                                          
      DO 360 IBR=1,LMSCND                                               
      SUMT1=SUMT1+EMTBTN(NODTHB,IBR)                                    
  360 SUMH1=SUMH1+EMHBTN(NODTHB,IBR)                                    
C*****COMPUTE THE AVERAGE SUBSEGMENT CONVECTIVE SENSIBLE HEAT GAIN IN   
C*****(FT**3)*(DEG F)/SEC FROM THE NODE. THIS IS DONE BY TAKING THE     
C*****AVERAGE SENSIBLE HEAT LEAVING ALL THE BRANCHES OF THE NODE AS     
C*****COMPUTED BY THE HEAT SINK AND MULTIPLYING IT BY THE FRACTION OF   
C*****THE AVERAGE 'ENERGY' THAT LEFT THE NODE AND WENT TO THE SUBSEGMENT
C*****AS COMPUTED DURING THE SUMMARY PERIOD.                            
      CVBKS1=EMTBTN(NODTHB,IBRCHB)*TAIR(TNSS+NODTHB)/SUMT1              
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****COMPUTE THE AVERAGE SUBSEGMENT CONVECTIVE LATENT HEAT GAIN IN     
C*****(FT**3)/SEC FROM THE NODE. THIS IS DONE BY TAKING THE AVERAGE     
C*****LATENT HEAT LEAVING ALL THE BRANCHES OF THE NODE AS COMPUTED BY   
C*****THE HEAT SINK AND MULTIPLYING IT BY THE FRACTION OF THE AVERAGE   
C*****'MOISTURE' THAT LEFT THE NODE AND WENT TO THE SUBSEGMENT AS       
C*****DURING THE SUMMARY PERIOD.                                        
      CVBKL1=EMHBTN(NODTHB,IBRCHB)*HRATIO(TNSS+NODTHB)/SUMH1            
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
      GO TO 550                                                         
C*****SUBSEGMENT IS NOT FIRST IN SECTION - FIND AVERAGE FLOW THAT       
C*****ENTERED BACK OF SUBSEGMENT - (FT**3)/SEC                          
  510 FLOBK=FLOFSS(ISS-1)-FLOBSS(ISS)                                   
C*****CHECK IF PREVIOUS SUBSEGMENT IS IN THIS ZONE                      
      ISSCLM=TABL44(ISS-1)                                              
      IF (ISSCL-ISSCLM) 513,520,513                                     
C*****PREVIOUS SUBSEGMENT NOT IN THIS CONTROLLED ZONE - CHECK IF        
C*****PREVIOUS SUBSEGMENT IS IN A DIFFERENT CONTROLLED ZONE             
  513 IF (TYPCL(ISSCLM)-1) 1040,515,530                                 
C*****PREVIOUS SUBSEGMENT IN A DIFFERENT CONTROLLED ZONE - FIND THE     
C*****SENSIBLE HEAT GAIN FROM IT AND THE SENSIBLE HEAT LOSS TO IT IN    
C*****(FT**3)*(DEG F)/SEC                                               
  515 CVBKS1=EMTFSS(ISS-1)*DTDXLS(ISEG-1)/STDBSS(ISS-1)                 
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****FIND THE LATENT HEAT GAIN FROM IT AND THE LATENT HEAT LOSS TO IT  
C*****IN (FT**3)/SEC                                                    
      CVBKL1=EMHFSS(ISS-1)*DTHXLS(ISEG-1)/SHRSS(ISS-1)                  
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
      GO TO 550                                                         
C*****PREVIOUS SUBSEGMENT IN THIS CONTROLLED ZONE - FIND THE SENSIBLE   
C*****HEAT GAIN FROM IT AND THE SENSIBLE HEAT LOSS TO IT IN (FT**3)*    
C*****(DEG F)/SEC                                                       
  520 CVBKS1=EMTFSS(ISS-1)*DTDXLS(ISEG)/STDBSS(ISS-1)                   
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****FIND THE LATENT HEAT GAIN FROM IT AND THE LATENT HEAT LOSS TO IT  
C*****IN (FT**3)/SEC                                                    
      CVBKL1=EMHFSS(ISS-1)*DTHXLS(ISEG)/SHRSS(ISS-1)                    
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
      GO TO 550                                                         
C*****PREVIOUS SUBSEGMENT NOT IN ANY CONTROLLED ZONE - FIND THE SENSIBLE
C*****HEAT GAIN FROM IT AND THE SENSIBLE HEAT LOSS TO IT IN (FT**3)*    
C*****(DEG F)/SEC                                                       
  530 CVBKS1=EMTFSS(ISS-1)*TAIR(ISS-1)/STDBSS(ISS-1)                    
      CVBKS2=-DTDXLS(ISEG)*EMTBSS(ISS)/STDBSS(ISS)                      
C*****FIND THE LATENT HEAT GAIN FROM IT AND THE LATENT HEAT LOSS TO IT  
C*****IN (FT**3)/SEC                                                    
      CVBKL1=EMHFSS(ISS-1)*HRATIO(ISS-1)/SHRSS(ISS-1)                   
      CVBKL2=-DTHXLS(ISEG)*EMHBSS(ISS)/SHRSS(ISS)                       
C*****NOTE - COMMENT LINES SIMILAR TO ABOVE NOT REPEATED                
C                                                                       
C*****CONVECTIVE LOAD AT 'FRONT' OF SUBSEGMENT                          
C                                                                       
  550 IF (ISS-ISSBCH) 770,560,1050                                      
  560 FLOAH=FLOBTN(NODTHF,IBRCHF)-FLOFSS(ISS)                           
      NODF=TABL13(ISCT)                                                 
      IF (TABL45(NODF)-ISSCL) 610,570,610                               
  570 IF (TYPTND(NODF)-4) 600,580,580                                   
  580 CVFTS1=EMTBTN(NODTHF,IBRCHF)*EMTBTN(NODTHF,NBRNCH)/EMTBTN(NODTHF,5
     1)                                                                 
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBTN(NODTHF,IBRCHF)*EMHBTN(NODTHF,NBRNCH)/EMHBTN(NODTHF,5
     1)                                                                 
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
      GO TO 810                                                         
  600 CVFTS1=EMTBTN(NODTHF,IBRCHF)*DTDXLS(ISEG)/TAVGTN(NODTHF)          
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBTN(NODTHF,IBRCHF)*DTHXLS(ISEG)/HAVGTN(NODTHF)          
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
      GO TO 810                                                         
  610 SUMT1=0.                                                          
      SUMH1=0.                                                          
      DO 620 IBR=1,LMSCND                                               
      SUMT1=SUMT1+EMTBTN(NODTHF,IBR)                                    
  620 SUMH1=SUMH1+EMHBTN(NODTHF,IBR)                                    
      CVFTS1=EMTBTN(NODTHF,IBRCHF)*TAIR(TNSS+NODTHF)/SUMT1              
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBTN(NODTHF,IBRCHF)*HRATIO(TNSS+NODTHF)/SUMH1            
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
      GO TO 810                                                         
  770 FLOAH=FLOBSS(ISS+1)-FLOFSS(ISS)                                   
      ISSCLP=TABL44(ISS+1)                                              
      IF (ISSCL-ISSCLP) 773,780,773                                     
  773 IF (TYPCL(ISSCLP)-1) 1040,775,790                                 
  775 CVFTS1=EMTBSS(ISS+1)*DTDXLS(ISEG+1)/STDBSS(ISS+1)                 
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBSS(ISS+1)*DTHXLS(ISEG+1)/SHRSS(ISS+1)                  
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
      GO TO 810                                                         
  780 CVFTS1=EMTBSS(ISS+1)*DTDXLS(ISEG)/STDBSS(ISS+1)                   
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBSS(ISS+1)*DTHXLS(ISEG)/SHRSS(ISS+1)                    
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
      GO TO 810                                                         
  790 CVFTS1=EMTBSS(ISS+1)*TAIR(ISS+1)/STDBSS(ISS+1)                    
      CVFTS2=-DTDXLS(ISEG)*EMTFSS(ISS)/STDBSS(ISS)                      
      CVFTL1=EMHBSS(ISS+1)*HRATIO(ISS+1)/SHRSS(ISS+1)                   
      CVFTL2=-DTHXLS(ISEG)*EMHFSS(ISS)/SHRSS(ISS)                       
C*****FIND THE SENSIBLE AND LATENT CORRECTION TERMS IN (FT**3)*(DEG F)/ 
C*****SEC AND (FT**3)/SEC. THESE TERMS ARE THEORETICALLY ZERO AND ARE   
C*****ONLY USED TO COMPENSATE FOR ERRORS IN THE AVERAGING PROCESS.      
  810 CVS3=DTDXLS(ISEG)*(FLOBK+FLOAH)                                   
      CVL3=DTHXLS(ISEG)*(FLOBK+FLOAH)                                   
C*****FIND THE SUBSEGMENT TOTAL CONVECTIVE SENSIBLE HEAT GAIN IN BTU/HR 
      DUMY7=RHOCP*(CVBKS1+CVBKS2-CVS3+CVFTS1+CVFTS2)*3600.0             
      NUMY7=DUMY7                                                       
C*****FIND THE SUBSEGMENT TOTAL CONVECTIVE LATENT HEAT GAIN IN BTU/HR   
      DUMY8=RHOWHT*(CVBKL1+CVBKL2-CVL3+CVFTL1+CVFTL2)*HVH2O*3600.0      
      NUMY8=DUMY8                                                       
C*****COMPUTE THE SUBSEGMENT SENSIBLE A.C. IN BTU/HR                    
      NUMY20=-(NUMY5+NUMY3+NUMY9+NUMY7)                                 
C*****COMPUTE THE SUBSEGMENT LATENT A.C. IN BTU/HR                      
      NUMY21=-(NUMY6+NUMY4+NUMY8)                                       
C*****COMPUTE THE SUBSEGMENT TOTAL A.C. IN BTU/HR                       
      NUMY22=NUMY20+NUMY21                                              
C*****COMPUTE THE SUBSEGMENT SENSIBLE A.C. IN BTU/SEC                   
      SACLSS(ISS)=NUMY20/3600.                                          
C*****COMPUTE THE SUBSEGMENT LATENT A.C. IN BTU/SEC                     
      LACLSS(ISS)=NUMY21/3600.                                          
C*****COMPUTE THE SUBSEGMENT STEADY-STATE SENSIBLE HEAT GAIN IN BTU/SEC 
      SHLSS(ISS)=(NUMY20+NUMY3)/3600.                                   
C*****COMPUTE THE SUBSEGMENT STEADY-STATE LATENT HEAT GAIN IN BTU/SEC   
      LHLSS(ISS)=(NUMY21+NUMY4)/3600.                                   
C*****SET THE SUBSEGMENT INSTANTANEOUS TEMPERATURES, HUMIDITIES AND HEAT
C*****RATES TO DESIGN                                                   
      TDBSS(ISS)=DTDXLS(ISEG)                                           
      TTMPSS(ISS)=DTDXLS(ISEG)                                          
      HUMSS(ISS)=DTHXLS(ISEG)                                           
      HTMPSS(ISS)=DTHXLS(ISEG)                                          
      SHLTSS(ISS)=(NUMY20+NUMY3+NUMY5)/3600.                            
      LHLTSS(ISS)=(NUMY21+NUMY4+NUMY6)/3600.                            
      JSUB=ISS-ISSL+1                                                   
C*****OPTIONAL OUTPUT OF CONVECTIVE LOAD TERMS FOR SUBSEGMENT           
      IF (HSOPT-2)  830,830,820                                         
  820 DUMY31=RHOCP*CVBKS1*3600.0                                        
      DUMY32=RHOCP*CVBKS2*3600.0                                        
      DUMY33=-RHOCP*CVS3*3600.0                                         
      DUMY34=RHOCP*CVFTS1*3600.0                                        
      DUMY35=RHOCP*CVFTS2*3600.0                                        
      DUMY41=RHOWHT*CVBKL1*HVH2O*3600.0                                 
      DUMY42=RHOWHT*CVBKL2*HVH2O*3600.0                                 
      DUMY43=-RHOWHT*CVL3*HVH2O*3600.0                                  
      DUMY44=RHOWHT*CVFTL1*HVH2O*3600.0                                 
      DUMY45=RHOWHT*CVFTL2*HVH2O*3600.0                                 
      WRITE (OUT,110) DUMY31,DUMY32,DUMY33,DUMY34,DUMY35,DUMY41,DUMY42,D
     1UMY43,DUMY44,DUMY45                                               
C*****WRITE OUT SUBSEGMENT NUMBER AND TOTALS                            
  830 WRITE (OUT,120) TABL11(ISCT),NUMLS(ISEG),JSUB,NUMY5,NUMY6,NUMY3,NU
     1MY4,NUMY9,NUMY7,NUMY8,NUMY1,NUMY2,NUMY20,NUMY21,NUMY22            
C*****INCREMENT ZONE TOTALS BY SUBSEGMENT TOTALS                        
      NUMY11=NUMY11+NUMY1                                               
      NUMY12=NUMY12+NUMY2                                               
      NUMY13=NUMY13+NUMY3                                               
      NUMY14=NUMY14+NUMY4                                               
      NUMY15=NUMY15+NUMY5                                               
      NUMY16=NUMY16+NUMY6                                               
      NUMY17=NUMY17+NUMY7                                               
      NUMY18=NUMY18+NUMY8                                               
      NUMY19=NUMY19+NUMY9                                               
      NUMY23=NUMY23+NUMY20                                              
      NUMY24=NUMY24+NUMY21                                              
      NUMY25=NUMY25+NUMY22                                              
C*****CALCULATIONS FOR THIS SUBSEGMENT COMPLETE                         
  840 CONTINUE                                                          
  850 CONTINUE                                                          
  855 CONTINUE                                                          
C*****ZONE COMPUTATIONS COMPLETE - WRITE ZONE TOTALS AND CONVECTIVE     
C*****LOAD BREAKDOWN                                                    
      WRITE (OUT,130)                                                   
      WRITE (OUT,140) NUMY15,NUMY16,NUMY13,NUMY14,NUMY19,NUMY17,NUMY18,N
     1UMY11,NUMY12,NUMY23,NUMY24,NUMY25                                 
 1040 CONTINUE                                                          
C*****END OF ZONE COMPUTATIONS                                          
      RETURN                                                            
C*****IMPOSSIBLE SITUATION - TERMINATE PROGRAM                          
 1050 WRITE (OUT,200)                                                   
      CALL EXIT (EXITCODE)                                                              
      END                                                               
