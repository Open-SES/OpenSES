      SUBROUTINE  GRID                                                  
C                                                                       
C     THIS SUBROUTINE CALCULATES THE INSTANTANEOUS VALUE OF THE HEAT    
C         RELEASE BY THE TRAIN.                                         
C         THIS NUMBER CONSISTS OF THE SUM OF THE STEADY-STATE SENSIBLE  
C         HEAT LOAD AND THE ACCELERATION AND BRAKING HEAT RELEASED      
C         THROUGH THE RESISTOR-GRID THERMAL INERTIA SIMULATION.         
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
                                                                        
C                                                                       
      DIMENSION  AA(4),BB(4),CC(4)                                      
C                                                                       
C                                                                       
C                                                                       
      DATA  AA/0.5, 0.29289319, 1.707106781, 0.166666667 /              
      DATA  BB/2.0, 1.0, 1.0, 2.0 /                                     
      DATA  CC/0.5, 0.292893219, 1.70706781, 0.5 /                      
      DATA  CALFRE /3.1 /                                               
C                                                                       
C     LOOP OVER EACH TRAIN WHICH IS IN OPERATION                        
C                                                                       
      DO  500  ITRAN = 1, NTRAN                                         
      NUMV = IINDEX(ITRAN)                                               
      ITYP = ITYPV(NUMV)                                                
C---- ADJUST RESISTOR GRID INPUT MASS TO COMPLY                         
C---- WITH EFFECTIVE MASS                                               
      RCPD = 1.5 * MCPGDV(ITYP)                                         
      RCPA = 1.5 * MCPGAV(ITYP)                                         
C     WRITE(OUT,990) TNUMV(NUMV),QDECV(NUMV),QACCV(NUMV),RMHTV(NUMV)    
C 990 FORMAT( 'TRAIN',I3, 3F20.5 )                                     
C                                                                       
C---- COMPLETE COMPUTATION OF TRAIN ENVIRONMENT                         
      IF( RGSUMV(NUMV) ) 60,60,50                                       
C---- TRAIN IS EITHER PARTIALLY OR ENTIRELY WITHIN SYSTEM               
   50 FOUT = ( 1.0 - RGSUMV(NUMV) / LV(ITYP) )                          
      TOUT = TDBAMB * FOUT                                              
      TAAVG = TAAVGV(NUMV) / LV(ITYP) + TOUT                            
      TWAVG = TWAVGV(NUMV) / LV(ITYP) + TOUT                            
      ANV = ANVV(NUMV) / LV(ITYP) + UV(NUMV) * FOUT                     
      GO TO 70                                                          
C---- TRAIN IS ENTIRELY OUTSIDE OF SYSTEM                               
   60 TAAVG = TDBAMB                                                    
      TWAVG = TDBAMB                                                    
      ANV = UV(NUMV)                                                    
   70 TWR4 = ( TWAVG +ABTEMP)**4                                        
C                                                                       
C     INTEGRATE TEMPERATURE OF DECELERATION RESISTOR GRIDS              
C                                                                       
      QDECI=QDECV(NUMV)/NPCARV(ITYP)                                    
      IF (RCPD) 290,290,260                                             
  260 TEMDGO = TGDECV(NUMV)                                             
      DO 280 J = 1,4                                                    
      TEMP = (TGDECV(NUMV) + TAAVG) /2.                                 
      REYDG = ANV * DIDCGV(ITYP) / AIR(TEMP,1)                          
      GRASHD = DIDCGV(ITYP)**3*(TGDECV(NUMV)-TAAVG)*AIR(TEMP,4)         
      GRASHD = ABS(GRASHD)                                              
      IF( REYDG - 0.01 ) 265,265,263                                    
  263 IF( GRASHD - 0.01 ) 265,265,264                                   
  264 IF(.33*REYDG**.6*AIR(TEMP,3)**.3-.53*CALFRE*GRASHD**.25*          
     1AIR(TEMP,3)**.25) 265,270,270                                     
  265 DERIV = QDECI /RCPD - ((DIDCGV(ITYP)**3*                          
     1        ABS(TGDECV(NUMV) - TAAVG)*AIR(TEMP,4))**.25 *             
     2        CALFRE*AIR(TEMP,2)*.53*AIR(TEMP,3)**.25/DIDCGV(ITYP))*    
     3        ACDECV(ITYP)*(TGDECV(NUMV) - TAAVG)/(RCPD*3600.)          
     4        - EMISDG(ITYP)*ARDECV(ITYP)*STEFAN*((TGDECV               
     5        (NUMV)+ABTEMP)**4 - TWR4 )/ (RCPD*3600.)                  
      GO TO 275                                                         
  270 DERIV = QDECI /RCPD - (AIR(TEMP,2)*.33*                           
     1        REYDG**.6*AIR(TEMP,3)**.3/DIDCGV(ITYP))*ACDECV(ITYP)*     
     2        (TGDECV(NUMV) - TAAVG)/(RCPD*3600.)                       
     3        - EMISDG(ITYP)*ARDECV(ITYP)*STEFAN*((TGDECV(NUMV)         
     4        +ABTEMP)**4 - TWR4 ) / (RCPD*3600.)                       
  275 TEM = AA(J)*(DERIV - BB(J)*QERDCV(NUMV))                          
C     WRITE(OUT,276) DERIV,REYDG                                        
C 276 FORMAT('DERIVDEC=',F10.2,'REYDG=',F10.2)                      
      W = TGDECV(NUMV)                                                  
      TGDECV(NUMV) = TGDECV(NUMV) + DEL2TH*TEM                          
      TEM = (TGDECV(NUMV) - W)/DEL2TH                                   
      QERDCV(NUMV) = QERDCV(NUMV) + 3. * TEM - CC(J)*DERIV              
  280 CONTINUE                                                          
      TGAVGD = (TGDECV(NUMV) + TEMDGO) / 2.                             
      TEMP = (TGAVGD + TAAVG)/2.                                        
      REYDG = ANV * DIDCGV(ITYP)/AIR(TEMP,1)                            
      GRASHD = DIDCGV(ITYP)**3*(TGAVGD-TAAVG)*AIR(TEMP,4)               
      GRASHD = ABS(GRASHD)                                              
      IF( REYDG - 0.01 ) 283,283,281                                    
  281 IF( GRASHD - 0.01 ) 283,283,282                                   
  282 IF(.33*REYDG**.6*AIR(TEMP,3)**.3-.53*CALFRE*GRASHD**.25*          
     1 AIR(TEMP,3)**.25) 283,286,286                                    
  283 QDEC = ((DIDCGV(ITYP)**3*ABS(TGAVGD - TAAVG)*                     
     2       AIR(TEMP,4))**.25*AIR(TEMP,2)*.53*CALFRE*AIR(TEMP,3)       
     3       **.25/DIDCGV(ITYP))*ACDECV(ITYP)*(TGAVGD                   
     4       - TAAVG)/3600.                                             
      QDECR = STEFAN*EMISDG(ITYP)*ARDECV(ITYP)*((TGAVGD+ABTEMP)**4      
     1        - TWR4 ) / 3600.                                          
      QDEC = QDEC + QDECR                                               
      GO TO 300                                                         
  286 QDEC = (AIR(TEMP,2)*.33*REYDG**.6*AIR(TEMP,3)**.3/DIDCGV(ITYP))   
     1       * ACDECV(ITYP) * (TGAVGD - TAAVG) / 3600.                  
      QDECR = STEFAN*EMISDG(ITYP)*ARDECV(ITYP)*((TGAVGD+ABTEMP)**4      
     1        - TWR4 ) / 3600.                                          
      QDEC = QDEC + QDECR                                               
      GO TO 300                                                         
  290 QDEC = QDECI                                                      
  300 CONTINUE                                                          
C     WRITE(OUT,305) QDEC, REYDG,TAAVG,TWAVG,ANV,                       
C    1  TGAVGD, TNUMV(NUMV)                                             
C 305 FORMAT('QDEC=',F10.2,' REYDG=',F10.2,4F10.2,' TRAIN ',A2)     
C                                                                       
C     INTEGRATE TEMPERATURE OF ACCELERATION RESISTOR GRIDS              
C                                                                       
      QACCI=QACCV(NUMV)/NPCARV(ITYP)                                    
      IF (RCPA) 190,190,160                                             
  160 TEMAGO = TGACCV(NUMV)                                             
      DO 180 J = 1,4                                                    
      TEMP = (TGACCV(NUMV) + TAAVG)/2.                                  
      REYAG = ANV*DIACGV(ITYP)/AIR(TEMP,1)                              
      GRASHA = DIACGV(ITYP)**3*(TGACCV(NUMV)-TAAVG)*AIR(TEMP,4)         
      GRASHA = ABS(GRASHA)                                              
      IF( REYAG - 0.01 ) 165,165,161                                    
  161 IF( GRASHA - 0.01 ) 165,165,162                                   
  162 IF(.33*REYAG**.6*AIR(TEMP,3)**.3-.53*CALFRE*GRASHA**.25*          
     1 AIR(TEMP,3)**.25) 165,170,170                                    
  165 DERIV = QACCI /RCPA - ((DIACGV(ITYP)**3*                          
     1        ABS(TGACCV(NUMV) - TAAVG)*AIR(TEMP,4))**.25               
     2        *AIR(TEMP,2)*.53*CALFRE*AIR(TEMP,3)**.25/DIACGV(ITYP))*   
     3        ACACCV(ITYP)*(TGACCV(NUMV) - TAAVG)/(RCPA*3600.)          
     4        - EMISAG(ITYP)*ARACCV(ITYP)*STEFAN                        
     5        *((TGACCV(NUMV)+ABTEMP)**4 - TWR4 ) / (RCPA*3600.)        
      GO TO 175                                                         
  170 DERIV = QACCI /RCPA - (AIR(TEMP,2)*.33*REYAG**.6                  
     1        *AIR(TEMP,3)**.3/DIACGV(ITYP))*ACACCV(ITYP)*              
     2        (TGACCV(NUMV) - TAAVG)/(RCPA*3600.)                       
     3        - EMISAG(ITYP)*ARACCV(ITYP)*STEFAN*((TGACCV(NUMV)+        
     4        ABTEMP)**4 - TWR4 )/( RCPA*3600.)                         
  175 TEM = AA(J)*(DERIV - BB(J)*QERACV(NUMV))                          
C     WRITE(OUT,176) DERIV,REYAG                                        
C 176 FORMAT('DERIVACC=',F10.2,'REYAG=',F10.2)                      
      W = TGACCV(NUMV)                                                  
      TGACCV(NUMV) = TGACCV(NUMV) + DEL2TH*TEM                          
      TEM = (TGACCV(NUMV) - W)/DEL2TH                                   
      QERACV(NUMV) = QERACV(NUMV) + 3.*TEM - CC(J)*DERIV                
  180 CONTINUE                                                          
      TGAVGA = (TGACCV(NUMV) + TEMAGO) / 2.                             
      TEMP = (TGAVGA + TAAVG)/2.                                        
      REYAG = ANV*DIACGV(ITYP)/AIR(TEMP,1)                              
      GRASHA = DIACGV(ITYP)**3*(TGAVGA-TAAVG)*AIR(TEMP,4)               
      GRASHA = ABS(GRASHA)                                              
      IF( REYAG - 0.01 ) 183,183,181                                    
  181 IF( GRASHA - 0.01 ) 183,183,182                                   
  182 IF(.33*REYAG**.6*AIR(TEMP,3)**.3-.53*CALFRE*GRASHA**.25*          
     1 AIR(TEMP,3)**.25) 183,186,186                                    
  183 QACC = ((DIACGV(ITYP)**3*ABS(TGAVGA-TAAVG)*                       
     1       AIR(TEMP,4))**.25*AIR(TEMP,2)*.53*CALFRE*AIR(TEMP,3)       
     2       **.25/DIACGV(ITYP))*ACACCV(ITYP)*(TGAVGA                   
     3       -TAAVG)/3600.                                              
      QACCR = STEFAN*EMISAG(ITYP)*ARACCV(ITYP)*((TGAVGA+ABTEMP)**4      
     1         - TWR4 ) / 3600.                                         
      QACC = QACC + QACCR                                               
      GO TO 200                                                         
  186 QACC = (AIR(TEMP,2)*.33*REYAG**.6*AIR(TEMP,3)**.3/DIACGV(ITYP))   
     1       * ACACCV(ITYP) * (TGAVGA - TAAVG) / 3600.                  
      QACCR = STEFAN*EMISAG(ITYP)*ARACCV(ITYP)*((TGAVGA+ABTEMP)**4      
     1         - TWR4 ) / 3600.                                         
      QACC = QACC + QACCR                                               
      GO TO 200                                                         
  190 QACC = QACCI                                                      
  200 CONTINUE                                                          
C     WRITE(OUT,205) QACC, REYAG,TGAVGA                                 
C 205 FORMAT('QACC=',F10.2,' REYAG=',F10.2 ,30X,F10.2)              
C                                                                       
      QPRPV(NUMV) = (NPCARV(ITYP)*(QACC+QDEC)+RMHTV(NUMV))/LV(ITYP)     
C                                                                       
C     TIME = FLOAT(NTIME) / 100.0                                       
C                                                                       
C     WRITE(OUT,206) TIME,QACC,QDEC,TGACCV(NUMV),TGDECV(NUMV)           
C 206 FORMAT(F7.2,' SECONDS    QACC=',F9.3,' QDEC=',F9.3,           
C    1  ' TGACC=',F9.2,' TGDEC=',F9.2 /)                                
C                                                                       
  500 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
