      SUBROUTINE CREAT3(NEQDTC,ICLST)                                   
C                                                                       
C     THIS SUBROUTINE COMPUTES THE COEFFICIENTS OF THE DYNAMIC THERMAL  
C     RESPONSE MATRIX (HUMIDITY) AND THE CONSTANT ELEMENTS OF THE COLUMN
C     FORCING MATRIX FOR A GIVEN ZONE CLUSTER                           
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'DSHRHS'                                                 
C                                                                       
C                                                                       
  403 FORMAT (A1)  
  404 FORMAT (T47,'OUTPUT OF HEAT SINK PRINT OPTION 2'////T38,      
     1'DYNAMIC HUMIDITY RESPONSE MATRIX  DTRM - ( NEQDTC =',I3,' )',    
     2/T38,56('-')/)                                                    
  410 FORMAT(10X)                                                       
  415 FORMAT (10G13.5)                                                  
  501 FORMAT( 'IMPOSSIBLE SITUATION IN CREATING DTRM (HUMIDITY)')      
C
      RHWHT = RHOWHT                                                    
C                                                                       
C     LOOP OVER LINE SEGMENTS AND VENT SEGMENTS                         
C                                                                       
      JJ = NLS + NVSEC                                                  
      DO 300 ISEG = 1,JJ                                                
      IF(ISEG - NLS) 5,5,10                                             
    5 ISSL = TABL3(ISEG)                                                
      IF(TABL44(ISSL)-ICLST) 300,15,300                                 
   10 IVS = ISEG-NLS                                                    
      ISSL = TABL5(IVS)                                                 
      IF(TABL44(ISSL)-ICLST) 300,20,300                                 
C                                                                       
C     LINE SEGMENT IN ZONE CLUSTER                                      
C                                                                       
   15 ISCT = TABL8(ISEG)                                                
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      ISSBCL = TABL3(ISEGL)                                             
      ISSBCH = TABL4(ISEGH)                                             
      ISSL = TABL3(ISEG)                                                
      ISSH = TABL4(ISEG)                                                
      GO TO 25                                                          
C                                                                       
C     VENT SEGMENT IN ZONE CLUSTER                                      
C                                                                       
   20 ISCT = NLSEC + IVS                                                
      ISSBCL = TABL5(IVS)                                               
      ISSL = ISSBCL                                                     
      ISSBCH = TABL6(IVS)                                               
      ISSH = ISSBCH                                                     
   25 NODTHB = TABL20(ISCT)/64                                          
      IBRCHB = TABL20(ISCT) - NODTHB*64                                 
      NODTHF = TABL21(ISCT)/64                                          
      IBRCHF = TABL21(ISCT) - NODTHF*64                                 
C                                                                       
C     LOOP OVER SUBSEGMENTS IN SEGMENT                                  
C                                                                       
      DO 280 ISS = ISSL,ISSH                                            
      IDSS = TABL40(ISS)                                                
      IF(ISSBCL - ISSBCH) 75,30,500                                     
C                                                                       
C     ONE SUBSEGMENT IN SECTION                                         
C                                                                       
   30 SUM = 0.0                                                         
      FLONET=FLOBTN(NODTHB,IBRCHB)+FLOBTN(NODTHF,IBRCHF)                
     1       -FLOFSS(ISS)-FLOBSS(ISS)                                   
      FLOCOR=RHWHT*FLONET*SHRSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))          
      NODB = TABL12(ISCT)                                               
      IF(TYPTND(NODB)-4) 33,31,31                                       
   31 SUM = EMHBTN(NODTHB,1)                                            
      IF(SUM) 32,32,37                                                  
   32 SUM = 1.0                                                         
      GO TO 37                                                          
   33 DO 35 I = 1,5                                                     
   35 SUM = SUM + EMHBTN(NODTHB,I)                                      
   37 NODRMB = TABL42(NODTHB)                                           
      DTRM(IDSS,NODRMB)=EMHBTN(NODTHB,IBRCHB)*RHWHT/SUM                 
      DTRM(IDSS,IDSS)=-RHWHT-FLOCOR                                     
      IF(TYPTND(NODB) - 3) 40,40,45                                     
   40 DTRM(NODRMB,IDSS)=EMHBSS(ISS)/(EMHBSS(ISS)+EMHFSS(ISS))           
      GO TO 50                                                          
   45 DTRM(NODRMB,NODRMB) = 1.0                                         
   50 SUM = 0.0                                                         
      NODF = TABL13(ISCT)                                               
      IF(TYPTND(NODF)-4) 53,51,51                                       
   51 SUM = EMHBTN(NODTHF,1)                                            
      IF(SUM) 52,52,57                                                  
   52 SUM = 1.0                                                         
      GO TO 57                                                          
   53 DO 55 I = 1,5                                                     
   55 SUM = SUM + EMHBTN(NODTHF,I)                                      
   57 NODRMF = TABL42(NODTHF)                                           
      DTRM(IDSS,NODRMF)=EMHBTN(NODTHF,IBRCHF)*RHWHT/SUM                 
      IF(TYPTND(NODF)-3) 60,60,65                                       
   60 DTRM(NODRMF,IDSS)=EMHFSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))           
      GO TO 70                                                          
   65 DTRM(NODRMF,NODRMF) = 1.0                                         
   70 GO TO 280                                                         
   75 IF(ISS-ISSBCL) 500,85,80                                          
   80 IF(ISS-ISSBCH) 155,120,500                                        
C                                                                       
C     FIRST SUBSEGMENT OF SECTION                                       
C                                                                       
   85 SUM = 0.0                                                         
      FLONET=FLOBTN(NODTHB,IBRCHB)+FLOBSS(ISS+1)-FLOBSS(ISS)-FLOFSS(ISS)
      FLOCOR=RHWHT*FLONET*SHRSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))          
      NODB = TABL12(ISCT)                                               
      IF(TYPTND(NODB)-4) 88,86,86                                       
   86 SUM = EMHBTN(NODTHB,1)                                            
      IF(SUM) 87,87,92                                                  
   87 SUM = 1.0                                                         
      GO TO 92                                                          
   88 DO 90 I = 1,5                                                     
   90 SUM = SUM + EMHBTN(NODTHB,I)                                      
   92 NODRMB = TABL42(NODTHB)                                           
      DTRM(IDSS,NODRMB)=EMHBTN(NODTHB,IBRCHB)*RHWHT/SUM                 
      DTRM(IDSS,IDSS)=-RHWHT-FLOCOR                                     
      IF(TYPTND(NODB) - 3) 95,95,100                                    
   95 DTRM(NODRMB,IDSS)=EMHBSS(ISS)/(EMHBSS(ISS)+EMHFSS(ISS))           
      GO TO 105                                                         
  100 DTRM(NODRMB,NODRMB) = 1.0                                         
  105 ISSCL = TABL44(ISS+1)                                             
      IF(ISSCL-ICLST) 115,110,115                                       
  110 DTRM(IDSS,IDSS+1)=EMHBSS(ISS+1)*RHWHT/(EMHBSS(ISS+1)+EMHFSS(ISS+1)
     1 )                                                                
      GO TO 280                                                         
  115 FORCE2(IDSS)=-EMHBSS(ISS+1)*DTHMLS(ISEG+1)*RHWHT/SHRSS(ISS+1)     
      FORCE3(IDSS)=-EMHBSS(ISS+1)*DTHELS(ISEG+1)*RHWHT/SHRSS(ISS+1)     
      GO TO 280                                                         
C                                                                       
C     LAST SUBSEGMENT OF SECTION                                        
C                                                                       
  120 SUM = 0.0                                                         
      FLONET=FLOBTN(NODTHF,IBRCHF)+FLOFSS(ISS-1)-FLOBSS(ISS)-FLOFSS(ISS)
      FLOCOR=RHWHT*FLONET*SHRSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))          
      NODF = TABL13(ISCT)                                               
      IF(TYPTND(NODF)-4) 123,121,121                                    
  121 SUM = EMHBTN(NODTHF,1)                                            
      IF(SUM) 122,122,127                                               
  122 SUM = 1.0                                                         
      GO TO 127                                                         
  123 DO 125 I = 1,5                                                    
  125 SUM = SUM + EMHBTN(NODTHF,I)                                      
  127 NODRMF = TABL42(NODTHF)                                           
      DTRM(IDSS,NODRMF)=EMHBTN(NODTHF,IBRCHF)*RHWHT/SUM                 
      DTRM(IDSS,IDSS)=-RHWHT-FLOCOR                                     
      IF(TYPTND(NODF)-3) 130,130,135                                    
  130 DTRM(NODRMF,IDSS)=EMHFSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))           
      GO TO 140                                                         
  135 DTRM(NODRMF,NODRMF) = 1.0                                         
  140 ISSCL = TABL44(ISS-1)                                             
      IF(ISSCL-ICLST) 150,145,150                                       
  145 DTRM(IDSS,IDSS-1)=EMHFSS(ISS-1)*RHWHT/(EMHFSS(ISS-1)+EMHBSS(ISS-1)
     1 )                                                                
      GO TO 280                                                         
  150 FORCE2(IDSS)=-EMHFSS(ISS-1)*DTHMLS(ISEG-1)*RHWHT/SHRSS(ISS-1)     
      FORCE3(IDSS)=-EMHFSS(ISS-1)*DTHELS(ISEG-1)*RHWHT/SHRSS(ISS-1)     
      GO TO 280                                                         
C                                                                       
C     NON BOUNDARY SUBSEGMENT OF SECTION                                
C                                                                       
  155 FLONET=FLOFSS(ISS-1)+FLOBSS(ISS+1)-FLOBSS(ISS)-FLOFSS(ISS)        
      FLOCOR=RHWHT*FLONET*SHRSS(ISS)/(EMHFSS(ISS)+EMHBSS(ISS))          
      DTRM(IDSS,IDSS)=-RHWHT-FLOCOR                                     
      ISSCL = TABL44(ISS-1)                                             
      IF(ISSCL-ICLST) 165,160,165                                       
  160 DTRM(IDSS,IDSS-1)=EMHFSS(ISS-1)*RHWHT/(EMHFSS(ISS-1)+EMHBSS(ISS-1)
     1 )                                                                
      GO TO 170                                                         
  165 FORCE2(IDSS)=-EMHFSS(ISS-1)*DTHMLS(ISEG-1)*RHWHT/SHRSS(ISS-1)     
      FORCE3(IDSS)=-EMHFSS(ISS-1)*DTHELS(ISEG-1)*RHWHT/SHRSS(ISS-1)     
  170 ISSCL = TABL44(ISS+1)                                             
      IF(ISSCL-ICLST) 180,175,180                                       
  175 DTRM(IDSS,IDSS+1)=EMHBSS(ISS+1)*RHWHT/(EMHFSS(ISS+1)+EMHBSS(ISS+1)
     1 )                                                                
      GO TO 280                                                         
  180 FORCE2(IDSS)=FORCE2(IDSS)-EMHBSS(ISS+1)*DTHMLS(ISEG+1)            
     1 *RHWHT/SHRSS(ISS+1)                                              
      FORCE3(IDSS)=FORCE3(IDSS)-EMHBSS(ISS+1)*DTHELS(ISEG+1)            
     1 *RHWHT/SHRSS(ISS+1)                                              
  280 CONTINUE                                                          
  300 CONTINUE                                                          
C                                                                       
C     LOOP OVER SECTIONS                                                
C                                                                       
      DO 360 ISCT = 1,NSEC                                              
      NODB = TABL12(ISCT)                                               
      NODCL = TABL45(NODB)                                              
      IF(NODCL-ICLST) 330,305,330                                       
  305 ISEGL = TABL9(ISCT)                                               
      IF(ISEGL) 310,500,315                                             
C                                                                       
C     VENT SEGMENT                                                      
C                                                                       
  310 ISSL = TABL5(-ISEGL)                                              
      ISSCL = TABL44(ISSL)                                              
      IF(ISSCL-ICLST) 312,330,312                                       
  312 NODTHB = TABL20(ISCT)/64                                          
      NODRMB = TABL42(NODTHB)                                           
      NODTHF = TABL21(ISCT)/64                                          
      FORCE2(NODRMB) = FORCE2(NODRMB)-EMHBTN(NODTHF,1)*                 
     1 EMHBTN(NODTHF,3)/EMHBTN(NODTHF,5)                                
      FORCE3(NODRMB) = FORCE3(NODRMB)-EMHBTN(NODTHF,1)*                 
     1 EMHBTN(NODTHF,4)/EMHBTN(NODTHF,5)                                
      GO TO 330                                                         
C                                                                       
C     LINE SEGMENT                                                      
C                                                                       
  315 ISSL = TABL3(ISEGL)                                               
  320 ISSCL = TABL44(ISSL)                                              
      IF(ISSCL-ICLST) 325,330,325                                       
  325 NODTHB = TABL20(ISCT)/64                                          
      NODRMB = TABL42(NODTHB)                                           
      FORCE2(NODRMB)=FORCE2(NODRMB)-EMHBSS(ISSL)*DTHMLS(ISEGL)          
     1 /SHRSS(ISSL)                                                     
      FORCE3(NODRMB)=FORCE3(NODRMB)-EMHBSS(ISSL)*DTHELS(ISEGL)          
     1 /SHRSS(ISSL)                                                     
  330 NODF = TABL13(ISCT)                                               
      NODCL= TABL45(NODF)                                               
      IF(NODCL-ICLST) 360,335,360                                       
  335 ISEGH = TABL10(ISCT)                                              
      IF(ISEGH) 340,500,345                                             
C                                                                       
C     VENT SEGMENT                                                      
C                                                                       
  340 ISSH = TABL6(-ISEGH)                                              
      ISSCL = TABL44(ISSH)                                              
      IF(ISSCL-ICLST) 342,360,342                                       
  342 NODTHF = TABL21(ISCT)/64                                          
      NODRMF = TABL42(NODTHF)                                           
      NODTHB = TABL20(ISCT)/64                                          
      FORCE2(NODRMF) = FORCE2(NODRMF)-EMHBTN(NODTHB,1)*                 
     1 EMHBTN(NODTHB,3)/EMHBTN(NODTHB,5)                                
      FORCE3(NODRMF) = FORCE3(NODRMF)-EMHBTN(NODTHB,1)*                 
     1 EMHBTN(NODTHB,4)/EMHBTN(NODTHB,5)                                
      GO TO 360                                                         
C                                                                       
C     LINE SEGMENT                                                      
C                                                                       
  345 ISSH = TABL4(ISEGH)                                               
  350 ISSCL = TABL44(ISSH)                                              
      IF(ISSCL-ICLST) 355,360,355                                       
  355 NODTHF = TABL21(ISCT)/64                                          
      NODRMF = TABL42(NODTHF)                                           
      FORCE2(NODRMF)=FORCE2(NODRMF)-EMHFSS(ISSH)*DTHMLS(ISEGH)          
     1 /SHRSS(ISSH)                                                     
      FORCE3(NODRMF)=FORCE3(NODRMF)-EMHFSS(ISSH)*DTHELS(ISEGH)          
     1 /SHRSS(ISSH)                                                     
  360 CONTINUE                                                          
C                                                                       
C     REMAINING COMPONENTS OF TYPE 1,2 AND 3 NODES                      
C                                                                       
      DO 400 I = 1,NNODE                                                
      NODCL = TABL45(I)                                                 
      IF(NODCL-ICLST) 400,365,400                                       
  365 IF(TYPTND(I)-1) 395,395,370                                       
  370 IF(TYPTND(I)-3) 375,375,400                                       
  375 NODTH = TABL22(I) + 1                                             
      NODRM = TABL42(NODTH)                                             
      DTRM(NODRM,NODRM) = -1.0                                          
      DTRM(NODRM+1,NODRM+1) = -1.0                                      
      DTRM(NODRM+2,NODRM+2) = -1.0                                      
      SUM = 0.0                                                         
      DO 380 J = 1,5                                                    
  380 SUM = SUM+EMHBTN(NODTH+1,J)                                       
      IF(SUM) 381,381,382                                               
  381 SUM = 1.0                                                         
  382 DTRM(NODRM,NODRM+1)=EMHBTN(NODTH+1,4)/SUM                         
      DTRM(NODRM+2,NODRM+1)=EMHBTN(NODTH+1,5)/SUM                       
      SUM = 0.0                                                         
      DO 385 J = 1,5                                                    
  385 SUM = SUM + EMHBTN(NODTH,J)                                       
      DTRM(NODRM+1,NODRM)=EMHBTN(NODTH,5)/SUM                           
      SUM = 0.0                                                         
      DO 390 J = 1,5                                                    
  390 SUM = SUM + EMHBTN(NODTH+2,J)                                     
      DTRM(NODRM+1,NODRM+2)=EMHBTN(NODTH+2,5)/SUM                       
      GO TO 400                                                         
  395 NODTH = TABL22(I)+1                                               
      NODRM = TABL42(NODTH)                                             
      DTRM(NODRM,NODRM) = -1.0                                          
  400 CONTINUE                                                          
      GO TO (510,510,402,510,510), HSOPT
  402 WRITE (OUT,403) CHAR(12)                                 
      WRITE (OUT,404) NEQDTC                                            
      DO 405 I=1,NEQDTC                                                 
      WRITE(OUT,410)                                                    
  405 WRITE(OUT,415) (DTRM(I,J), J=1,NEQDTC)                            
  510 RETURN                                                            
  500 WRITE(OUT,501)                                                    
      CALL EXIT (EXITCODE)                                                              
      END                                                               
