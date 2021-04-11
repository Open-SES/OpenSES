      SUBROUTINE FORCEL(HRATIO,HAM,IK,FCONST,ICLST,NEQDTC,NSSDTC,ITER)  
C                                                                       
C     THIS SUBROUTINE COMPUTES THE COEFFICIENTS OF THE COLUMN           
C     FORCING MATRIX (HUMIDITY)                                         
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'DSHRHS'                                                 
C                                                                       
C     ARRAY SIZES  XHR(LMEQRM),NUMMY1(LMEQRM),DUMMY1(LMEQRM)            
      DIMENSION    XHR(LMEQRM),NUMMY1(LMEQRM),DUMMY1(LMEQRM)                  
      DIMENSION  HRATIO(LMSSTN), FCONST(LMEQRM)                         
C
   87 FORMAT (//,T36,'HUMIDITY HEAT SINK FORCING MATRIX - HTSKFM(J), J=1
     1,',I3/   T36,54('-')/)                                            
   88 FORMAT (10G13.5)                                                  
C                                                                       
C     LOOP OVER SECTIONS                                                
C                                                                       
      DO 45 ISCT = 1,NSEC                                               
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
C                                                                       
C     CHECK IF LINE SEGMENT OR VENT SHAFT                               
C                                                                       
      IF(ISEGL) 5,5,15                                                  
C                                                                       
C     VENT SHAFT                                                        
C                                                                       
    5 ISSL = TABL5(-ISEGL)                                              
      IF(TABL44(ISSL)-ICLST) 45,10,45                                   
   10 ISSH = TABL6(-ISEGH)                                              
      ISEG = ISEGH                                                      
      GO TO 30                                                          
C                                                                       
C     LINE SEGMENT                                                      
C                                                                       
   15 ISEG = ISEGL                                                      
   20 ISSL = TABL3(ISEG)                                                
      IF(TABL44(ISSL)-ICLST) 37,25,37                                   
   25 ISSH = TABL4(ISEG)                                                
C                                                                       
C     LOOP OVER SUBSEGMENTS IN SEGMENT                                  
C                                                                       
   30 DO 35 ISS = ISSL,ISSH                                             
      IDSS = TABL40(ISS)                                                
      HTSKFM(IDSS) = -LSUMSS(ISS)                                       
     1 + FCONST(IDSS)                                                   
   35 CONTINUE                                                          
   37 IF(ISEG-ISEGH) 40,45,45                                           
   40 ISEG = ISEG + 1                                                   
      GO TO 20                                                          
   45 CONTINUE                                                          
C                                                                       
C     ASSIGN VALUES TO NODE ELEMENTS OF COLUMN FORCING MATRIX           
C                                                                       
C     LOOP OVER NODES                                                   
C                                                                       
      DO 85 I = 1,NNODE                                                 
      NODCL = TABL45(I)                                                 
      IF(ICLST-NODCL) 85,50,85                                          
   50 IF(TYPTND(I)-1) 55,55,60                                          
   55 NODTH = TABL22(I) + 1                                             
      NODRM = TABL42(NODTH)                                             
      HTSKFM(NODRM) = FCONST(NODRM)                                     
      GO TO 85                                                          
   60 IF(TYPTND(I)-3) 65,65,70                                          
   65 NODTH = TABL22(I) + 1                                             
      NODRM = TABL42(NODTH)                                             
      HTSKFM(NODRM) = FCONST(NODRM)                                     
      HTSKFM(NODRM+1) = FCONST(NODRM+1)                                 
      HTSKFM(NODRM+2) = FCONST(NODRM+2)                                 
      GO TO 85                                                          
   70 IF(TYPTND(I)-4) 75,75,80                                          
   75 NODTH = TABL22(I)+1                                               
      NODRM = TABL42(NODTH)                                             
      HTSKFM(NODRM) = EMHBTN(NODTH,1)*EMHBTN(NODTH,IK)                  
     1 / EMHBTN(NODTH,5)                                                
      GO TO 85                                                          
   80 NODTH = TABL22(I) + 1                                             
      NODRM = TABL42(NODTH)                                             
      HTSKFM(NODRM) = EMHBTN(NODTH,1)*HAM/HUMAMB                        
   85 CONTINUE                                                          
      GO TO (89,89,86,89,89), HSOPT                                     
   86 WRITE (OUT,87) NEQDTC                                             
      WRITE (OUT,88) (HTSKFM(J),J=1,NEQDTC)                             
   89 NOGO=0                                                            
      M=NEQDTC*NEQDTC                                                   
      IF(ITER.GT.1) GO TO 95                                            
C*****COMPRESS DTRM INTO M ADJACENT LOCATIONS IN DTRM2                  
      IPACK=LMEQRM-NEQDTC                                               
      IJ=0                                                              
      NM=0                                                              
      DO 94 K=1,NEQDTC                                                  
      DO 92 L=1,NEQDTC                                                  
      IJ=IJ+1                                                           
      NM=NM+1                                                           
   92 DTRM2(IJ)=DTRM2(NM)                                               
   94 NM=NM+IPACK                                                       
   95 CALL SIMQ1 (DTRM2,HTSKFM,XHR,NEQDTC,M,NUMMY1,DUMMY1,ITER,NOGO)    
      IF (NOGO .EQ. 1) CALL SIMERR(11)                                  
      IF (NSSDTC) 107,107,97                                            
   97 DO 100 I=1,NSSDTC                                                 
      ISS=TABL41(I)                                                     
  100 HRATIO(ISS)=XHR(I)*SHRSS(ISS)/(EMHBSS(ISS)+EMHFSS(ISS))           
  107 L=NSSDTC+1                                                        
      DO 115 I = L,NEQDTC                                               
      INODTH=-TABL41(I)                                                 
  115 HRATIO(TNSS+INODTH) = XHR(I)                                      
      RETURN                                                            
      END                                                               
