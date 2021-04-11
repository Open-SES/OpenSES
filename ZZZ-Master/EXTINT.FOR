      SUBROUTINE EXTINT( ISCTX, ISEGX, ISSX, ISS, IERR )                
C                                                                       
C     THIS SUBROUTINE CONVERTS EXTERNAL SECTION-SEGMENT-SUBSEGMENT      
C     NUMBERS TO INTERNAL SUBSEGMENT NUMBERS                            
C     IT OPERATES FOR BOTH LINE SEGMENTS AND VENT SEGMENTS.             
C     CONVERSION IS BASED UPON SEGMENT AND SUBSEGMENT NUMBERS, SECTION  
C     NUMBER IS ONLY CHECKED.                                           
C                                                                       
C     OUTPUT IS ISS. RANGE IS 1 TO TNSS, 0 WHEN ERROR OCCURS.           
C                                                                       
C     IERR IS ERROR INDICATOR:                                          
C         1 = O.K.                                                      
C         2 = ERROR                                                     
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C---- INITIALIZE ERROR INDICATOR                                        
      IERR=1                                                            
C---- SEARCH FOR LINE SEGMENT                                           
      DO 30 ILS=1,NLS                                                   
      IF (NUMLS(ILS)-ISEGX) 30,100,30                                   
   30 CONTINUE                                                          
C---- SEARCH FOR VENT SEGMENT                                           
      IF (NVSEC) 90,90,40                                               
   40 DO 60 IVS=1,NVSEC                                                 
      IF (NUMVS(IVS)-ISEGX) 60,200,60                                   
   60 CONTINUE                                                          
C---- NO LINE SEGMENT OR VENT SHAFT WITH THIS I.D.                      
   90 ISS=0                                                             
   95 IERR=2                                                            
      RETURN                                                            
C                                                                       
C---- LINE SEGMENT                                                      
  100 ISCT=TABL8(ILS)                                                   
      ISSL=TABL3(ILS)                                                   
      ISSH=TABL4(ILS)                                                   
      GO TO 300                                                         
C                                                                       
C---- VENT SHAFT SEGMENT                                                
  200 ISCT=NLSEC+IVS                                                    
      ISSL=TABL5(IVS)                                                   
      ISSH=TABL6(IVS)                                                   
C                                                                       
C---- COMPUTER INTERNAL SUBSEGMENT NUMBER                               
  300 IF (ISSX) 90,90,320                                               
  320 ISS=ISSL+ISSX-1                                                   
      IF (ISS-ISSH) 340,340,90                                          
C---- CHECK STATION NUMBER                                              
  340 ISCTX2=TABL11(ISCT)                                               
      IF (ISCTX2-ISCTX) 95,360,95                                       
  360 RETURN                                                            
      END                                                               
