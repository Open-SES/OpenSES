      SUBROUTINE INTEXT(ISS,ISCTX,ILSX,ISSX)                            
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C THIS SUBROUTINE CONVERTS THE INTERNAL NUMBERS TO EXTERNAL NUMBERS     
      ILS = TABL7(ISS)                                                  
      IF(ILS) 100,200,200                                               
C FOR VENTILATION SHAFT SEGMENTS                                        
  100 IVS = -ILS                                                        
      ISCT = NLSEC + IVS                                                
      ISSX = ISS - TABL5(IVS) + 1                                       
      ILSX = NUMVS(IVS)                                                 
      GO TO 240                                                         
C FOR LINE SEGMENTS                                                     
  200 ISCT = TABL8(ILS)                                                 
      ISSX = ISS - TABL3(ILS) + 1                                       
      ILSX = NUMLS(ILS)                                                 
  240 ISCTX = TABL11(ISCT)                                              
  250 RETURN                                                            
      END                                                               
