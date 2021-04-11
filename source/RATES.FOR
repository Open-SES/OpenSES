      SUBROUTINE RATES( NUMV, IROUTE, QACC, QDEC )                      
                                                                        
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INTEGER  DPV0, DPV1                                               
                                                                        
      DPV0=DPV(NUMV)                                                    
      DPV1=DPV0+1                                                       
    5 IF(TLPSV(NUMV)-TIMEV(DPV1,IROUTE)) 20,10,10                       
   10 DTIME = ( TIMEV(DPV1,IROUTE) - TIMEV(DPV0,IROUTE) ) /100.0        
      XLASTV(NUMV)=XLASTV(NUMV)+(SPEEDV(DPV1,IROUTE)+SPEEDV(DPV0,IROUTE)
     1 ) * DTIME / 2.0                                                  
      DPV(NUMV)=DPV(NUMV)+1                                             
      DPV0=DPV(NUMV)                                                    
      DPV1=DPV0+1                                                       
C---- IF THE TIME INTERVAL IS ZERO, GO TO THE NEXT POINT                
      KTIME = TIMEV(DPV1,IROUTE) - TIMEV(DPV0,IROUTE)                   
      IF( KTIME ) 10,10,15                                              
   15 DTIME = FLOAT( KTIME ) / 100.0                                    
      DUDTV(NUMV)=(SPEEDV(DPV1,IROUTE)-SPEEDV(DPV0,IROUTE))/DTIME       
      DGRDQV(NUMV) = (QGRIDV(DPV1,IROUTE)-QGRIDV(DPV0,IROUTE))/DTIME    
      GO TO 5                                                           
   20 DTIME2 = (TLPSV(NUMV) - TIMEV(DPV0,IROUTE) ) / 100.0              
      UV(NUMV)=SPEEDV(DPV0,IROUTE)+DUDTV(NUMV)*DTIME2                   
      XV(NUMV)=DBLE(XLASTV(NUMV)+(UV(NUMV)+SPEEDV(DPV0,IROUTE))*DTIME2/2
     1.0)                                                               
      QGRID = QGRIDV(DPV0,IROUTE) + DGRDQV(NUMV)*DTIME2                 
      IF( QGRID ) 60,50,50                                              
   50 QACC = QGRID                                                      
      QDEC = 0.0                                                        
      GO TO 70                                                          
   60 QACC = 0.0                                                        
      QDEC = - QGRID                                                    
   70 RETURN                                                            
      END                                                               
