      FUNCTION  TORQUE(  UTRN , ITYP  )                                 
C                                                                       
C     INPUT TO THIS SUBROUTINE IS TRAIN SPEED IN FEET / SECOND          
C          AND TYAIN TYPE.                                              
C     OUTPUT IS TRACTIVE EFFORT IN LBS PER MOTOR AT THE RIM OF THE WHEEL
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
                                                                        
C**** DETERMINE ON WHICH SECTION OF THE CURVE THE VELOCITY FALLS        
      IF( UTRN - SP2(ITYP) ) 10,10,40                                   
   10 IF( UTRN - SP1(ITYP) ) 20,20,30                                   
C**** ON FLAT PORTION                                                   
   20 TORQUE= TESP1( ITYP )                                             
      RETURN                                                            
C**** ON FIRST ORDER HYPERBOLIC PORTION                                 
   30 TORQUE = COATEV(ITYP) + COBTEV(ITYP) / UTRN                       
      RETURN                                                            
C**** ON SECOND ORDER HYPERBOLIC PORTION                                
   40 TORQUE = (COTEV(ITYP,3)/UTRN + COTEV(ITYP,2))/UTRN + COTEV(ITYP,1)
      RETURN                                                            
      END                                                               
