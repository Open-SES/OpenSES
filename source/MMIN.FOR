      SUBROUTINE MMIN( VALUE, ARRAY, JTIME )                             
                                                                        
C     THIS SUBROUTINE STORES THE MINIMUM VALUE IN AN ARRAY              
C     TOGETHER WITH THE TIME AT WHICH THIS MINIMUM OCCURED              
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
                                                                        
C                                                                       
                                                                        
      IF( VALUE - ARRAY ) 10,50,50                                      
C**** PRESENT VALUE IS LESS - STORE IT AND TIME                         
   10 ARRAY = VALUE                                                     
      JTIME = NTIME                                                     
C **                                                                    
   50 RETURN                                                            
      END                                                               
