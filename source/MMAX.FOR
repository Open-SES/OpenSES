      SUBROUTINE MMAX( VALUE, ARRAY, JTIME )                             
C     THIS SUBROUTINE STORES THE MAXIMUM VALUE IN AN ARRAY              
C     TOGETHER WITH THE TIME AT WHICH THIS MAXIMUM OCCURED              
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
                                                                        
      IF( VALUE - ARRAY ) 50,50,10                                      
C**** PRESENT VALUE IS GREATER - STORE IT AND TIME                      
   10 ARRAY = VALUE                                                     
      JTIME = NTIME                                                     
C **                                                                    
   50 RETURN                                                            
      END                                                               
