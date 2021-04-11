C*****THIS SUBROUTINE DIRECTS THE COMPUTATIONS THAT FIND THE AIR        
C*****FLOWRATE DERIVATIVES - HENCE ITS NAME QDERIV.                     
      SUBROUTINE QDERIV                                                 
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
C*****CREATE LOOP FORCING FUNCTIONS BY SUMMING SECTION FORCING FUNCTIONS
C                                                                       
C*****INITIALLY SET LOOP FORCING FUNCTIONS EQUAL TO ZERO                
      DO 10 ILP=1,NLOOP                                                 
10    OMEGLP(ILP)=0.0                                                   
C*****VENTILATION SHAFT FORCING FUNCTIONS - IF ANY                      
      IF(NVSEC) 30,30,20                                                
20    CALL OMEGA1                                                       
C*****LINE SECTION FORCING FUNCTIONS, INERTIAL TERMS, AND TRAIN HEAD    
C*****CHANGES - IF ANY                                                  
30    IF(NLSEC) 50,50,40                                                
40    CALL OMEGA2                                                       
C*****BRANCHED JUNCTION (3 BRANCHES OR MORE) FORCING FUNCTIONS AND TRAIN
C*****HEAD CHANGES - IF ANY                                             
50    IF(NBRJCT) 70,70,60                                               
60    CALL OMEGA3                                                       
C*****LOOP OVER SECTIONS ADDING SECTION FORCING FUNCTIONS TO LOOP       
C*****FORCING FUNCTIONS                                                 
70    DO 80 ISCT=1,NSEC                                                 
      NLOW=NDSS(ISCT)                                                   
      NHIGH=NDSS(ISCT+1)-1                                              
C*****LOOP OVER LOOPS PASSING THROUGH EACH SECTION ADDING SECTION       
C*****FORCING FUNCTION TO LOOP FORCING FUNCTIONS                        
      DO 80 ILKAN=NLOW,NHIGH                                            
      ILOOP=LKAN(ILKAN)                                                 
      JLOOP=IABS(ILOOP)                                                 
      ILOOP=JLOOP/ILOOP                                                 
80    OMEGLP(JLOOP)=OMEGLP(JLOOP)+ILOOP*OMEGS(ISCT)                     
C                                                                       
C*****IF NECESSARY COMPUTE                                              
C*****   (1)THE COEFFICIENTS OF THE AERODYNAMIC 'B' MATRIX (LOOP        
C*****      INERTIAL TERMS) AND STORE THEM IN ARRAY BLP                 
C*****   (2)THE TRAIN DRAG FROM THE TRAIN HEAD CHANGE                   
C                                                                       
C*****NECESSITY CHECK                                                   
      IF(KAERO) 140,90,140                                              
C*****SET PART OF ARRAY BLP THAT IS IN USE EQUAL TO ZERO                
90    DO 100 IBLP=1,BLPREQ                                              
100   BLP(IBLP)=0.0                                                     
C*****LOOP OVER SECTIONS                                                
      DO 110 ISCT=1,NSEC                                                
      NLOW=NDSS(ISCT)                                                   
      NHIGH=NDSS(ISCT+1)-1                                              
C*****LOOP OVER LOOPS THAT PASS THROUGH SECTIONS, COMPUTING LOCATIONS IN
C*****ARRAY BLP THAT BS MUST BE ADDED TO. IT SHOULD BE NOTED THAT       
C*****BECAUSE OF MATRIX SYMMETRY ONLY ONE HALF OF THE LOCATIONS ARE     
C*****COMPUTED AND USED. THIS IS REFLECTED IN THE RANGE OF THE INDEX    
C*****OF THE INNERMOST 'DO LOOP' OF THIS NEST.                          
      DO 110 ILKAN=NLOW,NHIGH                                           
      ILOOP=LKAN(ILKAN)                                                 
      JLOOP=IABS(ILOOP)                                                 
      ILOOP=JLOOP/ILOOP                                                 
      KKK=(JLOOP-1)*KBND1+1                                             
      DO 110 JLKAN=ILKAN,NHIGH                                          
      ILOOP2=LKAN(JLKAN)                                                
      JLOOP2=IABS(ILOOP2)                                               
      ILOOP2=JLOOP2/ILOOP2                                              
      IBLP=KKK+JLOOP2-JLOOP                                             
C*****ADD SECTION INERTIAL TERM TO LOOP INERTIAL TERM, USING SIGN TO    
C*****SHOW DIRECTION OF LOOP                                            
110   BLP(IBLP)=BLP(IBLP)+ILOOP*ILOOP2*BS(ISCT)                         
C*****CHECK FOR EXISTANCE OF OPERATIONAL TRAINS                         
      IF(NTRAN) 140,140,120                                             
C*****LOOP OVER OPERATIONAL TRAINS                                      
120   DO 130 ITRAN=1,NTRAN                                              
C*****INTERNAL OPERATING NUMBER OF TRAIN                                
      NUMV=IINDEX(ITRAN)                                                 
C*****TRAIN TYPE                                                        
      ITYP=ITYPV(NUMV)                                                  
130   DRAGV(NUMV)=RHOMAS*AV(ITYP)*DRAGV(NUMV)                           
140   CALL SIMQ2                                                        
      RETURN                                                            
      END                                                               
