      SUBROUTINE HOOKUP                                                 
C                                                                       
C                            TABLE OF CONTENTS                          
C                                                                       
C                                                                       
C  I      REFERENCES                                                    
C                                                                       
C  II     GENERAL  NOTES                                                
C                                                                       
C  III    DICTIONARY OF FORTRAN VARIABLES FOR SUBROUTINE HOOKUP         
C                                                                       
C  IV     LISTING OF SUBROUTINE HOOKUP                                  
C                                                                       
C                                                                       
C  I      REFERENCES                                                    
C                                                                       
C         (1) 'EFFICIENT CODE FOR STEADY-STATE FLOWS IN NETWORKS', BY   
C             ROBERT EPP AND ALVIN FOWLER, JOURNAL OF THE HYDRAULICS    
C             DIVISION, ASCE, VOL 96, NO. HY1, PAGES 43-56, JANUARY 1970
C                                                                       
C         (2) 'SHORTEST CONNECTION NETWORKS AND SOME GENERALIZATIONS',  
C             BY R.C. PRIM, THE BELL SYSTEM TECHNICAL JOURNAL, VOL 36,  
C             NO. 6, NOVEMBER,1957, PAGES 1389-1401                     
C                                                                       
C                                                                       
C  II     GENERAL SUBROUTINE NOTES                                      
C                                                                       
C         A. THIS SUBROUTINE HAS SEVEN MAJOR EXECUTION PHASES           
C            1. INITIALIZATION, SINGLE SECTION HOOKUP, AND CONSTRUCTION 
C               OF GEOMETRY TABLES NECESSARY TO PERFORM MULTIPLE SECTION
C               HOOKUP                                                  
C            2. FRAGMENTED NETWORK CHECK                                
C            3. LOOP CONSTRUCTION                                       
C            4. CONSTRUCTION OF ARRAYS LKAN, NDSS AND LPK               
C            5. DETERMINATION OF NEAR OPTIMAL LOOP NUMBERING            
C            6. OPTIONAL PRINTOUT OF ARRAYS LOOP, NDSS AND LKAN         
C            7. CONSTRUCTION OF ARRAYS TABL1 AND TABL2 USED FOR         
C               AERODYNAMIC JUNCTION(NODE) CALCULATIONS                 
C         B. THE DIMENSIONS OF ARRAYS ARE SHOWN IN THE DICTIONARY OF    
C            VARIABLES IN PARENTHESES AFTER THE ARRAY NAME. FOR EXAMPLE,
C            IL(LMNLOP) MEANS THAT IL IS DIMENSIONED LMNLOP.            
C         C. THE DIMENSIONING REQUIRED FOR ARRAYS LKAN, LOOP AND LPK HAS
C            BEEN ESTIMATED.                                            
C         D. VARIABLES LKAN, ND, NDS, AND NDSS ARE NOT LOCAL TO THIS    
C            SUBROUTINE - THEY ARE IN 'COMMON'.                         
C                                                                       
C                                                                       
C  III    DICTIONARY OF FORTRAN VARIABLES FOR SUBROUTINE HOOKUP         
C                                                                       
C IBACK  TRACE BACK PARAMETER WHICH TELLS THE PURGING ROUTINE WHERE TO  
C       RETURN                                                          
C                                                                       
C IKNTN  SUBSCRIPT USED IN REFERENCING ARRAY KNTN                       
C                                                                       
C IKYLP  A VARIABLE WHOSE MEANING DIFFERS THROUGHOUT THE SUBROUTINE.    
C       THEY ARE                                                        
C       1. DURING LOOP CONSTRUCTION IF A LOOP IS TO BE USED AS A KEY    
C          LOOP (I.E. THE FIRST LOOP USED) BY THE LOOP NUMBERING        
C          ALGORITHM IT IS ENTERED IN ARRAY IPL. IKYLP IS THE LOCATION  
C          OF THE NEXT ENTRY TO BE MADE IN IPL.                         
C       2. DURING THE LOOP NUMBERING ALGORITHM IKYLP IS THE ENTRY IN IPL
C          THAT IS CURRENTLY BEING USED AS THE KEY LOOP (I.E. FIRST LOOP
C          USED) IN A LOOP NUMBERING SCHEME. IT SHOULD BE NOTED THAT THE
C          LOOP NUMBERING ALGORITHM MAY REQUIRE TWO OR MORE KEY LOOPS.  
C          IF IT DOES THEN THE ADDITIONAL KEY LOOPS ARE FIRST GENERATED 
C          FROM IPL(JKYLP),JKYLP=NKYLP,NKYLP-1,NKYLP-2, ETC. IF JKYLP=0,
C          (NO MORE KEY LOOPS AVAILABLE FROM IPL) THEN ANY LOOP THAT HAS
C          NOT YET BEEN NUMBERED IS CHOSEN AS A KEY LOOP.               
C                                                                       
C IL(LMNLOP)  GIVES THE NEW LOOP NUMBER AS A FUNCTION OF THE OLD LOOP   
C       NUMBER. THE OLD LOOP NUMBERS ARE ASSIGNED TO THE LOOPS AS THEY  
C       ARE DETERMINED - THE FIRST LOOP FOUND BEING NUMBERED ONE, ETC.  
C       THE NEW LOOP NUMBERS ARE ASSIGNED TO THE LOOPS IN SUCH A MANNER 
C       THAT THE BANDWIDTH OF THE 'DQLP/DT' MATRIX IS MINIMIZED AS MUCH 
C       AS POSSIBLE.                                                    
C                                                                       
C ILOOP  LOOP NUMBER - OFTEN USED AS A SUBSCRIPT FOR REFERENCING LOOP   
C       ARRAYS                                                          
C                                                                       
C INODE  NODE NUMBER - OFTEN USED AS A SUBSCRIPT FOR REFERENCING NODE   
C       ARRAYS                                                          
C                                                                       
C IP(LMNLOP)  GIVES THE OLD LOOP NUMBER AS A FUNCTION OF THE NEW LOOP   
C       NUMBER. THE OLD LOOP NUMBERS ARE ASSIGNED TO THE LOOPS AS THEY  
C       ARE DETERMINED - THE FIRST LOOP FOUND BEING NUMBERED ONE, ETC.  
C       THE NEW LOOP NUMBERS ARE ASSIGNED TO THE LOOPS IN SUCH A MANNER 
C       THAT THE BANDWIDTH OF THE 'DQLP/DT' MATRIX IS MINIMIZED AS MUCH 
C       AS POSSIBLE.                                                    
C                                                                       
C IPL(LMNLOP)  A LIST OF LOOPS WHICH ARE TO BE USED AS KEY LOOPS BY THE 
C       LOOP NUMBERING ALGORITHM. A LOOP IS INCLUDED IN IPL AS A KEY    
C       LOOP IF IT SATISFYS THE HEURISTIC CRITERIA OF EITHER PASSING    
C       THROUGH A NODE WHOSE DEGREE IS TWO OR PASSING THROUGH MORE THAN 
C       SEVEN SECTIONS                                                  
C                                                                       
C ISCT   SECTION NUMBER - OFTEN USED AS A SUBSCRIPT FOR REFERENCING     
C       SECTION ARRAYS                                                  
C                                                                       
C JKYLP  SEE IKYLP                                                      
C                                                                       
C JNODE  THE NUMBER OF NODES IN THE REDUCED NETWORK. IF A NETWORK HAS   
C       TWO OR MORE NODES THAT ARE EXTERNAL TO IT (I.E. SOME OF ITS     
C       NODES REPRESENT SECTION TERMINATIONS AT THE ATMOSPHERE) THEN    
C       JNODE IS THE NUMBER OF NODES THAT ARE INTERNAL TO THE NETWORK   
C       PLUS ONE. IF A NETWORK HAS INTERNAL NODES ONLY THEN JNODE EQUALS
C       NNODE. THE NUMBER OF SECTIONS JOINING AT A NODE IS THE DEGREE OF
C       THE NODE. A NODE THAT IS INTERNAL TO THE NETWORK HAS TWO OR MORE
C       SECTIONS JOINING AT IT AND THUS IS ALWAYS OF DEGREE TWO OR      
C       GREATER. A NODE THAT IS EXTERNAL TO THE NETWORK ALWAYS HAS      
C       DEGREE ONE. NOW, SINCE BY DEFINITION NO ENERGY CHANGES OCCUR IN 
C       THE ATMOSPHERE ALL EXTERNAL NODES MAY BE REPLACED BY A SINGLE   
C       NODE (NODAMB) AT WHICH THE SECTIONS TERMINATING AT THE          
C       ATMOSPHERE MEET. THE PROCESS OF COMBINING ALL THESE EXTERNAL    
C       NODES INTO A SINGLE NODE IS KNOWN AS REDUCING THE NETWORK.      
C                                                                       
C JSCT   SECTION NUMBER - OFTEN USED AS A SUBSCRIPT FOR REFERENCING     
C       SECTION ARRAYS                                                  
C                                                                       
C KBND1  THE AERODYNAMIC SUBPROGRAM CONTINUALLY SOLVES THE SYSTEM OF    
C       EQUATIONS B*DQLP/DT=OMEGLP WHERE THE LEFT-HAND MATRIX B IS      
C       SYMMETRIC, POSITIVE DEFINITE AND OF KNOWN HALF BANDWIDTH KBND1. 
C       IN GENERAL, A BANDED MATRIX IS DESCRIBED BY AN UPPER BANDWIDTH  
C       (THE NUMBER OF UPPER DIAGONALS NECESSARY TO INCLUDE ALL THE     
C       NON-ZERO ELEMENTS PLUS ONE - THE PRINCIPLE DIAGONAL) AND A LOWER
C       BANDWIDTH (THE NUMBER OF LOWER DIAGONALS NECESSARY TO INCLUDE   
C       ALL THE NON-ZERO ELEMENTS PLUS ONE - THE PRINCIPLE DIAGONAL).   
C       HOWEVER, A SYMMETRIC MATRIX NEED ONLY BE DESCRIBED BY THE HALF  
C       BANDWIDTH, WHICH IS EITHER THE UPPER OR LOWER BANDWIDTH AND     
C       FORMALLY IS THE NUMBER M SUCH THAT C(I,J)=0.0 FOR ABS(I-J)      
C       GREATER THAN OR EQUAL TO M.                                     
C                                                                       
C KBND2,KBND3  CONSTANTS REQUIRED BY SUBROUTINE SIMQ2                   
C                                                                       
C KEYND  THE KEY NODE IN THE LOOP CURRENTLY BEING CONSTRUCTED. IF       
C       POSSIBLE, KEYND IS CHOSEN AS A NODE OF DEGREE TWO, OTHERWISE IT 
C       IS A NODE OF MINIMAL DEGREE.                                    
C                                                                       
C KM1    DURING LOOP CONSTRUCTION KM1 IS SET EQUAL TO ONE IF THE LOOP   
C       JUST CONSTRUCTED IS TO BE ENTERED IN IPL. IF KM1 EQUALS ZERO    
C       THEN NO ENTRY IS TO BE MADE IN IPL.                             
C                                                                       
C KMAX   POINTS TO THE ENTRY IN THE KEY LOOP LIST IPL WHICH GAVE, WHEN  
C       USED AS THE KEY LOOP, THE SMALLEST BANDWIDTH.                   
C                                                                       
C KNT    DURING LOOP CONSTRUCTION KNT IS THE LOCATION IN ARRAY LPSTC    
C       WHERE THE NEXT ENTRY WILL BE MADE.                              
C                                                                       
C KNTN(2*LMSECT)  GIVES FOR EACH NODE THE SECTIONS THAT ARE CONNECTED AT
C       IT. THE RANGE OF LOCATIONS IN KNTN THAT THE SECTION INFORMATION 
C       FOR EACH NODE IS STORED IS GIVEN BY NDP.                        
C                                                                       
C KPIPE(LMNODE)  IF NODE INODE HAS BEEN LINKED TO NODE NR1, KPIPE(INODE)
C       IS THE SIGNED NUMBER OF THE SECTION LINKING NODE INODE BACK     
C       TOWARDS NODE NR1. IT IS POSITIVE IF THE SECTION IS DIRECTED     
C       TOWARDS NODE INODE AND NEGATIVE IF THE SECTION IS DIRECTED AWAY 
C       FROM NODE INODE.                                                
C                                                                       
C LKAN(LMLPLK)  GIVES FOR EACH SECTION THE LOOPS WHOSE AIR VOLUME       
C       FLOW RATES QLP MUST BE ALGEBRAICALLY SUMMED TO GIVE THE SECTION 
C       AIR VOLUME FLOW RATE QS. THE SUMMATION CONVENTION IS IN         
C       ACCORDANCE WITH THE SIGNING OF LKAN. THE RANGE OF LOCATIONS IN  
C       LKAN THAT THE LOOP INFORMATION IS STORED FOR EACH SECTION IS    
C       GIVEN BY NDSS. THE NUMBER OF ENTRIES IN LKAN IS ALWAYS EQUAL TO 
C       THE NUMBER OF ENTRIES IN LOOP. SEE GENERAL SUBROUTINE NOTES     
C       ABOVE.                                                          
C                                                                       
C LMLPK  ARRAY SIZE LIMIT FOR VARIABLE LPK. IT EQUALS 3*LMNLOP. THIS    
C       REQUIREMENT OF 3*LMNLOP HAS BEEN ESTIMATED.                     
C                                                                       
C LMLPLK  ARRAY SIZE LIMIT FOR VARIABLES LOOP AND LKAN. IT EQUALS       
C       3*LMSECT. THIS REQUIREMENT OF 3*LMSECT HAS BEEN ESTIMATED.      
C                                                                       
C LMNLOP  ARRAY SIZE LIMIT FOR THE NUMBER OF LOOPS ALLOWED IN A NETWORK.
C       LMNLOP NEVER HAS TO BE GREATER THAN LMSECT MINUS ONE SINCE THE  
C       MAXIMUM NUMBER OF LOOPS A NETWORK CAN HAVE IS THE NUMBER OF     
C       SECTIONS MINUS ONE. QUITE OFTEN NETWORKS OCCUR THAT HAVE NLOOP  
C       ONLY 50-75 PERCENT OF NSEC.                                     
C                                                                       
C LMNODE  ARRAY SIZE LIMIT FOR THE NUMBER OF NODES ALLOWED IN A NETWORK.
C       LMNODE NEVER HAS TO BE GREATER THAN LMSECT PLUS ONE SINCE THE   
C       MAXIMUM NUMBER OF NODES A NETWORK CAN HAVE IS THE NUMBER OF     
C       SECTIONS PLUS ONE. QUITE OFTEN NETWORKS OCCUR THAT HAVE NNODE   
C       ONLY 80-90 PERCENT OF NSEC.                                     
C                                                                       
C LMSECT  ARRAY SIZE LIMIT FOR THE NUMBER OF SECTIONS ALLOWED IN A      
C       NETWORK.                                                        
C                                                                       
C LNODE(LMNODE)  A VARIABLE WHOSE MEANING DIFFERS THROUGHOUT THE        
C       SUBROUTINE. THEY ARE                                            
C       1. DURING THE INITIALIZATION PHASE OF THE SUBROUTINE IF         
C          LNODE(INODE)=1 THE NODE NUMBER INODE HAS BEEN AT LEAST       
C          TEMPORARILY DELETED FROM USE IN THE INTERNAL NODE NUMBERING  
C          BY THE REDUCTION OF THE NETWORK.                             
C       2. DURING THE FRAGMENTED NETWORK CHECK IF NODE INODE HAS BEEN   
C          LINKED TO THE NETWORK LNODE(INODE) EQUALS ONE, OTHERWISE     
C          LNODE(INODE) EQUALS ZERO.                                    
C       3. DURING LOOP CONSTRUCTION IF NODE INODE HAS BEEN LINKED TO    
C          NODE NR1, LNODE(INODE) IS THE NODE IMMEDIATELY PRECEDING NODE
C          INODE IN THE PATH FROM NODE NR1 TO NODE INODE. IF NODE INODE 
C          HAS NOT BEEN SO LINKED, LNODE(INODE) EQUALS ZERO.            
C                                                                       
C LOOP(LMLPLK)  GIVES FOR EACH LOOP THE SECTIONS THAT ARE IN IT. THE    
C       SIGNING CONVENTION OF LOOP IS IN ACCORDANCE WITH THE DIRECTION  
C       IN WHICH THE LOOP PASSES THROUGH THE SECTION. THE RANGE OF      
C       LOCATIONS IN LOOP THAT THE SECTION INFORMATION IS STORED FOR    
C       EACH LOOP IS GIVEN BY LOPT. THE SECTIONS ARE NOT NECESSARILY    
C       STORED IN LOOP IN THE PHYSICAL ORDER IN WHICH ONE COULD 'TRAVEL 
C       AROUND' THE LOOP. THE NUMBER OF ENTRIES IN LOOP IS ALWAYS EQUAL 
C       TO THE NUMBER OF ENTRIES IN LKAN. SEE GENERAL SUBROUTINE NOTES  
C       ABOVE.                                                          
C                                                                       
C LOPT(LMNLOP+1)  GIVES FOR EACH LOOP THE STARTING LOCATION IN LOOP OF  
C       ITS SECTION INFORMATION. THEREFORE, FOR EACH 'OLD' LOOP NUMBER  
C       ILOOP, THE SECTIONS THAT IT PASSES THROUGH ARE LOCATED IN LOOP  
C       FROM LOCATIONS LOPT(ILOOP) TO LOPT(ILOOP+1)-1. (ALSO SEE IP)    
C                                                                       
C LPK(LMLPK)  GIVES THE LOOPS THAT A LOOP IS ADJACENT TO IN THE SENSE   
C       OF SHARING ONE OR MORE SECTIONS. LOOPS I AND J ARE ADJACENT TO  
C       EACH OTHER IF THEY PASS THROUGH THE SAME SECTION OR SECTIONS.   
C       THE RANGE OF LOCATIONS IN LPK THAT THE LOOP INFORMATION FOR EACH
C       LOOP IS STORED IS GIVEN BY NND. SEE GENERAL SUBROUTINE NOTES    
C       ABOVE.                                                          
C                                                                       
C LPKT   A VARIABLE WHOSE MEANING DIFFERS THROUGHOUT THE SUBROUTINE.    
C       THEY ARE                                                        
C       1. DURING THE CONSTRUCTION OF LOOPS LPKT IS THE LOCATION OF THE 
C          NEXT ENTRY TO BE MADE IN ARRAY LOOP.                         
C       2. DURING THE CONSTRUCTION OF ARRAY LKAN LPKT IS THE TOTAL      
C          NUMBER OF ENTRIES TO BE MADE IN ARRAY LKAN.                  
C       3. DURING THE CONSTRUCTION OF ARRAY LPK LPKT IS THE LOCATION OF 
C          THE NEXT ENTRY TO BE MADE IN ARRAY LPK.                      
C                                                                       
C LPRP(LMSECT)  IF SECTION ISCT HAS BEEN REMOVED FROM THE NETWORK,      
C       LPRP(ISCT) IS ZERO, OTHERWISE LPRP(ISCT) EQUALS ONE.            
C                                                                       
C LPSTC(LMNODE-1)  THE LIST OF NODES THAT HAVE BEEN LINKED TO NR1 DURING
C       CONSTRUCTION OF AN LOOP                                         
C                                                                       
C MAXI  A VARIABLE USED DURING THE LOOP NUMBERING PHASE OF THE         
C       SUBROUTINE. IT HAS DIFFERENT MEANINGS IN THIS PHASE OF THE      
C       SUBROUTINE. THEY ARE                                            
C       1. IT IS USED TO COMPUTE NNRST.                                 
C       2. IT IS USED TO SORT NNRST AS A MINIMUM VALUE FOUND.           
C       3. IT IS USED TO DETERMINE THE HALF BANDWIDTH OF THE 'DQLP/DT'  
C          MATRIX GENERATED BY A LOOP NUMBERING. IN THIS CASE IT IS THE 
C          HALF BANDWIDTH MINUS ONE.                                    
C                                                                       
C MAXP  THE MINIMUM VALUE OF MAXI FOUND DURING THE LOOP NUMBERING PHASE 
C       OF THE SUBROUTINE FOR THE CASE WHEN MAXI IS BEING USED TO 
C       DETERMINE THE BANDWIDTH GENERATED BY A LOOP NUMBERING.                    
C                                                                       
C N1(LMSECT)  GIVES THE NODE NUMBER OF THE NODE AT THE BEGINNING (-) END
C       OF A SECTION AS A FUNCTION OF THE SECTION NUMBER                
C                                                                       
C N2(LMSECT)  GIVES THE NODE NUMBER OF THE NODE AT THE ENDING (+) END OF
C       A SECTION AS A FUNCTION OF THE SECTION NUMBER                   
C                                                                       
C NA (LMSCND*(LMSCND-1))    (DIMENSIONED THE MAXIMUM NUMBER OF LOOPS    
C       THAT MAY PASS THROUGH A NODE) NA IS A WORK ARRAY CONSISTING OF  
C       LOOPS PASSING THROUGH A NODE AND IS USED IN THE CONSTRUCTION OF 
C       ARRAY TABL2                                                     
C                                                                       
C NA1    NODE NUMBER - OFTEN THE ONE AT THE BEGINNING (-) END OF A      
C       SECTION                                                         
C                                                                       
C NA2    NODE NUMBER - OFTEN THE ONE AT THE ENDING (+) END OF A SECTION 
C                                                                       
C NB (LMSCND*(LMSCND-1))    (DIMENSIONED THE MAXIMUM NUMBER OF LOOPS    
C       THAT MAY PASS THROUGH A NODE) NB IS A WORK ARRAY CONSISTING OF  
C       SECTIONS MEETING AT A NODE AND IS USED IN THE CONSTRUCTION OF   
C       ARRAY TABL2                                                     
C                                                                       
C NCUNT  OUTPUT LINE COUNTER                                            
C                                                                       
C ND(LMNODE OR LMSECT - THE GREATER)  AN ARRAY WHOSE USAGE DIFFERS      
C       THROUGHOUT THE SUBROUTINE. AMONG ITS MEANINGS ARE               
C       1. IT GIVES THE NUMBER OF SECTIONS THAT FORM A NODE (THE DEGREE 
C          OF THE NODE) AS A FUNCTION OF THE NODE NUMBER. AS THE        
C          CONSTRUCTION OF THE LOOPS PROCEEDS ND IS REDUCED AS VARIOUS  
C          SECTIONS ARE PURGED FROM THE NETWORK.                        
C       2. DURING CONSTRUCTION OF ARRAY LKAN, ARRAY ND IS INITIALIZED SO
C          THAT ND(ISCT) IS THE NUMBER OF LOOPS CONTAINING SECTION ISCT.
C                                                                       
C NDL1  THE DEGREE OF NODE KEYND IF NODE KEYND IS NOT OF DEGREE TWO.    
C                                                                       
C NDP(LMNODE+1) GIVES FOR EACH NODE THE STARTING LOCATION IN KNTN OF ITS
C       SECTION INFORMATION. THEREFORE, FOR EACH NODE INODE, THE        
C       SECTIONS THAT ARE CONNECTED AT IT ARE LOCATED IN KNTN FROM      
C       LOCATIONS NDP(INODE) TO NDP(INODE+1)-1.                         
C                                                                       
C NDS(LMSECT+1)  USED DURING THE CONSTRUCTION OF ARRAY LKAN. WHEN LOOP  
C       ILOOP IS FOUND TO CONTAIN SECTION ISCT ILOOP IS ENTERED IN      
C       LKAN(NDS(ISCT)). NDS(ISCT) IS THEN INCREMENTED BY ONE. BECAUSE  
C       OF THE SPECIAL EQUIVALENCE BETWEEN NDSS AND NDS, NDSS IS        
C       CONSTRUCTED AUTOMATICALLY. BECAUSE ND(ISCT) IS INITIALIZED TO   
C       EQUAL THE NUMBER OF LOOPS CONTAINING SECTION ISCT, NDS IS       
C       INITIALIZED BY SETTING NDS(=1),NDS(ISCT)=NDS(ISCT-1)+ND(ISCT-1).
C       SINCE ND IS NO LONGER NEEDED AFTER THIS INITIALIZATION, NDS     
C       OVERLAPS NDSS BY AN EQUIVALENCE. WHEN ARRAY LKAN IS COMPLETED,  
C       NDS(ISCT) POINTS TO THE ENTRY IN LKAN WHICH BEGINS THE LOOP     
C       INFORMATION FOR SECTION ISCT+1. CONSEQUENTLY, BY THE SPECIAL    
C       EQUIVALENCE BETWEEN NDS AND NDSS, NDSS(ISCT) IS AUTOMATICALLY   
C       THE PROPER POINTER TO LKAN FOR SECTION ISCT.                    
C                                                                       
C NDSS(LMSECT+2)  FOR EACH SECTION NDSS GIVES THE STARTING LOCATION IN  
C       LKAN OF ITS LOOP INFORMATION. THEREFORE, FOR EACH SECTION ISCT, 
C       THE LOOP AIR VOLUME FLOW RATES QLP THAT MUST BE ALGEBRAICALLY   
C       SUMMED TO GIVE THE SECTION AIR VOLUME FLOW RATE QS ARE LOCATED  
C       IN LKAN FROM NDSS(ISCT) TO NDSS(ISCT+1)-1. THE SUMMATION        
C       CONVENTION IS IN ACCORDANCE WITH THE SIGNING OF LKAN.           
C                                                                       
C NLOW   LOWER RANGE OF 'DO' LOOP INDEX                                 
C                                                                       
C NHIGH  UPPER RANGE OF 'DO' LOOP INDEX                                 
C                                                                       
C NND(LMNLOP+1)  AFTER THE CONSTRUCTION OF ARRAY LPK IT GIVES FOR EACH  
C       LOOP THE STARTING LOCATION IN LPK OF ITS LOOP INFORMATION.      
C       THEREFORE, FOR EACH LOOP ILOOP, THE LOOPS THAT ARE ADJACENT TO  
C       IT ARE LOCATED IN LPK FROM LOCATIONS NND(ILOOP) TO              
C       NND(ILOOP+1)-1.                                                 
C                                                                       
C NLOOP  THE NUMBER OF LOOPS IN THE NETWORK. IT IS EQUAL TO THE NUMBER  
C       OF EQUATIONS THAT MUST BE SIMULTANEOUSLY SOLVED CONTINUALLY BY  
C       THE AERODYNAMIC SUBPROGRAM.                                     
C                                                                       
C NNODE  THE NUMBER OF NODES IN THE ACTUAL NETWORK. THIS MAY OR MAY NOT 
C       BE EQUAL TO THE NUMBER OF NODES IN THE REDUCED NETWORK. ALSO SEE
C       JNODE                                                           
C                                                                       
C NODAMB  SEE JNODE                                                     
C                                                                       
C NR1,NR2 IF NODE KEYND IS OF DEGREE TWO, NR1 AND NR2 ARE THE NODES     
C       ADJACENT TO IT. IF NODE KEYND HAS A DEGREE GREATER THAN TWO     
C       (I.E. NDL1), THEN NR2=KEYND AND NR1 IS A NODE ADJACENT TO KEYND.
C       IN BOTH CASES AN ADDITIONAL PATH FROM NR1 TO NR2 MUST BE FOUND  
C       TO COMPLETE THE LOOP.                                           
C                                                                       
C NRST(LMNODE)  AN ARRAY WHOSE USAGE DIFFERS THROUGHOUT THE SUBROUTINE. 
C       MEANINGS ARE                                                    
C       1. DURING LOOP CONSTRUCTION A LIST OF NODES OF DEGREE TWO.      
C          ALTHOUGH A NODE IS OF DEGREE TWO WHEN ENTERED IN NRST,       
C          SUBSEQUENT PURGING MAY ELIMINATE THE NODE ENTIRELY. HENCE,   
C          EACH ENTRY IN NRST MUST BE TESTED TO MAKE SURE IT IS STILL OF
C          DEGREE TWO.                                                  
C       2. DURING LOOP NUMBERING A LIST OF UNNUMBERED LOOPS ADJACENT TO 
C          LOOP IP(K).                                                  
C       3. DURING OPTIONAL PRINTOUT OF ARRAYS LOOP, NDSS AND LKAN IT    
C          STORES FOR PRINTING THE EXTERNAL NUMBERS OF ALL THE SECTIONS 
C          IN THE SINGLE LOOP BEING PRINTED.                            
C                                                                       
C NNRST(LMNODE/2)  THE ENTRIES OF NNRST GIVE THE TOTAL NUMBER OF        
C       UNNUMBERED LOOPS ADJACENT TO THE CORRESPONDING ENTRY IN NRST.   
C                                                                       
C NSEC   THE NUMBER OF SECTIONS IN THE NETWORK                          
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C
      INTEGER   IL(LMNLOP), IP(LMNLOP), IPL(LMNLOP), KNTN(2*LMSECT)
      INTEGER   KPIPE(LMNODE), LNODE(LMNODE), LOOP(LMLPLK)
      INTEGER   LOPT(LMNLOP+1), LPK(LMLPK), LPRP(LMSECT)
      INTEGER   LPSTC(LMNODE-1), N1(LMSECT), N2(LMSECT)
      INTEGER   NDP(LMNODE+1), NND(LMNLOP+1), NNRST(LMNODE/2)
      INTEGER   NRST(LMNODE), NA(LMSCND*(LMSCND-1))
      INTEGER   NB(LMSCND*(LMSCND-1))
      EQUIVALENCE (NNRST(1),NRST((LMNODE/2)+1))                  
                                                                       
                                                                       
1     FORMAT (A1)
10    FORMAT ( / ,30X,'SECTION',I5)                                     
20    FORMAT ( / ,30X,I5)                                               
30    FORMAT ( / ,10X,I5,' ELEMENTS REQUIRED',I5,' ELEMENTS AVAILABLE') 
40    FORMAT (T40,'LOOP TABLE',//,' LOOP     EXTERNAL NUMBERS OF SECTION
     1S                        ',/10X,52('-'),/,
     2'NUMBER    INTERNAL NUMBERS OF SECTIONS-CONTAINED IN ARRAY LOOP')                
50    FORMAT ( / ,I4,5X,22I5,/,T10,22I5)                                
60    FORMAT (T10,22I5,/,T10,22I5)                                   
70    FORMAT (/////,T4,'HALF-BANDWIDTH OF AERODYNAMIC RESPONSE MATRIX ='
     1,I3,//,T4,'ARRAY BLP DATA',I6,' ELEMENTS REQUIRED',/,T18,I6,
     2          ' ELEMENTS AVAILABLE')                                                          
80    FORMAT (46X,'CONTINUITY TABLE',//,'INTERNAL  EXTERNAL  STORAGE LOC
     1ATIONS   LOOPS THAT PASS THROUGH SECTION - SIGN SHOWS DIRECTION WI
     2TH RESPECT TO SECTION',/,'SECTION   SECTION     IN ARRAY LKAN',/,
     3'NUMBER    NUMBER      FROM       TO')                   
90    FORMAT ( / ,I4,I10,I12,I9,2X,16I5,/,37X,16I5)                     
100   FORMAT (16X,'THERE HAS BEEN AN UNHAPPY HOOKUP')               
110   FORMAT (16X,'TABLES OF LOOPS PASSING THROUGH BRANCHED JUNCTIONS',
     1///,8X,'INTERNAL  EXTERNAL   LOOP      FROM SECTION         TO SEC
     2TION',/,10X,'NODE      NODE    NUMBER  INTERNAL  EXTERNAL  INTERNA
     3L  EXTERNAL',/, 9X,'NUMBER    NUMBER',12X,'NUMBER    NUMBER    NUM
     4BER    NUMBER',/)                                          
120   FORMAT ( / ,I13,I10,5X,'STRAIGHT-THROUGH JUNCTION OR PORTAL')     
130   FORMAT ( / ,I13,I10,2I9,3I10)                                     
C                                                                       
C*****INITIALIZATION, SINGLE SECTION HOOKUP, AND CONSTRUCTION OF        
C*****GEOMETRY TABLES NECESSARY TO PERFORM MULTIPLE SECTION HOOKUP      
C                                                                       
      NDSS(1)=1                                                         
C*****CHECK FOR SINGLE OR MULTIPLE SECTION HOOKUP                       
      IF (NSEC-1) 1810,140,150                                          
C*****PERFORM SINGLE SECTION HOOKUP AND BRANCH TO OPTIONAL PRINTOUT     
140   NLOOP=1                                                           
      KBND1=1                                                           
      BLPREQ=1           
      IP(1)=1                                                           
      LKAN(1)=1                                                         
      LOOP(1)=1                                                         
      LOPT(1)=1                                                         
      LOPT(2)=2                                                         
      NDSS(2)=2                                                         
      GO TO 1720                                                        
C*****ARRAY INITIALIZATION REQUIRED FOR MULTIPLE SECTION HOOKUP         
150   DO 160 INODE=1,NNODE                                              
      LNODE(INODE)=0                                                    
      ND(INODE)=0                                                       
      NDP(INODE)=0                                                      
160   TABL1(INODE)=1                                                    
      NDP(NNODE+1)=0                                                    
      TABL1(NNODE+1)=1                                                  
      NUMY1=LMNLOP+1                                                    
      DO 170 ILOOP=1,NUMY1                                              
170   NND(ILOOP)=0                                                      
      DO 180 ISCT=1,NSEC                                                
180   LPRP(ISCT)=1                                                      
C*****COPY NETWORK GEOMETRY INTO TABLES REQUIRED BY HOOKUP, COMPUTE THE 
C*****NUMBER OF NODES OF DEGREE ONE AND CHECK FOR ISOLATED SECTIONS     
      NUMY3=0                                                           
      DO 240 ISCT=1,NSEC                                                
      N1(ISCT)=TABL12(ISCT)                                             
      N2(ISCT)=TABL13(ISCT)                                             
      NUMY1=N1(ISCT)                                                    
      NUMY2=N2(ISCT)                                                    
      NUMY1=TABL16(NUMY1)                                               
      NUMY2=TABL16(NUMY2)                                               
      IF (NUMY1-1) 190,190,200                                          
190   NUMY3=NUMY3+1                                                     
200   IF (NUMY2-1) 210,210,220                                          
210   NUMY3=NUMY3+1                                                     
220   IF (NUMY1+NUMY2-2) 230,230,240                                    
230   CALL EERROR (196)                                                  
      WRITE (OUT,10) TABL11(ISCT)                                       
      GO TO 1810                                                        
240   CONTINUE                                                          
C*****CHECK FOR ERROR OF NETWORK HAVING ONE AND ONLY ONE DEGREE ONE NODE
      IF (NUMY3-1) 250,250,260                                          
250   CALL EERROR (197)                                                  
      GO TO 1810                                                        
260   JNODE=NNODE                                                       
C*****FIND FIRST NODE OF DEGREE ONE (IF ANY) AND USE IT AS THE AMBIENT  
C*****NODE                                                              
      DO 290 ISCT=1,NSEC                                                
      NUMY1=N1(ISCT)                                                    
      IF (TABL16(NUMY1)-1) 280,270,280                                  
270   NODAMB=NUMY1                                                      
      GO TO 300                                                         
280   NUMY1=N2(ISCT)                                                    
      IF (TABL16(NUMY1)-1) 290,270,290                                  
290   CONTINUE                                                          
      GO TO 430                                                         
C*****CHANGE ALL NODES OF DEGREE ONE TO THE AMBIENT NODE (REDUCING THE  
C*****NETWORK) AND COMPUTE THE NUMBER OF NODES IN THE REDUCED NETWORK   
300   DO 340 ISCT=1,NSEC                                                
      INODE=N1(ISCT)                                                    
      IF (TABL16(INODE)-1) 320,310,320                                  
310   N1(ISCT)=NODAMB                                                   
      JNODE=JNODE-1                                                     
      INODE=NODAMB                                                      
320   LNODE(INODE)=1                                                    
      INODE=N2(ISCT)                                                    
      IF (TABL16(INODE)-1) 340,330,340                                  
330   N2(ISCT)=NODAMB                                                   
      JNODE=JNODE-1                                                     
      INODE=NODAMB                                                      
340   LNODE(INODE)=1                                                    
      JNODE=JNODE+1                                                     
C*****SINCE THE INTERNAL NODE NUMBERING IS NO LONGER CONSECUTIVE IT IS  
C*****NOW COMPRESSED TO A CONSECUTIVE NUMBERING FROM ONE TO JNODE. FOR  
C*****EXAMPLE, IF AFTER NETWORK REDUCTION INTERNAL NODE NUMBERS 1,2,4,5,
C*****6,8,11, AND 12 ARE IN USE BUT NODE NUMBERS 3,7,9,10 ARE NOT THEN: 
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER  4 ARE RENUMBERED 3   
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER  5 ARE RENUMBERED 4   
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER  6 ARE RENUMBERED 5   
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER  8 ARE RENUMBERED 6   
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER 11 ARE RENUMBERED 7   
C*****  ALL NODES HAVING THE INTERNAL NODE NUMBER 12 ARE RENUMBERED 8   
350   DO 360 INODE=1,JNODE                                              
      NLOW=INODE                                                        
      IF (LNODE(INODE)) 360,370,360                                     
360   CONTINUE                                                          
      GO TO 430                                                         
370   NLOW=NLOW+1                                                       
      DO 380 INODE=NLOW,NNODE                                           
380   LNODE(INODE-1)=LNODE(INODE)                                       
      DO 420 ISCT=1,NSEC                                                
      IF (N1(ISCT)-NLOW) 400,390,390                                    
390   N1(ISCT)=N1(ISCT)-1                                               
400   IF (N2(ISCT)-NLOW) 420,410,410                                    
410   N2(ISCT)=N2(ISCT)-1                                               
420   CONTINUE                                                          
      GO TO 350                                                         
C*****CONSTRUCT ARRAYS ND, NDP, AND KNTN                                
430   DO 440 ISCT=1,NSEC                                                
      NUMY1=N1(ISCT)                                                    
      ND(NUMY1)=ND(NUMY1)+1                                             
      NUMY1=N2(ISCT)                                                    
440   ND(NUMY1)=ND(NUMY1)+1                                             
      NDP(1)=1                                                          
      DO 450 INODE=1,JNODE                                              
450   NDP(INODE+1)=NDP(INODE)+ND(INODE)                                 
      DO 460 ISCT=1,NSEC                                                
      NA1=N1(ISCT)                                                      
      IKNTN=NDP(NA1)                                                    
      NDP(NA1)=IKNTN+1                                                  
      KNTN(IKNTN)=ISCT                                                  
      NA2=N2(ISCT)                                                      
      IKNTN=NDP(NA2)                                                    
      NDP(NA2)=IKNTN+1                                                  
460   KNTN(IKNTN)=ISCT                                                  
      DO 470 INODE=1,JNODE                                              
470   NDP(INODE)=NDP(INODE)-ND(INODE)                                   
C                                                                       
C*****FRAGMENTED NETWORK CHECK                                          
C                                                                       
      LNODE(1)=1                                                        
      DO 480 INODE=2,JNODE                                              
480   LNODE(INODE)=0                                                    
      K=1                                                               
      NRST(1)=1                                                         
      L=1                                                               
490   NA1=NRST(K)                                                       
      K=K+1                                                             
C*****FIND ALL NODES ADJACENT TO NODE NA1                               
      NLOW=NDP(NA1)                                                     
      NHIGH=NDP(NA1+1)-1                                                
      DO 530 IKNTN=NLOW,NHIGH                                           
      ISCT=KNTN(IKNTN)                                                  
      NA2=N2(ISCT)                                                      
      IF (NA2-NA1) 510,500,510                                          
500   NA2=N1(ISCT)                                                      
C*****IF NODE NA2 HAS NOT PREVIOUSLY BEEN LINKED TO NODE ONE, ENTER NA2 
C*****IN NRST AND SET LNODE(NA2) TO ONE                                 
510   IF (LNODE(NA2)) 530,520,530                                       
520   LNODE(NA2)=1                                                      
      L=L+1                                                             
      NRST(L)=NA2                                                       
530   CONTINUE                                                          
      IF (L-K) 540,490,490                                              
C*****CHECK IF ALL NODES HAVE BEEN LINKED TO NODE ONE                   
540   IF (L-JNODE) 550,580,550                                          
550   CALL EERROR (189)                                                  
C*****WRITE OUT SECTIONS THAT ARE FRAGMENTED FROM THE NODE HAVING THE   
C*****INTERNAL NUMBER ONE                                               
C     WRITE(OUT,11) TABL15(1)                                           
      DO 570 ISCT=1,NSEC                                                
      NUMY1=N1(ISCT)                                                    
      IF (LNODE(NUMY1)) 570,560,570                                     
560   WRITE (OUT,20) TABL11(ISCT)                                       
570   CONTINUE                                                          
      GO TO 1810                                                        
C                                                                       
C*****LOOP CONSTRUCTION                                                 
C                                                                       
C*****ENTER ALL DEGREE TWO NODES IN NRST                                
580   K=0                                                               
      DO 600 INODE=1,JNODE                                              
      IF (ND(INODE)-2) 600,590,600                                      
590   K=K+1                                                             
      NRST(K)=INODE                                                     
600   CONTINUE                                                          
C*****COMPUTE NUMBER OF LOOPS AND INITIALIZE SUBSCRIPTS                 
      NLOOP=NSEC-JNODE+1                                                
      CALL CHECKI (NLOOP,1,LMNLOP,199)                                  
      IF (NEW .EQ. 1) GOTO 1810                                             
      LPKT=1                                                            
      IKYLP=1                                                           
      NLP=0                                                             
620   NLP=NLP+1                                                         
      LOPT(NLP)=LPKT                                                    
630   DO 640 INODE=1,JNODE                                              
640   LNODE(INODE)=0                                                    
C*****CHECK IF ANY DEGREE TWO NODES ARE AVAILABLE                       
650   IF (K) 660,660,690                                                
660   NDL1=NSEC+1                                                       
C*****IF NO DEGREE TWO NODES ARE AVAILABLE, FIND A NODE OF MINIMAL      
C*****DEGREE                                                            
      DO 680 INODE=1,JNODE                                              
      IF (ND(INODE)*(NDL1-ND(INODE))) 680,680,670                       
670   KEYND=INODE                                                       
      NDL1=ND(INODE)                                                    
680   CONTINUE                                                          
      NR2=KEYND                                                         
      ND(KEYND)=NDL1-1                                                  
      GO TO 710                                                         
690   KEYND=NRST(K)                                                     
C*****KEYND MAY FAIL TO BE OF DEGREE TWO DESPITE BEING TAKEN FROM NRST  
C*****IF SO, GO BACK AND TRY AGAIN                                      
      K=K-1                                                             
      IF (ND(KEYND)-2) 650,700,650                                      
700   ND(KEYND)=0                                                       
710   IKNTN=NDP(KEYND)                                                  
C*****SELECT A SECTION CONNECTED TO KEYND                               
720   ISCT=KNTN(IKNTN)                                                  
      IKNTN=IKNTN+1                                                     
C*****IF SECTION ISCT HAS NOT YET BEEN REMOVED FROM NETWORK, REMOVE IT  
      IF (LPRP(ISCT)) 730,720,730                                       
730   LPRP(ISCT)=0                                                      
      KM1=0                                                             
      NR1=N2(ISCT)                                                      
      J=1                                                               
      IF (NR1-KEYND) 750,740,750                                        
740   J=-1                                                              
      NR1=N1(ISCT)                                                      
C*****THE PURGED SECTION BECOMES, WITH APPROPRIATE SIGN, PART OF LOOP   
C*****NLP                                                               
750   LOOP(LPKT)=J*ISCT                                                 
      LPKT=LPKT+1                                                       
      ND(NR1)=ND(NR1)-1                                                 
C*****IF KEYND WAS OF DEGREE TWO, ND(KEYND) IS NOW ZERO AND A SECOND    
C*****SECTION FROM KEYND MUST BE REMOVED                                
      IF (ND(KEYND)) 800,760,800                                        
760   ISCT=KNTN(IKNTN)                                                  
      IKNTN=IKNTN+1                                                     
      IF (LPRP(ISCT)) 770,760,770                                       
770   LPRP(ISCT)=0                                                      
      NR2=N1(ISCT)                                                      
      J=1                                                               
      IF (NR2-KEYND) 790,780,790                                        
780   NR2=N2(ISCT)                                                      
      J=-1                                                              
790   LOOP(LPKT)=J*ISCT                                                 
      LPKT=LPKT+1                                                       
      ND(NR2)=ND(NR2)-1                                                 
C*****IF NODES NR1 AND NR2 ARE NOT THE SAME, THEN ANOTHER PATH          
C*****BETWEEN NR1 AND NR2 MUST BE FOUND TO COMPLETE THE LOOP            
      IF (NR1-NR2) 800,1020,800                                         
C*****A TREE ROOTED AT NR1 IS GROWN IN ARRAYS LPSTC, LNODE AND KPIPE    
C*****UNTIL IT REACHES NR2 AND GIVES THE PATH REQUIRED TO COMPLETE THE  
C*****LOOP                                                              
800   LPSTC(1)=NR1                                                      
      KK=1                                                              
      KNT=1                                                             
      LNODE(NR1)=KEYND                                                  
810   NA1=LPSTC(KK)                                                     
      KK=KK+1                                                           
      NLOW=NDP(NA1)                                                     
      NHIGH=NDP(NA1+1)-1                                                
      DO 860 IKNTN=NLOW,NHIGH                                           
      ISCT=KNTN(IKNTN)                                                  
      IF (LPRP(ISCT)) 820,860,820                                       
820   J=-1                                                              
      NA2=N1(ISCT)                                                      
      IF (NA2-NA1) 840,830,840                                          
830   J=1                                                               
      NA2=N2(ISCT)                                                      
840   IF (LNODE(NA2)) 860,850,860                                       
850   LNODE(NA2)=NA1                                                    
      KNT=KNT+1                                                         
      LPSTC(KNT)=NA2                                                    
      KPIPE(NA2)=ISCT*J                                                 
860   CONTINUE                                                          
C*****CHECK IF TREE HAS REACHED NODE NR2                                
      IF (LNODE(NR2)) 920,870,920                                       
C*****IF KK IS GREATER THAN KNT NO OTHER PATH BETWEEN NODES NR1 AND     
C*****NR2 EXISTS AND THE ATTEMPT TO CONSTRUCT THIS LOOP IS ABORTED      
870   IF (KK-KNT) 810,810,880                                           
880   LPKT=LPKT-2                                                       
      IF (ND(KEYND)) 890,900,890                                        
890   LPKT=LPKT+1                                                       
900   IBACK=3                                                           
      GO TO 1040                                                        
910   NR1=NR2                                                           
      IBACK=4                                                           
      GO TO 1040                                                        
C*****TRACE TREE FROM NR2 BACK TO NR1, INSERTING SECTIONS IN LOOP       
920   J=NR2                                                             
930   ISCT=KPIPE(J)                                                     
C*****SET KM1=1 IF LOOP NLP SATISFIES HEURISTIC CRITERIA FOR USE AS A   
C*****KEY LOOP BY LOOP NUMBERING ALGORITHM                              
      IF (NDP(J+1)-NDP(J)-2) 950,940,950                                
940   KM1=1                                                             
950   J=LNODE(J)                                                        
      LOOP(LPKT)=ISCT                                                   
      LPKT=LPKT+1                                                       
      CALL CHECKI (LPKT,1,LMLPLK,200)                                   
      IF (NEW .EQ. 1) GOTO 1810                                             
      IF (J-NR1) 930,970,930                                            
970   IF (NDP(KEYND+1)-NDP(KEYND)-2) 980,990,980                        
980   IF (LPKT-LOPT(NLP)-7) 1000,1000,990                               
990   KM1=1                                                             
1000  IF (KM1) 1010,1020,1010                                           
1010  IPL(IKYLP)=NLP                                                    
      IKYLP=IKYLP+1                                                     
1020  IBACK=1                                                           
      GO TO 1040                                                        
1030  CONTINUE                                                          
C*****CHECK IF LOOP CONSTRUCTION IS COMPLETE                            
      IF (NLOOP-NLP) 1150,1150,620                                      
1040  J=ND(NR1)                                                         
C*****IF NODE NR1 HAS DEGREE TWO, ENTER IT IN NRST                      
      IF (J-2) 1060,1050,1060                                           
1050  K=K+1                                                             
      NRST(K)=NR1                                                       
      GO TO 1130                                                        
C*****IF NODE NR1 HAS DEGREE ONE, PURGE THE TAIL                        
1060  IF (J-1) 1130,1070,1130                                           
1070  ND(NR1)=0                                                         
      IKNTN=NDP(NR1)                                                    
1080  ISCT=KNTN(IKNTN)                                                  
      IF (LPRP(ISCT)) 1100,1090,1100                                    
1090  IKNTN=IKNTN+1                                                     
      GO TO 1080                                                        
1100  LPRP(ISCT)=0                                                      
      L1=N1(ISCT)                                                       
      IF (L1-NR1) 1120,1110,1120                                        
1110  L1=N2(ISCT)                                                       
1120  NR1=L1                                                            
      ND(NR1)=ND(NR1)-1                                                 
      GO TO 1040                                                        
1130  GO TO (1140,1030,910,630), IBACK                                  
1140  IBACK=2                                                           
      NR1=NR2                                                           
      GO TO 1040                                                        
1150  LOPT(NLOOP+1)=LPKT                                                
C                                                                       
C*****CONSTRUCTION OF ARRAYS LKAN, NDSS AND LPK                         
C                                                                       
C*****CONSTRUCT ARRAY LKAN                                              
      NKYLP=IKYLP-1                                                     
      LPKT=LPKT-1                                                       
      DO 1160 ISCT=1,NSEC                                               
1160  ND(ISCT)=0                                                        
C*****NOTE THAT ND, NDS, AND NDSS ARE EQUIVALENCED IN A SPECIAL WAY     
      IF (LPKT) 1170,1810,1170                                          
1170  DO 1180 I=1,LPKT                                                  
      J=IABS(LOOP(I))                                                   
1180  ND(J)=ND(J)+1                                                     
      J=1                                                               
      NDS(1)=1                                                          
      DO 1190 ISCT=2,NSEC                                               
1190  NDS(ISCT)=NDS(ISCT-1)+ND(ISCT-1)                                  
      KM1=1                                                             
      K=2                                                               
      ISCT=LOPT(2)                                                      
C*****FIND SECTIONS IN LOOP KM1=K-1                                     
      DO 1210 I=1,LPKT                                                  
      JJ=LOOP(I)                                                        
      J=IABS(JJ)                                                        
      L=NDS(J)                                                          
      NDS(J)=L+1                                                        
C*****CHECK IF ALL SECTIONS IN LOOP KM1 HAVE BEEN PROCESSED             
C*****IF SO, PROCEED TO NEXT LOOP                                       
      IF (I-ISCT) 1210,1200,1200                                        
1200  KM1=K                                                             
      K=K+1                                                             
      ISCT=LOPT(K)                                                      
1210  LKAN(L)=ISIGN(KM1,JJ)                                             
C*****ESTABLISH ARRAY LPK                                               
      LPKT=1                                                            
      J=1                                                               
      DO 1280 ILOOP=1,NLOOP                                             
      NND(ILOOP)=LPKT                                                   
      KPS=LPKT                                                          
      K=LOPT(ILOOP+1)-1                                                 
C*****FIND SECTIONS FORMING LOOP ILOOP                                  
      DO 1270 L=J,K                                                     
      ISCT=IABS(LOOP(L))                                                
      L1=NDSS(ISCT)                                                     
      L2=NDSS(ISCT+1)-1                                                 
C*****FIND LOOPS CONTAINING EACH SECTION                                
      DO 1260 L3=L1,L2                                                  
      NUMY1=LKAN(L3)                                                    
      NA1=IABS(NUMY1)                                                   
      IF (NA1-ILOOP) 1220,1260,1220                                     
1220  IF (KPS-LPKT) 1230,1250,1230                                      
1230  KNA2=LPKT-1                                                       
C*****CHECK IF NA1 IS ALREADY IN LIST OF LOOPS ADJACENT TO LOOP ILOOP   
      DO 1240 NA2=KPS,KNA2                                              
      IF (LPK(NA2)-NA1) 1240,1260,1240                                  
1240  CONTINUE                                                          
1250  LPK(LPKT)=NA1                                                     
      LPKT=LPKT+1                                                       
      CALL CHECKI (LPKT,1,LMLPK,41)                                     
      IF (NEW .EQ. 1) GOTO 1810                                             
1260  CONTINUE                                                          
1270  CONTINUE                                                          
1280  J=K+1                                                             
      NND(NLOOP+1)=LPKT                                                 
C                                                                       
C*****DETERMINATION OF NEAR-OPTIMAL LOOP NUMBERING                      
C                                                                       
      IKYLP=NKYLP                                                       
      JKYLP=NKYLP                                                       
      MAXP=LMNLOP                                                       
1290  DO 1300 ILOOP=1,NLOOP                                             
1300  IL(ILOOP)=0                                                       
      K=1                                                               
      IF (IKYLP) 1310,1380,1310                                         
C*****SELECT A KEY LOOP FROM IPL AND ASSIGN IT NUMBER ONE               
1310  I=IPL(IKYLP)                                                      
      IL(I)=1                                                           
      IP(1)=I                                                           
      L=1                                                               
C*****FIND UNNUMBERED LOOPS ADJACENT TO LOOP K AND ENTER THEM IN NRST   
1320  NLP=IP(K)                                                         
      L1=NND(NLP)                                                       
      L2=NND(NLP+1)-1                                                   
      IF (L1-L2) 1330,1330,1360                                         
1330  J=0                                                               
      DO 1350 ILOOP=L1,L2                                               
      JLOOP=LPK(ILOOP)                                                  
      IF (IL(JLOOP)) 1350,1340,1350                                     
1340  J=J+1                                                             
      NRST(J)=JLOOP                                                     
1350  CONTINUE                                                          
      IF (J) 1430,1360,1430                                             
1360  K=K+1                                                             
      IF (K-L) 1320,1320,1370                                           
C*****NO UNNUMBERED LOOPS ADJACENT TO NUMBERED LOOPS CAN BE FOUND,      
C*****THEREFORE, TEST IF NUMBERING IS COMPLETE                          
1370  IF (K-NLOOP-1) 1380,1530,1380                                     
C*****IF THE LOOP NUMBERING IS NOT COMPLETE FIND A FRESH KEY LOOP       
C*****AND CONTINUE                                                      
1380  IF (JKYLP) 1410,1390,1410                                         
1390  DO 1400 ILOOP=1,NLOOP                                             
C*****ANY UNNUMBERED LOOP CAN BE USED AS A KEY LOOP OF LAST RESORT      
      IF (IL(ILOOP)) 1400,1420,1400                                     
1400  CONTINUE                                                          
1410  ILOOP=IPL(JKYLP)                                                  
      JKYLP=JKYLP-1                                                     
      IF (IL(ILOOP)) 1380,1420,1380                                     
1420  L=K                                                               
      IP(K)=ILOOP                                                       
      IL(ILOOP)=K                                                       
      GO TO 1320                                                        
C*****COMPUTE NNRST IF J IS GREATER THAN 1                              
1430  IF (J-1) 1440,1510,1440                                           
1440  DO 1470 I=1,J                                                     
      JLOOP=NRST(I)                                                     
      L1=NND(JLOOP)                                                     
      L2=NND(JLOOP+1)-1                                                 
      MAXI=0                                                             
      DO 1460 L3=L1,L2                                                  
      NA1=LPK(L3)                                                       
      IF (IL(NA1)) 1460,1450,1460                                       
1450  MAXI=MAXI+1                                                         
1460  CONTINUE                                                          
1470  NNRST(I)=MAXI                                                      
      IM=1                                                              
      IM1=1                                                             
C*****SORT ARRAY NNRST IN ASCENDING ORDER                               
      DO 1500 I=2,J                                                     
      MAXI=NNRST(IM)                                                     
      DO 1490 L3=I,J                                                    
      KNA1=NNRST(L3)                                                    
      IF (KNA1-MAXI) 1480,1490,1490                                      
1480  IM=L3                                                             
      MAXI=KNA1                                                          
1490  CONTINUE                                                          
      KNA1=NNRST(IM)                                                    
      KNA2=NRST(IM)                                                     
      NNRST(IM)=NNRST(IM1)                                              
      NRST(IM)=NRST(IM1)                                                
      NNRST(IM1)=KNA1                                                   
      NRST(IM1)=KNA2                                                    
      IM=I                                                              
1500  IM1=I                                                             
C*****ASSIGN NUMBERS TO LOOPS IN NRST                                   
1510  DO 1520 I=1,J                                                     
      L=L+1                                                             
      NA1=NRST(I)                                                       
      IP(L)=NA1                                                         
1520  IL(NA1)=L                                                         
      K=K+1                                                             
      GO TO 1320                                                        
C*****IF MAXP=0, THE NEAR OPTIMAL NUMBERING HAS JUST BEEN RECONSTRUCTED 
1530  IF (MAXP) 1540,1630,1540                                          
1540  IM1=1                                                             
      L1=1                                                              
      MAXI=0                                                             
C*****COMPUTE MAXIMAL ABSOLUTE DIFFERENCE OF THE NUMBERS OF THE         
C*****ADJACENT LOOPS                                                    
      DO 1590 ILOOP=2,NLOOP                                             
      L2=NND(ILOOP)-1                                                   
      IF (L1-L2) 1550,1550,1580                                         
1550  JLOOP=IL(IM1)                                                     
      DO 1570 L=L1,L2                                                   
      L3=LPK(L)                                                         
      K=IABS(JLOOP-IL(L3))                                              
      IF (K-MAXI) 1570,1570,1560                                         
1560  MAXI=K                                                             
1570  CONTINUE                                                          
1580  IM1=ILOOP                                                         
1590  L1=L2+1                                                           
C*****CHECK IF THIS IS THE BEST LOOP NUMBERING SO FAR                   
      IF (MAXI-MAXP) 1600,1610,1610                                      
1600  MAXP=MAXI                                                          
      KMAX=IKYLP                                                        
1610  IKYLP=IKYLP-1                                                     
      JKYLP=NKYLP                                                       
C*****CHECK IF ALL KEY LOOPS HAVE BEEN TRIED                            
      IF (IKYLP) 1620,1620,1290                                         
1620  IKYLP=KMAX                                                        
      KBND1=MAXP                                                        
      MAXP=0                                                            
C*****RECONSTRUCT THE BEST NUMBERING IF NECESSARY                       
      IF (IKYLP-1) 1630,1630,1290                                       
C*****COMPLETE THE HALF-BANDWIDTH COMPUTATION                           
1630  KBND1=KBND1+1                                                     
C*****COMPUTE STORAGE REQUIREMENT FOR THE 'DQDTLP' MATRIX               
      BLPREQ=KBND1*NLOOP                                                
C*****CHECK FOR 'BLP' ARRAY OVERFLOW                                    
      IF (LMBLP-BLPREQ) 1640,1650,1650                                  
1640  CALL EERROR (188)                                                  
      WRITE (OUT,30) BLPREQ,LMBLP                                       
      GO TO 1810                                                        
1650  KNA1=NDSS(NSEC+1)-1                                               
C*****ADJUST LKAN TO LOOP NUMBERING                                     
      DO 1660 I=1,KNA1                                                  
      LP=LKAN(I)                                                        
      J=IABS(LP)                                                        
      J=IL(J)                                                           
1660  LKAN(I)=ISIGN(J,LP)                                               
C*****SORT ARRAY LKAN SO THAT THE LOOPS CONTAINING A GIVEN SECTION      
C*****APPEAR IN ASCENDING ORDER                                         
      DO 1710 ISCT=1,NSEC                                               
      J=NDSS(ISCT)                                                      
      L2=NDSS(ISCT+1)-1                                                 
      L1=J+1                                                            
      K=J                                                               
1670  IF (L1-L2) 1680,1680,1710                                         
1680  MINI=LKAN(J)                                                       
      MINI=IABS(MINI)                                                     
C*****SORT THE LOOPS CONTAINING SECTION ISCT                            
      DO 1700 L=L1,L2                                                   
      NUMY1=LKAN(L)                                                     
      NUMY1=IABS(NUMY1)                                                 
      IF (MINI-NUMY1) 1700,1700,1690                                     
1690  MINI=NUMY1                                                         
      J=L                                                               
1700  CONTINUE                                                          
      KK=LKAN(K)                                                        
      LKAN(K)=LKAN(J)                                                   
      LKAN(J)=KK                                                        
      K=L1                                                              
      J=K                                                               
      L1=L1+1                                                           
      GO TO 1670                                                        
1710  CONTINUE                                                          
1720  KBND2=KBND1-1                                                     
      KBND3=KBND1*NLOOP-KBND2                                           
C                                                                       
C*****OPTIONAL PRINTOUT OF ARRAYS LOOP, NDSS AND LKAN                   
C                                                                       
      GO TO (1820,1730,1730,1730,1730,1730,1820,1820), SUPOPT           
C*****WRITE SECTIONS THAT FORM EACH LOOP                                
1730  WRITE (OUT, 1) CHAR(12)
      WRITE (OUT,40)                                                    
      NLINE=50                                                          
      NCUNT=7                                                           
      DO 1770 ILOOP=1,NLOOP                                             
      K=IP(ILOOP)                                                       
      NLOW=LOPT(K)                                                      
      NHIGH=LOPT(K+1)-1                                                 
      DO 1740 J=NLOW,NHIGH                                              
      IDD=LOOP(J)                                                       
      IDDL=IABS(IDD)                                                    
      J11=J-NLOW+1                                                      
1740  NRST(J11)=TABL11(IDDL)*IDD/IDDL                                   
      IF (NCUNT-NLINE) 1760,1750,1750                                   
1750  NCUNT=7                                                           
      WRITE (OUT, 1) CHAR(12)                                            
      WRITE (OUT,40)
1760  NCUNT=NCUNT+3                                                     
      WRITE (OUT,50) ILOOP,(NRST(J),J=1,J11)                            
1770  WRITE (OUT,60) (LOOP(J),J=NLOW,NHIGH)                             
C*****WRITE HALF BANDWIDTH OF AERO MATRIX                               
      WRITE (OUT,70) KBND1, BLPREQ, LMBLP                                              
C*****WRITE LOOPS THAT PASS THROUGH EACH SECTION                        
      NCUNT=8                                                           
      WRITE (OUT, 1) CHAR(12)
      WRITE (OUT,80)
      DO 1800 ISCT=1,NSEC                                               
      ISCTX=TABL11(ISCT)                                                
      NLOW=NDSS(ISCT)                                                   
      NHIGH=NDSS(ISCT+1)-1                                              
      IF (NCUNT-NLINE) 1790,1780,1780                                   
1780  NCUNT=8            
      WRITE (OUT, 1) CHAR(12)                                               
      WRITE (OUT,80) 
1790  NCUNT=NCUNT+2                                                     
1800  WRITE (OUT,90) ISCT,ISCTX,NLOW,NHIGH,(LKAN(J),J=NLOW,NHIGH)       
      GO TO 1820                                                        
1810  WRITE (OUT,100)                                                   
      GO TO 2140                                                        
C                                                                       
C*****CONSTRUCTION OF PORTION OF ARRAY TABL2 WHICH TEMPORARILY SHOWS FOR
C*****EACH NODE THAT IS A MULTIPLY BRANCHED JUNCTION THE LOOPS THAT PASS
C*****THROUGH IT AND THE SECTIONS THE LOOPS PASS FROM TO. CONSTRUCTION  
C*****OF LIMITS OF ARRAY TABL2 (I.E. ARRAY TABL1) REQUIRED FOR EACH NODE
C                                                                       
C*****INITIALIZE ENTIRE ARRAY TABL2 TO ZERO                             
1820  DO 1830 ITBL2=1,LMTBL2                                            
1830  TABL2(ITBL2)=0                                                    
C*****INITIALIZE LOCATION IN ARRAY TABL2 WHERE THE LOOP THROUGH NODE    
C*****DATA IS BEING STORED                                              
      ITBL2=1                                                           
C*****LOOP OVER ALL NODES, CONSTRUCTING ARRAY TABL2 FOR EACH NODE THAT  
C*****IS A MULTIPLY BRANCHED JUNCTION.                                  
      DO 2010 INODE=1,NNODE                                             
      CALL CHECKI (ITBL2+2,1,LMTBL2,202)                                
C*****FIND NUMBER OF BRANCHES AT THE JUNCTION                           
      NBR=TABL16(INODE)                                                 
C*****CHECK IF NODE IS A MULTIPLY-BRANCHED JUNCTION                     
      IF (3-NBR) 1840,1840,2010                                         
C*****NODE IS A MULTIPLY BRANCHED JUNCTION                              
C*****INITIALIZE LOCATION IN ARRAYS NA AND NB WHERE DATA WILL BE STORED 
1840  INANB=0                                                           
C*****LOOP OVER ALL BRANCHES OF NODE                                    
      DO 1850 IBR=1,NBR                                                 
C*****FIND SECTION NUMBER OF BRANCH                                     
      ISCT=TABL17(INODE,IBR)                                            
      JSCT=IABS(ISCT)                                                   
C*****FIND RANGE OF LOCATIONS IN ARRAY LKAN WHERE LOOPS PASSING THROUGH 
C*****SECTION ARE STORED                                                
      NLOW=NDSS(JSCT)                                                   
      NHIGH=NDSS(JSCT+1)-1                                              
C*****LOOP OVER ALL LOOPS THAT PASS THROUGH SECTION                     
      DO 1850 ILKAN=NLOW,NHIGH                                          
      INANB=INANB+1                                                     
C*****STORE LOOP NUMBERS IN ARRAY NA                                    
      NA(INANB)=LKAN(ILKAN)                                             
C*****STORE SECTION NUMBERS REPETITIVELY IN ARRAY NB                    
1850  NB(INANB)=ISCT                                                    
C*****FIND TOTAL NUMBER OF ENTRIES MADE IN ARRAYS NA AND NB             
      NANB=INANB                                                        
C*****LOOP OVER ALL ENTRIES IN ARRAYS NA AND NB                         
      DO 2000 INANB=1,NANB                                              
C*****FIND A LOOP NUMBER                                                
      LOOPA=NA(INANB)                                                   
C*****IF LOOP NUMBER FOUND IS ZERO THEN NO MORE INFORMATION CAN BE FOUND
C*****FROM THIS PAIR OF ENTRIES IN ARRAYS NA AND NB                     
      IF (LOOPA) 1860,2000,1860                                         
C*****SET THIS ENTRY IN ARRAY NA TO ZERO SO THAT IT WILL NOT AGAIN BE   
C*****CONSIDERED                                                        
1860  NA(INANB)=0                                                       
C*****LOOP OVER REMAINING ENTRIES IN ARRAY NA, SEARCHING FOR THE OTHER  
C*****ENTRY OF THE LOOP NUMBER - ONLY TWO ENTRIES OF THE LOOP NUMBER    
C*****WILL EXIST SINCE A LOOP CAN ONLY PASS THROUGH A NODE ONCE.        
      DO 1880 JNANB=INANB,NANB                                          
      IF (IABS(LOOPA)-IABS(NA(JNANB))) 1880,1870,1880                   
C*****LOOPB IS SECOND ENTRY                                             
1870  LOOPB=NA(JNANB)                                                   
C*****SET THIS ENTRY IN ARRAY NA TO ZERO SO THAT IT WILL NOT AGAIN BE   
C*****CONSIDERED                                                        
      NA(JNANB)=0                                                       
C*****ASSUME FIRST ENTRY USED FROM ARRAY NB IS 'FROM' SECTION AND SECOND
C*****ENTRY IS 'TO' SECTION                                             
      ISCTA=NB(INANB)                                                   
      ISCTB=NB(JNANB)                                                   
      GO TO 1890                                                        
1880  CONTINUE                                                          
C*****USE A BINARY CODE TO DETERMINE WHETHER LOOP LOOPA GOES FROM       
C*****SECTION ISCTB TO SECTION ISCTA OR FROM SECTION ISCTA TO SECTION   
C*****ISCTB                                                             
1890  NUMY1=0                                                           
      NUMY2=0                                                           
      NUMY3=0                                                           
      IF (LOOPA) 1910,1910,1900                                         
1900  NUMY1=1                                                           
1910  IF (LOOPB) 1930,1930,1920                                         
1920  NUMY2=1                                                           
1930  IF (ISCTA) 1950,1950,1940                                         
1940  NUMY3=1                                                           
1950  NBINRY=2*(2*NUMY1+NUMY2)+NUMY3+1                                  
C*****STORE LOOP NUMBER                                                 
      TABL2(ITBL2)=IABS(LOOPA)                                          
      GO TO (1970,1960,1970,1960,1960,1970,1960,1970), NBINRY           
C*****LOOP LOOPA GOES FROM SECTION ISCTB TO SECTION ISCTA               
1960  TABL2(ITBL2+1)=IABS(ISCTB)                                        
      TABL2(ITBL2+2)=IABS(ISCTA)                                        
      GO TO 1980                                                        
C*****LOOP LOOPA GOES FROM SECTION ISCTA TO SECTION ISCTB               
1970  TABL2(ITBL2+1)=IABS(ISCTA)                                        
      TABL2(ITBL2+2)=IABS(ISCTB)                                        
C*****INCREMENT LOCATION IN ARRAY TABL2 WHERE THE LOOP THROUGH NODE DATA
C*****IS BEING STORED                                                   
1980  ITBL2=ITBL2+3                                                     
C*****TABL1 GIVES THE RANGE OF LOCATIONS IN ARRAY TABL2 WHERE THE NODE  
C*****IS STORED. FOR NODE INODE THAT IS A MULTIPLY BRANCHED JUNCTION THE
C*****NODE DATA IS STORED IN ARRAY TABL2 FROM LOCATIONS TABL1(INODE)    
C*****TO TABL1(INODE+1)-1.                                              
C*****CONSTRUCT PORTION OF TABL1 FOR NODES HAVING NODE NUMBERS GREATER  
C*****THAN INODE                                                        
      NLOW=INODE+1                                                      
      NHIGH=NNODE+1                                                     
      DO 1990 KNODE=NLOW,NHIGH                                          
1990  TABL1(KNODE)=ITBL2                                                
2000  CONTINUE                                                          
2010  CONTINUE                                                          
C*****COMPUTE TOTAL NUMBER OF LOCATIONS USED SO FAR IN ARRAY TABL2      
      NTBL2=ITBL2-1                                                     
C*****IT IS NOW NECESSARY TO CREATE FOR EACH NODE THAT IS A MULTIPLY    
C*****BRANCHED JUNCTION 3*LMTRRT+1 ADDITIONAL ENTRIES IN TABL2. THIS    
C*****EXPANSION MUST BE DONE IN ORDER TO ACCOMADATE THE 3*LMTRRT ENTRIES
C*****FOR THE TRAIN-AT-NODE DATA AND THE ENTRY REQUIRED FOR THE NODE    
C*****TYPE. LOOP OVER ALL NODES.                                        
      DO 2050 INODE=1,NNODE                                             
C*****COMPUTE CURRENT RANGE OF LOCATIONS OF NODE DATA IN ARRAY TABL2    
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
C*****CHECK IF NODE IS A MULTIPLY BRANCHED JUNCTION                     
      IF (NHIGH-NLOW) 2050,2020,2020                                    
C*****NODE IS A MULTIPLY BRANCHED JUNCTION. COMPUTE NUMBER OF ENTRIES IN
C*****ARRAY TABL2 THAT MUST BE MOVED AHEAD 3*LMTRRT+1 LOCATIONS - I.E.  
C*****THOSE FOR ALL NODES HAVING NUMBERS GREATER THAN INODE.            
2020  NHIGH=NTBL2-TABL1(INODE+1)+1                                      
C*****MOVE NODE DATA AHEAD 3*LMTRRT+1 LOCATIONS IN ARRAY TABL2          
      DO 2030 ITBL2=1,NHIGH                                             
      JTBL2=NTBL2-ITBL2+1                                               
2030  TABL2(JTBL2+3*LMTRRT+1)=TABL2(JTBL2)                              
C*****RECOMPUTE AND CHECK TOTAL NUMBER OF ENTRIES REQUIRED IN ARRAY     
C*****TABL2                                                             
      NTBL2=NTBL2+3*LMTRRT+1                                            
      CALL CHECKI (NTBL2,1,LMTBL2,202)                                  
C*****RECONSTRUCT PORTION OF TABL1 FOR NODES HAVING NODE NUMBERS GREATER
C*****THAN INODE                                                        
      NLOW=INODE+1                                                      
      NHIGH=NNODE+1                                                     
      DO 2040 KNODE=NLOW,NHIGH                                          
2040  TABL1(KNODE)=TABL1(KNODE)+3*LMTRRT+1                              
2050  CONTINUE                                                          
C*****FOR EACH NODE THAT IS A MULTIPLY BRANCHED JUNCTION SET PORTION OF 
C*****ARRAY TABL2 THAT WILL CONTAIN TRAIN-AT-NODE DATA AND NODE TYPE TO 
C*****ZERO                                                              
      DO 2080 INODE=1,NNODE                                             
C*****COMPUTE RANGE OF LOCATIONS OF NODE DATA IN ARRAY TABL2.           
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
C*****CHECK IF NODE IS A MULTIPLY BRANCHED JUNCTION                     
      IF (NHIGH-NLOW) 2080,2060,2060                                    
C*****NODE IS A MULTIPLY BRANCHED JUNCTION, COMPUTE LOWER RANGE OF      
C*****LOCATIONS OF TRAIN-AT-NODE DATA                                   
2060  NLOW=NHIGH-3*LMTRRT+1                                             
C*****INITIALLY ASSUME NODES THAT ARE MULTIPLY BRANCHED JUNCTIONS       
C*****AS TYPE 7                                                         
      TABL2(NLOW-1)=7                                                   
      DO 2070 ITBL2=NLOW,NHIGH                                          
2070  TABL2(ITBL2)=0                                                    
2080  CONTINUE                                                          
      GO TO (2140,2090,2090,2090,2090,2090,2140,2140), SUPOPT           
C*****OPTIONAL PRINTOUT OF PORTION OF ARRAY TABL2 COMPUTED HEREIN       
2090  WRITE (OUT,  1) CHAR(12)
      WRITE (OUT,110)
C*****LOOP OVER ALL NODES                                               
      DO 2130 INODE=1,NNODE                                             
C*****FIND EXTERNAL NODE NUMBER                                         
      INODEX=TABL15(INODE)                                              
C*****FIND RANGE OF LOCATIONS IN ARRAY TABL2 WHERE NODE DATA IS STORED  
      NLOW=TABL1(INODE)                                                 
      NHIGH=TABL1(INODE+1)-1                                            
C*****CHECK IF NODE IS A MULTILPY BRANCHED JUNCTION                     
      IF (NHIGH-NLOW) 2100,2110,2110                                    
C*****NODE IS NOT A MULTIPLY BRANCHED JUNCTION - WRITE TITLE AND GO     
C*****TO END OF LOOP                                                    
2100  WRITE (OUT,120) INODE,INODEX                                      
      GO TO 2130                                                        
C*****NODE IS A MULTIPLY BRANCHED JUNCTION. COMPUTE UPPER RANGE OF      
C*****LOCATIONS OF LOOP DATA IN ARRAY TABL2                             
2110  NHIGH=NHIGH-3*LMTRRT-1                                            
C*****LOOP OVER ALL LOOPS PASSING THROUGH NODE                          
      DO 2120 ITBL2=NLOW,NHIGH,3                                        
C*****FIND LOOP NUMBER                                                  
      ILOOP=TABL2(ITBL2)                                                
C*****FIND NUMBER OF 'FROM' SECTION                                     
      ISCTA=TABL2(ITBL2+1)                                              
C*****FIND EXTERNAL NUMBER OF 'FROM' SECTION                            
      ISCTAX=IABS(ISCTA)                                                
      ISCTAX=TABL11(ISCTAX)                                             
C*****FIND NUMBER OF 'TO' SECTION                                       
      ISCTB=TABL2(ITBL2+2)                                              
C*****FIND EXTERNAL NUMBER OF 'TO' SECTION                              
      ISCTBX=IABS(ISCTB)                                                
      ISCTBX=TABL11(ISCTBX)                                             
2120  WRITE (OUT,130) INODE,INODEX,ILOOP,ISCTA,ISCTAX,ISCTB,ISCTBX      
2130  CONTINUE                                                          
2140  RETURN                                                            
      END                                                               
