      SUBROUTINE  ALCHAR( INUM, IICHAR )                                 
C                                                                       
C                                              ( DATE: 15 AUGUST 1997 ) 
C     
C     THIS SUBROUTINE CONVERTS AN INTEGER NUMBER, 'INUM'                
C     INTO A TWO-CHARACTER ALPHAMERIC VARIABLE, 'ICHAR'.                
C     FOR NUMBERS GREATER THAN TWO DIGITS, ONLY THE LAST                
C     TWO DIGITS ARE RETURNED.                                          
C                                                                       
      CHARACTER*2 JCHAR, IICHAR
      DIMENSION JCHAR(100)
C                                                                       
      DATA  JCHAR /   ' 1',' 2',' 3',' 4',' 5',' 6',' 7',               
     1 ' 8',' 9','10','11','12','13','14','15','16','17','18','19','20',
     2 '21','22','23','24','25','26','27','28','29','30','31','32','33',
     3 '34','35','36','37','38','39','40','41','42','43','44','45','46',
     4 '47','48','49','50','51','52','53','54','55','56','57','58','59',
     5 '60','61','62','63','64','65','66','67','68','69','70','71','72',
     6 '73','74','75','76','77','78','79','80','81','82','83','84','85',
     7 '86','87','88','89','90','91','92','93','94','95','96','97','98',
     8 '99','00'  /                                                     
C                                                                       
      J = INUM - ((INUM-1) / 100 ) * 100                                
      IICHAR = JCHAR( J )                                                
C                                                                       
      RETURN                                                            
      END                                                               
