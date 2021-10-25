      ******************************************************************
      * BALANCE LINE WITH DB2 ACCESS
      * READ 2 FILES (ORDER BY KEYS, FIND MATCHES, COMPLETE DATA, 
      * INSERT INTO A DB2 TABLE AND WRITE THE MATCHES INTO AN OUT FILE
      ******************************************************************      
       IDENTIFICATION DIVISION.                                         
      *-----------------------------------------------------------------
       PROGRAM-ID. BLWITHDB.                                            
       AUTHOR. EFINARDI.                                                 
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.                                            
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.                                            
      *-----------------------------------------------------------------
       FILE-CONTROL.                                                    
           SELECT MSTFILE  ASSIGN  TO  AS-R-MSTFILE                     
                  FILE STATUS IS FS-STATUS.                             
           SELECT AUXFILE  ASSIGN  TO  AS-R-AUXFILE                     
                  FILE STATUS IS FS-STATUS.                             
           SELECT OUTFILE  ASSIGN  TO  UT-S-OUTFILE                     
                  FILE STATUS IS FS-STATUS.                             
      *                                                                 
      *-----------------------------------------------------------------
       DATA DIVISION.                                                   
      *-----------------------------------------------------------------
       FILE SECTION.                                                    
       FD  MSTFILE                                                      
           RECORDING MODE F                                             
           RECORD 018                                                   
           BLOCK 0.                                                     
      *                                                                 
       01  MSTFILE-REC-FD    PIC  X(018).                               


       FD  AUXFILE                                                      
           RECORDING MODE F                                             
           RECORD 27                                                    
           BLOCK 0.                                                     
      *                                                                 
       01  AUXFILE-REC-FD    PIC  X(027).                               
      *                                                                 
       FD  OUTFILE                                                      
           RECORDING MODE F                                             
           RECORD 150                                                   
           BLOCK 0.                                                     
      *                                                                 
       01  OUTFILE-REC-FD    PIC  X(150).                               
      *                                                                 
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.                                         
      *-----------------------------------------------------------------
      *                                                                 
       01  MSTFILE-REC.                                            
           03  MSTFILE-CD-A                 PIC S9(09) COMP.            
           03  MSTFILE-NR-A                 PIC S9(04) COMP.            
           03  MSTFILE-NR-C                 PIC S9(09) COMP.            
           03  MSTFILE-CD-P                 PIC S9(09) COMP.            
           03  MSTFILE-CD-C                 PIC S9(09) COMP.            
      *                                                                 
       01  OUTFILE-REC.                                            
           03  OUTFILE-NR-A                 PIC S9(04) COMP.            
           03  OUTFILE-NR-C                 PIC S9(09) COMP.            
           03  OUTFILE-CD-P                 PIC S9(09) COMP.            
           03  OUTFILE-CD-C                 PIC S9(09) COMP.            
           03  OUTFILE-NM-P                 PIC  X(120).                
      *                                                                 
       77  NM-PROG                          PIC  X(16) VALUE            
           '*** BLWITHDB ***'.                                           
       77  VERSION                          PIC  X(06) VALUE 'VRS001'.  
       77  ABEND                            PIC  X(07) VALUE 'ABEND'. 
       01  PGMSB001                         PIC  X(08) VALUE 'PGMSB001'.
       01  PGMSB002                         PIC  X(08) VALUE 'PGMSB002'.
      *                                                                 
       01  FS-STATUS                        PIC  X(02).                 
       01  DFHEIBLK.                                                    
           03  FILLER                       PIC  X(24).                 
           03  EIBCALEN                     PIC S9(04) COMP.            
           03  FILLER                       PIC  X(59).                 
      *                                                                 
      * --- Book of file AUXFILE                                 
-INC AUXKFILE
      *                                                                 
      *--- Book of subroutine PGMSB001                                      
-INC PGMKB001                                                           
      *                                                                 
      *--- Book of subroutine PGMSB002 recover CD-D                        
       01 PGMKB002.                                                     
-INC PGMKB002                                                           
      *                                                                 
      *--- Book of file OUTFILE
-INC OUTFILE                                                           
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.                                           
      *-----------------------------------------------------------------
      *                                                                 
       01  CTL-EOF-MSTFILE                  PIC  X(01).                 
           88  END-MSTFILE                             VALUE 'Y'.       
       01  CTL-EOF-AUXFILE                  PIC  X(01).                 
           88  END-AUXFILE                             VALUE 'Y'.       
       01  CTL-MATCH                        PIC  X(01).                 
           88  MATCH                                VALUE 'Y'.       
      *                                                                 
       01  QT-REC-MSTFILE                   PIC  9(07) VALUE 0.         
       01  QT-REC-AUXFILE                   PIC  9(07) VALUE 0.         
       01  QT-REC-BLINE                     PIC  9(07) VALUE 0.         
       01  QT-REC-OUTFILE                   PIC  9(07) VALUE 0.         
      *                                                                 
       01  GDA-SQLCODE                      PIC +999999999.             
      *                                                                 
       01  KEY-MSTFILE                      PIC  9(13) VALUE 0.         
       01  FILLER REDEFINES KEY-MSTFILE.                                
           03 KEY-NR-A-MSTFILE              PIC  9(04).                 
           03 KEY-NR-C-MSTFILE              PIC  9(09).                 
       01  KEY-AUXFILE                      PIC  9(13) VALUE 0.         
       01  FILLER REDEFINES KEY-AUXFILE.                                
           03 KEY-NR-A-AUXFILE              PIC  9(04).                 
           03 KEY-NR-C-AUXFILE              PIC  9(09).                 
      *                                                                 
      *--- Book of table to insert data.                                 
-INC DB2KTAB1                                                           
      *                                                                 
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.                                              
      *-----------------------------------------------------------------
      *                                                                 
      *-----------------------------------------------------------------
       000000-MAIN                                              SECTION.
      *-----------------------------------------------------------------
      *                                                                 
           PERFORM 100000-INIT.                       
           PERFORM 200000-BALANCE-LINE                                  
             UNTIL END-AUXFILE OR END-MSTFILE.                          
      *
           PERFORM 330000-WRITE-TRL-OUTFILE.
                                                                        
           DISPLAY '999 ' NM-PROG ' TOTALS'                     
           DISPLAY '999 ' NM-PROG ' READ FROM AUXFILE....: '     
                   QT-REC-AUXFILE.                                      
           DISPLAY '999 ' NM-PROG ' READ FROM MSTFILE....: '     
                   QT-REC-MSTFILE.                                      
           DISPLAY '999 ' NM-PROG ' MATCHED RECORDS......: '     
                   QT-REC-BLINE.                                        
           DISPLAY '999 ' NM-PROG ' WRITTEN INTO OUTFILE.: '     
                   QT-REC-OUTFILE.                                      
      *                                                                 
           CLOSE  AUXFILE                                               
                  MSTFILE                                               
                  OUTFILE.                                              
      *                                                                 
           IF QT-REC-OUTFILE = 0                                        
              MOVE 4  TO  RETURN-CODE                                   
           END-IF.                                                      
      *                                                                 
       000000-OUT.                                                      
           STOP RUN.                                                    
      *                                                                 
      *-----------------------------------------------------------------
       100000-INIT                                              SECTION.
      *-----------------------------------------------------------------
      *                                                                
           OPEN INPUT  MSTFILE.                                         
      *
           IF FS-STATUS NOT = '00'
              PERFORM 999001-ERROR-001
           END-IF.
      *
           OPEN INPUT  AUXFILE.                                         
      *                                                                 
           IF FS-STATUS NOT = '00'
              PERFORM 999002-ERROR-002
           END-IF.
      *
           OPEN OUTPUT OUTFILE.                                         
      *                                                                 
           IF FS-STATUS NOT = '00'
              PERFORM 999003-ERROR-003
           END-IF.
      *
           PERFORM 300000-READ-AUXFILE.                                 
           PERFORM 310000-READ-MSTFILE.                                 
      *                                                                 
           PERFORM 320000-WRITE-HDR-OUTFILE.                           

       100000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       200000-BALANCE-LINE                                      SECTION.
      *-----------------------------------------------------------------
      *                                                                 
           IF KEY-MSTFILE = KEY-AUXFILE                                 
              ADD 1  TO  QT-REC-BLINE                                   
              IF MATCH                                               
                 PERFORM 210000-VALIDATE-DATA                           
                 PERFORM 220000-RECOVERY-DATA                             
                 PERFORM 230000-RECOVERY-MORE-DATA                            
                 PERFORM 240000-INSERT-DATA                                                     
              END-IF                                                    
              PERFORM 310000-READ-MSTFILE                               
           ELSE                                                         
              IF KEY-MSTFILE < KEY-AUXFILE                              
                 PERFORM 310000-READ-MSTFILE                            
              ELSE                                                      
                 PERFORM 300000-READ-AUXFILE                            
              END-IF                                                    
           END-IF.                                                      
      *                                                                 
       200000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       210000-VALIDATE-AUX-DATA SECTION.                                    
      *-----------------------------------------------------------------
      *--- this variable is declared inside the book AUXKFILE        
           IF AUXFILE-NR-A EQUAL ZEROS
              PERFORM 999005-ERROR-005
           END-IF.
      *--- this variable is declared inside the book AUXKFILE                                                                 
           IF AUXFILE-NR-C EQUAL ZEROS
              PERFORM 999006-ERROR-006
           END-IF.
      *--- this variable is declared inside the book AUXKFILE                                                                 
           IF AUXFILE-CD-P LT ZEROS
              PERFORM 999007-ERROR-007
           END-IF.
      *--- this variable is declared inside the book AUXKFILE                                                                 
           IF AUXFILE-CD-C LE ZEROS
              PERFORM 999008-ERROR-008
           END-IF.
      *--- this variable is declared inside the book AUXKFILE
           IF AUXFILE-NM-P EQUAL SPACES
              PERFORM 999009-ERROR-009
      *                                                                 
           IF MSTFILE-NR-A EQUAL ZEROS
              PERFORM 999010-ERROR-010
           END-IF.
      *                                                                 
           IF MSTFILE-NR-C EQUAL ZEROS
              PERFORM 999011-ERROR-011
           END-IF.
      *                                                                 
           IF MSTFILE-CD-P LT ZEROS
              PERFORM 999012-ERROR-012
           END-IF.
      *                                                                 
           IF MSTFILE-CD-C LE ZEROS
              PERFORM 999013-ERROR-013
           END-IF.
      *                                                                 
       210000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       220000-RECOVERY-DATA SECTION.                                      
      *-----------------------------------------------------------------
      *                                                                 
           INITIALIZE PGMSB002-AREA                                     
            REPLACING ALPHANUMERIC  BY  SPACES                          
                      NUMERIC       BY  ZEROS.                          
      *                                                                 
           MOVE 1             TO  PGMSB002-CD-FUNCTION.                    
           MOVE AUXFILE-NR-A  TO  PGMSB002-CD-D.                
           MOVE 'Y'           TO  PGMSB002-FLAG-BASIC-DATA.         
      *                                                                 
           MOVE LENGTH OF PGMSB002-AREA  TO  EIBCALEN.                  
           CALL PGMSB002 USING DFHEIBLK PGMSB002-AREA.                  
      *                                                                 
           IF PGMSB002-CD-RTN NOT = 0
              PERFORM 999014-ERROR-014
           END-IF.
      *                                                                 
       220000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       230000-RECOVERY-MORE-DATA SECTION.                                     
      *-----------------------------------------------------------------
      * --- VARIABLES DECLARED INSIDE THE BOOK PGMKB001                                                                   
           INITIALIZE PARM-DATA                                        
            REPLACING ALPHANUMERIC  BY  SPACES                          
                      NUMERIC       BY  ZEROS.                          
      *                                                                 
           MOVE 'B'                  TO  PARM-ENVIRONMENT.                
           MOVE AUXFILE-NR-A         TO  PARM-NR-A.                    
           MOVE AUXFILE-NR-C         TO  PARM-NR-C.                   
      *                                                                 
           MOVE LENGTH OF PARM-DADOS TO EIBCALEN.
           CALL PGMSB001 USING DFHEIBLK PARM-DATA.                     
      * CONSIDER IT SHOULDN'T ABEND, JUST NEED TO BE WRITTEN AT SYSOUT
           IF PARM-RETURN-CODE NOT EQUAL ZEROS                           
              MOVE SPACES            TO  PARM-NAME              
              DISPLAY '888 ' NM-PROG ' RETURN OF SB001 '
                      PARM-RETURN-CODE ' - ' PARM-NR-A ' - ' PARM-NR-C
           END-IF.                                                      
      *                                                                 
       230000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       240000-INSERT-DATA SECTION.                                     
      *-----------------------------------------------------------------
      * VAR DECLARED IN THE BOOK PGMKB002, RECOVERED AT 220000 SECTION                                                                
           MOVE PGMSB002-CD-S           TO  TAB1-CD-S.            
      *
           MOVE AUXFILE-NR-C            TO  TAB1-NR-C.                
           MOVE MSTFILE-CD-P            TO  TAB1-CD-P.           
           MOVE MSTFILE-CD-C            TO  TAB1-CD-C.  
      * VAR DECLARED IN THE BOOK PGMKB001, RECOVERED AT 230000 SECTION               
           MOVE PARM-NAME               TO  TAB1-NM-P.           
      *                                                                
           EXEC SQL
              INSERT
                INTO SOMEDB.TABLE1
                   ( NR_C,
                     CD_P ,
                     CD_C ,
                     NM-P ,
              VALUES
                   ( :TAB1-NR-C ,
                     :TAB1-CD-P ,
                     :TAB1-CD-C ,
                     :TAB1-NM-P )
           END-EXEC.
      *                                                                 
           IF SQLCODE = 0
              PERFORM 250000-WRITE-REC-OUTFILE                          
           ELSE
              PERFORM 999015-ERROR-015
              END-IF
           END-IF.
      *                                                                 
       240000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       250000-WRITE-REC-OUTFILE SECTION.                                
      *-----------------------------------------------------------------
      *                                                                 
           INITIALIZE OUTREC-DATA                                  
            REPLACING ALPHANUMERIC  BY  SPACES                          
                      NUMERIC       BY  ZEROS.
      *                                                                 
           MOVE MSTFILE-NR-A          TO  OUTFILE-NR-A
           MOVE MSTFILE-NR-C          TO  OUTFILE-NR-C
           MOVE AUXFILE-NR-A          TO  OUTFILE-CD-P
           MOVE AUXFILE-NR-C          TO  OUTFILE-CD-C
           MOVE PARM-NAME             TO  OUTFILE-NM-P

           WRITE OUTFILE-REC-FD  FROM  OUTFILE-REC.                   
      *                                                                 
           IF FS-STATUS NOT = '00'
              PERFORM 999004-ERROR-004
           END-IF.
      *
           ADD 1  TO  QT-REC-OUTFILE.                                   
      *                                                                 
       250000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       300000-READ-AUXFILE SECTION.                                     
      *-----------------------------------------------------------------
      *                                                                 
           READ AUXFILE  INTO  AUXFILE-REC                            
               AT END                                                   
                  MOVE 'Y'         TO  CTL-EOF-AUXFILE                  
               NOT AT END                                               
                  ADD 1              TO  QT-REC-AUXFILE                   
                  MOVE AUXFILE-NR-A  TO  KEY-NR-A-AUXFILE
                  MOVE AUXFILE-NR-C  TO  KEY-NR-C-AUXFILE
           END-READ.                                                    
      *                                                                 
       300000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       310000-READ-MSTFILE SECTION.                                     
      *-----------------------------------------------------------------
      *                                                                 
           READ MSTFILE  INTO  MSTFILE-RECISTRO                         
               AT END                                                   
                  MOVE 'Y'            TO  CTL-EOF-MSTFILE               
               NOT AT END                                               
                  ADD 1               TO  QT-REC-MSTFILE                
                  MOVE MSTFILE-NR-A   TO  KEY-NR-A-MSTFILE
                  MOVE MSTFILE-NR-C   TO  KEY-NR-C-MSTFILE
           END-READ.                                                    
      *                                                                 
       310000-OUT.                                                      
           EXIT.                                                        
      *
      *-----------------------------------------------------------------
       320000-WRITE-HDR-OUTFILE SECTION.                               
      *-----------------------------------------------------------------
      *                                                                 
           INITIALIZE OUTFILE-HEADER                                   
            REPLACING ALPHANUMERIC  BY  SPACES                          
                      NUMERIC       BY  ZEROS.                          
      *                                                                 
           MOVE 0                         TO  OUTFILE-CD-TIP-REC.
           MOVE 'OUTFILE'                 TO  OUTFILE-FILE-NM.
           MOVE MSTFILE-CD-C              TO  OUTFILE-CD-M.
      *                                                                 
           WRITE OUTFILE-REC-FD  FROM  OUTFILE-REC.                   
      *                                                                 
           IF FS-STATUS NOT = '00'
              PERFORM 999004-ERROR-004
           END-IF.
      *                                                                 
       320000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       330000-WRITE-TRL-OUTFILE SECTION.                               
      *-----------------------------------------------------------------
      *                                                                 
           INITIALIZE OUTFILE-TRAILER                                  
            REPLACING ALPHANUMERIC  BY  SPACES                          
                      NUMERIC       BY  9.                              
      *                                                                 
           MOVE 9                      TO  OUTFILE-CD-TYPE-REC.
           MOVE QT-REC-OUTFILE         TO  OUTFILE-TOTAL-REC-TRL.
      *                                                                 
           WRITE OUTFILE-REC-FD  FROM  OUTFILE-REC.                   
      *                                                                 
           IF FS-STATUS NOT = '00'
              PERFORM 999004-ERROR-004
           END-IF.
      *                                                                 
       330000-OUT.                                                      
           EXIT.                                                        
      *                                                                 
      *-----------------------------------------------------------------
       900000-ERROR SECTION.                                             
      *-----------------------------------------------------------------
      *                                                                 
       999001-ERROR-001.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' ERROR OPENING MASTER FILE'.    
           DISPLAY '888 ' NM-PROG ' ERROR 001'.                         
           DISPLAY '888 ' NM-PROG ' FILE STATUS: ' FS-STATUS.          
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999002-ERROR-002.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' ERROR OPENING AUXILIARY FILE'.    
           DISPLAY '888 ' NM-PROG ' ERROR 002'.                         
           DISPLAY '888 ' NM-PROG ' FILE STATUS: ' FS-STATUS.          
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999003-ERROR-003.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' ERROR OPENING OUT FILE'.    
           DISPLAY '888 ' NM-PROG ' ERROR 003'.                         
           DISPLAY '888 ' NM-PROG ' FILE STATUS: ' FS-STATUS.          
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999004-ERROR-004.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' ERROR WRITING OUT FILE'.    
           DISPLAY '888 ' NM-PROG ' ERROR 004'.                         
           DISPLAY '888 ' NM-PROG ' FILE STATUS: ' FS-STATUS.          
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999005-ERROR-005.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT AUXFILE-NR-A'.     
           DISPLAY '888 ' NM-PROG ' ERROR 005'. 
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-AUXFILE                        
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999006-ERROR-006.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT AUXFILE-NR-C'.       
           DISPLAY '888 ' NM-PROG ' ERROR 006'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-AUXFILE                
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999007-ERROR-007.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT AUXFILE-CD-P'.    
           DISPLAY '888 ' NM-PROG ' ERROR 007'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-AUXFILE                
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999008-ERROR-008.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT AUXFILE-CD-C'.      
           DISPLAY '888 ' NM-PROG ' ERROR 008'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-AUXFILE                
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999009-ERROR-009.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT AUXFILE-NM-P'.         
           DISPLAY '888 ' NM-PROG ' ERROR 009'.
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-AUXFILE                                    
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999010-ERROR-010.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT MSTFILE-NR-A'.           
           DISPLAY '888 ' NM-PROG ' ERROR 010'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-MSTFILE                
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999011-ERROR-011.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT MSTFILE-NR-C'.        
           DISPLAY '888 ' NM-PROG ' ERROR 011'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-MSTFILE                
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999012-ERROR-012.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT MSTFILE-CD-P'.    
           DISPLAY '888 ' NM-PROG ' ERROR 012'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-MSTFILE                
           PERFORM 999999-ABEND.                                        
      *
       999013-ERROR-013.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' INVALID DATA AT MSTFILE-CD-C'.    
           DISPLAY '888 ' NM-PROG ' ERROR 013'.                    
           DISPLAY '888 ' NM-PROG ' RECORD NUMBER: ' QT-REC-MSTFILE                
           PERFORM 999999-ABEND.                                        
      *
       999014-ERROR-014.                                                 
      *                                                                 
           DISPLAY '888 ' NM-PROG ' ERROR CALLING PGMSB002'.     
           DISPLAY '888 ' NM-PROG ' RETURN CODE ' PGMSB002-CD-RTN.      
           DISPLAY '888 ' NM-PROG ' RETURN TEXT ' PGMSB002-TX-RTN.      
           DISPLAY '888 ' NM-PROG ' SQLCODE     ' PGMSB002-SQLCODE.     
           DISPLAY '888 ' NM-PROG ' ERROR 014'.                         
           PERFORM 999999-ABEND.                                        
      *                                                                 
       999015-ERROR-015.                                                 
      *                                                                 
           MOVE SQLCODE  TO GDA-SQLCODE.                                
           DISPLAY '888 ' NM-PROG ' ERROR INSERT TAB1'.      
           DISPLAY '888 ' NM-PROG ' NUMBER  ' TAB1-NR-C.           
           DISPLAY '888 ' NM-PROG ' CODE    ' TAB1-CD-P.               
           DISPLAY '888 ' NM-PROG ' SQLCODE ' GDA-SQLCODE.             
           DISPLAY '888 ' NM-PROG ' ERROR 015'.                         
           PERFORM 999999-ABEND.                                        
      *                                                                 
      *-----------------------------------------------------------------
       999999-ABEND.                                                    
      *-----------------------------------------------------------------
      *                                                                 
           EXEC SQL
              ROLLBACK
           END-EXEC.
      *                                                                 
           DISPLAY '888 ' NM-PROG '       C A N C E L E D      '.    
      *                                                                 
           CLOSE  AUXFILE                                               
                  MSTFILE                                               
                  OUTFILE.                                              
      *                                                                 
           CALL ABEND.                                                
      *                                                                 
       999999-OUT.                                                      
           EXIT.                                                        
      *                                                                 