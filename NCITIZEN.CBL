       IDENTIFICATION DIVISION. 
       PROGRAM-ID. NCITIZEN.
      ******************************************************************
      *                      NAKSHATRA CITIZEN
      *                      (VEDIC ASTROLOGY)
      ******************************************************************
      *    ABOUT
      *          PGM SELECTS SQL QUERIES OF DATA FROM DB2 LIKE
      *          GENDER, ALIVE STATUS AND AGE. RANGE IS BEING
      *          COUNTED IN VARIABLES AND MOVED TO PROPER BOUNDRIES
      *          AND GROUPS OF NAKSHATRAS, AGE
      *          LAST BUT NOT LEAST IT DOES PERCENTAGE STATISTICS
      *          SHOWS GRAPHS LIKE: 40% = |####      |
      *                             90% = |######### |
      *
      ******************************************************************
      *          COPYRIGHT:  GNU GPLV3 LICENSE 2023
      *          AUTHOR:     PRZEMYSLAW ADAM KUPISZ
      *          VERSION:    ALPHA
      *
      *    WARNING
      *          CODE WAS NOT COMPILED AND RUN
      *          JUST PARSED AND SYSNTAX CHECKED FOR THAT MOMENT
      *          WRITTEN IN LEGACY VSCODE WITHOUT GNUCOBOL EXTENSION
      *
      *    PURPOSE
      *          TRAINING AND COGNITIVE OBJECTIVES OF COBOL:
      *                      -SQL TO DB2 CONNECTION
      *                      -EMBEDDED SQL
      *                      -RECORDS WRITE TO THE DATASET
      *          SINGLE FILE CODE (NO INCLUDES, NO COPYBOOKS, NO CALLS)
      *          PLUS JCL FILE TO RUN WITH DD EXAMPLE
      ******************************************************************
      *    ARGUMENTS FROM JCL TO PRINT STATISTICS FOR
      *    THE LAST 120 YEARS WITH 10 YEARS INTERVALS AS DEFAULT
      ******************************************************************
      *    JCL PARM='010' EQ 12 TABLES FOR EVERY 10 YEARS FROM 120
      *    JCL PARM='001' EQ 120 TABLES FOR EVERY YEAR FROM 120
      *    JCL PARM='120' EQ 1 TABLE FOR ALL 120 YEARS OF STATISTICS
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
       SELECT RECLOG ASSIGN TO NCLOG
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL
       FILE STATUS IS LK-FS

       DATA DIVISION.
       FILE SECTION.
       FD RECLOG.
      ******************************************************************
       01 RECV01-DS.
      ******************************************************************       
      ******************************************************************
      

      ******************************************************************      
       WORKING-STORAGE SECTION.
      ******************************************************************
      *  BEGIN SQL VARIABLES & STRUCTURES
      ****************************************************************** 
           EXEC SQL  
              INCLUDE SQLCA  
           END-EXEC.
      *
           EXEC SQL BEGIN DECLARE SECTION
           END-EXEC.
       01 WS-QREC01.
        05 WS-QBDATE   PIC   X(10).
        05 WS-QGENDER  PIC   9.
        05 WS-QALIVE   PIC   9.
       01 WS-QDATE-LIMIT  PIC   X(10).
       01 WS-QDATE-CURRENT-DB2  PIC   X(10).
           EXEC SQL END DECLARE SECTION
           END-EXEC.
      *
      *HELP QUERY TO DETECT DB2 DATE FORMAT
           EXEC SQL  
              DECLARE CSR2 CURSOR FOR  
              SELECT CURRENT DATE FROM SYSIBM.SYSDUMMY1
           END-EXEC.
      *MAIN QUERY TO SELECT MULTIPLE ROWS FOR PROCESSING
           EXEC SQL  
              DECLARE CRS1 CURSOR FOR  
              SELECT BDATE, GENDER, ALIVE 
      *       INTO :WS-QBDATE, :WS-QGENDER, :WS-QALIVE
              FROM CITIZENS
      **        WHERE BDATE >= DATE :WS-QDATE-LIMIT *> RM DATE?
              WHERE BDATE BETWEEN :WS-QDATE-CURRENT-DB2
              AND :WS-QDATE-LIMIT        
              FOR FETCH ONLY
              *> CHECK DATE STANDARD FIRST TO IMPL
           END-EXEC.  
      ******************************************************************
      *  END OF SQL VARIABLES & STRUCTURES
      ****************************************************************** 

       01 WS-ALL.
         05 WS-ARGS.
          10 WS-INTERVAL-VAL          PIC   9(4).
          88 PASS VALUES ARE 01 THRU 120.
         05 WS-ADATE                  PIC   9(8).
         05 WS-USER                   PIC   X(8).
         05 WS-DB2-DATE-FORMAT        PIC   X(10).
                 *> IT GOES TO DATASET OUTPUT 
         05 WS-WREC01 OCCURS 1 TO 120 TIMES DEPENDING ON 
                                         WS-INTERVAL-VAL.
           10 WS-W-DATE                PIC   X(10).
            15 WS-W-YEAR                   PIC   9(4).
            15 WS-W-MONTH                  PIC   99.
            15 WS-W-DAY                    PIC   99.
           10 WS-W-GENDER              PIC   9.
           10 WS-W-ALIVE               PIC   9.
      
      ******************************************************************
      * VARIABLES FOR PROC-NAKSHATRA-COMPUTE AND PROC-ANALYSE-DATA
      ******************************************************************
       01-WS-STATS.
        05 WS-MONTH-DAY      PIC   9(4).  

        05 WS-CTAB. *> OCCURS 1 TO 10 TIMES DEP ON
         10 WS-NTAB OCCURS 27 TIMES.
          15 WS-N   PIC   9(9) COMP-5.
          14 WS-U   PIC   9(9) COMP-5.
      *  05 WS-NTAB-NAMES

      * 'ASWINI'
      * 'BHARANI'
      * 'KRITTIKA'
      * 'ROHINI'
      * 'MRIGASIRA'
      * 'ARDRA'
      * 'PUNARVASU' 
      * 'PUSJA'    
      * 'ASZLESZA'      
      * 'MAGHA'
      * 'PURVA PHALGUNI'
      * 'UTTARA PHALGUNI'
      * 'HASTA'
      * 'CAJTRA'      
      * 'SWATI'      
      * 'WAJSIAKHA'      
      * 'ANURADHA'      
      * 'DZJESZTHA'      
      * 'MULA'      
      * 'PURVA ASZADHA'      
      * 'UTTARA ASZADHA'      
      * 'SRAWANA'      
      * 'DHANISZTA'      
      * 'SATABHISZAK'      
      * 'PURVA BHADRA'      
      * 'UTTARA BHADRA'
      * 'REVATI'
      
      
      
      
      
      ******************************************************************
       LINKAGE SECTION.
       01 PARM-BUFFER.
         05 PARM-LENGTH               PIC   S9(4) COMP.
         05 PARM-DATA                 PIC   X(256).
       01 LK-FS                       PIC   99.
      ******************************************************************
      *    PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING PARM-BUFFER, LK-FS.
           IF NOT PARM-LENGTH > 0 THEN
              DISPLAY 'WARNING: JCL PARM IS EMPTY. ',
              'AUTO COMPLEMENT INTERVAL SET TO 10'
              MOVE 10 TO WS-INTERVAL-VAL
              MOVE 1 TO RETURN-CODE
           ELSE
           MOVE PARM-DATA(1:3) TO WS-INTERVAL-VAL
           IF NOT PASS THEN
              DISPLAY 
              'WARNING: JCL PARM RANGE IS WRONG. TRY 001 TO 120 ',
              'AUTO COMPLEMENT INTERVAL SET TO 10'
              MOVE 10 TO WS-INTERVAL-VAL
              MOVE 2 TO RETURN-CODE
           ELSE
              ACCEPT WS-ADATE   FROM DATE YYYYMMDD
              ACCEPT WS-USER    FROM USERNAME
      D    DISPLAY 'DEBUG: ', WS-INTERVAL-VAL, 'PARAMETER FROM JCL'
      D    DISPLAY 'DEBUG: ', WS-USER, WS-ADATE, 'USER, CURRENT DATE'
              PERFORM PARA-SQL-QUERIES
              *> CATCH STH IF ERR ?
              PERFORM PARA-ANALYSE-DATA
              PERFORM PARA-WRITE-TO-DATASET
              
      *        MOVE 0 TO RETURN-CODE
           END-IF
           END-IF
      D    DISPLAY 'DEBUG: RC=', RETURN-CODE
           STOP RUN.
      ******************************************************************
      * STOP RUN PGM
      ******************************************************************
      PARA-SQL-QUERIES.
           EXEC SQL
              OPEN CSR2
           END-EXEC.
  
      * FETCH QUERY
           EXEC SQL
              FETCH CSR2
              INTO  WS-QDATE-CURRENT-DB2
           END-EXEC
           
           EVALUATE SQLCODE
              WHEN ZERO
      *****************************************
           IF WS-DB2-DATE-FORMAT(5:1) = '-' THEN
              *> YYYY/MM/DD
              DISPLAY 'T'
           ELSE 
              IF WS-DB2-DATE-FORMAT(3:1) = '/' THEN
                 *> MM/DD/YYYY
                 DISPLAY 'T'
              ELSE
                 *> MM/DD/YYY
                 DISPLAY 'T'
              END-IF
           END-IF
      * END CHECKING DB2 DATE FORMAT  
      ******************************************               
              WHEN OTHER
      ************** DB2 Error Handling Routine
           DISPLAY '*************************************************'
           DISPLAY '**** E R R O R M E S S A G E S ****'
           DISPLAY '*************************************************'
           DISPLAY '* Problem in paragraph: ' ws-paragraph
           DISPLAY '* Problem-msg: ' ws-error-msg
           DISPLAY '*'
           DISPLAY '* SQLCODE: ' SQLCODE
           DISPLAY '*************************************************'
           CALL "CEE3ABD" USING ABEND-CODE , TIMING           
           END-EVALUATE
      ******************************************************************     
           EXEC SQL
              CLOSE CSR2
           END-EXEC.     
           EXEC SQL
              OPEN CSR1
           END-EXEC.
      *  IMPLEMENT HERE LOOP PERFORM FOR GENERATING DYNAMIC TABLES
      *    1 - 120
           PERFORM UNTIL SQLCODE = 100
           EXEC SQL
           SELECT BDATE,
                  GENDER,
                  ALIVE
           INTO
              :WS-QBDATE,
              :WS-QGENDER,
              :WS-QALIVE,
           FROM CITIZENS
      *     WHERE BDATE >= DATE :WS-QDATE-LIMIT
           WHERE BDATE BETWEEN :WS-QDATE-CURRENT-DB2
           AND :WS-QDATE-LIMIT  
      *       AND   X = :Y
           END-EXEC
           
           IF (SQLCODE NOT = 0 AND 100) THEN
      ************** DB2 Error Handling Routine
           DISPLAY '*************************************************'
           DISPLAY '**** E R R O R M E S S A G E S ****'
           DISPLAY '*************************************************'
           DISPLAY '* Problem in paragraph: ' ws-paragraph
           DISPLAY '* Problem-msg: ' ws-error-msg
           DISPLAY '*'
           DISPLAY '* SQLCODE: ' SQLCODE
           DISPLAY '*************************************************'
           CALL "CEE3ABD" USING ABEND-CODE , TIMING
           END-IF 
      * COMPUTES NAKSHATRA 
           PERFORM PARA-NAKSHATRA-COMPUTE
           END-PERFORM

           EXEC SQL
              CLOSE CSR1
           END-EXEC.   
           EXIT.
       PARA-NAKSHATRA-COMPUTE.
      ****************************************************************** 
      *
      ******************************************************************
           
           EVALUATE WS-MONTH-DAY *>MONTH DAY
              WHEN 0413
                 COMPUTE WS-U(I,27) = WS-U(I,27) + 1
              WHEN 0414 THRU 0426
                 COMPUTE WS-N(I,1) = WS-N(I,1) + 1
              WHEN 0427
                 COMPUTE WS-U(I,1) = WS-U(I,1) + 1
              WHEN 0428 THRU 0510
                 COMPUTE WS-N(I,2) = WS-N(I,2) + 1
              WHEN 0511
                 COMPUTE WS-U(I,2) = WS-U(I,2) + 1
      *
              WHEN 0512 THRU 0524
                 COMPUTE WS-N(I,3) = WS-N(I,3) + 1
              WHEN 0525
                 COMPUTE WS-U(I,3) = WS-U(I,3) + 1
              WHEN 0526 THRU 0607
                 COMPUTE WS-N(I,4) = WS-N(I,4) + 1
              WHEN 0608
                 COMPUTE WS-U(I,4) = WS-U(I,4) + 1
              WHEN 0609 THRU 0620
                 COMPUTE WS-N(I,5) = WS-N(I,5) + 1
              WHEN 0621
                 COMPUTE WS-U(I,5) = WS-U(I,5) + 1
              WHEN 0622 THRU 0704
                 COMPUTE WS-N(I,6) = WS-N(I,6) + 1
              WHEN 0705
                 COMPUTE WS-U(I,6) = WS-U(I,6) + 1
              WHEN 0706 THRU 0718
                 COMPUTE WS-N(I,7) = WS-N(I,7) + 1
              WHEN 0719
                 COMPUTE WS-U(I,7) = WS-U(I,7) + 1              
              WHEN 0720 THRU 0801
                 COMPUTE WS-N(I,8) = WS-N(I,8) + 1
              WHEN 0802
                 COMPUTE WS-U(I,8) = WS-U(I,8) + 1
              WHEN 0803 THRU 0815
                 COMPUTE WS-N(I,9) = WS-N(I,9) + 1
              WHEN 0816
                 COMPUTE WS-U(I,9) = WS-U(I,9) + 1
              WHEN 0817 THRU 0829
                 COMPUTE WS-N(I,10) = WS-N(I,10) + 1
              WHEN 0830
                 COMPUTE WS-U(I,10) = WS-U(I,10) + 1
              WHEN 0831 THRU 0912
                 COMPUTE WS-N(I,11) = WS-N(I,11) + 1
              WHEN 0913
                 COMPUTE WS-U(I,11) = WS-U(I,11) + 1
              WHEN 0914 THRU 0925
                 COMPUTE WS-N(I,12) = WS-N(I,12) + 1
              WHEN 0926
                 COMPUTE WS-U(I,12) = WS-U(I,12) + 1              
              WHEN 0927 THRU 1009
                 COMPUTE WS-N(I,13) = WS-N(I,13) + 1
              WHEN 1010
                 COMPUTE WS-U(I,13) = WS-U(I,13) + 1
              WHEN 1011 THRU 1022
                 COMPUTE WS-N(I,14) = WS-N(I,14) + 1
              WHEN 1023
                 COMPUTE WS-U(I,14) = WS-U(I,14) + 1
              WHEN 1024 THRU 1105
                 COMPUTE WS-N(I,15) = WS-N(I,15) + 1
              WHEN 1106
                 COMPUTE WS-U(I,15) = WS-U(I,15) + 1
              WHEN 1107 THRU 1118
                 COMPUTE WS-N(I,16) = WS-N(I,16) + 1
              WHEN 1119
                 COMPUTE WS-U(I,16) = WS-U(I,16) + 1
              WHEN 1120 THRU 1201
                 COMPUTE WS-N(I,17) = WS-N(I,17) + 1
              WHEN 1202
                 COMPUTE WS-U(I,17) = WS-U(I,17) + 1              
              WHEN 1203 THRU 1214
                 COMPUTE WS-N(I,18) = WS-N(I,18) + 1
              WHEN 1215
                 COMPUTE WS-U(I,18) = WS-U(I,18) + 1
              WHEN 1216 THRU 1227 *> MULA
                 COMPUTE WS-N(I,19) = WS-N(I,19) + 1
              WHEN 1228
                 COMPUTE WS-U(I,19) = WS-U(I,19) + 1
      *        WHEN 1229 THRU 0110 *> REVERTES DATE DECEMBER -> JANUARY
              WHEN 1229 THRU 1231  *> DEC-JUN
                 COMPUTE WS-N(I,20) = WS-N(I,20) + 1
              WHEN 0101 THRU 1010  *> DEV-JUN
                 COMPUTE WS-N(I,20) = WS-N(I,20) + 1
              WHEN 0111
                 COMPUTE WS-U(I,20) = WS-U(I,20) + 1
              WHEN 0112 THRU 0123  *> UTTARA ASZADHA
                 COMPUTE WS-N(I,21) = WS-N(I,21) + 1
              WHEN 0124
                 COMPUTE WS-U(I,21) = WS-U(I,21) + 1
              WHEN 0125 THRU 0205
                 COMPUTE WS-N(I,22) = WS-N(I,22) + 1
              WHEN 0206
                 COMPUTE WS-U(I,22) = WS-U(I,22) + 1              
              WHEN 0207 THRU 0218
                 COMPUTE WS-N(I,23) = WS-N(I,23) + 1
              WHEN 0219
                 COMPUTE WS-U(I,23) = WS-U(I,23) + 1
              WHEN 0220 THRU 0303
                 COMPUTE WS-N(I,24) = WS-N(I,24) + 1
              WHEN 0304
                 COMPUTE WS-U(I,24) = WS-U(I,24) + 1
              WHEN 0305 THRU 0316
                 COMPUTE WS-N(I,25) = WS-N(I,25) + 1
              WHEN 0317
                 COMPUTE WS-U(I,25) = WS-U(I,25) + 1
              WHEN 0318 THRU 0330
                 COMPUTE WS-N(I,26) = WS-N(I,26) + 1
              WHEN 0331
                 COMPUTE WS-U(I,26) = WS-U(I,26) + 1
              WHEN 0401 THRU 0412
                 COMPUTE WS-N(I,27) = WS-N(I,27) + 1
      *        WHEN 0511
      *           COMPUTE WS-U(I,27) = WS-U(I,) + 1

           END-EVALUATE

           
           EXIT.
       PARA-ANALYSE-DATA.
           
           EXIT.
       PARA-WRITE-TO-DATASET.
           OPEN EXTEND RECLOG.  *> OUTPUT TO ERASE EXISTING LOG
           EVALUATE LK-FS       *> FILE STATUS
              WHEN 35           *>CAN NOT OPEN DS
           DISPLAY 'WARNING: ',
           'PRINT ONLY TO SPOOL, DATASET CAN NOT BE OPEN'
              WHEN 05           *>DS DOES NOT EXIST, CREATING NEW DS
           DISPLAY 'INFO:    ',
           'FIRST RUN, DATASET DOES NOT EXIST. CREATING NEW DATASET.'
           END-EVALUATE
           MOVE WS-RECV01 TO RECV01-DS
           WRITE REC01-DS
           END-WRITE.
           CLOSE RECLOG.
           EXIT.