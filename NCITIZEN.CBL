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
      *                      -RECORDS WRITE TO DATASET
      *          NO INCLUDES, COPYBOOKS, CALLS JUST SINGLE FILE
      *
      ******************************************************************
      *    ARGUMENTS FROM JCL TO PRINT STATISTICS FOR EVERY 120 YEARS
      *       BY 10 YEARS
      *    INPUT-NUMBER 1 TO 10 EXAMPLE 2 FOR 120 BY 10 + 120 BY 10
      *    INTERVAL = CURRENT YEAR - 120 YEARS IF 1 IS SELECTED
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
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
           EXEC SQL END DECLARE SECTION
           END-EXEC.
      *
           EXEC SQL  
              DECLARE STUDCUR CURSOR FOR  
              SELECT BDATE, GENDER, ALIVE FROM CITIZENS  
              WHERE BDATE <: WS-QDATE-LIMIT 
              *> CHECK DATE STANDARD FIRST TO IMPL
           END-EXEC.  


       01 WS-ALL.
         05 WS-ARGS.
          10 WS-INTERVAL-VAL          PIC   9(4).
          88 PASS VALUES ARE 01 THRU 10.
         05 WS-ADATE                  PIC   9(8).
         05 WS-USER                   PIC   X(8).
         05 WS-DB2-DATE-FORMAT        PIC   X(10).
                 *> IT GOES TO DATASET OUTPUT 
         05 WS-WREC01 OCCURS 1 TO 10 TIMES DEPENDING ON 
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
      
      
      
      
      
      *************************************
       LINKAGE SECTION.
       01 PARM-BUFFER.
         05 PARM-LENGTH               PIC   S9(4) COMP.
         05 PARM-DATA                 PIC   X(256).
       PROCEDURE DIVISION USING PARM-BUFFER.
           IF NOT PARM-LENGTH > 0 THEN
              DISPLAY 'WARNING: JCL PARM IS EMPTY. ',
              'AUTO COMPLEMENT INTERVAL SET TO 1'
              MOVE 1 TO WS-INTERVAL-VAL
              MOVE 1 TO RETURN-CODE
           ELSE
           MOVE PARM-DATA(1:2) TO WS-INTERVAL-VAL
           IF NOT PASS THEN
              DISPLAY 
              'WARNING: JCL PARM RANGE IS WRONG. TRY 01 TO 10 ',
              'AUTO COMPLEMENT INTERVAL SET TO 1'
              MOVE 1 TO WS-INTERVAL-VAL
              MOVE 2 TO RETURN-CODE
           ELSE
              ACCEPT WS-ADATE   FROM DATE YYYYMMDD
              ACCEPT WS-USER    FROM USERNAME

              PERFORM PROC-QUERY
              *> CATCH STH IF ERR ?
              PERFORM PROC-NAKSHATRA-COMPUTE
              PERFORM PROC-ANALYSE-DATA
              PERFORM PROC-WRITE-TO-DATASET
              
              MOVE 0 TO RETURN-CODE
           END-IF
           END-IF
      D    DISPLAY 'DEBUG: RC=', RETURN-CODE
           STOP RUN.
      ******************************************************************
       PROC-QUERY.
           EXEC SQL
              OPEN STUDCUR
           END-EXEC.

      * START CHECKING DB2 DATE FORMAT
           EXEC SQL  
              DECLARE STUDCUR CURSOR FOR  
              SELECT CURRENT DATE FROM SYSIBM.SYSDUMMY1 
           END-EXEC.  
      * FETCH QUERY


      * MAYBE MOVE TO INCLUDE ?
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
           

           EXEC SQL
              CLOSE STUDCUR
           END-EXEC.
           EXIT.
       PROC-NAKSHATRA-COMPUTE.
      ****************************************************************** 
      *
      ******************************************************************
           EVALUATE WS-MONTH-DAY *>MONTH DAY
              WHEN 0413
                 COMPUTE WS-U(I,27) = WS-U(I,) + 1
              WHEN 0414 THRU 0426
                 COMPUTE WS-N(I,1) = WS-N(I,1) + 1
              WHEN 0427
                 COMPUTE WS-U(I,1) = WS-U(I,) + 1
              WHEN 0528 THRU 0510
                 COMPUTE WS-N(I,2) = WS-N(I,2) + 1
              WHEN 0511
                 COMPUTE WS-U(I,2) = WS-U(I,) + 1

           END-EVALUATE
           EXIT.
       PROC-ANALYSE-DATA.
           
           EXIT.
       PROC-WRITE-TO-DATASET.

           EXIT.