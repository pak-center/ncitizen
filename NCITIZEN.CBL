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
      *                      -READ/WRITE RECORDS TO THE DATASET
      *          SINGLE FILE CODE (NO INCLUDES, NO COPYBOOKS, NO CALLS)
      *          PLUS JCL FILE TO RUN WITH DD EXAMPLE AND FOR PARM
      ******************************************************************
      *    ARGUMENTS FROM JCL TO PRINT STATISTICS FOR
      *    THE LAST 120 YEARS WITH 10 YEARS INTERVALS AS DEFAULT
      *    FIRST PARM IS SQ,MQ OR DS. LIKE _Q=SQL, DS=DATASET 
      *    LIKE PARM='MQ,120' OR 'DS,030' 
      ******************************************************************
      *    JCL PARM='010' EQ 12 TABLES FOR EVERY 10 YEARS FROM 120
      *    JCL PARM='001' EQ 120 TABLES FOR EVERY YEAR FROM 120
      *    JCL PARM='120' EQ 1 TABLE FOR ALL 120 YEARS OF STATISTICS
      *    WARNING
      *       NUMBER MUST GIVES MOD 0 WHEN DIVIDING 120 BY IT E.G.
      *       120 / 40 = 3 CORRECT, 120 / 70 = 1,7 WRONG
      *       WHEN WRONG NR IS GIVEN PGM SETS IT TO DEFAULT 10 AS BELOW
      *       120 / 10 = 12 INTERVALS FOR 10 YEARS LONG
      *    
      *    THERE ARE TWO MODES TO CHOOSE FOR SQL QUERIES:
      *  1) MULTIPLE ROWS SQL QUERY FOR ALL NAKSHATRAS IS NOT EFFICIENT!
      *    BUT SIMPLE AND WE HAVE SURE IT WILL WORK ON EVERY DB2 CONFIG
      *  2) CHANGES TO ONE ROW SQL QUERIES FOR EVERY NAKSHATRA GIVES
      *    MORE PERFORMANCE BUT MAY NOT WORK FOR ALL DATABASES VERSIONS
      ******************************************************************
      *    IMPLEMENTATION OF DB2 CONFIGURATION
      *       WE IMPLY DB2 HAS VARIABLES: 
      *       DATE, GENDER: M OR F FOR M=MALE,  F=FAMALE
      *       ALIVE STATUS: A OR D FOR A=ALIVE, D=DEAD   
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT RECLOGW ASSIGN TO NCLOGW 
           ORGANIZATION IS SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL 
           FILE STATUS IS LK-FS-W.  
      *    
           SELECT OPTIONAL RECLOGR ASSIGN TO NCLOGR 
           ORGANIZATION IS SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL 
           FILE STATUS IS LK-FS-R. 
       DATA DIVISION.
       FILE SECTION. 
       FD RECLOGW
           BLOCK CONTAINS 0 RECORDS *>TODO AFTER IMPL. OF RECORDS 
      *    TODO:
      *    WHEN RECV01 IS COMPLETED CHECK RECORD LENGTH MIN & MAX
      *    AND CHANGE VALUES 100 AND 900 TO NEW, PROPER ONES      
           RECORD IS VARYING IN SIZE FROM 100 to 900 *>CHANGE RANGE !
           DEPENDING ON REC01-DS-LEN *> TODO IN WS-
           RECORDING MODE IS V 
           DATA RECORD IS REC01-DS.
      ******************************************************************
       01 REC01-DS. 
      ******************************************************************       
      * TODO: BINARY FORM OF DATA INSIDE THE STRUCTURE
      ******************************************************************
        05 REC01-DS-HEADER.
         07 REC01-DS-LEN. *>TODO
        05 REC01-DS-TABLE-STATS.
        05 REC01-DS-SUMMARY.

      ******************************************************************      
       FD RECLOGR    
           BLOCK CONTAINS 0 RECORDS *>TODO AFTER IMPL. OF RECORDS 
      *    TODO:
      *    WHEN RECV01 IS COMPLETED CHECK RECORD LENGTH MIN & MAX
      *    AND CHANGE VALUES 100 AND 900 TO NEW, PROPER ONES      
           RECORD IS VARYING IN SIZE FROM 100 to 900 *>CHANGE RANGE !
           DEPENDING ON REC01-DS-R-LEN *> TODO IN WS-
           RECORDING MODE IS V 
           DATA RECORD IS REC01-DS-R. 
      * ----------------------------------------------------------------
       01 REC01-DS-R. 
        05 REC01-DS-R-HEADER.
         07 REC01-DS-R-LEN. *>TODO
        05 REC01-DS--RTABLE-STATS.
        *> HERE MAY BE MORE RTABLE-STATS FROM 1-120
        05 REC01-DS-R-SUMMARY.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *  BEGIN SQL VARIABLES & STRUCTURES
      ****************************************************************** 
           EXEC SQL  
              INCLUDE SQLCA  
           END-EXEC. 
      *     EXEC SQL
      *        INCLUDE DCLEMP *> DCLGEN FOR FUTURE USE
      *     END-EXEC.
      * ---------------------------------------------------------------
           EXEC SQL BEGIN DECLARE SECTION
           END-EXEC. 
       01 WS-QREC01. 
      *  05 WS-QREC01-DATE-DB2       PIC   X(10). *> CHAR FOR DB2 DATE
      * ---------------------------------------------------------------
        05 WS-QREC01-COMMON. 
         07 WS-QREC01-INTERVAL-DECIMAL   PIC   S999 COMP-3 VALUE 10.
      * ---------------------------------------------------------------   
         07 WS-QREC01-GENDER              PIC   X. *> DB2 CHAR  
         88 WS-QREC01-GENDER-FLAG-MALE    VALUE 'M'. 
         88 WS-QREC01-GENDER-FLAG-FEMALE  VALUE 'F'. 
      * ---------------------------------------------------------------  
         07 WS-QREC01-ALIVE               PIC   X. *> DB2 CHAR 
         88 WS-QREC01-ALIVE-FLAG-ALIVE    VALUE 'A'. 
         88 WS-QREC01-ALIVE-FLAG-DEAD     VALUE 'D'. 
      * ---------------------------------------------------------------   
        05 WS-QREC01-SQ.
         07 WS-QREC01-SQ-DATE            PIC   X(10).
      *   07 WS-QREC01-DATE-CURRENT-DB2   PIC   X(10).
      *    TABLE FOR ROWSET PROCESSING FOR SQ QUERY
        05 WS-QREC01-SQ-ROWSET. *> MAX SIZE OF ROWSET IS 32,757
         10 WS-QREC01-SQ-ROWSET-Y        PIC S9(04) COMP *>INT DB2
               OCCURS 3200 TIMES INDEXED BY INX-SQ-Y. 
         10 WS-QREC01-SQ-ROWSET-MD        PIC S9(04) COMP *>INT DB2
               OCCURS 3200 TIMES INDEXED BY INX-SQ-MD. 
      *   10 WS-QREC01-SQ-ROWSET-D        PIC S9(04) COMP *>INT DB2
      *         OCCURS 2300 TIMES INDEXED BY INX-SQ-D. 
         10 WS-QREC01-SQ-ROWSET-GENDER   PIC X     OCCURS 3200 TIMES
                                                   INDEXED BY INX-SQ-G.  
         10 WS-QREC01-SQ-ROWSET-ALIVE    PIC X     OCCURS 3200 TIMES
                                                   INDEXED BY INX-SQ-A.         
      * ---------------------------------------------------------------  
        05 WS-QREC01-MQ.
         07 WS-QREC01-DATE-LOWER         PIC   X(10). 
         07 WS-QREC01-DATE-UPPER         PIC   X(10). 
         07 WS-QREC01-DATE-UPPER-I       PIC   X(10). 
         07 WS-QREC01-MQ-YEAR            PIC   S9(4)  COMP.
         07 WS-QREC01-MQ-NR              PIC   S9(8)  COMP.
        05 WS-QREC01-MQ-ROWSET. *> MAX SIZE OF ROWSET IS 32,757
         07 WS-0QREC01-MQ-ROWSET-Y   PIC S9(04) COMP *>INT DB2
               OCCURS 121 TIMES INDEXED BY INX-MQ-Y. 
         07 WS-0QREC01-MQ-ROWSET-NR PIC S9(08) COMP *>INT DB2
               OCCURS 121 TIMES INDEXED BY INX-MQ-N.         
      * ---------------------------------------------------------------
           EXEC SQL END DECLARE SECTION 
           END-EXEC. 
      ******************************************************************
      *    INITIALIZE VARIABLES FOR SQL EXECS
      ******************************************************************
      * --------------------------------------------------------------- 
      *    SETS FIRST DAY AND MONTH OF THE YEAR E.G. 2023/01/01   
      * ---------------------------------------------------------------
           EXEC SQL  
              DECLARE CD01 CURSOR FOR 
              SELECT 
              THIS_YEAR(CURRENT DATE) 
              FROM SYSIBM.SYSDUMMY1 
              FOR FETCH ONLY
           END-EXEC.
      * --------------------------------------------------------------- 
      *    SETS LAST DAY AND MONTH OF THE YEAR E.G. 2023/12/31   
      * ---------------------------------------------------------------
           EXEC SQL  
              DECLARE CD02 CURSOR FOR 
              SELECT 
              THIS_YEAR(CURRENT DATE) + 1 YEARS - 1 DAYS  
              FROM SYSIBM.SYSDUMMY1 
              FOR FETCH ONLY
           END-EXEC.
      *ADDED DECIMAL INTERVAL FOR LOOP FIRST ITERATION 
            EXEC SQL  
              DECLARE CD03 CURSOR FOR 
              SELECT 
              DATE(:WS-QREC01-DATE-UPPER) + 
               :WS-QREC01-INTERVAL-DECIMAL YEARS 
              FROM SYSIBM.SYSDUMMY1 
              FOR FETCH ONLY
           END-EXEC.     
      ******************************************************************
      *    SQ - MAIN QUERY TO SELECT MULTIPLE ROWS FOR PROCESSING
      *         WORST PERFORMANCE AND HEAVY LOAD   
      ******************************************************************
           EXEC SQL 
              DECLARE SQ01 CURSOR WITH ROWSET POSITIONING FOR  
              SELECT 
              CAST((YEAR(CURRENT DATE) - YEAR(BDATE)) AS SMALLINT), 
              *>AGE FROM YEAR(BDATE), 
              CAST(((MONTH(BDATE) * 100) + DAY(BDATE)) AS SMALLINT)
      *        CAST(MONTH(BDATE) AS SMALLINT),
      *        CAST(DAY(BDATE) AS SMALLINT),
              GENDER, 
              ALIVE 
              FROM CITIZENS 
              WHERE YEAR(BDATE) 
              BETWEEN 
              YEAR(ADD_YEARS(DATE(:WS-QREC01-DATE-LOWER), 
              :WS-QREC01-INTERVAL-DECIMAL)) *>MUST BE NEGATIVE, *(-1)
              AND 
              YEAR(ADD_YEARS(DATE(:WS-QREC01-DATE-LOWER), 
              :WS-QREC01-INTERVAL-DECIMAL)) 
              FOR FETCH ONLY 
           END-EXEC.  
      ******************************************************************
      *    MQ - ALTERNATIVE ONE ROW QUERIES FOR BETTER PERFORMANCE 
      *         27 NAKSHATRAS WITH VARIATIONS OF QUERIES TODO 
      ******************************************************************
      * ---------------------------------------------------------------
      *    DYNAMIC SQL SQUERY FOR TOTAL STATS SECTION 
      *    RUNS X4 INSIDE LOOP (ALIVE, GENDER) FOR EVERY INTERVAL
      *    A,M A,F D,M D,F
      * ---------------------------------------------------------------
           EXEC SQL 
              DECLARE MQ01 CURSOR WITH ROWSET POSITIONING FOR 
              SELECT *> FOR GLOBAL COUNT_BIG()
              CAST(YEAR(BDATE) AS SMALLINT), COUNT(YEAR(BDATE)) *>AS NR 
              FROM CITIZENS 
              WHERE 
              ALIVE = :WS-QREC01-ALIVE 
              AND 
              GENDER = :WS-QREC01-GENDER 
              AND 
              YEAR(BDATE) 
              BETWEEN 
              YEAR(DATE(WS-QREC01-DATE-LOWER)) - *>2023/01/01 
              WS-QINTERVAL-DECIMAL YEARS 
              AND 
              YEAR(DATE(WS-QREC01-DATE-UPPER-I)) - *>2023/12/31 
              WS-QINTERVAL-DECIMAL YEARS 
              GROUP BY YEAR(BDATE) 
              FOR FETCH ONLY 
           END-EXEC. 
      ******************************************************************
      *    MQ - NAKSHATRAS SQL QUERIES VARIATIONS
      *         WITH VARIABLES
      ******************************************************************
      *    TODO
      * ---------------------------------------------------------------


      ******************************************************************
      *    END OF SQL VARIABLES & STRUCTURES
      ****************************************************************** 
      ******************************************************************       
      *    RECORDS RELATED WITH FD AND RECORDS INSIDE DATASET
      *    STRUCTURE IS BINARY
      *    STRUCTURE MUST BE READ BY PGM AND PRINT - DISPLAY TO THE USER
      ******************************************************************
       01 REC01.
      * ---------------------------------------------------------------
        05 REC01-HEADER.
         10 REC01-RLENGTH         PIC   9(4) COMP-5. *> 65535
         10 FILLER                 PIC   X(76).
         10 REC01-CRC             PIC   A(30) VALUE 
                                   'AUTHOR: PRZEMYSLAW ADAM KUPISZ'.
         10 REC01-HLINE           PIC   X(80) VALUES ALL '*'.
         10 REC01-PGM.
          15 REC01-PGM-START-DATE PIC   99(4)/99/99.
         10 FILLER                 PIC   X(40) VALUES ALL '@'.
      *
         10 REC01-USER            PIC   X(8).  
      *   10 REC01-CURRENT-DATE-DB2    PIC   X(10).           
         10 REC01-INTERVAL-VAL    PIC   S999 COMP-3 VALUE 10. 
         10 REC01-INTERVAL-COUNT  PIC   S9(3) VALUE 120. *>MAX YEARS  

      * ---------------------------------------------------------------        
      * CHECK 05 date1 FORMAT DATE "%m/%d/@Y".       
      * ---------------------------------------------------------------
        05 REC01-TABLE-STATS.
         10 REC01-TAB OCCURS 1 TO 120 TIMES DEPENDING ON 
                       REC01-INTERVAL-COUNT INDEXED BY I.
      *   15 REC01-CITIZEN OCCURS 2 TIMES. *> 1-ALIVE, 2-DEAD
      *    20 REC01-MALE           PIC   9(9) COMP-5. *>(18) MAX
      *    20 REC01-FEMALE         PIC   9(9) COMP-5.
         15 REC01-NTAB OCCURS 27 TIMES INDEXED BY INX-N. 
          20 WS-N                  PIC   9(9) COMP-5.
          20 WS-U                  PIC   9(9) COMP-5.
          20 REC01-NTAB-AGE OCCURS 120 TIMES INDEXED BY INX-AGE. 
      *     25 REC01-NTAB-AGE      PIC 9(9) COMP-5. 
      *     25 REC01-NTAB-AGE-U    PIC 9(9) COMP-5. 
          23 REC01-NTAB-CITIZEN OCCURS 2 TIMES. *> INDEXED BY INX-AC. 
          *> 1-ALIVE,2-DEAD
           25 REC01-NTAB-CITIZEN-MALE   PIC 9(9) COMP-5. 
           25 REC01-NTAB-CITIZEN-FEMALE PIC 9(9) COMP-5. 
          23 REC01-NTAB-UNKNOWN OCCURS 2 TIMES. *> INDEXED BY INX-AU. 
          *> 1-ALIVE,2-DEAD
           25 REC01-NTAB-UNKNOWN-MALE   PIC 9(9) COMP-5. 
           25 REC01-NTAB-UNKNOWN-FEMALE PIC 9(9) COMP-5. 
      
      * ---------------------------------------------------------------
        05 REC01-TOTAL-SUMMARY.   
      *   10 REC01-T-CITIZEN                PIC   9(9) COMP-5.
         10 REC01-T-CITIZEN-ALIVE          PIC   9(9) COMP-5.
         10 REC01-T-CITIZEN-ALIVE-MALE     PIC   9(9) COMP-5.
         10 REC01-T-CITIZEN-ALIVE-FEMALE   PIC   9(9) COMP-5.
      *   
         10 REC01-T-NAKSH-MAX-CITIZEN      PIC   9(9) COMP-5.
         10 REC01-T-NAKSH-MIN-CITIZEN      PIC   9(9) COMP-5.
         10 REC01-T-NAKSH-MAX-CITIZEN-M    PIC   9(9) COMP-5.
         10 REC01-T-NAKSH-MIN-CITIZEN-F    PIC   9(9) COMP-5.
         
      * ---------------------------------------------------------------        
      *    THIS PART MUST BE REWRITTEN AND MOVED TO RECV01
      * ---------------------------------------------------------------
       01 WS-PGM-INPUT.
         05 WS-PGM-ARGS.
           15 WS-PGM-MODE                 PIC   AA. *> 'SQ','MQ','DS'
           15 WS-PGM-SEPARATOR            PIC   X VALUE ','.
           15 WS-PGM-INTERVAL-VAL         PIC   S999 COMP-3 VALUE 10.
      *     88 WS-PGM-PASS VALUES ARE 1 THRU 120.
         05 WS-PGM-RESULT                 PIC   S9(3).
         05 WS-PGM-REMINDER               PIC   S9(3).
      * ---------------------------------------------------------------
         05 WS-TAB-COUNTER            PIC   S9(3) VALUE 1.
         05 WS-DB2-DATE-FORMAT        PIC   X(10).
      ******************************************************************   
      *     10 WS-W-DATE                PIC   X(10).
      *      15 WS-W-YEAR                   PIC   9(4).
      *      15 WS-W-MONTH                  PIC   99.
      *      15 WS-W-DAY                    PIC   99.
       01 WS-SQ-COMMON.
        05 WS-SQ-INX01    PIC   S9(4) COMP-5 VALUE 3200. *>SQ TABLE L
      ******************************************************************
      *    VARIABLES FOR PARA-SQL-MQ.
      ******************************************************************
       01 WS-SQL-MQ.
        05 WS-SQL-MQ-I                PIC   S9(3).
        05 WS-Q-DB2-CURRENT-Y         PIC   S9(4) COMP.
      ******************************************************************
      *    VARIABLES FOR PROC-NAKSHATRA-COMPUTE AND PROC-ANALYSE-DATA
      ******************************************************************
       01 WS-STATS.
        05 WS-MONTH-DAY               PIC   S9(4) COMP. 
       01 WS-NC-FLAG                  PIC   S9(3)  COMP-5.
       01 WS-NU  PIC S9 BINARY VALUE 0.
       88 WS-NU-FLAG-N VALUE 0.
       88 WS-NU-FLAG-U VALUE 1.
      ******************************************************************
      *    FS
       01 WS-EOF-FLAG                 PIC   A.
      ******************************************************************
      *    FOR ABEND CALL ROUTINE TO BETTER DBG (SQL QUERY ERROR) 
       01  ABEND-CODE                 PIC   S9(9) BINARY.
       01  TIMING                     PIC   S9(9) BINARY.
      ******************************************************************
       LINKAGE SECTION.
       01 PARM-BUFFER.
         05 PARM-LENGTH               PIC   S9(4) COMP.
         05 PARM-DATA                 PIC   X(256).
       01 LK-FS-W. *>                    PIC   99.
       01 LK-FS-R.
      ******************************************************************
      *    PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING PARM-BUFFER, LK-FS-W, LK-FS-R. 
      * ---------------------------------------------------------------
      *    ARGS CHECKING COMPLETE
      * ---------------------------------------------------------------
           EVALUATE PARM-LENGTH ALSO PARM-DATA(1:2) 
           WHEN 2 ALSO 'DS' 
            PERFORM PARA-READ-FROM-DATASET 
            PERFORM PARA-CREATE-VIEW 
           WHEN 2 THRU 6 ALSO 'SQ' 
              *> SLOW & NOT EFFICIENT (SINGLE) SQL QUERY
            PERFORM PARA-VALIDATE-ARGS  
            PERFORM PARA-SQL-DATE-DB2 
            PERFORM PARA-SQL-SQ *> (PARA-NAKSHATRA-COMPUTE INSIDE)
            PERFORM PARA-ANALYSE-DATA
            PERFORM PARA-CREATE-VIEW
            PERFORM PARA-WRITE-TO-DATASET
           WHEN 2 THRU 6 ALSO 'MQ' 
              *> FASTER & EFFCIENT SQL (MULTI) QUERIES
              *> BUT NEEDS A LOT OF EXEC SQL FOR NAKSHATRAS
            PERFORM PARA-VALIDATE-ARGS
            PERFORM PARA-SQL-DATE-DB2 
            PERFORM PARA-SQL-MQ
            PERFORM PARA-ANALYSE-DATA
            PERFORM PARA-CREATE-VIEW
            PERFORM PARA-WRITE-TO-DATASET
           WHEN OTHER 
            DISPLAY 'WARNING: WRONG PARM IN JCL. PARM=',
             PARM-DATA(1:PARM-LENGTH)
            DISPLAY ' NOTHING TO DO... SORRY. EXITS PGM' 
            MOVE 2 TO RETURN-CODE 
           END-EVALUATE   
      * ---------------------------------------------------------------
      D    DISPLAY 'DEBUG: RC=', RETURN-CODE
           STOP RUN.
      ******************************************************************
      *    STOP RUN PGM
      ******************************************************************
       PARA-VALIDATE-ARGS.
      ******************************************************************
      *    TEXT TO NUMERIC CONVERSION & LENGTH CHECK 1-120
      ******************************************************************     
           COMPUTE WS-PGM-INTERVAL-VAL = 
           FUNCTION NUMVAL(PARM-DATA(4:(PARM-LENGTH - 3)))
           IF NOT (WS-PGM-INTERVAL-VAL > 0 AND 
            WS-PGM-INTERVAL-VAL <= 120)
            MOVE 10 TO WS-PGM-INTERVAL-VAL *> SETS DEFAULT INTERVAL
            DISPLAY 
              'WARNING: JCL PARM RANGE IS WRONG. TRY 001 TO 120 ',
              'AUTO COMPLEMENT INTERVAL SET TO 10'
            MOVE 2 TO RETURN-CODE  
           END-IF
      * ---------------------------------------------------------------    
      *    TAKES SOME INFO FROM OS TO PRINT IN SUMMARY VIEW
      * ---------------------------------------------------------------
           ACCEPT REC01-PGM-START-DATE FROM DATE YYYYMMDD
           ACCEPT REC01-USER           FROM USERNAME
      D    DISPLAY 'DEBUG: ', WS-ARGS, ' PARAMETERS FROM JCL'
      D    DISPLAY 'DEBUG: ', RECV01-USER, RECV01-PGM-START-DATE,
      D             'USER, CURRENT DATE'            
      * ---------------------------------------------------------------
      *    CHANGE VAL FROM JCL (1-120) TO PROPER FOR TAB OCCURS  
      *    DIVIDE 120 BY INTERVAL-VAL  
      *    CHECKING OF CORRECTIVNESS FOR DIVIDE
      * ---------------------------------------------------------------
           PERFORM UNTIL WS-PGM-REMINDER = 0
           DIVIDE 120 BY WS-PGM-INTERVAL-VAL 
           GIVING WS-PGM-RESULT REMAINDER WS-PGM-REMINDER
           IF WS-PGM-REMINDER IS NOT ZERO THEN
              COMPUTE WS-PGM-INTERVAL-VAL = WS-PGM-INTERVAL-VAL + 1
           END-IF
           END-PERFORM
           MOVE WS-PGM-INTERVAL-VAL TO REC01-INTERVAL-VAL *>4SQL Q
           DIVIDE REC01-INTERVAL-VAL INTO REC01-INTERVAL-COUNT *>4TAB         
      
           EXIT.      
      ******************************************************************
      *    PARAGRAPH SQL DATE DB2
      ******************************************************************
       PARA-SQL-DATE-DB2. 

           IF PARM-DATA(1:1) = 'S' THEN 
            COMPUTE WS-QREC01-INTERVAL-DECIMAL = -1 * 
                    WS-QREC01-INTERVAL-DECIMAL 
           END-IF 

           EXEC SQL
              OPEN CD01 
           END-EXEC. 
           PERFORM PARA-SQL-ERROR-CHECK 
      *     
           EXEC SQL
           FETCH CD01 INTO 
              :WS-QREC01-DATE-LOWER 
           END-EXEC 
           PERFORM PARA-SQL-ERROR-CHECK 
           MOVE REC01-INTERVAL-VAL TO WS-QREC01-INTERVAL-DECIMAL 
      *VERIFY
           EXEC SQL
              CLOSE CD01 
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
      * ---------------------------------------------------------------  
           EXEC SQL
              OPEN CD02
           END-EXEC. 
           PERFORM PARA-SQL-ERROR-CHECK 
      *     
           EXEC SQL
           FETCH CD02 INTO 
              :WS-QREC01-DATE-UPPER 
           END-EXEC        
           PERFORM PARA-SQL-ERROR-CHECK 
      *TODO
           EXEC SQL
              CLOSE CD02
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
      * ---------------------------------------------------------------     
           EXEC SQL
              OPEN CD03
           END-EXEC. 
           PERFORM PARA-SQL-ERROR-CHECK 
      *
           EXEC SQL
           FETCH CD03 INTO 
              :WS-QREC01-DATE-UPPER-I 
           END-EXEC 
           PERFORM PARA-SQL-ERROR-CHECK 
      *TODO
           EXEC SQL
              CLOSE CD03
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
      ******************************************************************
           EXIT.
      ******************************************************************
      *    PARAGRAPH SQL SQ     
      ******************************************************************     
       PARA-SQL-SQ.    
           EXEC SQL
              OPEN SQ01 
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
      *  IMPLEMENT HERE LOOP PERFORM FOR GENERATING DYNAMIC TABLES
      *    1 - 120
           SET I TO 1 
           MOVE REC01-INTERVAL-VAL TO WS-PGM-INTERVAL-VAL *> NEEDED?
           PERFORM REC01-INTERVAL-COUNT TIMES 
      ******************************************************************
           *> SQL QUERY HERE
           PERFORM UNTIL SQLCODE = 100 
           EXEC SQL 
           FETCH NEXT ROWSET FROM SQ01 FOR 3200 ROWS 
           INTO 
           :WS-QREC01-SQ-ROWSET-Y, *>INT 4 BYTES DB2
           :WS-QREC01-SQ-ROWSET-MD, *>INT 4 BYTES DB2
      *     :WS-QREC01-SQ-ROWSET-D, *>INT 4 BYTES DB2
           :WS-QREC01-SQ-ROWSET-GENDER, *> CHAR DB2
           :WS-QREC01-SQ-ROWSET-ALIVE *> CHAR DB2
           END-EXEC 
           PERFORM PARA-SQL-ERROR-CHECK 
      ******************************************************************
      *    COMPUTES NAKSHATRA 
      ******************************************************************
           PERFORM PARA-NAKSHATRA-COMPUTE *>TODO FOR ROWSET!
           END-PERFORM

      ******************************************************************        
           ADD REC01-INTERVAL-VAL TO WS-PGM-INTERVAL-VAL
           MOVE WS-PGM-INTERVAL-VAL TO WS-QREC01-INTERVAL-DECIMAL
           SET I UP BY 1 
           END-PERFORM 

           EXEC SQL
              CLOSE SQ01 
           END-EXEC.   
           PERFORM PARA-SQL-ERROR-CHECK 
           EXIT.
      ******************************************************************
       PARA-SQL-MQ. 
      ******************************************************************
      *    EXEC SQL QUERIES FOM MULTI - BEST PERFORMANCE
      ******************************************************************
      *     PERFORM  *> TODO
           EXEC SQL 
            OPEN MQ01 
           END-EXEC. 
           PERFORM PARA-SQL-ERROR-CHECK 
      * ---------------------------------------------------------------    
           PERFORM
            VARYING WS-SQL-MQ-I
            FROM REC01-INTERVAL-COUNT BY REC01-INTERVAL-COUNT
            UNTIL WS-SQL-MQ-I > 120
              *>TODO NEED PARAM CHANGE E.G. GENDER M OR F
      ******************************************************************
      *    4 QUERIES FOR SUMMARY RAPORT WITH GENDER AND ALIVE STATUS 
      *    TOTAL CITIZENS COUNT 
      ******************************************************************
            PERFORM UNTIL SQLCODE = 100 
           EXEC SQL 
            FETCH NEXT ROWSET FROM MQ01 FOR 121 ROWS INTO 
            :WS-0QREC01-MQ-ROWSET-Y, 
            :WS-0QREC01-MQ-ROWSET-NR 
           END-EXEC  
           PERFORM PARA-SQL-ERROR-CHECK 
      *    TODO LOOP FOR 121 ROWS TO COUNT
      *----------------
      *     PERFORM PARA-NAKSHATRA-COMPUTE 
      *----------------
            END-PERFORM 
      *      
           END-PERFORM 
      * ---------------------------------------------------------------
           EXEC SQL
            CLOSE MQ01
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
           EXIT.
      ******************************************************************
       PARA-NAKSHATRA-COMPUTE.
      ****************************************************************** 
      *    TODO !
      *    BECAUSE OF 2 DIFFERENT MODES (SQ & MQ) WE NEED 2 TYPES OF
      *    RECORDS AND DIFFERENT PROCEEDING WAYS OR MORE MQ QUERIES
      ******************************************************************
              *> WS-QBDATE
              *> WS-QGENDER
              *> WS-QALIVE
      *    EXTRACT AGE FROM CURRENT YEAR - YEAR-OF-BIRTH AND +1 TO 
      *    AGE OCCURS 120 PIC 9(8) COMP-5. WITH INDEX 
      ******************************************************************        
           *>TODO:
           *> LEGACY CODE MODIFY IT FOR ROWSET SUPPORT TOMORROW
           *> MOVE TO LOOP FOR ARRAY_2000 !
           EVALUATE WS-NC-FLAG 
      * ---------------------------------------------------------------
      *    SQ 01
      * ---------------------------------------------------------------
            WHEN 101   *>SQ01
      * ---------------------------------------------------------------
      *    LOOP FOR TABLE_2000 COMPUTATION => RECV01
      * ---------------------------------------------------------------
            SET INX-SQ-Y TO 1
            SET INX-SQ-MD TO 1
      *      SET INX-SQ-D TO 1
            *>
            SET INX-SQ-G TO 1
            SET INX-SQ-A TO 1
            PERFORM UNTIL EXIT *> WS-SQ-INX01 = 2000 DEC BY 1
      * ---------------------------------------------------------------      
      *    CHANGE YEAR OF BIRTH TO AGE FROM VALIDATED RANGE 1-120
      * ---------------------------------------------------------------
      *    TODO IMPL QDATE FOR AGE 1 - 120 YEARS
      *    COMPUTE GOES TO NOT TO 1 VAR BUT TO TABLE_1-120 VARS
      *    TODO SELECTS PROPER NAKSHATRA AND SETS INDEX FOR IT
      *     SUBTRACT WS-QREC01-SQ-ROWSET-Y(INX-SQ-Y) FROM 
      *    WS-Q-DB2-CURRENT-Y GIVING WS-QREC01-SQ-ROWSET-Y(INX-SQ-Y) 
           EVALUATE WS-QREC01-SQ-ROWSET-Y(INX-SQ-Y) 
            WHEN 1 THRU 120 
              CONTINUE  
            WHEN ZERO 
              MOVE 1 TO WS-QREC01-SQ-ROWSET-Y(INX-SQ-Y) 
            WHEN OTHER 
              MOVE 120 TO WS-QREC01-SQ-ROWSET-Y(INX-SQ-Y) 
           END-EVALUATE  
      * ---------------------------------------------------------------
      *    RM COMPUTE TO SET INDEX TO 1-27 OF NAKSHATRAS ...   
      * ---------------------------------------------------------------
      
      *     COMPUTE WS-MONTH-DAY = WS-QREC01-SQ-ROWSET-M * 100 
      *                          + WS-QREC01-SQ-ROWSET-D 
           EVALUATE WS-QREC01-SQ-ROWSET-MD 
              WHEN 0413 
                 SET INX-N TO 27
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1 
              WHEN 0414 THRU 0426 
                 SET INX-N TO 1
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0427
                 SET INX-N TO 1
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0428 THRU 0510
                 SET INX-N TO 2
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0511
                 SET INX-N TO 2
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
      *
              WHEN 0512 THRU 0524
                 SET INX-N TO 3
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0525
                 SET INX-N TO 3
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0526 THRU 0607
              SET INX-N TO 4
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0608
              SET INX-N TO 4
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0609 THRU 0620
              SET INX-N TO 5
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0621
              SET INX-N TO 5
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0622 THRU 0704
              SET INX-N TO 6
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0705
              SET INX-N TO 6
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0706 THRU 0718
              SET INX-N TO 7 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0719
              SET INX-N TO 7
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0720 THRU 0801
              SET INX-N TO 8 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0802 
              SET INX-N TO 8 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0803 THRU 0815
              SET INX-N TO 9 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0816
              SET INX-N TO 9
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0817 THRU 0829
              SET INX-N TO 10 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0830
              SET INX-N TO 10
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0831 THRU 0912
              SET INX-N TO 11 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0913
              SET INX-N TO 11 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0914 THRU 0925
              SET INX-N TO 12 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0926
              SET INX-N TO 12 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0927 THRU 1009
              SET INX-N TO 13 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1010
              SET INX-N TO 13 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1011 THRU 1022
              SET INX-N TO 14 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1023
              SET INX-N TO 14 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1024 THRU 1105 
              SET INX-N TO 15 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1106 
              SET INX-N TO 15 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1107 THRU 1118 
              SET INX-N TO 16 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1119 
              SET INX-N TO 16 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1120 THRU 1201 
              SET INX-N TO 17 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1202 
              SET INX-N TO 17 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 1203 THRU 1214 
              SET INX-N TO 18 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1215 
              SET INX-N TO 18 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1216 THRU 1227 *> MULA
              SET INX-N TO 19 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1228 
              SET INX-N TO 19 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
      *        WHEN 1229 THRU 0110 *> REVERTES DATE DECEMBER -> JANUARY
              WHEN 1229 THRU 1231  *> DEC-JUN
                 SET INX-N TO 20 
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0101 THRU 0110  *> DEV-JUN
              SET INX-N TO 20 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0111 
              SET INX-N TO 20 
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0112 THRU 0123  *> UTTARA ASZADHA
                 SET INX-N TO 21 
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0124
              SET INX-N TO 21 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0125 THRU 0205
              SET INX-N TO 22 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0206
              SET INX-N TO 22 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0207 THRU 0218
              SET INX-N TO 23 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0219
              SET INX-N TO 23 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0220 THRU 0303 
              SET INX-N TO 24 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0304 
              SET INX-N TO 24 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0305 THRU 0316 
              SET INX-N TO 25 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0317 
              SET INX-N TO 25 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0318 THRU 0330
              SET INX-N TO 26 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0331 
              SET INX-N TO 26 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0401 THRU 0412 
              SET INX-N TO 27 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
           END-EVALUATE
      * ---------------------------------------------------------------
      *TODO INSIDE ONE OF 27 N. PUTS GENDER,ALIVE STATUS
           EVALUATE WS-QREC01-SQ-ROWSET-ALIVE(INX-SQ-A) 
            WHEN 'A' 
             EVALUATE WS-QREC01-SQ-ROWSET-GENDER(INX-SQ-G) ALSO 
                      WS-NU-FLAG-N  
              WHEN 'M' ALSO TRUE 
              COMPUTE REC01-NTAB-CITIZEN-MALE(I,INX-N,INX-AGE,1) 
              = REC01-NTAB-CITIZEN-MALE(I,INX-N,INX-AGE,1) + 1 
              WHEN 'M' ALSO FALSE 
              COMPUTE REC01-NTAB-UNKNOWN-MALE(I,INX-N,INX-AGE,1) 
              = REC01-NTAB-UNKNOWN-MALE(I,INX-N,INX-AGE,1) + 1 
              WHEN 'F'  ALSO TRUE 
              COMPUTE REC01-NTAB-CITIZEN-FEMALE(I,INX-N,INX-AGE,1) 
              = REC01-NTAB-CITIZEN-FEMALE(I,INX-N,INX-AGE,1) + 1 
             WHEN 'F'  ALSO FALSE  
              COMPUTE REC01-NTAB-UNKNOWN-FEMALE(I,INX-N,INX-AGE,1) 
              = REC01-NTAB-UNKNOWN-FEMALE(I,INX-N,INX-AGE,1) + 1 
             END-EVALUATE 
            WHEN 'D'  
             EVALUATE WS-QREC01-SQ-ROWSET-GENDER(INX-SQ-G) 
              WHEN 'M'  ALSO TRUE 
              COMPUTE REC01-NTAB-CITIZEN-MALE(I,INX-N,INX-AGE,2) 
              = REC01-NTAB-CITIZEN-MALE(I,INX-N,INX-AGE,2) + 1 
              WHEN 'M'  ALSO FALSE 
              COMPUTE REC01-NTAB-UNKNOWN-MALE(I,INX-N,INX-AGE,2) 
              = REC01-NTAB-UNKNOWN-MALE(I,INX-N,INX-AGE,2) + 1 
              WHEN 'F'  ALSO TRUE 
              COMPUTE REC01-NTAB-CITIZEN-FEMALE(I,INX-N,INX-AGE,2) 
              = REC01-NTAB-CITIZEN-FEMALE(I,INX-N,INX-AGE,2) + 1 
              WHEN 'F'  ALSO FALSE 
              COMPUTE REC01-NTAB-UNKNOWN-FEMALE(I,INX-N,INX-AGE,2) 
              = REC01-NTAB-UNKNOWN-FEMALE(I,INX-N,INX-AGE,2) + 1 
             END-EVALUATE       
            END-EVALUATE
      *---------------------------------------------------------------
            COMPUTE WS-SQ-INX01 = WS-SQ-INX01 - 1 
            IF WS-SQ-INX01 <= 0 THEN 
             EXIT PERFORM 
            END-IF 
      *---------------------------------------------------------------
      *    INDEXES
           *> YEAR, MONTH, DAY
           SET INX-SQ-Y UP BY 1 
           SET INX-SQ-MD UP BY 1 
      *     SET INX-SQ-D UP BY 1 
           *> GENDER, ALIVE
           SET INX-SQ-G UP BY 1 
           SET INX-SQ-A UP BY 1 
      * ---------------------------------------------------------------         
            END-PERFORM 
      * ---------------------------------------------------------------
            WHEN 201   *>MQ01
      * ---------------------------------------------------------------      
            CONTINUE 
      * ---------------------------------------------------------------
            WHEN OTHER 
            DISPLAY 'ERROR: SOMETHING WENT WRONG... WS-NC-FLAG=',
             WS-NC-FLAG 
           END-EVALUATE 
           EXIT.
      ******************************************************************     
       PARA-ANALYSE-DATA.
           EXIT.
      ******************************************************************     
       PARA-CREATE-VIEW.       
      ******************************************************************
      *    WELCOME SCREEN MSG
      ******************************************************************
           DISPLAY REC01-HLINE
           DISPLAY REC01-HLINE
           DISPLAY REC01-HLINE
           DISPLAY 'NCITIZEN - (VEDIC ASTROLOGY) NAKSHATRA CITIZEN',
           ' SOFTWARE (C) GNU GPLV3 2023 PRZEMYSLAW ADAM KUPISZ'
           DISPLAY 'SUBMITTED FOR USER: ', REC01-USER,
           ' AT ', REC01-PGM-START-DATE
           DISPLAY 'INTERVAL SET TO: ', REC01-INTERVAL-VAL
           DISPLAY REC01-HLINE
      ******************************************************************
      *    TABLE STATISTICS AND % VIEW
      ****************************************************************** 
      *    LOOP FOR TABLES PRINT
      ******************************************************************
           PERFORM REC01-INTERVAL-COUNT TIMES 
           DISPLAY REC01-HLINE 
           DISPLAY 'TABLE ', WS-TAB-COUNTER,
           ' FROM ',REC01-INTERVAL-COUNT
           DISPLAY REC01-HLINE 
           COMPUTE WS-TAB-COUNTER = WS-TAB-COUNTER + 1
      ******************************************************************     
      *    PART FOR SUMMARY REBUILD LEGACY CODE TO NEW RECV01
      ******************************************************************
      *     ADD RECV01-FEMALE(1) RECV01-MALE(1) *> ALIVE
      *         RECV01-FEMALE(2) RECV01-MALE(2) *> DEAD
      *     TO RECV01-S-TOTAL-CP
      *     ON SIZE ERROR DISPLAY 'PANIC: ',
      *     'PIC CLAUSE RECV01-S-TOTAL-CP ',
      *     'NEEDS TO BE GREATER THEN (9)! SUGGEST CHANGE TO (18)' 
      * ---------------------------------------------------------------
      *     SUBTRACT RECV01-FEMALE(2) RECV01-MALE(2) 
      *     FROM RECV01-S-TOTAL-CP GIVING RECV01-S-TALIVE-CP
           DISPLAY REC01-HLINE
           END-PERFORM
      *NTAB-NAMES
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
      *    SUMMARY
      ****************************************************************** 
           DISPLAY REC01-HLINE
           DISPLAY REC01-HLINE(1:1), '  ', 'SUMMARY' 
           DISPLAY REC01-HLINE
           
           DISPLAY 'TOTAL ALIVE CITIZEN POPULATION:        ', 
           REC01-T-CITIZEN-ALIVE
           DISPLAY 'TOTAL ALIVE MALE CITIZEN POPULATION:   ', 
           REC01-T-CITIZEN-ALIVE-MALE
           DISPLAY 'TOTAL ALIVE FEMALE CITIZEN POPULATION: ',
            REC01-T-CITIZEN-ALIVE-FEMALE
           
           DISPLAY 'MAXIMUM NAKSHATRA QUANTITY IN POPULATION: ',
            REC01-T-NAKSH-MAX-CITIZEN
           DISPLAY 'MINIMUM NAKSHATRA QUANTITY IN POPULATION: ',
            REC01-T-NAKSH-MIN-CITIZEN
           
           DISPLAY REC01-HLINE
           EXIT.
      ******************************************************************
       PARA-WRITE-TO-DATASET.
           OPEN EXTEND RECLOGW.  *> OUTPUT TO NOT ERASE EXISTING LOG
           EVALUATE LK-FS-W       *> FILE STATUS
              WHEN '35'           *> CAN NOT OPEN DS
           DISPLAY 'WARNING: ',
           'PRINT TO SPOOL ONLY, CANNOT OPEN DATASET.'
              WHEN '05'           *> DS DOES NOT EXIST, CREATING NEW DS
           DISPLAY 'INFO:    ',
           'FIRST RUN, DATASET DOES NOT EXIST. CREATING NEW DATASET.'
           END-EVALUATE
           MOVE REC01 TO REC01-DS
           WRITE REC01-DS
           END-WRITE.
           CLOSE RECLOGW.
           IF LK-FS-W NOT = '00'
            DISPLAY 'WARNING: ',
            'CLOSE INPUT RECLOGW FAILED WITH RC=', LK-FS-W 
            *>STOP RUN
           END-IF
           EXIT.
      ******************************************************************     
      *TODO: FEATURE IF JCL PARM IS (D,*) THEN READ FOR PRINT TO SPOOL
       PARA-READ-FROM-DATASET. 
           OPEN INPUT RECLOGR 
           IF LK-FS-R NOT = '00'
            DISPLAY 'PANIC: ',
            'OPEN INPUT RECLOGR FAILED WITH RC=', LK-FS-R 
            STOP RUN
           END-IF
           PERFORM UNTIL WS-EOF-FLAG = 'Y' 
           READ REC01-DS-R INTO REC01 
           AT END *> '10' EOF
           MOVE 'Y' TO WS-EOF-FLAG 
           NOT AT END 
           *> TODO: IMPL VB HANDLING WHEN REC01 STRUCT STABILIZES
           *> MOVE 2-4 BYTES TO REC01_LENGTH, 
           *> MOVE DS-R(2-4:REC_L - 2-4) TO BUFFER 
           MOVE REC01-DS-R TO REC01 *>CHANGE TO REC-LENGTH FOR V
           *>CHECK CRC TODO 
           PERFORM PARA-CREATE-VIEW           
           END-READ 
           END-PERFORM 
           CLOSE RECLOGR 
           IF LK-FS-R NOT = '00'
            DISPLAY 'WARNING: ',
            'CLOSE INPUT RECLOGR FAILED WITH RC=', LK-FS-R 
           END-IF
           EXIT. 
      ******************************************************************    
       PARA-SQL-ERROR-CHECK.
      ******************************************************************
           EVALUATE SQLCODE 
           WHEN ZERO 
           CONTINUE 
           WHEN 100 
           CONTINUE
           WHEN OTHER 
      ************** DB2 Error Handling Routine
           DISPLAY '*************************************************'
           DISPLAY '**** E R R O R M E S S A G E S ****'
           DISPLAY '*************************************************'
           DISPLAY '* Problem in paragraph: ' *>ws-paragraph
           DISPLAY '* Problem-msg: ' *>ws-error-msg
           DISPLAY '*'
           DISPLAY '* SQLCODE: ' SQLCODE
           DISPLAY '*************************************************'
           CALL "CEE3ABD" USING ABEND-CODE , TIMING    
           END-EVALUATE 
           EXIT. 
      ******************************************************************
      *    THIS PART OF CODE IS NOT NEEDED
      *    WE TAKE CURRENT DATE STRAIGHT FROM DB2 NOT BY ZOS SERVICE
      *     IF WS-DB2-DATE-FORMAT(5:1) = '-' THEN
      *        *> YYYY/MM/DD
      *        DISPLAY 'TEST'
      *     ELSE 
      *        IF WS-DB2-DATE-FORMAT(3:1) = '/' THEN
      *           *> MM/DD/YYYY
      *           DISPLAY 'TESY'
      *        ELSE
      *           *> MM/DD/YYY
      *           DISPLAY 'TEST'
      *        END-IF
      *     END-IF
      *    END CHECKING DB2 DATE FORMAT  
      ******************************************************************
