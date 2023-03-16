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
           DEPENDING ON RECV01-DS-LEN *> TODO IN WS-
           RECORDING MODE IS V 
           DATA RECORD IS RECV01-DS.
      ******************************************************************
       01 RECV01-DS. 
      ******************************************************************       
      * TODO: BINARY FORM OF DATA INSIDE THE STRUCTURE
      ******************************************************************
        05 RECV01-DS-HEADER.
         07 RECV01-DS-LEN. *>TODO
        05 RECV01-DS-TABLE-STATS.
        05 RECV01-DS-SUMMARY.

      ******************************************************************      
       FD RECLOGR    
           BLOCK CONTAINS 0 RECORDS *>TODO AFTER IMPL. OF RECORDS 
      *    TODO:
      *    WHEN RECV01 IS COMPLETED CHECK RECORD LENGTH MIN & MAX
      *    AND CHANGE VALUES 100 AND 900 TO NEW, PROPER ONES      
           RECORD IS VARYING IN SIZE FROM 100 to 900 *>CHANGE RANGE !
           DEPENDING ON RECV01-DS-R-LEN *> TODO IN WS-
           RECORDING MODE IS V 
           DATA RECORD IS RECV01-DS-R. 
      * ----------------------------------------------------------------
       01 RECV01-DS-R. 
        05 RECV01-DS-R-HEADER.
         07 RECV01-DS-R-LEN. *>TODO
        05 RECV01-DS--RTABLE-STATS.
        *> HERE MAY BE MORE RTABLE-STATS FROM 1-120
        05 RECV01-DS-R-SUMMARY.
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
         88 WS-QREC01-DENDER-FLAG-FEMALE  VALUE 'F'. 
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
         10 WS-QREC01-SQ-ROWSET-BDATE    PIC X(10) OCCURS 2000 TIMES. 
         10 WS-QREC01-SQ-ROWSET-GENDER   PIC X     OCCURS 2000 TIMES. 
         10 WS-QREC01-SQ-ROWSET-ALIVE    PIC X     OCCURS 2000 TIMES.          
      * ---------------------------------------------------------------  
        05 WS-QREC01-MQ.
         07 WS-QREC01-DATE-LOWER         PIC   X(10). 
         07 WS-QREC01-DATE-UPPER         PIC   X(10). 
         07 WS-QREC01-DATE-UPPER-I       PIC   X(10). 
         07 WS-QREC01-MQ-YEAR            PIC   S9(4)  COMP-5.
         07 WS-QREC01-MQ-NR              PIC   S9(9)  COMP-5.
        05 WS-QREC01-MQ-ROWSET. *> MAX SIZE OF ROWSET IS 32,757
         07 WS-0QREC01-MQ-ROWSET-BDATE   PIC X(10) OCCURS 121 TIMES.
         07 WS-0QREC01-MQ-ROWSET-NR PIC S9(9) COMP-5 OCCURS 121 TIMES.        
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
              BDATE, 
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
              DECLARE MQ01 CURSOR FOR 
              SELECT 
              YEAR(BDATE), COUNT(*) *>AS NR 
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
       01 RECV01.
      * ---------------------------------------------------------------
        05 RECV01-HEADER.
         10 RECV01-RLENGTH         PIC   9(4) COMP-5. *> 65535
         10 FILLER                 PIC   X(76).
         10 RECV01-CRC             PIC   A(30) VALUE 
                                   'AUTHOR: PRZEMYSLAW ADAM KUPISZ'.
         10 RECV01-HLINE           PIC   X(80) VALUES ALL '*'.
         10 RECV01-PGM.
          15 RECV01-PGM-START-DATE PIC   99(4)/99/99.
         10 FILLER                 PIC   X(40) VALUES ALL '@'.
      *
         10 RECV01-USER            PIC   X(8).  
      *   10 RECV01-CURRENT-DATE-DB2    PIC   X(10).           
         10 RECV01-INTERVAL-VAL    PIC   S999 COMP-3 VALUE 10. 
         10 RECV01-INTERVAL-COUNT  PIC   S9(3) VALUE 120. *>MAX YEARS  

      * ---------------------------------------------------------------        
      * CHECK 05 date1 FORMAT DATE "%m/%d/@Y".       
      * ---------------------------------------------------------------
        05 RECV01-TABLE-STATS.
         10 RECV01-TAB OCCURS 1 TO 120 TIMES DEPENDING ON 
                       RECV01-INTERVAL-COUNT INDEXED BY I.
         15 RECV01-CITIZEN OCCURS 2 TIMES. *> 1-ALIVE, 2-DEAD
          20 RECV01-MALE           PIC   9(9) COMP-5. *>(18) MAX
          20 RECV01-FEMALE         PIC   9(9) COMP-5.
         15 RECV01-NTAB OCCURS 27 TIMES. *>INDEXED BY
          20 WS-N                  PIC   9(9) COMP-5.
          20 WS-U                  PIC   9(9) COMP-5.
          20 RECV01-NTAB-AGE OCCURS 120 TIMES. *>INDEXED BY
           25 RECV01-NTAB-AGE      PIC 9(9) COMP-5. 
           25 RECV01-NTAB-AGE-U    PIC 9(9) COMP-5. 
          20 RECV01-NTAB-CITIZEN OCCURS 2 TIMES. 
           25 RECV01-NTAB-CITIZEN-MALE   PIC 9(9) COMP-5. 
           25 RECV01-NTAB-CITIZEN-FEMALE PIC 9(9) COMP-5. 
      * ---------------------------------------------------------------
        05 RECV01-SUMMARY.   
         10 RECV01-S-TOTAL-CP      PIC   9(9) COMP-5.
         10 RECV01-S-TALIVE-CP     PIC   9(9) COMP-5.
         10 RECV01-S-NMAX-PQ       PIC   9(9) COMP-5.
         10 RECV01-S-NMIN-PQ       PIC   9(9) COMP-5.
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
      ******************************************************************
      *    VARIABLES FOR PARA-SQL-MQ.
      ******************************************************************
       01 WS-SQL-MQ.
        05 WS-SQL-MQ-I                PIC   S9(3).
      ******************************************************************
      *    VARIABLES FOR PROC-NAKSHATRA-COMPUTE AND PROC-ANALYSE-DATA
      ******************************************************************
       01 WS-STATS.
        05 WS-MONTH-DAY               PIC   9(4).  
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
           ACCEPT RECV01-PGM-START-DATE FROM DATE YYYYMMDD
           ACCEPT RECV01-USER           FROM USERNAME
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
           MOVE WS-PGM-INTERVAL-VAL TO RECV01-INTERVAL-VAL *>4SQL Q
           DIVIDE RECV01-INTERVAL-VAL INTO RECV01-INTERVAL-COUNT *>4TAB         
      
           EXIT.      
      ******************************************************************
      *    PARAGRAPH SQL DATE DB2
      ******************************************************************
       PARA-SQL-DATE-DB2. 
           EXEC SQL
              OPEN CD01
           END-EXEC. 
           PERFORM PARA-SQL-ERROR-CHECK 
      *     
           EXEC SQL
           FETCH CD01 INTO 
              :WS-QREC01-DATE-LOWER 
           END-EXEC 
           EVALUATE SQLCODE 
            WHEN ZERO 
           MOVE RECV01-INTERVAL-VAL TO WS-QREC01-INTERVAL-DECIMAL
           WHEN OTHER
            PERFORM PARA-SQL-ERROR-CHECK 
           END-EVALUATE
      *
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
      *
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
      *
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
           MOVE RECV01-INTERVAL-VAL TO WS-PGM-INTERVAL-VAL *> NEEDED?
           PERFORM RECV01-INTERVAL-COUNT TIMES 
      ******************************************************************
           *> SQL QUERY HERE
           PERFORM UNTIL SQLCODE = 100 
           EXEC SQL 
           FETCH NEXT ROWSET FROM SQ01 FOR 2000 ROWS 
           INTO 
           :WS-QREC01-SQ-ROWSET-BDATE, 
           :WS-QREC01-SQ-ROWSET-GENDER, 
           :WS-QREC01-SQ-ROWSET-ALIVE 
           END-EXEC 
           PERFORM PARA-SQL-ERROR-CHECK 
      ******************************************************************
      *    COMPUTES NAKSHATRA 
      ******************************************************************
           PERFORM PARA-NAKSHATRA-COMPUTE *>TODO FOR ROWSET!
           END-PERFORM

      ******************************************************************        
           ADD RECV01-INTERVAL-VAL TO WS-PGM-INTERVAL-VAL
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
      *     
           PERFORM
            VARYING WS-SQL-MQ-I
            FROM RECV01-INTERVAL-COUNT BY RECV01-INTERVAL-COUNT
            UNTIL WS-SQL-MQ-I > 120
              *>TODO
      ******************************************************************
      *    4 QUERIES FOR SUMMARY RAPORT WITH GENDER AND ALIVE STATUS 
      *    TOTAL CITIZENS COUNT 
      ******************************************************************
           EXEC SQL 
            FETCH MQ01 INTO 
            :WS-QREC01-MQ-YEAR, 
            :WS-QREC01-MQ-NR 
           END-EXEC  
           PERFORM PARA-SQL-ERROR-CHECK 
      *    TODO MOVE TO RECV01 

           END-PERFORM 
      *     
           EXEC SQL
            CLOSE MQ01
           END-EXEC.
           PERFORM PARA-SQL-ERROR-CHECK 
           EXIT.
      ******************************************************************
       PARA-NAKSHATRA-COMPUTE.
      ****************************************************************** 
      *    TODO !
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
           EVALUATE TRUE 
           WHEN WS-QREC01-ALIVE-FLAG-ALIVE 
            EVALUATE TRUE 
              WHEN WS-QREC01-GENDER-FLAG-MALE 
              COMPUTE RECV01-MALE(I,1) = RECV01-MALE(I,1) + 1
              WHEN WS-QREC01-DENDER-FLAG-FEMALE 
              COMPUTE RECV01-FEMALE(I,1) = RECV01-FEMALE(I,1) + 1
            END-EVALUATE 
           WHEN WS-QREC01-ALIVE-FLAG-DEAD 
            EVALUATE TRUE 
              WHEN WS-QREC01-GENDER-FLAG-MALE 
              COMPUTE RECV01-MALE(I,2) = RECV01-MALE(I,2) + 1
              WHEN WS-QREC01-DENDER-FLAG-FEMALE 
              COMPUTE RECV01-FEMALE(I,2) = RECV01-FEMALE(I,2) + 1
            END-EVALUATE       
           END-EVALUATE
      * ---------------------------------------------------------------
           EVALUATE WS-MONTH-DAY *>MONTH DAY FROM QBDATE AS INT TODO
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
              WHEN 0101 THRU 0110  *> DEV-JUN
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
      ******************************************************************     
       PARA-ANALYSE-DATA.
           EXIT.
      ******************************************************************     
       PARA-CREATE-VIEW.       
      ******************************************************************
      *    WELCOME SCREEN MSG
      ******************************************************************
           DISPLAY RECV01-HLINE
           DISPLAY RECV01-HLINE
           DISPLAY RECV01-HLINE
           DISPLAY 'NCITIZEN - (VEDIC ASTROLOGY) NAKSHATRA CITIZEN',
           ' SOFTWARE (C) GNU GPLV3 2023 PRZEMYSLAW ADAM KUPISZ'
           DISPLAY 'SUBMITTED FOR USER: ', RECV01-USER,
           ' AT ', RECV01-PGM-START-DATE
           DISPLAY 'INTERVAL SET TO: ', RECV01-INTERVAL-VAL
           DISPLAY RECV01-HLINE
      ******************************************************************
      *    TABLE STATISTICS AND % VIEW
      ****************************************************************** 
      *    LOOP FOR TABLES PRINT
      ******************************************************************
           PERFORM RECV01-INTERVAL-COUNT TIMES 
           DISPLAY RECV01-HLINE 
           DISPLAY 'TABLE ', WS-TAB-COUNTER,
           ' FROM ',RECV01-INTERVAL-COUNT
           DISPLAY RECV01-HLINE 
           COMPUTE WS-TAB-COUNTER = WS-TAB-COUNTER + 1
      ******************************************************************     
      *    PART FOR SUMMARY
      ******************************************************************
           ADD RECV01-FEMALE(1) RECV01-MALE(1) *> ALIVE
               RECV01-FEMALE(2) RECV01-MALE(2) *> DEAD
           TO RECV01-S-TOTAL-CP
           ON SIZE ERROR DISPLAY 'PANIC: ',
           'PIC CLAUSE RECV01-S-TOTAL-CP ',
           'NEEDS TO BE GREATER THEN (9)! SUGGEST CHANGE TO (18)' 
      * ---------------------------------------------------------------
           SUBTRACT RECV01-FEMALE(2) RECV01-MALE(2) 
           FROM RECV01-S-TOTAL-CP GIVING RECV01-S-TALIVE-CP
           DISPLAY RECV01-HLINE
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
           DISPLAY RECV01-HLINE
           DISPLAY RECV01-HLINE(1:1), '  ', 'SUMMARY' 
           DISPLAY RECV01-HLINE
           
           DISPLAY 'TOTAL CITIZEN POPULATION: ', RECV01-S-TOTAL-CP
           DISPLAY 'ALIVE CITIZEN POPULATION: ', RECV01-S-TALIVE-CP
           DISPLAY 'MAXIMUM NAKSHATRA QUANTITY IN POPULATION: ',
            RECV01-S-NMAX-PQ
           DISPLAY 'MINIMUM NAKSHATRA QUANTITY IN POPULATION: ',
            RECV01-S-NMIN-PQ
           
           DISPLAY RECV01-HLINE
           EXIT.
      ******************************************************************
       PARA-WRITE-TO-DATASET.
           OPEN EXTEND RECLOGW.  *> OUTPUT TO NOT ERASE EXISTING LOG
           EVALUATE LK-FS-W       *> FILE STATUS
              WHEN 35           *> CAN NOT OPEN DS
           DISPLAY 'WARNING: ',
           'PRINTS TO SPOOL ONLY, CANNOT OPEN DATASET.'
              WHEN 05           *> DS DOES NOT EXIST, CREATING NEW DS
           DISPLAY 'INFO:    ',
           'FIRST RUN, DATASET DOES NOT EXIST. CREATING NEW DATASET.'
           END-EVALUATE
           MOVE RECV01 TO RECV01-DS
           WRITE RECV01-DS
           END-WRITE.
           CLOSE RECLOGW.
           EXIT.
      ******************************************************************     
      *TODO: FEATURE IF JCL PARM IS (D,*) THEN READ FOR PRINT TO SPOOL
       PARA-READ-FROM-DATASET. 
           OPEN INPUT RECLOGR 
           PERFORM UNTIL WS-EOF-FLAG = 'Y' 
           READ RECV01-DS-R INTO RECV01 
           AT END 
           MOVE 'Y' TO WS-EOF-FLAG 
           NOT AT END 
           MOVE RECV01-DS-R TO RECV01 *>CHANGE TO REC-LENGTH FOR V
           *>CHECK CRC TODO 
           PERFORM PARA-CREATE-VIEW           
           END-READ 
           END-PERFORM 
           CLOSE RECLOGR 
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
