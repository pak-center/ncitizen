//NCTZEN1 JOB NCITIZEN,CLASS=A,MSGCLASS=Y,REGION=0M,NOTIFY=&SYSUID
//* *******************************************************************
//*             JCL TO RUN NCITIZEN COBOL OBJECT MEMBER               *
//*             THIS JCL CODE WAS NOT SYNTAX CHECKED OR SUBMITTED     *
//*             DO THIS BY YOURSELF BEFORE SUBMITTING                 *
//*             COPYRIGHT:  GNU GPLV3 LICENSE 2023                    *
//*             AUTHOR:     PRZEMYSLAW ADAM KUPISZ                    *
//*             VERSION:    ALPHA                                     *
//* *******************************************************************
//*             PLEASE CHOOSE ONLY ONE PARAMETER:
//*     MQ        MULTI  SQL QUERIES (OPTIMIZED)
//*     SQ        SINGLE SQL QUERY   (WORSE PERFORMANCE)
//*     DS        PRINT TABLES AND SUMMARY FROM DATASET ONLY (NO SQL)
//*
//* RANGE FOR PARM AFTER 'MQ,' : 1-120 YEARS INTERVAL FOR TABLE VIEW
//* *******************************************************************
//          SET USERP1='MQ,010'
//**        SET USERP1='SQ,010'
//**        SET USERP1='DS'
//* -------------------------------------------------------------------
//STEP1 EXEC PGM=NCITIZEN,PARM='&USERP1'
//*STEPLIB   DD DSN= ,DISP=SHR
//* -------------------------------------------------------------------
//* NCLOGW NEEDED TO SAVE RESULTS OF SQL QUERIES
//NCLOGW    DD DSN=NCITIZEN.1.LOG,DISP=(MOD,CTLG,UNCATLG),DCB=()
//*
//* NLGOGR NEEDED IF PARM='DS' IS SELECTED
//NLGOGR    DD DSN=NCITIZEN.2.LOG,DISP=SHR,DCB=()
//*     TODO: DD DSN NEED TO BE FINISHED (DCB, VOLUME, SPACE...)
//* -------------------------------------------------------------------
//* BELOW DEFAULT SETTINGS
//SYSPRINT DD SYSOUT=*
//SYSOUT DD SYSOUT=*
//SYSDUMP DD SYSOUT=*
//