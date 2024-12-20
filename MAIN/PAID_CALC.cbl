      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 PAID_CALC.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-20.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
           01 INPUT-PAID.
               03 INPUT-USER        PIC 9(6).
               03 INPUT-PAID        PIC 9(4).

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01  DBNAME                  PIC  X(30) VALUE SPACE.
           01  USERNAME                PIC  X(30) VALUE SPACE.
           01  PASSWD                  PIC  X(10) VALUE SPACE.
           01  EMP-REC-VARS.
               03  EMP-ID               PIC  X(6).
               03  EMP-NAME             PIC  X(20).
               03  EMP-JOIN_DATE        PIC  X(10).
               03  EMP-STATUS           PIC  X(4).
           01  PLB-REC-VARS.
               03  BALANCE-ID           PIC  X(8).
               03  B-EMP-ID             PIC  X(6).
               03  BALANCE-STATUS       PIC  X(4).
               03  GRANT-DAYS           PIC  X(10).
               03  HOLD-DAYS            PIC  X(4).
               03  LAST-UPD-DATE        PIC  X(10).
           01  PLH-REC-VARS.
               03  HISTORY-ID          PIC  X(8).
               03  H-BALANCE-ID        PIC  X(8).
               03  ACQ-DATE            PIC  X(10).
               03  ACQ-DAYS            PIC  X(4).
               03  INSERT-DATETIME     PIC  X(19).
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.  
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************             
       MAIN-RTN.
           PERFORM INPUT-PAID.
           PERFORM CONNECT-TO-DATABASE.
           PERFORM PAID-CALC.

           STOP RUN.
      
      ******************************************************************
       INPUT-PAID.
      ******************************************************************
           DISPLAY "ユーザID : >> ".
           ACCEPT INPUT-USER FROM CONSOLE.
           DISPLAY "有給取得日数 : >> ".
           ACCEPT INPUT-PAID FROM CONSOLE.

           DISPLAY "入社日 = " INPUT-USER.
           DISPLAY "週所定労働日数 = " INPUT-PAID.

      ******************************************************************
       CONNECT-TO-DATABASE.
      ******************************************************************
      *    CONNECT TO DATABASE
           MOVE  "testdb@db"       TO   DBNAME.
           MOVE  "postgres"        TO   USERNAME.
           MOVE  SPACE             TO   PASSWD.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.
      
      ******************************************************************
       PAID-CALC.
      ******************************************************************
           EXEC SQL 
               DECLARE EMP_CURSOR CURSOR FOR
               SELECT EMP_MASTER.EMP_ID, EMP_NAME, 
                      HOLD_DAYS,
                      ACQ_DATE,ACQ_DAYS,INSERT_DATETIME,
               FROM EMP_MASTER
               INNER JOIN PL_BALANCE 
               ON EMP_MASTER.EMP_ID = PL_BALANCE.B_EMP_ID
               INNER JOIN PL_HISTORY 
               ON PL_BALANCE.BALANCE_ID = PL_HISTORY.H_BALANCE_ID
               ORDER BY EMP_NAME ASC
               
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      ******************************************************************
       ERROR-RTN.
      ******************************************************************
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE " " NO ADVANCING.
           EVALUATE SQLCODE
              WHEN +10
                 DISPLAY "Record not found"
              WHEN -01
                 DISPLAY "Connection failed"
              WHEN -20
                 DISPLAY "Internal error"
              WHEN -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE: " SQLSTATE
                 DISPLAY SQLERRMC
                 
                 *> TO RESTART TRANSACTION, PERFORM ROLLBACK.
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
                 
                 *> Alternative rollback using OCESQL library.
                 CALL "OCESQLStartSQL" END-CALL
                 CALL "OCESQLExec" USING
                     BY REFERENCE SQLCA
                     BY REFERENCE "ROLLBACK" & x"00"
                 END-CALL
                 CALL "OCESQLEndSQL" END-CALL
                 
              WHEN OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE: " SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
      ******************************************************************