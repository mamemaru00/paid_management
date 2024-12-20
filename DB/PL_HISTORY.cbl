      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 PL_HISTORY.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-16.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  TEST-DATA-PLH.
           03 FILLER       PIC X(49) 
              VALUE "00000001000000012024-12-0801.02024-12-05 15:11:00".
           03 FILLER       PIC X(49) 
              VALUE "00000002000000022024-12-1500.52024-12-05 09:11:00".
           03 FILLER       PIC X(49) 
              VALUE "00000003000000032024-12-2001.52024-12-05 17:11:00".
       
       01  TEST-DATA-PLH-R   REDEFINES TEST-DATA-PLH.
           03  TEST-TBL-PLH    OCCURS  10 TIMES.
               05  TEST-HISTORY-ID          PIC  X(8).
               05  TEST-H-BALANCE-ID        PIC  X(8).
               05  TEST-ACQ-DATE            PIC  X(10).
               05  TEST-ACQ-DAYS            PIC  X(4).
               05  TEST-INSERT-DATETIME     PIC  X(19).

       01  IDX                     PIC  99 VALUE 0.
       01  SYS-TIME                PIC  9(08).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
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
           DISPLAY "*** INSERTTBL STARTED ***".

           MOVE  "testdb@db"       TO   DBNAME.
           MOVE  "postgres"        TO   USERNAME.
           MOVE  SPACE             TO   PASSWD.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           
           EXEC SQL
               CREATE TABLE PL_HISTORY
               (
                   HISTORY_ID         VARCHAR(8) NOT NULL,
                   H_BALANCE_ID       VARCHAR(8) NOT NULL,
                   ACQ_DATE           DATE NOT NULL,
                   ACQ_DAYS           DECIMAL(3,1) NOT NULL,
                   INSERT_DATETIME    TIMESTAMP,
                   CONSTRAINT PLH_0 PRIMARY KEY (HISTORY_ID)
               )
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               MOVE TEST-HISTORY-ID(IDX)        TO HISTORY-ID
               MOVE TEST-H-BALANCE-ID(IDX)      TO H-BALANCE-ID
               MOVE TEST-ACQ-DATE(IDX)          TO ACQ-DATE
               MOVE TEST-ACQ-DAYS(IDX)          TO ACQ-DAYS
               MOVE TEST-INSERT-DATETIME(IDX)   TO INSERT-DATETIME
           
               EXEC SQL
                 INSERT INTO PL_HISTORY VALUES
                 (:HISTORY-ID, :H-BALANCE-ID, :ACQ-DATE, :ACQ-DAYS
                 , :INSERT-DATETIME)
               END-EXEC
               IF SQLCODE NOT = ZERO 
                   PERFORM ERROR-RTN
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           EXEC SQL COMMIT WORK END-EXEC.

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           DISPLAY "*** INSERTTBL FINISHED ***".
           STOP RUN.
 
      ******************************************************************
       ERROR-RTN.
      ******************************************************************
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE " " NO ADVANCING.
           EVALUATE SQLCODE
              WHEN  +10
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection falied"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE: "  SQLSTATE
                 DISPLAY SQLERRMC
              *> TO RESTART TRANSACTION, DO ROLLBACK.
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE: "  SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
      ******************************************************************  

