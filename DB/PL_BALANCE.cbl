      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 PL_BALANCE.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-16.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  TEST-DATA-PLB.
           03 FILLER       PIC X(42) 
               VALUE "00000001000001TRUE2024-12-0115.52024-12-05".
           03 FILLER       PIC X(42) 
               VALUE "00000002000002TRUE2024-01-0112.02024-12-05".
           03 FILLER       PIC X(42) 
               VALUE "00000003000003TRUE2024-09-0110.02024-07-05".
       
       01  TEST-DATA-PLB-R   REDEFINES TEST-DATA-PLB.
           03  TEST-TBL-PLB    OCCURS  10 TIMES.
               05  TEST-BALANCE-ID          PIC  X(8).
               05  TEST-B-EMP-ID              PIC  X(6).
               05  TEST-BALANCE-STATUS      PIC  X(4).
               05  TEST-GRANT-DAYS          PIC  X(10).
               05  TEST-HOLD-DAYS           PIC  X(4).
               05  TEST-LAST-UPD-DATE       PIC  X(10).

       01  IDX                     PIC  99 VALUE 0.
       01  SYS-TIME                PIC  9(08).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  PLB-REC-VARS.
           03  BALANCE-ID           PIC  X(8).
           03  B-EMP-ID               PIC  X(6).
           03  BALANCE-STATUS       PIC  X(4).
           03  GRANT-DAYS           PIC  X(10).
           03  HOLD-DAYS            PIC  X(4).
           03  PLB-LAST-UPD-DATE    PIC  X(10).
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
               CREATE TABLE PL_BALANCE
               (
                   BALANCE_ID     VARCHAR(8) NOT NULL,
                   B_EMP_ID         VARCHAR(6) NOT NULL,
                   BALANCE_STATUS BOOLEAN NOT NULL,
                   GRANT_DAYS     DATE NOT NULL,
                   HOLD_DAYS      DECIMAL(3,1) NOT NULL,
                   LAST_UPD_DATE  DATE,
                   CONSTRAINT PLB_0 PRIMARY KEY (BALANCE_ID)
               )
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               MOVE TEST-BALANCE-ID(IDX)        TO BALANCE-ID
               MOVE TEST-B-EMP-ID(IDX)            TO B-EMP-ID
               MOVE TEST-BALANCE-STATUS(IDX)    TO BALANCE-STATUS
               MOVE TEST-GRANT-DAYS(IDX)        TO GRANT-DAYS 
               MOVE TEST-HOLD-DAYS(IDX)         TO HOLD-DAYS
               MOVE TEST-LAST-UPD-DATE(IDX)     TO PLB-LAST-UPD-DATE
               EXEC SQL
                 INSERT INTO PL_BALANCE VALUES
                 (:BALANCE-ID, :B-EMP-ID, :BALANCE-STATUS, :GRANT-DAYS, 
                 :HOLD-DAYS,:PLB-LAST-UPD-DATE)
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

