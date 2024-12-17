      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 INSERTTBL.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-16.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  TEST-DATA.
           03 FILLER       PIC X(38) 
               VALUE "001001HOKKAI TARO         2024-01-0101".
           03 FILLER       PIC X(38) 
               VALUE "002002AOMORI JIRO         2024-06-0102".
           03 FILLER       PIC X(38) 
               VALUE "003003AKITA SABURO        2024-07-0101".

      *>  01  TEST-DATA-PLB.
      *>      03 FILLER       PIC X(25) 
      *>         VALUE "0010010000101202024-12-17".
      *>      03 FILLER       PIC X(25) 
      *>         VALUE "0020020000201202024-12-19".
      *>      03 FILLER       PIC X(25) 
      *>         VALUE "0030020000201202024-01-02".

       01  TEST-DATA-R   REDEFINES TEST-DATA.
           03  TEST-TBL    OCCURS  10 TIMES.
               05  TEST-EMP-ID         PIC  9(3).
               05  TEST-PLB-ID         PIC  9(3).
               05  TEST-NAME           PIC  X(20).
               05  TEST-JOIN_DATE      PIC  X(10).
               05  TEST-STATUS         PIC  X(2).

       01  IDX                     PIC  99 VALUE 0.
       01  CNT-1                   PIC  99 VALUE 0.
       01  SYS-TIME                PIC  9(08).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  EMP-REC-VARS.
           03  EMP-ID                PIC  9(3).
           03  PLB-ID                PIC  9(3).
           03  EMP-NAME              PIC  X(20).
           03  EMP-JOIN_DATE         PIC  X(10).
           03  EMP-STATUS            PIC  X(2).
           03  EMP-CRATE-AT          PIC  X(19).
           03  EMP-UPDATE-AT         PIC  X(19).
      *>  01  PLB-REC-VARS.
      *>      03  PLB-ID                PIC  9(3).
      *>      03  EMP-ID                PIC  9(3).
      *>      03  HISTORY-ID            PIC  9(5).
      *>      03  BASE-ID               PIC  9(2).
      *>      03  PLB-PAID-DAYS         PIC  9(4).
      *>      03  PLB-LAST-UPD-DATE     PIC  X(10).
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
           IF  SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

           EXEC SQL
                CREATE TABLE EMP_MASTER
                (
                    EMP_ID     NUMERIC(3,0) NOT NULL,
                    PLB_ID     NUMERIC(3,0) NOT NULL,
                    EMP_NAME   CHAR(20),
                    EMP_JOIN_DATE DATE,
                    EMP_STATUS CHAR(2),
                    EMP_CRATE_AT TIMESTAMP,
                    EMP_UPDATE_AT TIMESTAMP,
                    CONSTRAINT IEMP_0 PRIMARY KEY (EMP_ID)
                )
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

           
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               MOVE TEST-EMP-ID(IDX)      TO EMP-ID
               MOVE TEST-PLB-ID(IDX)      TO PLB-ID
               MOVE TEST-NAME(IDX)        TO EMP-NAME
               MOVE TEST-JOIN_DATE(IDX)   TO EMP-JOIN_DATE
               MOVE TEST-STATUS(IDX)      TO EMP-STATUS
               EXEC SQL
                 INSERT INTO EMP_MASTER VALUES
                  (:EMP-ID,:PLB-ID,:EMP-NAME,:EMP-JOIN_DATE,:EMP-STATUS)
               END-EXEC
               IF  SQLCODE NOT = ZERO 
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

