      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 EMP_LIST.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-19.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
           01  IDX                     PIC  99 VALUE 0.
           01  SYS-TIME                PIC  9(08).

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
               03  LAST-UPD-DATE    PIC  X(10).
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           DISPLAY "*** EMP_LIST STARTED ***".

      *    CONNECT TO DATABASE
           MOVE  "testdb@db"       TO   DBNAME.
           MOVE  "postgres"        TO   USERNAME.
           MOVE  SPACE             TO   PASSWD.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    DECLARE CURSOR FOR FETCHING EMPLOYEE AND BALANCE DATA
           EXEC SQL
               DECLARE EMP_CURSOR CURSOR FOR
               SELECT EMP_MASTER.EMP_ID, EMP_NAME, EMP_JOIN_DATE, 
                      EMP_STATUS,
                      PL_BALANCE.BALANCE_ID, BALANCE_STATUS, GRANT_DAYS,
                      HOLD_DAYS, LAST_UPD_DATE
               FROM EMP_MASTER
               INNER JOIN PL_BALANCE 
               ON EMP_MASTER.EMP_ID = PL_BALANCE.B_EMP_ID
               ORDER BY EMP_NAME ASC

           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    OPEN CURSOR
           EXEC SQL
               OPEN EMP_CURSOR
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    DISPLAY HEADER
           DISPLAY "---------------------------------------------".
           DISPLAY "従業員有給情報一覧".
           DISPLAY "---------------------------------------------".

      *    FETCH DATA IN A LOOP
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH EMP_CURSOR
                   INTO :EMP-ID, :EMP-NAME, :EMP-JOIN_DATE, :EMP-STATUS,
                        :BALANCE-ID, :BALANCE-STATUS, :GRANT-DAYS,
                        :HOLD-DAYS, :LAST-UPD-DATE
               END-EXEC
               IF SQLCODE = 0
                   DISPLAY "-------------------------------------"
                   DISPLAY "名前        : " EMP-NAME 
                   DISPLAY "入社日       : " EMP-JOIN_DATE
                   DISPLAY "有給付与日   : " GRANT-DAYS
                   DISPLAY "有給付与日数 : " HOLD-DAYS
               END-IF
           END-PERFORM.


      *    CLOSE CURSOR
           EXEC SQL
               CLOSE EMP_CURSOR
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    DISCONNECT
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           DISPLAY "*** EMP_LIST FINISHED ***".
           STOP RUN.
      
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

      



      



