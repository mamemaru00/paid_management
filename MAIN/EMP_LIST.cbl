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

      *    DECLARE CURSOR FOR FETCHING EMPLOYEE DATA
           EXEC SQL
               DECLARE EMP_CURSOR CURSOR FOR
               SELECT EMP_ID, EMP_NAME, EMP_JOIN_DATE, EMP_STATUS
               FROM EMP_MASTER
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    OPEN CURSOR
           EXEC SQL
               OPEN EMP_CURSOR
           END-EXEC.
           IF SQLCODE NOT = ZERO PERFORM ERROR-RTN STOP RUN.

      *    FETCH DATA IN A LOOP
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH EMP_CURSOR
                   INTO :EMP-ID, :EMP-NAME, :EMP-JOIN_DATE, :EMP-STATUS
               END-EXEC
               IF SQLCODE = 0
                   DISPLAY "EMPLOYEE DATA:"
                   DISPLAY "ID       : " EMP-ID
                   DISPLAY "NAME     : " EMP-NAME
                   DISPLAY "JOIN DATE: " EMP-JOIN_DATE
                   DISPLAY "STATUS   : " EMP-STATUS
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

      



      



