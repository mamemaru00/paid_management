      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 INPUT_PROGRAM.
       AUTHOR.                     mamemaru00.
       DATE-WRITTEN.               2024-12-12.

      ******************************************************************

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
           01 INPUT-DATE.
               03 WS-INPUT-DATE        PIC 9(8).
               03 WS-INPUT-YEAR        PIC 9(4).
               03 WS-INPUT-MONTH       PIC 9(2).
               03 WS-INPUT-DAY         PIC 9(2).
           
           01 CURRENT-DATETIME.
               03 WS-YEAR              PIC 9(4).
               03 WS-MONTH             PIC 9(2).
               03 WS-DAY               PIC 9(2).
               03 WS-HOUR              PIC 9(2).
               03 WS-MINUTE            PIC 9(2).
               03 WS-SECOND            PIC 9(2).
           
           01 NEW-DATE.
               03 WS-NEW-YEAR          PIC 9(4).
               03 WS-NEW-MONTH         PIC 9(2).
               03 WS-NEW-DAY           PIC 9(2).
           
           01 WS-CURRENT-DATETIME      PIC X(17).
           01 WS-TOTAL-MONTH           PIC 9(2).
           01 WS-FORMATTED-DATETIME    PIC X(20).
           01 WS-FUTURE-DATE           PIC X(20).
           01 WS-ADD-MONTH             PIC 9(2) VALUE 6.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           PERFORM INPUT-DATA.
           PERFORM GET-CURRENT-DATETIME.
           PERFORM CALC-LEAP-YEAR.
           PERFORM CALC-DATETIME.
           PERFORM DISPLAY-DATETIME.

           STOP RUN.

      ******************************************************************
       INPUT-DATA.
      ******************************************************************
           DISPLAY "日付を入力してください (YYYYMMDD): >> ".
           ACCEPT WS-INPUT-DATE FROM CONSOLE.

           DISPLAY "法定付与日 = " WS-INPUT-DATE.

      ******************************************************************
       GET-CURRENT-DATETIME.
      ******************************************************************
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATETIME.

           MOVE WS-CURRENT-DATETIME(1:4) TO WS-YEAR.
           MOVE WS-CURRENT-DATETIME(5:2) TO WS-MONTH.
           MOVE WS-CURRENT-DATETIME(7:2) TO WS-DAY.
           MOVE WS-CURRENT-DATETIME(9:2) TO WS-HOUR.
           MOVE WS-CURRENT-DATETIME(11:2) TO WS-MINUTE.
           MOVE WS-CURRENT-DATETIME(13:2) TO WS-SECOND.

           ADD 9 TO WS-HOUR.
           IF WS-HOUR > 23
               SUBTRACT 24 FROM WS-HOUR
               ADD 1 TO WS-DAY
           END-IF.

      ******************************************************************
       CALC-LEAP-YEAR.
      ******************************************************************
           MOVE WS-INPUT-DATE(1:4) TO WS-INPUT-YEAR.
           MOVE WS-INPUT-DATE(5:2) TO WS-INPUT-MONTH.
           MOVE WS-INPUT-DATE(7:2) TO WS-INPUT-DAY.

           IF (WS-INPUT-YEAR MOD 4 = 0 AND WS-INPUT-YEAR MOD 100 NOT = 0) OR
              (WS-INPUT-YEAR MOD 400 = 0)
               DISPLAY "Leap year"
           ELSE
               DISPLAY "Not a leap year"
           END-IF.

      ******************************************************************
       CALC-DATETIME.
      ******************************************************************
           COMPUTE WS-TOTAL-MONTH = FUNCTION NUMVAL(WS-INPUT-MONTH) + WS-ADD-MONTH.

           IF WS-TOTAL-MONTH > 12
               COMPUTE WS-NEW-YEAR = WS-INPUT-YEAR + (WS-TOTAL-MONTH / 12)
               COMPUTE WS-NEW-MONTH = WS-TOTAL-MONTH MOD 12
           ELSE
               MOVE WS-INPUT-YEAR TO WS-NEW-YEAR
               MOVE WS-TOTAL-MONTH TO WS-NEW-MONTH
           END-IF.

           IF WS-NEW-MONTH = 0
               SUBTRACT 1 FROM WS-NEW-YEAR
               MOVE 12 TO WS-NEW-MONTH
           END-IF.

           STRING WS-NEW-YEAR "/" WS-NEW-MONTH "/" WS-INPUT-DAY
           INTO WS-FUTURE-DATE.

      ******************************************************************
       DISPLAY-DATETIME.
      ******************************************************************
           STRING
                 WS-YEAR "/" WS-MONTH "/" WS-DAY " "
                 WS-HOUR ":" WS-MINUTE ":" WS-SECOND
           INTO WS-FORMATTED-DATETIME.

           DISPLAY "現在の日本時間: " WS-FORMATTED-DATETIME.
           DISPLAY "6ヶ月後: " WS-FUTURE-DATE.
