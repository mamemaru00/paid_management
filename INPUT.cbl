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
           
           01 FUTURE-DATES.
               03 WS-FUTURE-YEAR       PIC 9(4).
               03 WS-FUTURE-MONTH      PIC 9(2).
               03 WS-FUTURE-DAY        PIC 9(2).
           
           01 FUTURE-YEARS.
               03 WS-FUTURE-ONE-YEAR   PIC X(20).
               03 WS-FUTURE-TWO-YEAR   PIC X(20).
               03 WS-FUTURE-THREE-YEAR PIC X(20).
               03 WS-FUTURE-FOUR-YEAR  PIC X(20).
               03 WS-FUTURE-FIVE-YEAR  PIC X(20).
               03 WS-FUTURE-SIX-YEAR   PIC X(20).
           
           01 LEAP-CALC.
               03 DATA1                PIC 9(4).
               03 DATA2                PIC 9(4).
               03 DATA3                PIC 9(4).
           
           01 WK-DAYS                  PIC 9(2).
           01 WS-CURRENT-DATETIME      PIC X(17).
           01 WS-TOTAL-MONTH           PIC 9(2).
           01 WS-FORMATTED-DATETIME    PIC X(20).
           01 WS-FUTURE-DATE           PIC X(20).
           01 WS-ADD-MONTH             PIC 9(2) VALUE 6.
           01 IDX                      PIC 9(2) VALUE 1.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           PERFORM INPUT-DATA.
           PERFORM GET-CURRENT-DATETIME.
           PERFORM CALC-DATETIME.
           PERFORM CALC-ONE-YEAR.
           PERFORM DISPLAY-DATETIME.

           STOP RUN.

      ******************************************************************
       INPUT-DATA.
      ******************************************************************
           DISPLAY "入社日 (YYYYMMDD): >> ".
           ACCEPT WS-INPUT-DATE FROM CONSOLE.
           DISPLAY "週所定労働日数 : >> ".
           ACCEPT WK-DAYS FROM CONSOLE.

           DISPLAY "入社日 = " WS-INPUT-DATE.
           DISPLAY "週所定労働日数 = " WK-DAYS.

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
       CALC-DATETIME.
      ******************************************************************
           MOVE WS-INPUT-DATE(1:4) TO WS-INPUT-YEAR
           MOVE WS-INPUT-DATE(5:2) TO WS-INPUT-MONTH.
           MOVE WS-INPUT-DATE(7:2) TO WS-INPUT-DAY.

           COMPUTE WS-TOTAL-MONTH = 
           FUNCTION NUMVAL(WS-INPUT-MONTH) + WS-ADD-MONTH.

           IF WS-TOTAL-MONTH > 12
               COMPUTE WS-NEW-YEAR = WS-INPUT-YEAR + 1
               COMPUTE WS-NEW-MONTH = WS-TOTAL-MONTH - 12
           ELSE
               MOVE WS-INPUT-YEAR TO WS-NEW-YEAR
               MOVE WS-TOTAL-MONTH TO WS-NEW-MONTH
           END-IF.

        *>    TODO 閏年計算処理未完成
        *>    COMPUTE DATA1 = FUNCTION MOD(WS-NEW-YEAR 4).
        *>    COMPUTE DATA2 = FUNCTION MOD(WS-NEW-YEAR 100).
        *>    COMPUTE DATA3 = FUNCTION MOD(WS-NEW-YEAR 400).

        *>    DISPLAY DATA1.
        *>    DISPLAY DATA2.      
        *>    DISPLAY DATA3.
        *>    DISPLAY WS-NEW-MONTH.

        *>    IF ((DATA1 = 0 AND DATA2 NOT = 0) OR (DATA3 = 0)) AND 
        *>    WS-NEW-MONTH < 7 
        *>        DISPLAY "Leap year"
        *>        COMPUTE WS-INPUT-DAY = WS-INPUT-DAY - 1
        *>    ELSE
        *>        DISPLAY "Not a leap year"
        *>    END-IF.

           STRING WS-NEW-YEAR
                  "/" WS-NEW-MONTH
                  "/" WS-INPUT-DAY
           INTO WS-FUTURE-DATE.

      ******************************************************************
       CALC-ONE-YEAR.
      ******************************************************************
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 6
               COMPUTE WS-FUTURE-YEAR = WS-NEW-YEAR + IDX
               MOVE WS-NEW-MONTH TO WS-FUTURE-MONTH
               MOVE WS-INPUT-DAY TO WS-FUTURE-DAY

               EVALUATE IDX
                   WHEN 1
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-ONE-YEAR
                   WHEN 2
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-TWO-YEAR
                   WHEN 3
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-THREE-YEAR
                   WHEN 4
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-FOUR-YEAR
                   WHEN 5
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-FIVE-YEAR
                   WHEN 6
                       STRING WS-FUTURE-YEAR
                              "/" WS-FUTURE-MONTH
                              "/" WS-FUTURE-DAY
                       INTO WS-FUTURE-SIX-YEAR
               END-EVALUATE
           END-PERFORM.
      ******************************************************************
       DISPLAY-DATETIME.
      ******************************************************************
           STRING
                 WS-YEAR "/" WS-MONTH "/" WS-DAY " "
                 WS-HOUR ":" WS-MINUTE ":" WS-SECOND
           INTO WS-FORMATTED-DATETIME.

           DISPLAY "現在の日本時間: " WS-FORMATTED-DATETIME.
           DISPLAY "------------- ----------".
           DISPLAY "期間           法定付与日".
           DISPLAY "6 months     " WS-FUTURE-DATE " 10 days".
           DISPLAY "1 year       " WS-FUTURE-ONE-YEAR " 11 days".
           DISPLAY "2 years      " WS-FUTURE-TWO-YEAR " 12 days".
           DISPLAY "3 years      " WS-FUTURE-THREE-YEAR " 14 days".
           DISPLAY "4 years      " WS-FUTURE-FOUR-YEAR " 16 days".
           DISPLAY "5 years      " WS-FUTURE-FIVE-YEAR " 18 days".
           DISPLAY "6 years      " WS-FUTURE-SIX-YEAR " 20 days".
           


