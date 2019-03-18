       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDICINESHOWINVEN.
       AUTHOR. AUSTIN GRIFFIN.

      ****************************************************************
      *This program takes three data files of warehouse IDs, Vendor IDs,
      * Product IDs, and then sorts and merges all this files into
      * an SD file. It will then take array data from that new merged file
      * an perform data processing. It will also perform data verification
      * and concatenation as well.
      *
      *If there is a bad warehouse record in one of the files, the program will
      * instead skip processing that warehouse and put that record into a file named
      * bad-warehouse-records.txt.
      * **************************************************************
      *This program also checks if the data that is brought is in correct,
      * for example, the price should be numeric, if not, it will leave that
      * area blank due to validation checking. 
      *
      *At the end of the program, a message will be display upon the console
      * that will notify the user at the end of the program of
      * how many warehouse variables that the program received is invalid.
      *****************************************************************
      *This program will implement a table-search to lookup and expand the VENDOR-ID.
      * If a VENDOR-ID cannot be found, the program will move "INVALID" and the invalid ID
      * to the output instead.
      *****************************************************************
      *This program will also accumulate the total stock sales for each vendor
      * that is listed. It will then take the accumulation and print out a total amount.
      *
      *It will also accumulate the total stock sales for a vendor, the warehouse, and the
      * grand total of all stock sales from the 3 warehouses combined. The program
      * will then print out these totals as well. There are storage variables
      * for these under workstation. WS-TOTAL-PROD-PRICE, WS-VENDOR-COST, 
      *WS-WH-COST, WS-GRAND-TOTAL.
      ******************************************************************
      *This program implements a multi-level control break with 3 different
      * levels, these are run for WAREHOUSE-ID, VENDOR-ID, AND PRODUCT-ID. 
      * Each of these have their own storage under WorkStation so that an
      * evaluate statement may be used to compare and decide when to perform
      * the control breaks.
      ****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * SELECT THE UNSORTED FILES
          SELECT UNSORTED-FILE-1
             ASSIGN TO "PR4F18-NV10.TXT"
             ORGANIZATION IS LINE SEQUENTIAL.

          SELECT UNSORTED-FILE-2
             ASSIGN TO "PR4F18-CA20.TXT"
             ORGANIZATION IS LINE SEQUENTIAL.

         SELECT UNSORTED-FILE-3
            ASSIGN TO "PR4F18-WA30.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

      * SELECT THE SORTED FILES
         SELECT SORTED-FILE-1
            ASSIGN TO "PR4F18-NV10-SORTED.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

         SELECT SORTED-FILE-2
            ASSIGN TO "PR4F18-CA20-SORTED.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

         SELECT SORTED-FILE-3
            ASSIGN TO "PR4F18-WA30-SORTED.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

      * SELECT THE MERGE FILE
         SELECT MERGED-FILE
            ASSIGN TO "MERGEDWAREHOUSEFILE.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

      * SELECT THE SORT FILE
         SELECT SORT-FILE
            ASSIGN TO "SORT.TMP".

      * SELECT THE PRINT FILE
         SELECT PRINT-FILE
            ASSIGN TO "SALESOUTPUT.TXT".

      * SELECT THE ERROR FILE
         SELECT ERROR-FILE
            ASSIGN TO "BAD-WAREHOUSE-RECORDS.TXT".
			

       DATA DIVISION.
       FILE SECTION.
      * UNSORTED FILE DIVISION
       FD UNSORTED-FILE-1
          RECORD CONTAINS 128 CHARACTERS.

       01 UNSORTED-RECORD-1.
          05 UR1-WH-ID            PIC X(4).
          05 UR1-VND-ID           PIC X.
          05 UR1-PDT-ID           PIC X(3).
          05 UR1-PRODUCT-DATA OCCURS 5 TIMES.
             10 UR1-PDT-NAME      PIC X(13).
             10 UR1-PDT-SIZE      PIC A.
             10 UR1-PDT-TYPE      PIC A.
             10 UR1-PDT-STOCK     PIC S9(4).
             10 UR1-PDT-COST      PIC S999V99.

       FD UNSORTED-FILE-2
          RECORD CONTAINS 128 CHARACTERS.

       01 UNSORTED-RECORD-2.
          05 UR2-WH-ID            PIC X(4).
          05 UR2-VND-ID           PIC X.
          05 UR2-PDT-ID           PIC X(3).
          05 UR2-PRODUCT-DATA OCCURS 5 TIMES.
             10 UR2-PDT-NAME      PIC X(13).
             10 UR2-PDT-SIZE      PIC A.
             10 UR2-PDT-TYPE      PIC A.
             10 UR2-PDT-STOCK     PIC S9(4).
             10 UR2-PDT-COST      PIC S999V99.

       FD UNSORTED-FILE-3
          RECORD CONTAINS 128 CHARACTERS.

       01 UNSORTED-RECORD-3.
          05 UR3-WH-ID            PIC X(4).
          05 UR3-VND-ID           PIC X.
          05 UR3-PDT-ID           PIC X(3).
          05 UR3-PRODUCT-DATA OCCURS 5 TIMES.
             10 UR3-PDT-NAME      PIC X(13).
             10 UR3-PDT-SIZE      PIC A.
             10 UR3-PDT-TYPE      PIC A.
             10 UR3-PDT-STOCK     PIC S9(4).
             10 UR3-PDT-COST      PIC S999V99.

      * SORTED FILE DIVISION
       FD SORTED-FILE-1
          RECORD CONTAINS 128 CHARACTERS.

       01 SORTED-RECORD-1.
          05 SR1-WH-ID            PIC X(4).
          05 SR1-VND-ID           PIC X.
          05 SR1-PDT-ID           PIC X(3).
          05 SR1-PRODUCT-DATA OCCURS 5 TIMES.
             10 SR1-PDT-NAME      PIC X(13).
             10 SR1-PDT-SIZE      PIC A.
             10 SR1-PDT-TYPE      PIC A.
             10 SR1-PDT-STOCK     PIC S9(4).
             10 SR1-PDT-COST      PIC S999V99.

       FD SORTED-FILE-2
          RECORD CONTAINS 128 CHARACTERS.

       01 SORTED-RECORD-2.
          05 SR2-WH-ID            PIC X(4).
          05 SR2-VND-ID           PIC X.
          05 SR2-PDT-ID           PIC X(3).
          05 SR2-PRODUCT-DATA OCCURS 5 TIMES.
             10 SR2-PDT-NAME      PIC X(13).
             10 SR2-PDT-SIZE      PIC A.
             10 SR2-PDT-TYPE      PIC A.
             10 SR2-PDT-STOCK     PIC S9(4).
             10 SR2-PDT-COST      PIC S999V99.

       FD SORTED-FILE-3
          RECORD CONTAINS 128 CHARACTERS.

       01 SORTED-RECORD-3.
          05 SR3-WH-ID            PIC X(4).
          05 SR3-VND-ID           PIC X.
          05 SR3-PDT-ID           PIC X(3).
          05 SR3-PRODUCT-DATA OCCURS 5 TIMES.
             10 SR3-PDT-NAME      PIC X(13).
             10 SR3-PDT-SIZE      PIC A.
             10 SR3-PDT-TYPE      PIC A.
             10 SR3-PDT-STOCK     PIC S9(4).
             10 SR3-PDT-COST      PIC S999V99.


       FD MERGED-FILE
          RECORD CONTAINS 128 CHARACTERS.

       01 MERGED-RECORDS.
          05 MR-WH-ID             PIC X(4).
          05 MR-VND-ID            PIC X.
          05 MR-PDT-ID            PIC X(3).
          05 MR-PRODUCT-DATA OCCURS 5 TIMES.
             10 MR-PDT-NAME       PIC X(13).
             10 MR-PDT-SIZE       PIC A.
             10 MR-PDT-TYPE       PIC A.
             10 MR-PDT-STOCK      PIC S9(4).
             10 MR-PDT-COST       PIC S999V99.

      * SD DESCRPTION FOR SORT AND MERGE
       SD SORT-FILE
          RECORD CONTAINS 128 CHARACTERS.

       01 SORT-RECORD.
          05 S-WH-ID            PIC X(4).
          05 S-VND-ID           PIC X.
          05 FILLER             PIC X(123).

      * OUTPUT FILE
       FD PRINT-FILE
          RECORD CONTAINS 66 CHARACTERS.
       01 PRINT-REC               PIC X(66).

       FD ERROR-FILE
          RECORD CONTAINS 128 CHARACTERS.
       01 ERROR-REC               PIC X(128).

       WORKING-STORAGE SECTION.

       01 FLAGS-N-SWITCHES.
          05 EOF-FLAG             PIC X      VALUE ' '.
             88 NO-MORE-DATA                 VALUE 'N'.
             88 MORE-RECORDS                 VALUE 'Y'.
          05 FIRST-RECORD         PIC XXX    VALUE 'YES'.
          05 WS-WAREHOUSE-ID-HOLD PIC X(4).
          05 WS-VENDOR-ID-HOLD    PIC X.
          05 WS-PRODUCT-ID-HOLD   PIC X(3).
          05 WS-STORED-ARRAY-SUB  PIC 9.
		  05 WHOLE-PRICE          PIC S9999999V99.
		  05 WS-TOTAL-PROD-PRICE  PIC S99999999V99.
          05 WS-VENDOR-COST       PIC S99999999V99.
		  05 WS-WH-COST           PIC S9999999999V99.
          05 WS-GRAND-TOTAL       PIC S9999999999V99.
		  05 BYPASS-VENDOR-HEADER PIC X(3)   VALUE 'NO'.
          05 TOTAL-ERROR-RECORDS  PIC 99     VALUE 00.

       01 WS-CURRENT-DATE.
          05 RUN-YEAR             PIC 9(2).
          05 RUN-MONTH            PIC 9(2).
          05 RUN-DAY              PIC 9(2).

      * TABLE FOR VENDOR NAME
       01 VENDOR-TEXT.
          05        PIC X(12) VALUE "IMadeInHouse".
          05        PIC X(12) VALUE "TTansia Corp".
          05        PIC X(12) VALUE "DDENIO Corp ".
          05        PIC X(12) VALUE "VVISSON Corp".
          05        PIC X(12) VALUE "NETON Ltd   ".
          05        PIC X(12) VALUE "AAmel Ltd   ".
          05        PIC X(12) VALUE "WWest Corp  ".
       01 VENDOR-TABLE REDEFINES VENDOR-TEXT.
          05 VENDOR-ITEM OCCURS 7 TIMES
          INDEXED BY X1.
             10 VENDOR-TEXT-ID   PIC X.
             10 VENDOR-TEXT-NAME PIC X(11).

       01 ARRAY-SUB PIC 9 VALUE 1.

      ************** THIS IS WHERE HEADERS SECTION BEGINS *************

       01 HEADING-ONE.
          05                      PIC X(35)  VALUE ' '.
          05 DRAKEA-LTD           PIC X(10)   VALUE 'DR. CHEEB '.
          05                      PIC X(21)  VALUE ' '.
       01 HEADING-TWO.
          05                      PIC X(10)   VALUE ' '.
          05 RUN-MONTH-OUT        PIC X(2).
          05                      PIC X       VALUE '/'.
          05 RUN-DAY-OUT          PIC X(2).
          05                      PIC X(3)    VALUE '/20'.
          05 RUN-YEAR-OUT         PIC X(2).
          05                      PIC X(12)   VALUE ' '.
          05 W-INVENTORY          PIC X(10)   VALUE 'INVENTORY '.
          05 W-REPORT             PIC X(6)    VALUE 'REPORT'.
          05                      PIC X(18)   VALUE ' '.

       01 WAREHOUSE-HEADER.
          05                      PIC X(2)    VALUE ' '.
          05 W-WAREHOUSE          PIC X(11)   VALUE 'WAREHOUSE: '.
          05 WAREHOUSE-ID-OUT     PIC X(4).
          05                      PIC X(49)   VALUE ' '.

       01 VENDOR-HEADER.
          05                      PIC X(5)    VALUE ' '.
          05 W-VENDOR             PIC X(8)    VALUE 'VENDOR: '.
          05 VENDOR-NAME          PIC X(11).
          05                      PIC X(42)   VALUE ' '.

       01 PRODUCT-HEADER-ONE.
          05                      PIC X(8)    VALUE ' '.
          05 W-PRODUCT-N          PIC X(7)    VALUE 'PRODUCT'.
          05                      PIC X(7)    VALUE ' '.
          05 W-PROD-I             PIC X(4)    VALUE 'PROD'.
          05                      PIC X(4)    VALUE ' '.
          05 W-PRODUCT-S          PIC X(7)    VALUE 'PRODUCT'.
          05                      PIC X(4)    VALUE ' '.
          05 W-PROD-T             PIC X(4)    VALUE 'PROD'.
          05                      PIC X(4)    VALUE ' '.
          05 W-IN                 PIC X(2)    VALUE 'IN'.
          05                      PIC X(6)    VALUE ' '.
          05 W-TOTAL              PIC X(5)    VALUE 'TOTAL'.
          05                      PIC X(4)    VALUE ' '.
       01 PRODUCT-HEADER-TWO.
          05                      PIC X(10)   VALUE ' '.
          05 W-NAME               PIC X(4)    VALUE 'NAME'.
          05                      PIC X(9)    VALUE ' '.
          05 W-ID                 PIC X(2)    VALUE 'ID'.
          05                      PIC X(6)    VALUE ' '.
          05 W-SIZE               PIC X(4)    VALUE 'SIZE'.
          05                      PIC X(6)    VALUE ' '.
          05 W-TYPE               PIC X(4)    VALUE 'TYPE'.
          05                      PIC X(3)    VALUE ' '.
          05 W-STOCK              PIC X(5)    VALUE 'STOCK'.
          05                      PIC X(4)    VALUE ' '.
          05 W-COST               PIC X(4)    VALUE 'COST'.
          05                      PIC X(5)    VALUE ' '.
      *********** THIS IS WHERE HEADERS SECTION ENDS *************

      *********** THIS IS WHERE DETAIL SECTION BEGINS ************
       01 DETAIL-LINE-1.
          05                      PIC X(5)    VALUE ' '.
          05 PRODUCT-NAME-OUT     PIC X(13).
          05                      PIC X(4)    VALUE ' '.
          05 PRODUCT-ID-OUT       PIC X(3).
          05                      PIC X(2)    VALUE ' '.
          05 PRODUCT-SIZE-OUT     PIC X(11).
          05                      PIC X(2)    VALUE ' '.
          05 PRODUCT-TYPE-OUT     PIC X(5).
          05                      PIC X(3)    VALUE ' '.
          05 IN-STOCK-OUT         PIC Z99.
          05                      PIC X(3)    VALUE ' '.
		  05 TOTAL-PROD-OUT       PIC $$$,$$9.99.
       01 DETAIL-LINE-3.
          05                      PIC X(21)   VALUE ' '.
          05 PRODUCT-WORD         PIC X(9)    VALUE 'PRODUCT: '.
          05 PRODUCT-NAME-OUT-3   PIC X(13).
          05                      PIC X       VALUE ' '.
          05 TOTAL-W             
		  PIC X(9)    VALUE 'TOTAL:  '.
		  05 ALL-PRODUCT-TOTAL    PIC $,$$$,$$9.99.
      *********** THIS IS WHERE DETAIL SECTION ENDS *************

      *********** THIS IS WHERE TOTAL HEADERS BEGINS ************
       01 VENDOR-TOTAL-HEADER.
          05                      PIC X(13)   VALUE ' '.
          05 TOTAL-FOR-W          PIC X(10)   VALUE 'TOTAL FOR '.
          05 VENDOR-W             PIC X(8)    VALUE 'VENDOR: '.
          05 VENDOR-NAME-OUT      PIC X(11).
          05                      PIC X(11)   VALUE ' '.
          05 VENDOR-TOTAL-AMOUNT  PIC $,$$$,$$9.99.

       01 WAREHOUSE-TOTAL-HEADER.
          05                      PIC X(10)   VALUE ' '.
          05 TOTAL-FOR-W          PIC X(10)   VALUE 'TOTAL FOR '.
          05 WAREHOUSE-W          PIC X(11)   VALUE 'WAREHOUSE: '.
		  05 WAREHOUSE-TOT-ID-OUT PIC X(4).
          05                      PIC X(17)   VALUE ' '.
          05 WAREHOUSE-TOTAL-AMT  PIC $$,$$$,$$9.99.

       01 GRAND-TOTAL-HEADER.
          05                      PIC X(22)   VALUE ' '.
          05 GRAND-TOTAL-W        PIC X(12)   VALUE 'GRAND TOTAL '.
          05 COST-W               PIC X(5)    VALUE 'COST:'.
          05                      PIC X(12)   VALUE ' '.
		  05 GTL-COST             PIC $$$,$$$,$$9.99.
      ********** THIS IS WHERE TOTAL HEADERS END *************

       01 BLANK-PRINT-REC.
          05                      PIC X(66)   VALUE ' '.

       PROCEDURE DIVISION.
       000-CONTROL-MODULE.
          PERFORM 50-SORT-FILE-ROUTINE
          PERFORM 100-HOUSEKEEPING-ROUTINE
          PERFORM 125-READ-IN-FILE
          PERFORM 700-END-OF-JOB
          PERFORM 900-EOF-ROUTINE
       .

      * THIS IS FOR THE SORT AND MERGE OF FILES
       50-SORT-FILE-ROUTINE.
          SORT SORT-FILE
          ON ASCENDING KEY S-WH-ID
                           S-VND-ID
          USING UNSORTED-FILE-1
          GIVING SORTED-FILE-1

          SORT SORT-FILE
          ON ASCENDING KEY S-WH-ID
                           S-VND-ID
          USING UNSORTED-FILE-2
          GIVING SORTED-FILE-2

          SORT SORT-FILE
          ON ASCENDING KEY S-WH-ID
                           S-VND-ID
          USING UNSORTED-FILE-3
          GIVING SORTED-FILE-3

          MERGE SORT-FILE
          ON ASCENDING KEY S-WH-ID
                           S-VND-ID
          USING SORTED-FILE-1, SORTED-FILE-2, SORTED-FILE-3
          GIVING MERGED-FILE

       .

       100-HOUSEKEEPING-ROUTINE.
          OPEN INPUT MERGED-FILE
             OUTPUT PRINT-FILE
             OUTPUT ERROR-FILE
          ACCEPT WS-CURRENT-DATE FROM DATE
          MOVE RUN-MONTH TO RUN-MONTH-OUT
          MOVE RUN-DAY TO RUN-DAY-OUT
          MOVE RUN-YEAR TO RUN-YEAR-OUT
          PERFORM 150-HEADER-ROUTINE
       .

       125-READ-IN-FILE.
          PERFORM UNTIL NO-MORE-DATA
             READ MERGED-FILE
                AT END
                   MOVE 'N'TO EOF-FLAG
                NOT AT END
                   PERFORM 300-READ-IN-FILE-ROUTINE
             END-READ
          END-PERFORM
       .

       150-HEADER-ROUTINE.
          WRITE PRINT-REC FROM HEADING-ONE
                AFTER ADVANCING 1 LINE
          WRITE PRINT-REC FROM HEADING-TWO
		        AFTER ADVANCING 1 LINE
       .

       175-WAREHOUSE-HEADER-ROUTINE.
          MOVE MR-WH-ID TO WAREHOUSE-ID-OUT
		  MOVE MR-WH-ID TO WAREHOUSE-TOT-ID-OUT
          WRITE PRINT-REC FROM WAREHOUSE-HEADER
                AFTER ADVANCING 2 LINES
       .

       200-VENDOR-HEADER.
      * TABLE SEARCH TO  EXPAND VENDOR ID
          SET X1 TO 1
          SEARCH VENDOR-ITEM 
          AT END
      * CONCATONATE A STRING HERE FOR INVALID VENDOR IDS
		     STRING
               "INVALID" DELIMITED BY ' '
               ' ' DELIMITED BY SIZE
               WS-VENDOR-ID-HOLD DELIMITED BY ' '
               '  ' DELIMITED BY SIZE
               INTO VENDOR-NAME
               MOVE VENDOR-NAME TO VENDOR-NAME-OUT
               MOVE VENDOR-TEXT-NAME(X1) TO VENDOR-NAME
             WHEN MR-VND-ID EQUALS VENDOR-TEXT-ID(X1)
                MOVE VENDOR-TEXT-NAME(X1) TO VENDOR-NAME-OUT
                MOVE VENDOR-TEXT-NAME(X1) TO VENDOR-NAME
          END-SEARCH

             WRITE PRINT-REC FROM VENDOR-HEADER
                   AFTER ADVANCING 2 LINES
       .

       225-PRODUCT-HEADER.
          WRITE PRINT-REC FROM PRODUCT-HEADER-ONE
                AFTER ADVANCING 2 LINE
          WRITE PRINT-REC FROM PRODUCT-HEADER-TWO
                AFTER ADVANCING 1 LINE
          WRITE PRINT-REC FROM BLANK-PRINT-REC
                AFTER ADVANCING 1 LINE
       .

       300-READ-IN-FILE-ROUTINE.
      * IF STATEMENT TO CHECK IF THE WAREHOUSES ARE VALID
          IF MR-WH-ID EQUAL TO "NV10" OR "CA20" OR "WA30"
      * CHECK CONDITION FOR CONTROL-BREAK
             EVALUATE TRUE
                WHEN FIRST-RECORD = 'YES'
                   MOVE 'NO' TO FIRST-RECORD
                   MOVE MR-WH-ID TO WS-WAREHOUSE-ID-HOLD
                   MOVE MR-VND-ID TO WS-VENDOR-ID-HOLD
                   MOVE MR-PDT-ID TO WS-PRODUCT-ID-HOLD
                
                   PERFORM 175-WAREHOUSE-HEADER-ROUTINE
                   PERFORM 200-VENDOR-HEADER
                   PERFORM 225-PRODUCT-HEADER

                WHEN MR-WH-ID NOT EQUAL TO WS-WAREHOUSE-ID-HOLD
                   MOVE MR-WH-ID TO WS-WAREHOUSE-ID-HOLD
                   PERFORM 350-WAREHOUSE-CONTROL-BREAK
      * CALL THE WAREHOUSE, VENDOR, AND PRODUCT HEADERS
                   PERFORM 175-WAREHOUSE-HEADER-ROUTINE
                   PERFORM 200-VENDOR-HEADER
                   PERFORM 225-PRODUCT-HEADER
                WHEN MR-VND-ID NOT EQUAL TO WS-VENDOR-ID-HOLD
                   MOVE MR-VND-ID TO WS-VENDOR-ID-HOLD
                   PERFORM 375-VENDOR-CONTROL-BREAK
      * CALL THE VENDOR AND PRODUCT HEADERS
                   PERFORM 200-VENDOR-HEADER
                   PERFORM 225-PRODUCT-HEADER
                   
                WHEN MR-PDT-ID NOT EQUAL TO WS-PRODUCT-ID-HOLD
                   MOVE MR-PDT-ID TO WS-PRODUCT-ID-HOLD
                   PERFORM 400-PRODUCT-CONTROL-BREAK
      * CALL THE PRODUCT HEADERS
                   PERFORM 225-PRODUCT-HEADER

             END-EVALUATE

      * GO THROUGH THE ARRAY 1 BY 1 AND CHECK THE DATA
      * ALSO PUT THE DATA IN THE CORRECT PLACES FOR PRINTING
             PERFORM VARYING ARRAY-SUB
                     FROM 1 BY 1 UNTIL ARRAY-SUB > 5

      * DATA VERIFICATION FOR NAME
             IF MR-PDT-NAME(ARRAY-SUB) NOT EQUAL TO SPACES
                MOVE MR-PDT-NAME(ARRAY-SUB) TO PRODUCT-NAME-OUT
                MOVE MR-PDT-NAME(ARRAY-SUB) TO PRODUCT-NAME-OUT-3
             END-IF

      * IF ALL THE NAME VARIABLES ARE EQUAL TO THE FIRST
      * PASS SPACES SO THAT WE DONT HAVE TO PRINT THE NAME MULTIPLE TIMES
             IF MR-PDT-NAME(ARRAY-SUB) EQUAL TO 
             MR-PDT-NAME(ARRAY-SUB - 1)
                MOVE SPACES TO PRODUCT-NAME-OUT
             END-IF



             MOVE MR-PDT-ID TO PRODUCT-ID-OUT
      * DATA VERIFICATION FOR SIZING
             EVALUATE TRUE
                 WHEN MR-PDT-SIZE(ARRAY-SUB) EQUAL TO "X"
                    MOVE "EXTRA LARGE" TO PRODUCT-SIZE-OUT
                 WHEN MR-PDT-SIZE(ARRAY-SUB) EQUAL TO "L"
                    MOVE "LARGE     " TO PRODUCT-SIZE-OUT
                 WHEN MR-PDT-SIZE(ARRAY-SUB) EQUAL TO "M"
                    MOVE "MEDIUM    " TO PRODUCT-SIZE-OUT
                 WHEN MR-PDT-SIZE(ARRAY-SUB) EQUAL TO "S"
                    MOVE "SMALL     " TO PRODUCT-SIZE-OUT
                 WHEN MR-PDT-SIZE(ARRAY-SUB) EQUAL TO "A"
                    MOVE "SAMPLE    " TO PRODUCT-SIZE-OUT
                 WHEN OTHER
                    STRING
                       "BAD" DELIMITED BY ' '
                       ' ' DELIMITED BY SIZE
                       MR-PDT-SIZE(ARRAY-SUB) DELIMITED BY ' '
                       '   ' DELIMITED BY SIZE
                       INTO PRODUCT-SIZE-OUT
                END-EVALUATE
      * DATA VERIFICATION FOR TYPE
             IF MR-PDT-TYPE(ARRAY-SUB) EQUAL TO "C" OR "O"
                IF MR-PDT-TYPE(ARRAY-SUB) EQUAL TO "C"
                   MOVE "CREAM" TO PRODUCT-TYPE-OUT
                ELSE IF MR-PDT-TYPE(ARRAY-SUB) EQUAL TO "O"
                   MOVE "OIL  " TO PRODUCT-TYPE-OUT
                END-IF
             END-IF
      * DATA VERIFICATION FOR STOCK
             IF MR-PDT-STOCK(ARRAY-SUB) IS NUMERIC
                MOVE MR-PDT-STOCK(ARRAY-SUB) TO IN-STOCK-OUT
             ELSE
                MOVE ZEROES TO IN-STOCK-OUT
                MOVE ZEROES TO MR-PDT-STOCK(ARRAY-SUB)
             END-IF

      * DATA VERIFICATION FOR TOTAL
      * ALSO DO THE ADDITION FOR TOTAL PRICE
             IF MR-PDT-COST(ARRAY-SUB) IS NUMERIC
                MULTIPLY MR-PDT-COST(ARRAY-SUB) BY 
                MR-PDT-STOCK(ARRAY-SUB) GIVING WHOLE-PRICE

                MOVE WHOLE-PRICE TO TOTAL-PROD-OUT
                ADD WHOLE-PRICE TO WS-TOTAL-PROD-PRICE
                MOVE ZEROES TO WHOLE-PRICE
             ELSE
                MOVE ZEROES TO TOTAL-PROD-OUT
             END-IF

             PERFORM 800-WRITE-DETAIL-LINE-1
		  
             END-PERFORM

      * PRINT OUT THE TOTAL FOR COSTUME
            MOVE WS-TOTAL-PROD-PRICE TO ALL-PRODUCT-TOTAL
            WRITE PRINT-REC FROM DETAIL-LINE-3
                  AFTER ADVANCING 2 LINES

      * ADD THE TOTAL COST TO VENDOR-COST ACCUMULATOR, USED LATER IN VENDOR-CONTROL-BREAK
            ADD WS-TOTAL-PROD-PRICE TO WS-VENDOR-COST
      * ADD THE TOTAL COST TO WAREHOUSE-COST ACCUMULATOR, 
      * USED LATER IN WAREHOUSE-CONTROL-BREAK
            ADD WS-TOTAL-PROD-PRICE TO WS-WH-COST
	  
      * ADD THE TOTAL COST TO GRAND TOTAL ACCUMULATOR, THEN RESET TOTAL COST
            ADD WS-TOTAL-PROD-PRICE TO WS-GRAND-TOTAL
            MOVE ZEROES TO WS-TOTAL-PROD-PRICE
      * IF WAREHOUSE IS NOT "NA10", "CA20" OR "WA30" RUN THIS
         ELSE
      * WRITE THE BAD RECORD TO THE ERROR FILE
            WRITE ERROR-REC FROM MERGED-RECORDS
            AFTER ADVANCING 1 LINE


      * ADD ONE TO THE TOTAL AMOUNT OF BAD WAREHOUSE ENTRIES
            ADD 1 TO TOTAL-ERROR-RECORDS
         END-IF
       .

      * MAJOR CONTROL BREAK
       350-WAREHOUSE-CONTROL-BREAK.
          MOVE WS-VENDOR-COST TO VENDOR-TOTAL-AMOUNT
          WRITE PRINT-REC FROM VENDOR-TOTAL-HEADER
                AFTER ADVANCING 2 LINES.
          MOVE ZEROES TO WS-VENDOR-COST

      * STOP THE PROGRAM FROM PRINTING THE TOTAL-VENDOR TWICE
		  MOVE 'YES' TO BYPASS-VENDOR-HEADER

      * PRINT THE WAREHOUSE COST TOTAL
          MOVE WS-WH-COST TO WAREHOUSE-TOTAL-AMT
          WRITE PRINT-REC FROM WAREHOUSE-TOTAL-HEADER
                AFTER ADVANCING 2 LINES.
          WRITE PRINT-REC FROM BLANK-PRINT-REC
                AFTER ADVANCING 2 LINES.
          MOVE ZEROES TO WS-WH-COST

          MOVE WS-WAREHOUSE-ID-HOLD TO WAREHOUSE-ID-OUT

          MOVE MR-VND-ID TO WS-VENDOR-ID-HOLD

          MOVE SPACES TO WS-WAREHOUSE-ID-HOLD
          MOVE MR-WH-ID TO WS-WAREHOUSE-ID-HOLD

       .
      * INTERMEDIATE CONTROL BREAK
       375-VENDOR-CONTROL-BREAK.
	      IF BYPASS-VENDOR-HEADER EQUAL TO 'NO'
      * PRINT THE VENDOR COST TOTAL
             MOVE WS-VENDOR-COST TO VENDOR-TOTAL-AMOUNT
             WRITE PRINT-REC FROM VENDOR-TOTAL-HEADER
                   AFTER ADVANCING 3 LINES
             WRITE PRINT-REC FROM BLANK-PRINT-REC
                   AFTER ADVANCING 1 LINE
             MOVE ZEROES TO WS-VENDOR-COST
          END-IF
      * CHANGE THE VARIABLE AGAIN SO THAT IT CAN PRINT THE HEADERS
          IF BYPASS-VENDOR-HEADER EQUAL TO 'YES'
             MOVE 'NO' TO BYPASS-VENDOR-HEADER
          END-IF

          PERFORM 400-PRODUCT-CONTROL-BREAK

          
          MOVE ZEROES TO WS-VENDOR-ID-HOLD
          MOVE MR-VND-ID TO WS-VENDOR-ID-HOLD
       .
      * MINOR CONTROL BREAK
       400-PRODUCT-CONTROL-BREAK.
          WRITE PRINT-REC FROM BLANK-PRINT-REC
                AFTER ADVANCING 1 LINE.

          MOVE SPACES TO WS-PRODUCT-ID-HOLD
          MOVE MR-PDT-ID TO WS-PRODUCT-ID-HOLD

      * ADDITION FOR VENDOR TOTAL
          ADD WS-TOTAL-PROD-PRICE TO WS-VENDOR-COST

       .

       700-END-OF-JOB.
      * FORCE PRINT THE LAST VENDOR TOTAL
          MOVE WS-VENDOR-COST TO VENDOR-TOTAL-AMOUNT
          WRITE PRINT-REC FROM VENDOR-TOTAL-HEADER
		        AFTER ADVANCING 3 LINES
      * FORCE PRINT THE LAST WAREHOUSE TOTAL
          MOVE WS-WH-COST TO WAREHOUSE-TOTAL-AMT
          WRITE PRINT-REC FROM WAREHOUSE-TOTAL-HEADER
                AFTER ADVANCING 2 LINES
      * FORCE PRINT THE GRAND TOTAL
          MOVE WS-GRAND-TOTAL TO GTL-COST
          WRITE PRINT-REC FROM GRAND-TOTAL-HEADER
                AFTER ADVANCING 2 LINES
      * DISPLAY THE NUMBER OF BAD WAREHOUSE ENTRIES ON CONSOLE
          DISPLAY "Number of Invalid Warehouse Records: " 
          TOTAL-ERROR-RECORDS UPON CONSOLE
       .

       800-WRITE-DETAIL-LINE-1.
          WRITE PRINT-REC FROM DETAIL-LINE-1
                 AFTER ADVANCING 1 LINES.
       .

       900-EOF-ROUTINE.
          CLOSE MERGED-FILE
             PRINT-FILE
             ERROR-FILE
          STOP RUN
       .
