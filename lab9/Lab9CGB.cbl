       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BKBRK.
       AUTHOR.         GARRETT BURNS.
      ******************************************************************
      *
      *             LAB 9
      *
      *  READS TWO STUDENT FILES WHICH ARE NOT SORTED AND NEED TO BE
      *  MERGED.  THIS PROGRAM CONTAINS A MULTI-LEVEL CONTROL BREAK ON
      *  DEPT AND CLASS.  IN ORDER TO RUN THIS PROGRAM, YOU WILL NEED TO
      *  SORT THE INCOMING DATA FILES AND MERGE THEM PRIOR TO PROCESSING
      *  THE DATA FOR THIS LAB.
      *
      *
      *  NOTE:  USE A BASIC SORT WITH A USING/GIVING
      *         MERGE THE TWO FILES
      *         THERE ARE TWO KEYS THIS IS SORTED ON DEPT AND CLASS
      *         YOU WILL NEED ADDITIONAL SELECT STATEMENTS
      *         YOU WILL NEED SD DESCRIPTION OF THE SORT AND MERGE FILES
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.

           SELECT UNSORTED-FILE1
               ASSIGN TO "UNSORTEDSTUDENT1.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT UNSORTED-FILE2
               ASSIGN TO "UNSORTEDSTUDENT2.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-FILE1
               ASSIGN TO "SORTEDSTUDENT1.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-FILE2
               ASSIGN TO "SORTEDSTUDENT2.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT MERGED-STUDENT-FILE
               ASSIGN TO "MERGEDSORTEDSTUDENT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "STUDENTCGB".

      *YOU NEED SELECT STATEMENT(S) FOR THE SD FILES
      
           SELECT SORT-FILE
               ASSIGN TO "SORTFILE.TMP".
               
           SELECT MERGE-FILE
               ASSIGN TO "MERGEFILE.TMP".

      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD UNSORTED-FILE1
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  UNSORTED-RECORD1.
           05  UR1-DEPT-CODE                    PIC A(4).
           05  UR1-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).

      *
       FD UNSORTED-FILE2
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  UNSORTED-RECORD2.
           05  UR2-DEPT-CODE                    PIC A(4).
           05  UR2-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
       FD STUDENT-FILE1
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  STUDENT-RECORD1.
           05  SR1-DEPT-CODE                    PIC A(4).
           05  SR1-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).

       FD STUDENT-FILE2
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  STUDENT-RECORD2.
           05  SR2-DEPT-CODE                    PIC A(4).
           05  SR2-CLASS-CODE                   PIC X(5).
           05  FILLER                          PIC X(32).
      *
        FD MERGED-STUDENT-FILE
           RECORD CONTAINS 41 CHARACTERS.
      *
       01  MERGED-STU-REC.
           05  MS-DEPT-CODE                    PIC A(4).
           05  MS-CLASS-CODE                   PIC X(5).
           05  MS-NAME                         PIC X(20).
           05  MS-TEST OCCURS 4 TIMES          PIC 9(3).

      *YOU WILL NEED SD DESCRIPTION(S) FOR THE SORT AND MERGE FILES

        SD SORT-FILE
           RECORD CONTAINS 41 CHARACTERS.

       01 SORTFILE.
           05  SF-DEPT-CODE                    PIC A(4).
           05  SF-CLASS-CODE                   PIC X(5).
           05  SF-NAME                         PIC X(20).
           05  SF-TEST OCCURS 4 TIMES          PIC 9(3).
           
        SD MERGE-FILE
           RECORD CONTAINS 41 CHARACTERS.
           
       01 MERGEFILE.
           05  MF-DEPT-CODE                    PIC A(4).
           05  MF-CLASS-CODE                   PIC X(5).
           05  MF-NAME                         PIC X(20).
           05  MF-TEST OCCURS 4 TIMES          PIC 9(3).



       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-LINE                     PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
       05  SUB               PIC 9       VALUE ZERO.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC S9      VALUE +1.
           05  LINE-COUNT                  PIC S9(2)   VALUE +0.
           05  PAGE-NO                     PIC S9(2)   VALUE +0.

      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)  VALUE +0.
           05  DF-TEST-GRADE                PIC S9(5)  VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
           05  DF-GRADE                     PIC X.
           05  DF-TOTAL-STUDENTS            PIC S99 VALUE +0.
           05  DF-DEPT-TOTAL                PIC S99 VALUE +0.
           05  DF-DEPT-HOLD                 PIC A(4).
           05  DF-CLASS-HOLD                PIC X(5).
           05  DF-CLASS-TOTAL               PIC S9(5)  VALUE +0.
      *
       01  HEADING-ONE.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7)  VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(13) VALUE 'CGB'.
           05                              PIC X(5)  VALUE 'PAGE'.
           05  H1-PAGE-NO                  PIC Z9.
      *
       01  HEADING-TWO.
           05                              PIC X(5)  VALUE SPACES.
           05                              PIC X(20) VALUE
                                               'DEPARTMENT CODE  '.
           05                              PIC X(5)  VALUE SPACES.
           05 H2-DEPT-CODE                 PIC A(4).
      *
       01  HEADING-THREE.
           05                              PIC X(10) VALUE SPACES.
           05                              PIC X(12) VALUE
                                              'CLASS CODE  '.
           05  H3-CLASS-CODE               PIC X(5).
      *
       01  HEADING-FOUR.
           05                              PIC X(19) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(3) VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7)  VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                              PIC X(7) VALUE SPACES.
           05  DL-NAME                     PIC X(20).
           05                              PIC X(7).
           05  DL-TEST OCCURS 4 TIMES      PIC XXXBBBBB.
           05  DL-GRADE                    PIC X.

      *
       01  DEPT-GROUP-LINE.
               05                              PIC X(45)   VALUE
                            'TOTAL NUMBER OF STUDENTS FOR DEPARTMENT '.
           05  DGL-DEPT-CODE               PIC X(4).
           05                              PIC X(6)    VALUE ' IS   '.
           05  DGL-DEPT-TOTAL              PIC ZZZ9.
      *
       01  CLASS-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL NUMBER OF STUDENTS FOR CLASS '.
           05  CGL-CLASS-CODE              PIC X(5).
           05                              PIC X(5)    VALUE ' IS  '.
           05  CGL-CLASS-TOTAL             PIC ZZZ9.
      *
       01  OVER-ALL-TOTAL.
           05                              PIC X(54)  VALUE
                           'TOTAL STUDENTS FOR ALL DEPARTMENTS IS '.
           05  OAT-TOTAL                   PIC ZZZZ9.

      *
       PROCEDURE DIVISION.
      *
       10-MAIN-ROUTINE.
           PERFORM 15-SORTMERGE-STUDENT-FILE
           PERFORM 20-HSKPING-ROUTINE
           PERFORM 30-READ-STUDENT-FILE
           PERFORM 600-FINAL-ROUTINE

       .
       15-SORTMERGE-STUDENT-FILE.
      *CODE YOUR SORT AND MERGE ROUTINES HERE

          SORT SORT-FILE
             ON ASCENDING KEY SF-DEPT-CODE, SF-CLASS-CODE
             USING UNSORTED-FILE1
             GIVING STUDENT-FILE1
          SORT SORT-FILE
             ON ASCENDING KEY SF-DEPT-CODE, SF-CLASS-CODE
             USING UNSORTED-FILE2
             GIVING STUDENT-FILE2
             
          MERGE MERGE-FILE
             ON ASCENDING KEY MF-DEPT-CODE, MF-CLASS-CODE
             USING STUDENT-FILE1, STUDENT-FILE2
             GIVING MERGED-STUDENT-FILE

       .
       20-HSKPING-ROUTINE.
           OPEN INPUT  MERGED-STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE


           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
           PERFORM 40-HEADING-ROUTINE
       .
       30-READ-STUDENT-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ MERGED-STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 100-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM

       .
       40-HEADING-ROUTINE.
           ADD 1 TO PAGE-NO
           MOVE PAGE-NO TO H1-PAGE-NO
           WRITE REPORT-LINE FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING
       .
       100-PROCESS-STUDENT-RECORD.

      *  CHECK FOR CONTROL BREAKS HERE

             EVALUATE TRUE
                   WHEN FIRST-RECORD = 'YES'
                       MOVE 'NO' TO FIRST-RECORD
                       MOVE MS-DEPT-CODE TO DF-DEPT-HOLD
                       MOVE MS-CLASS-CODE TO DF-CLASS-HOLD
                       PERFORM 450-PRINT-DEPT-HEADER
                       PERFORM 475-PRINT-CLASS-HEADER
                   WHEN MS-DEPT-CODE NOT = DF-DEPT-HOLD
                       PERFORM 300-DEPT-BREAK
                       PERFORM 450-PRINT-DEPT-HEADER
                       PERFORM 475-PRINT-CLASS-HEADER
                   WHEN MS-CLASS-CODE NOT = DF-CLASS-HOLD
                       PERFORM 400-CLASS-BREAK
                       PERFORM 475-PRINT-CLASS-HEADER
             END-EVALUATE

           MOVE MS-NAME TO DL-NAME

       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

            MOVE MS-TEST(SUB) TO DL-TEST(SUB)
            ADD MS-TEST(SUB) TO DF-TEST-TOTAL

       END-PERFORM

           DIVIDE DF-TEST-TOTAL BY 4
                  GIVING DF-TEST-AVERAGE ROUNDED

           MOVE DF-TEST-AVERAGE TO DF-TEST-GRADE

           EVALUATE TRUE
               WHEN DF-TEST-GRADE > 89
                   MOVE 'A' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 80 AND DF-TEST-GRADE <= 89
                   MOVE 'B' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 70 AND DF-TEST-GRADE <= 79
                   MOVE 'C' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 60 AND DF-TEST-GRADE <= 69
                   MOVE 'D' TO DF-GRADE
               WHEN DF-TEST-GRADE < 60
                   MOVE 'F' TO DF-GRADE
           END-EVALUATE

           MOVE DF-GRADE TO DL-GRADE

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           ADD 1 TO DF-TOTAL-STUDENTS
           ADD 1 TO DF-CLASS-TOTAL
           ADD 1 TO DF-DEPT-TOTAL
           MOVE ZEROS TO DF-TEST-AVERAGE
           MOVE ZEROS TO DF-TEST-TOTAL
           MOVE ZEROS TO DF-TEST-GRADE
       .

       200-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           ADD PROPER-SPACING TO LINE-COUNT
       .

       300-DEPT-BREAK.

      * handle department break here
              PERFORM 400-CLASS-BREAK
              MOVE DF-DEPT-HOLD TO DGL-DEPT-CODE
              MOVE DF-DEPT-TOTAL TO DGL-DEPT-TOTAL
              MOVE DEPT-GROUP-LINE TO REPORT-LINE
              MOVE 3 TO PROPER-SPACING
              PERFORM 200-WRITE-A-LINE
              MOVE 1 TO PROPER-SPACING
              MOVE ZEROS TO DGL-DEPT-TOTAL
              MOVE ZEROS TO DF-DEPT-TOTAL
              MOVE MS-DEPT-CODE TO DF-DEPT-HOLD


       .
      *
       400-CLASS-BREAK.

      *handle class break here

              MOVE DF-CLASS-HOLD TO CGL-CLASS-CODE
              MOVE DF-CLASS-TOTAL TO CGL-CLASS-TOTAL
              MOVE CLASS-GROUP-LINE TO REPORT-LINE
              MOVE 3 TO PROPER-SPACING
              PERFORM 200-WRITE-A-LINE
              MOVE 1 TO PROPER-SPACING
              MOVE ZEROS TO DF-CLASS-TOTAL
              MOVE ZEROS TO CGL-CLASS-TOTAL
              MOVE MS-CLASS-CODE TO DF-CLASS-HOLD

       .
      *
       450-PRINT-DEPT-HEADER.
           MOVE MS-DEPT-CODE TO H2-DEPT-CODE
           WRITE REPORT-LINE FROM HEADING-TWO
               AFTER ADVANCING 2 LINES

       .

       475-PRINT-CLASS-HEADER.
           MOVE MS-CLASS-CODE TO H3-CLASS-CODE

           WRITE REPORT-LINE FROM HEADING-THREE
               AFTER ADVANCING 2 LINES

           WRITE REPORT-LINE FROM HEADING-FOUR
               AFTER ADVANCING 2 LINES

       .



        500-PRINT-FINAL-TOTALS.

           MOVE DF-TOTAL-STUDENTS TO OAT-TOTAL
           MOVE OVER-ALL-TOTAL TO REPORT-LINE
           MOVE 3 TO PROPER-SPACING
           PERFORM 200-WRITE-A-LINE

       .


       600-FINAL-ROUTINE.
            PERFORM 300-DEPT-BREAK
            PERFORM 500-PRINT-FINAL-TOTALS

           CLOSE MERGED-STUDENT-FILE
                 STUDENT-REPORT-FILE

           STOP RUN
       .


