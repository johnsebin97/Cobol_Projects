IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       AUTHOR. CLAUDE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-REPORT ASSIGN TO "PAYROLL.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID              PIC 9(5).
           05 EMP-NAME            PIC X(30).
           05 EMP-RATE            PIC 9(3)V99.
           05 EMP-HOURS           PIC 9(3)V99.
           05 EMP-TYPE            PIC X.
               88 FULLTIME        VALUE "F".
               88 PARTTIME        VALUE "P".
       
       FD PAYROLL-REPORT.
       01 REPORT-LINE             PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-FLAGS.
           05 EOF-FLAG            PIC X VALUE 'N'.
               88 EOF             VALUE 'Y'.
       
       01 WS-CALCULATIONS.
           05 WS-REGULAR-PAY      PIC 9(7)V99.
           05 WS-OVERTIME-PAY     PIC 9(7)V99.
           05 WS-GROSS-PAY        PIC 9(7)V99.
           05 WS-TAX              PIC 9(7)V99.
           05 WS-NET-PAY          PIC 9(7)V99.
       
       01 WS-CONSTANTS.
           05 WS-TAX-RATE         PIC V99 VALUE 0.20.
           05 WS-OVERTIME-RATE    PIC V99 VALUE 1.50.
           05 WS-REGULAR-HOURS    PIC 99 VALUE 40.
       
       01 WS-REPORT-HEADER.
           05 FILLER              PIC X(80) VALUE
           "EMP-ID  NAME                  RATE   HOURS    GROSS      TAX     NET".
       
       01 WS-REPORT-LINE.
           05 RPT-EMP-ID          PIC 9(5).
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-EMP-NAME        PIC X(20).
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-EMP-RATE        PIC $$$9.99.
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-EMP-HOURS       PIC 999.99.
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-GROSS-PAY       PIC $$,$$9.99.
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-TAX             PIC $$,$$9.99.
           05 FILLER              PIC X(2) VALUE SPACES.
           05 RPT-NET-PAY         PIC $$,$$9.99.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INIT
           PERFORM 200-PROCESS-RECORDS UNTIL EOF
           PERFORM 300-CLEANUP
           STOP RUN.
       
       100-INIT.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-REPORT
           WRITE REPORT-LINE FROM WS-REPORT-HEADER
           WRITE REPORT-LINE FROM SPACES.
       
       200-PROCESS-RECORDS.
           READ EMPLOYEE-FILE
               AT END
                   SET EOF TO TRUE
               NOT AT END
                   PERFORM 210-CALCULATE-PAY
                   PERFORM 220-GENERATE-REPORT-LINE.
       
       210-CALCULATE-PAY.
           IF EMP-HOURS > WS-REGULAR-HOURS
               COMPUTE WS-REGULAR-PAY = EMP-RATE * WS-REGULAR-HOURS
               COMPUTE WS-OVERTIME-PAY = (EMP-HOURS - WS-REGULAR-HOURS) *
                                         EMP-RATE * WS-OVERTIME-RATE
               COMPUTE WS-GROSS-PAY = WS-REGULAR-PAY + WS-OVERTIME-PAY
           ELSE
               COMPUTE WS-GROSS-PAY = EMP-RATE * EMP-HOURS
               MOVE 0 TO WS-OVERTIME-PAY.
           
           COMPUTE WS-TAX ROUNDED = WS-GROSS-PAY * WS-TAX-RATE
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TAX.
       
       220-GENERATE-REPORT-LINE.
           MOVE EMP-ID TO RPT-EMP-ID
           MOVE EMP-NAME TO RPT-EMP-NAME
           MOVE EMP-RATE TO RPT-EMP-RATE
           MOVE EMP-HOURS TO RPT-EMP-HOURS
           MOVE WS-GROSS-PAY TO RPT-GROSS-PAY
           MOVE WS-TAX TO RPT-TAX
           MOVE WS-NET-PAY TO RPT-NET-PAY
           WRITE REPORT-LINE FROM WS-REPORT-LINE.
       
       300-CLEANUP.
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-REPORT.