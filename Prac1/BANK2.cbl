       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.


       DATA DIVISION.
       FILE SECTION.
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM               PIC  9(35).
           02 MOV-TARJETA           PIC  9(16).
           02 MOV-ANO               PIC   9(4).
           02 MOV-MES               PIC   9(2).
           02 MOV-DIA               PIC   9(2).
           02 MOV-HOR               PIC   9(2).
           02 MOV-MIN               PIC   9(2).
           02 MOV-SEG               PIC   9(2).
           02 MOV-IMPORTE-ENT       PIC  S9(7).
           02 MOV-IMPORTE-DEC       PIC   9(2).
           02 MOV-CONCEPTO          PIC  X(35).
           02 MOV-SALDOPOS-ENT      PIC  S9(9).
           02 MOV-SALDOPOS-DEC      PIC   9(2).


       WORKING-STORAGE SECTION.
       77 FSM                       PIC   X(2).

       78 BLACK                   VALUE      0.
       78 BLUE                    VALUE      1.
       78 GREEN                   VALUE      2.
       78 CYAN                    VALUE      3.
       78 RED                     VALUE      4.
       78 MAGENTA                 VALUE      5.
       78 YELLOW                  VALUE      6.
       78 WHITE                   VALUE      7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO               PIC   9(4).
               10 MES               PIC   9(2).
               10 DIA               PIC   9(2).
           05 HORA.
               10 HORAS             PIC   9(2).
               10 MINUTOS           PIC   9(2).
               10 SEGUNDOS          PIC   9(2).
               10 MILISEGUNDOS      PIC   9(2).
           05 DIF-GMT               PIC  S9(4).

       01 KEYBOARD-STATUS           PIC  9(4).
           88 ENTER-PRESSED       VALUE     0.
           88 PGUP-PRESSED        VALUE  2001.
           88 PGDN-PRESSED        VALUE  2002.
           88 UP-ARROW-PRESSED    VALUE  2003.
           88 DOWN-ARROW-PRESSED  VALUE  2004.

       77 LAST-MOV-NUM             PIC  9(35).
       77 PRESSED-KEY              PIC   9(1).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 HAY-SALDO-DISPLAY.
           05 SALDO-ENT SIGN IS LEADING SEPARATE
               LINE 12 COL 33 PIC -9(7) FROM MOV-SALDOPOS-ENT.
           05 SEPARADOR LINE 12 COL 41 VALUE ",".
           05 SALDO-DEC LINE 12 COL 42 PIC 99 FROM MOV-SALDOPOS-DEC.
           05 MONEDA LINE 12 COL 45 VALUE "EUR".



       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank" LINE 2 COLUMN 26
               WITH FOREGROUND-COLOR IS CYAN.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA LINE 4 COLUMN 32.
           DISPLAY "-" LINE 4 COLUMN 34.
           DISPLAY MES LINE 4 COLUMN 35.
           DISPLAY "-" LINE 4 COLUMN 37.
           DISPLAY ANO LINE 4 COLUMN 38.
           DISPLAY HORAS LINE 4 COLUMN 44.
           DISPLAY ":" LINE 4 COLUMN 46.
           DISPLAY MINUTOS LINE 4 COLUMN 47.

       PCONSULTA-SALDO.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00 AND 05
               GO TO PSYS-ERR.

           MOVE 0 TO LAST-MOV-NUM.


       LECTURA-MOV.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO LAST-MOV-FOUND.
              IF MOV-TARJETA = TNUM
                  IF LAST-MOV-NUM < MOV-NUM
                      MOVE MOV-NUM TO LAST-MOV-NUM.
              GO LECTURA-MOV.

       LAST-MOV-FOUND.
           CLOSE F-MOVIMIENTOS.

           DISPLAY "Consulta de saldo" LINE 8 COLUMN 30.
           DISPLAY "El saldo de tu cuenta" LINE 10 COLUMN 19.
           DISPLAY TNUM LINE 10 COLUMN 41.
           DISPLAY "es" LINE 10 COLUMN 58.

           IF LAST-MOV-NUM = 0
               GO TO NO-MOVIMIENTOS.

           MOVE LAST-MOV-NUM TO MOV-NUM.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00 
               GO TO PSYS-ERR.

           READ F-MOVIMIENTOS INVALID KEY GO PSYS-ERR.
           DISPLAY HAY-SALDO-DISPLAY.

           CLOSE F-MOVIMIENTOS.
           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 33.
           GO TO EXIT-ENTER.

       NO-MOVIMIENTOS.
           DISPLAY "0" LINE 12 COLUMN 34.
           DISPLAY "." LINE 12 COLUMN 35.
           DISPLAY "00" LINE 12 COLUMN 36.
           DISPLAY "EUR" LINE 12 COLUMN 39.

           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 33.
           GO TO EXIT-ENTER.

       PSYS-ERR.

           CLOSE F-MOVIMIENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 9 COLUMN 25
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COLUMN 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 33.

       EXIT-ENTER.
           ACCEPT PRESSED-KEY LINE 24 COLUMN 80
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.
