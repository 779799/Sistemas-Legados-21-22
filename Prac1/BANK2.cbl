       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-MOVIMIENTOS ASSIGN TO DISK
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
       77 PRESSED-KEY              PIC   9(4).

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
           DISPLAY(2 26) "Cajero Automatico UnizarBank"
               WITH FOREGROUND-COLOR IS 1.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY(4 32) DIA.
           DISPLAY(4 34) "-".
           DISPLAY(4 35) MES.
           DISPLAY(4 37) "-".
           DISPLAY(4 38) ANO.
           DISPLAY(4 44) HORAS.
           DISPLAY(4 46) ":".
           DISPLAY(4 47) MINUTOS.

       PCONSULTA-SALDO.
           OPEN INPUT F-MOVIMIENTOS.
           IF FSM <> 30
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

           DISPLAY(8 30) "Consulta de saldo".
           DISPLAY(10 19) "El saldo de tu cuenta".
           DISPLAY(10 41) TNUM.
           DISPLAY(10 58) "es".

           IF LAST-MOV-NUM = 0
               GO TO NO-MOVIMIENTOS.

           MOVE LAST-MOV-NUM TO MOV-NUM.
           OPEN INPUT F-MOVIMIENTOS.
           IF FSM <> 30
               GO TO PSYS-ERR.

           READ F-MOVIMIENTOS INVALID KEY GO PSYS-ERR.
           DISPLAY HAY-SALDO-DISPLAY.

           CLOSE F-MOVIMIENTOS.
           DISPLAY(24 33) "Enter - Aceptar".
           GO TO EXIT-ENTER.

       NO-MOVIMIENTOS.
           DISPLAY(12 34) "0".
           DISPLAY(12 35) ".".
           DISPLAY(12 36) "00".
           DISPLAY(12 39) "EUR".

           DISPLAY(24 33) "Enter - Aceptar".
           GO TO EXIT-ENTER.

       PSYS-ERR.

           CLOSE F-MOVIMIENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY(9 25) "Ha ocurrido un error interno"
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY(11 32) "Vuelva mas tarde"
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY(24 33) "Enter - Aceptar".

       EXIT-ENTER.
           ACCEPT(24 80) PRESSED-KEY
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.
