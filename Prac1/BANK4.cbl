       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK4.

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

       01 KEYBOARD-STATUS           PIC   9(4).
           88 ENTER-PRESSED       VALUE      0.
           88 PGUP-PRESSED        VALUE   2001.
           88 PGDN-PRESSED        VALUE   2002.
           88 UP-ARROW-PRESSED    VALUE   2003.
           88 DOWN-ARROW-PRESSED  VALUE   2004.
           88 ESC-PRESSED         VALUE   2005.

       77 LAST-USER-MOV-NUM        PIC   9(35).
       77 LAST-MOV-NUM             PIC   9(35).

       77 EURENT-USUARIO           PIC   S9(7).
       77 EURDEC-USUARIO           PIC    9(2).
       77 SALDO-USUARIO-ENT        PIC   S9(9).
       77 SALDO-USUARIO-DEC        PIC    9(2).
       77 CENT-SALDO-USER          PIC  S9(11).
       77 CENT-IMPOR-USER          PIC    9(9).

       77 CON                      PIC   X(35) VALUE "Retirada".
       77 PRESSED-KEY              PIC    9(4).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).



       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.


       01 ENTRADA-USUARIO.
           05 FILLER BLANK ZERO AUTO UNDERLINE
               LINE 11 COL 40 PIC 9(7) USING EURENT-USUARIO.
           05 FILLER BLANK ZERO UNDERLINE
               LINE 11 COL 48 PIC 9(2) USING EURDEC-USUARIO.

       01 SALDO-DISPLAY.
           05 FILLER SIGN IS LEADING SEPARATE
               LINE 10 COL 32 PIC -9(7) FROM SALDO-USUARIO-ENT.
           05 FILLER LINE 10 COL 40 VALUE ".".
           05 FILLER LINE 10 COL 41 PIC 99 FROM SALDO-USUARIO-DEC.
           05 FILLER LINE 10 COL 44 VALUE "EUR".

       01 SALDO-DISPLAY-FINAL.
           05 FILLER SIGN IS LEADING SEPARATE
               LINE 11 COL 44 PIC -9(7) FROM SALDO-USUARIO-ENT.
           05 FILLER LINE 11 COL 52 VALUE ".".
           05 FILLER LINE 11 COL 53 PIC 99 FROM SALDO-USUARIO-DEC.
           05 FILLER LINE 11 COL 56 VALUE "EUR".




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




       CONSULTA-ULTIMO-MOVIMIENTO SECTION.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 30
              GO TO PSYS-ERR.

           MOVE 0 TO LAST-MOV-NUM.

       LEER-ULTIMO-MOV-READ.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO LAST-MOV-FOUND.

           IF MOV-NUM > LAST-MOV-NUM
               MOVE MOV-NUM TO LAST-MOV-NUM.

           GO TO LEER-ULTIMO-MOV-READ.

       LAST-MOV-FOUND.
           CLOSE F-MOVIMIENTOS.




       CONSULTA-SALDO-USUARIO SECTION.
           OPEN INPUT F-MOVIMIENTOS.
           IF FSM <> 30
               GO TO PSYS-ERR.

           MOVE 0 TO LAST-USER-MOV-NUM.
           MOVE 0 TO MOV-NUM.


       LECTURA-MOV-USER.
           READ F-MOVIMIENTOS NEXT RECORD
              AT END GO LAST-USER-MOV-FOUND.

              IF MOV-TARJETA = TNUM
                  IF LAST-USER-MOV-NUM < MOV-NUM
                      MOVE MOV-NUM TO LAST-USER-MOV-NUM.
              GO LECTURA-MOV-USER.

       LAST-USER-MOV-FOUND.
           CLOSE F-MOVIMIENTOS.

           IF LAST-USER-MOV-NUM = 0 THEN
               MOVE 0 TO SALDO-USUARIO-ENT
               MOVE 0 TO SALDO-USUARIO-DEC
               MOVE 0 TO CENT-SALDO-USER
               GO TO PANTALLA-RETIRADA
           END-IF.

           MOVE LAST-USER-MOV-NUM TO MOV-NUM.

           OPEN INPUT F-MOVIMIENTOS.
           IF FSM <> 30
               GO TO PSYS-ERR.

           READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR.

           MOVE MOV-SALDOPOS-ENT TO SALDO-USUARIO-ENT.
           MOVE MOV-SALDOPOS-DEC TO SALDO-USUARIO-DEC.
           COMPUTE CENT-SALDO-USER = (SALDO-USUARIO-ENT * 100)
                                     + SALDO-USUARIO-DEC.

           CLOSE F-MOVIMIENTOS.




       PANTALLA-RETIRADA SECTION.
           INITIALIZE EURENT-USUARIO.
           INITIALIZE EURDEC-USUARIO.

           DISPLAY(24 1) "Enter - Aceptar".
           DISPLAY(24 66) "ESC - Cancelar".

           DISPLAY(8 30) "Retirar efectivo".
           DISPLAY(10 19) "Saldo Actual: ".

           DISPLAY SALDO-DISPLAY.

           DISPLAY(11 19) "Indique la cantidad:         ".
           DISPLAY(11 47) ".".
           DISPLAY(11 51) "EUR".

           ACCEPT ENTRADA-USUARIO ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO PANTALLA-RETIRADA
           END-IF.

           COMPUTE CENT-IMPOR-USER = (EURENT-USUARIO * 100)
                                     + EURDEC-USUARIO.

           IF CENT-IMPOR-USER > CENT-SALDO-USER THEN
               DISPLAY(15 19) "Indique una cantidad menor!!"
                   WITH BACKGROUND-COLOR RED
               GO TO PANTALLA-RETIRADA
           END-IF.




       INSERTAR-MOVIMIENTO SECTION.

           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 30
              GO TO PSYS-ERR.

           SUBTRACT CENT-IMPOR-USER FROM CENT-SALDO-USER.
           COMPUTE SALDO-USUARIO-ENT = (CENT-SALDO-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-USER, 100)
               TO SALDO-USUARIO-DEC.

       ESCRITURA.
           ADD 1 TO LAST-MOV-NUM.

           MOVE LAST-MOV-NUM            TO MOV-NUM.
           MOVE TNUM                    TO MOV-TARJETA.
           MOVE ANO                     TO MOV-ANO.
           MOVE MES                     TO MOV-MES.
           MOVE DIA                     TO MOV-DIA.
           MOVE HORAS                   TO MOV-HOR.
           MOVE MINUTOS                 TO MOV-MIN.
           MOVE SEGUNDOS                TO MOV-SEG.

           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURENT-USUARIO          TO MOV-IMPORTE-ENT.

           MOVE EURDEC-USUARIO          TO MOV-IMPORTE-DEC.
           MOVE CON                     TO MOV-CONCEPTO.

           MOVE SALDO-USUARIO-ENT       TO MOV-SALDOPOS-ENT.
           MOVE SALDO-USUARIO-DEC       TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.
           CLOSE F-MOVIMIENTOS.



       FINALIZACION SECTION.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY(8 30) "Retirar efectivo".
           DISPLAY(10 19) "Por favor, retire los billetes".
           DISPLAY(11 17) "El saldo resultante es de:".

           DISPLAY SALDO-DISPLAY-FINAL.

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
