       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK5.

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

       01 KEYBOARD-STATUS           PIC   9(4).
           88 ENTER-PRESSED       VALUE      0.
           88 PGUP-PRESSED        VALUE   2001.
           88 PGDN-PRESSED        VALUE   2002.
           88 UP-ARROW-PRESSED    VALUE   2003.
           88 DOWN-ARROW-PRESSED  VALUE   2004.
           88 ESC-PRESSED         VALUE   2005.

       77 LAST-USER-MOV-NUM        PIC   9(35).
       77 LAST-MOV-NUM             PIC   9(35).

       77 EURENT-USUARIO           PIC    9(7).
       77 BILL10-USUARIO           PIC    9(5).
       77 BILL20-USUARIO           PIC    9(5).
       77 BILL50-USUARIO           PIC    9(5).
       77 SALDO-USUARIO-ENT        PIC   S9(9).
       77 SALDO-USUARIO-DEC        PIC    9(2).
       77 CENT-SALDO-USER          PIC  S9(11).
       77 CENT-IMPOR-USER          PIC    9(9).
       77 CENT-ACUMULADOR          PIC   9(11).

       77 CON                      PIC   X(35) VALUE "Ingreso".
       77 PRESSED-KEY              PIC    9(4).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).



       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.


       01 ENTRADA-USUARIO.
           05 FILLER BLANK ZERO AUTO UNDERLINE
               LINE 14 COL 41 PIC 9(5) USING BILL10-USUARIO.
           05 FILLER BLANK ZERO AUTO UNDERLINE
               LINE 15 COL 41 PIC 9(5) USING BILL20-USUARIO.
           05 FILLER BLANK ZERO AUTO UNDERLINE
               LINE 16 COL 41 PIC 9(5) USING BILL50-USUARIO.

       01 SALDO-DISPLAY.
           05 FILLER SIGN IS LEADING SEPARATE
               LINE 10 COL 33 PIC -9(7) FROM SALDO-USUARIO-ENT.
           05 FILLER LINE 10 COL 41 VALUE ".".
           05 FILLER LINE 10 COL 42 PIC 99 FROM SALDO-USUARIO-DEC.
           05 FILLER LINE 10 COL 45 VALUE "EUR".

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
           DISPLAY "Cajero Automatico UnizarBank" LINE 2 COLUMN 26
               WITH FOREGROUND-COLOR IS 1.


           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA LINE 4 COLUMN 32.
           DISPLAY "-" LINE 4 COLUMN 34.
           DISPLAY MES LINE 4 COLUMN 35.
           DISPLAY "-" LINE 4 COLUMN 37.
           DISPLAY ANO LINE 4 COLUMN 38.
           DISPLAY HORAS LINE 4 COLUMN 44.
           DISPLAY ":" LINE 4 COLUMN 46.
           DISPLAY MINUTOS LINE 4 COLUMN 47.




       CONSULTA-ULTIMO-MOVIMIENTO SECTION.

           INITIALIZE CENT-ACUMULADOR.

           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00
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
           IF FSM <> 00
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
               GO TO PANTALLA-INGRESO
           END-IF.

           MOVE LAST-USER-MOV-NUM TO MOV-NUM.

           OPEN INPUT F-MOVIMIENTOS.
           IF FSM <> 00
               GO TO PSYS-ERR.

           READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR.

           MOVE MOV-SALDOPOS-ENT TO SALDO-USUARIO-ENT.
           MOVE MOV-SALDOPOS-DEC TO SALDO-USUARIO-DEC.
           COMPUTE CENT-SALDO-USER = (SALDO-USUARIO-ENT * 100)
                                     + SALDO-USUARIO-DEC.

           CLOSE F-MOVIMIENTOS.




       PANTALLA-INGRESO SECTION.
           INITIALIZE BILL10-USUARIO.
           INITIALIZE BILL20-USUARIO.
           INITIALIZE BILL50-USUARIO.
           INITIALIZE EURENT-USUARIO.
           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 1.
           DISPLAY "ESC - Cancelar" LINE 24 COLUMN 66.
           DISPLAY "Ingresar efectivo" LINE 8 COLUMN 30.
           DISPLAY "Saldo Actual: " LINE 10 COLUMN 19.

           DISPLAY SALDO-DISPLAY.

           DISPLAY "Por favor, introduzca billetes" LINE 11 COLUMN 19.
           DISPLAY "Cantidad de billetes introducidas:" 
               LINE 13 COLUMN 19.
           DISPLAY "Billetes de 10 EUR:         " LINE 14 COLUMN 21.
           DISPLAY "Billetes de 20 EUR:         " LINE 15 COLUMN 21.
           DISPLAY "Billetes de 50 EUR:         " LINE 16 COLUMN 21.
       CONF2.
           ACCEPT ENTRADA-USUARIO ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO PANT
           END-IF.

           COMPUTE CENT-IMPOR-USER = (BILL10-USUARIO * 1000) 
               + (BILL20-USUARIO * 2000) + (BILL50-USUARIO * 5000).

           




       INSERTAR-MOVIMIENTO SECTION.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00
              GO TO PSYS-ERR.

           ADD CENT-IMPOR-USER TO CENT-SALDO-USER
               ON SIZE ERROR GO TO PSYS-ERR.
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

           MOVE CENT-IMPOR-USER          TO MOV-IMPORTE-ENT.

           MOVE CON                     TO MOV-CONCEPTO.

           MOVE SALDO-USUARIO-ENT       TO MOV-SALDOPOS-ENT.
           MOVE SALDO-USUARIO-DEC       TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.
           CLOSE F-MOVIMIENTOS.




       PANT SECTION.

           COMPUTE EURENT-USUARIO = (CENT-IMPOR-USER / 100).

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ingresar efectivo" LINE 8 COLUMN 30.
           DISPLAY "Se han recibido correctamente:" LINE 10 COLUMN 19.
           DISPLAY EURENT-USUARIO LINE 10 COLUMN 50.
           DISPLAY "EUR" LINE 10 COLUMN 58.
           DISPLAY "El saldo resultante es de:" LINE 11 COLUMN 19.

           DISPLAY SALDO-DISPLAY-FINAL.


           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 33.


           GO TO EXIT-ENTER.

       PSYS-ERR.

           CLOSE F-MOVIMIENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 9 COLUMN 25
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COLUMN 32
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 33.

       EXIT-ENTER.
           ACCEPT PRESSED-KEY LINE 24 COLUMN 80
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.
