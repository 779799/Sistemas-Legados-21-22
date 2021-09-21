       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK3.

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

       78 BLACK                     VALUE    0.
       78 BLUE                      VALUE    1.
       78 GREEN                     VALUE    2.
       78 CYAN                      VALUE    3.
       78 RED                       VALUE    4.
       78 MAGENTA                   VALUE    5.
       78 YELLOW                    VALUE    6.
       78 WHITE                     VALUE    7.

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
           88 ENTER-PRESSED         VALUE    0.
           88 PGUP-PRESSED          VALUE 2001.
           88 PGDN-PRESSED          VALUE 2002.
           88 UP-ARROW-PRESSED      VALUE 2003.
           88 DOWN-ARROW-PRESSED    VALUE 2004.
           88 ESC-PRESSED           VALUE 2005.
       77 PRESSED-KEY               PIC   9(4).

       77 DIA1-USUARIO              PIC   9(2).
       77 MES1-USUARIO              PIC   9(2).
       77 ANO1-USUARIO              PIC   9(4).
       77 DIA2-USUARIO              PIC   9(2).
       77 MES2-USUARIO              PIC   9(2).
       77 ANO2-USUARIO              PIC   9(4).

       77 EURENT1-USUARIO           PIC  S9(7).
       77 EURDEC1-USUARIO           PIC   9(2).
       77 EURENT2-USUARIO           PIC  S9(7).
       77 EURDEC2-USUARIO           PIC   9(2).

       77 FECHA-MIN                 PIC   9(8).
       77 FECHA-MOV                 PIC   9(8).
       77 FECHA-MAX                 PIC   9(8).
       77 CENT-MIN                  PIC  S9(9).
       77 CENT-MOV                  PIC  S9(9).
       77 CENT-MAX                  PIC  S9(9).

       77 MOV-EN-PANTALLA           PIC   9(2).
       77 LINEA-MOV-ACTUAL          PIC   9(2).
       77 MOV-VALIDO                PIC   9(1).
       77 MODULO-LIN-ACTUAL         PIC   9(1).

       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.

       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-MOV                 PIC  9(35).

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 FILTRO-MOVIMIENTOS.
           05 DIA-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 37 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 40 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 43 PIC 9(4) USING ANO1-USUARIO.
           05 DIA-MAX BLANK ZERO BEEP AUTO UNDERLINE
               LINE 13 COL 50 PIC 9(2) USING DIA2-USUARIO.
           05 MES-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 53 PIC 9(2) USING MES2-USUARIO.
           05 ANO-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 56 PIC 9(4) USING ANO2-USUARIO.
           05 EUR-ENT-MIN BLANK ZERO AUTO UNDERLINE
               SIGN IS LEADING SEPARATE
               LINE 15 COL 30 PIC -9(7) USING EURENT1-USUARIO.
           05 EUR-DEC-MIN BLANK ZERO AUTO UNDERLINE
               LINE 15 COL 39 PIC 9(2) USING EURDEC1-USUARIO.
           05 EUR-ENT-MAX BLANK ZERO AUTO UNDERLINE
               SIGN IS LEADING SEPARATE
               LINE 15 COL 48 PIC -9(7) USING EURENT2-USUARIO.
           05 EUR-DEC-MAX BLANK ZERO UNDERLINE
               LINE 15 COL 57 PIC 9(2) USING EURDEC2-USUARIO.

       01 FILA-MOVIMIENTO-PAR.

           05 MOV-DIA-PAR LINE LINEA-MOV-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-MOV-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-MES-PAR LINE LINEA-MOV-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-MOV-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-ANO-PAR LINE LINEA-MOV-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM MOV-ANO.
           05 MOV-HOR-PAR LINE LINEA-MOV-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-HOR.
           05 SEPARADOR-PAR-3 LINE LINEA-MOV-ACTUAL COL 15
               FOREGROUND-COLOR YELLOW PIC A FROM ":".
           05 MOV-MIN-PAR LINE LINEA-MOV-ACTUAL COL 16
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-MIN.
           05 SEPARADOR-PAR-4 LINE LINEA-MOV-ACTUAL COL 18
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-CONCEPTO-PAR LINE LINEA-MOV-ACTUAL COL 19
               FOREGROUND-COLOR YELLOW PIC X(35) FROM MOV-CONCEPTO.
           05 SEPARADOR-5-PAR LINE LINEA-MOV-ACTUAL COL 54
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-IMPORTE-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 55
               FOREGROUND-COLOR YELLOW PIC S9(7) FROM MOV-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-MOV-ACTUAL COL 63
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 MOV-IMPORTE-DEC-PAR LINE LINEA-MOV-ACTUAL COL 64
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-IMPORTE-DEC.
           05 SEPARADOR-7-PAR LINE LINEA-MOV-ACTUAL COL 66
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-SALDOPOS-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 67
               FOREGROUND-COLOR YELLOW PIC S9(9)
               FROM MOV-SALDOPOS-ENT.
           05 SEPARADOR-8-PAR LINE LINEA-MOV-ACTUAL COL 77
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 MOV-SALDOPOS-DEC-PAR LINE LINEA-MOV-ACTUAL COL 78
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-SALDOPOS-DEC.

       01 FILA-MOVIMIENTO-IMPAR.
           05 MOV-DIA-IMPAR LINE LINEA-MOV-ACTUAL COL 02
               PIC 99 FROM MOV-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-MOV-ACTUAL COL 04
               PIC A FROM "-".
           05 MOV-MES-IMPAR LINE LINEA-MOV-ACTUAL COL 05
               PIC 99 FROM MOV-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-MOV-ACTUAL COL 07
               PIC A FROM "-".
           05 MOV-ANO-IMPAR LINE LINEA-MOV-ACTUAL COL 08
               PIC 9(4) FROM MOV-ANO.
           05 MOV-HOR-IMPAR LINE LINEA-MOV-ACTUAL COL 13
               PIC 99 FROM MOV-HOR.
           05 SEPARADOR-IMPAR-3 LINE LINEA-MOV-ACTUAL COL 15
               PIC A FROM ":".
           05 MOV-MIN-IMPAR LINE LINEA-MOV-ACTUAL COL 16
               PIC 99 FROM MOV-MIN.
           05 SEPARADOR-IMPAR-4 LINE LINEA-MOV-ACTUAL COL 18
               PIC A FROM "|".
           05 MOV-CONCEPTO-IMPAR LINE LINEA-MOV-ACTUAL COL 19
               PIC X(35) FROM MOV-CONCEPTO.
           05 SEPARADOR-5-IMPAR LINE LINEA-MOV-ACTUAL COL 54
               PIC A FROM "|".
           05 MOV-IMPORTE-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 55
               PIC S9(7) FROM MOV-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-MOV-ACTUAL COL 63
               PIC A FROM ",".
           05 MOV-IMPORTE-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 64
               PIC 99 FROM MOV-IMPORTE-DEC.
           05 SEPARADOR-7-IMPAR LINE LINEA-MOV-ACTUAL COL 66
               PIC A FROM "|".
           05 MOV-SALDOPOS-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 67
               PIC S9(9) FROM MOV-SALDOPOS-ENT.
           05 SEPARADOR-8-IMPAR LINE LINEA-MOV-ACTUAL COL 77
               PIC A FROM ",".
           05 MOV-SALDOPOS-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 78
               PIC 99 FROM MOV-SALDOPOS-DEC.


       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

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

       PCONSULTA-MOV.

           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE DIA2-USUARIO.
           INITIALIZE MES2-USUARIO.
           INITIALIZE ANO2-USUARIO.

           INITIALIZE EURENT1-USUARIO.
           INITIALIZE EURDEC1-USUARIO.
           INITIALIZE EURENT2-USUARIO.
           INITIALIZE EURDEC2-USUARIO.

           DISPLAY(8 8) "Se  mostraran los ultimos movimientos,".
           DISPLAY(8 47) "de mas a menos recientes.".

           DISPLAY(10 8) "Alternativamente, indique un intervalo".
           DISPLAY(10 47) "de fechas y/o cantidades.".

           DISPLAY(13 20) "Entre las fechas   /  /     y   /  /    ".
           DISPLAY(15 15) "Cantidad entre         .   EUR y         .   EUR".

           DISPLAY(24 01) "Enter - Aceptar".
           DISPLAY(24 65) "ESC - Cancelar".

           ACCEPT FILTRO-MOVIMIENTOS ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO PCONSULTA-MOV.

           IF DIA2-USUARIO = 0
               IF MES2-USUARIO = 0
                   IF ANO2-USUARIO = 0
                       MOVE 99   TO DIA2-USUARIO
                       MOVE 99   TO MES2-USUARIO
                       MOVE 9999 TO ANO2-USUARIO.

           IF EURENT2-USUARIO = 0
               IF EURDEC2-USUARIO = 0
                   IF EURENT1-USUARIO = 0
                       IF EURDEC1-USUARIO = 0
                           MOVE 9999999  TO EURENT2-USUARIO
                           MOVE 99       TO EURDEC2-USUARIO
                           MOVE -9999999  TO EURENT1-USUARIO
                           MOVE 99        TO EURDEC1-USUARIO.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           OPEN INPUT F-MOVIMIENTOS.
               IF FSM <> 30
                   GO TO PSYS-ERR.

       POSICIONAR-FINAL.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO PLECTURA-MOV.
               GO TO POSICIONAR-FINAL.

       PLECTURA-MOV.
           DISPLAY(7 8) "FECHA".
           DISPLAY(7 18) "|".
           DISPLAY(7 35) "CONCEPTO".
           DISPLAY(7 54) "|".
           DISPLAY(7 57) "IMPORTE".
           DISPLAY(7 66) "|".
           DISPLAY(7 71) "SALDO".

           DISPLAY(24 2) "Re. pag - Esp. anteriores".
           DISPLAY(24 33) "ESC - Salir".
           DISPLAY(24 54) "Av. pag - Esp. posteriores".

           MOVE 0 TO MOV-EN-PANTALLA.
           MOVE 7 TO LINEA-MOV-ACTUAL.


       LEER-PRIMEROS.
           READ F-MOVIMIENTOS PREVIOUS RECORD AT END GO WAIT-ORDER.
               MOVE 1 TO MOV-VALIDO.

               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   ADD 1 TO LINEA-MOV-ACTUAL
                   ADD 1 TO MOV-EN-PANTALLA
                   MOVE MOV-NUM TO
                       REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA)
                   MOVE 0 TO MOV-VALIDO
                   PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO.

               IF MOV-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

               GO TO LEER-PRIMEROS.

       WAIT-ORDER.

           ACCEPT(24 80) PRESSED-KEY ON EXCEPTION

              IF ESC-PRESSED THEN
                  CLOSE F-MOVIMIENTOS
                  EXIT PROGRAM
              END-IF

              IF PGDN-PRESSED THEN
                  GO TO FLECHA-ABAJO
              END-IF

              IF PGUP-PRESSED THEN
                  GO TO FLECHA-ARRIBA
              END-IF

           END-ACCEPT.

           GO TO WAIT-ORDER.

       FLECHA-ABAJO.
           MOVE REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA) TO MOV-NUM.
           READ F-MOVIMIENTOS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-VIEJO.

       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO MOV-NUM.
           READ F-MOVIMIENTOS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-NUEVO.

       LEER-VIEJO.
           READ F-MOVIMIENTOS PREVIOUS RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO MOV-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   MOVE 2 TO MOV-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-VIEJO.

       LEER-NUEVO.
           READ F-MOVIMIENTOS NEXT RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO MOV-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF MOV-VALIDO = 1
                   MOVE 3 TO MOV-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-NUEVO.

       CONTROL-PANTALLA.
           IF MOV-VALIDO = 2 THEN
               MOVE 0 TO MOV-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF MOV-VALIDO = 3 THEN
                   MOVE 0 TO MOV-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.

       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.

           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.

           MOVE MOV-NUM TO REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       REORDENAR-2.
           MOVE MOV-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.


           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               ADD 1 TO CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.

           MOVE MOV-NUM TO REGISTROS-EN-PANTALLA(1).

           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 8 TO LINEA-MOV-ACTUAL.
           MOVE 1 TO CONTADOR.

           PERFORM MOV-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO MOV-NUM
               PERFORM READ-MOVIMIENTO THRU READ-MOVIMIENTO
               PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO
               ADD 1 TO LINEA-MOV-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.

       READ-MOVIMIENTO.
           READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR.

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


       FILTRADO.

           IF TNUM NOT = MOV-TARJETA
               MOVE 0 TO MOV-VALIDO.

           COMPUTE FECHA-MIN = (ANO1-USUARIO * 10000)
                               + (MES1-USUARIO * 100)
                               + DIA1-USUARIO.

           COMPUTE FECHA-MOV = (MOV-ANO * 10000)
                               + (MOV-MES * 100)
                               + MOV-DIA.

           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.

           IF FECHA-MIN > FECHA-MOV
               MOVE 0 TO MOV-VALIDO.
           IF FECHA-MAX < FECHA-MOV
               MOVE 0 TO MOV-VALIDO.

           COMPUTE CENT-MIN = (EURENT1-USUARIO * 100)
                              + (EURDEC1-USUARIO).

           COMPUTE CENT-MOV = (MOV-IMPORTE-ENT * 100)
                              + (MOV-IMPORTE-DEC).

           COMPUTE CENT-MAX = (EURENT2-USUARIO * 100)
                              + (EURDEC2-USUARIO).

           IF CENT-MIN > CENT-MOV
               MOVE 0 TO MOV-VALIDO.
           IF CENT-MAX < CENT-MOV
               MOVE 0 TO MOV-VALIDO.


       MOSTRAR-MOVIMIENTO.

           MOVE FUNCTION MOD(LINEA-MOV-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.

           IF MODULO-LIN-ACTUAL = 0
               DISPLAY FILA-MOVIMIENTO-PAR
           ELSE
               DISPLAY FILA-MOVIMIENTO-IMPAR.
