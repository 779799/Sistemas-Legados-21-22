       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK9.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL F-TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TRF-NUM
           FILE STATUS IS FSTRF.


       DATA DIVISION.
       FILE SECTION.
       FD F-TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "transferencias.ubd".
       01 TRANSFERENCIA-REG. 
           02 TRF-NUM              PIC  9(35).
           02 TRF-TARJETA          PIC  9(16).
           02 TRF-ANO              PIC   9(4).
           02 TRF-MES              PIC   9(2).
           02 TRF-DIA              PIC   9(2).
           02 TRF-HOR              PIC   9(2).
           02 TRF-MIN              PIC   9(2).
           02 TRF-SEG              PIC   9(2).
           02 TRF-CONCEPTO         PIC  X(18).
           02 TRF-IMPORTE-ENT      PIC  S9(7).
           02 TRF-IMPORTE-DEC      PIC   9(2).


       WORKING-STORAGE SECTION.
       77 FSTRF                     PIC   X(2).

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

       77 FECHA-MIN                 PIC   9(8).
       77 FECHA-TRF                 PIC   9(8).
       77 FECHA-MAX                 PIC   9(8).

       77 TRF-EN-PANTALLA           PIC   9(2).
       77 LINEA-TRF-ACTUAL          PIC   9(2).
       77 TRF-VALIDO                PIC   9(1).
       77 MODULO-LIN-ACTUAL         PIC   9(1).

       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.

       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-TRF                 PIC  9(35).

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 FILTRO-TRANSFERENCIAS.
           05 DIA-MIN BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 37 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 40 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 43 PIC 9(4) USING ANO1-USUARIO.
           05 DIA-MAX BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 50 PIC 9(2) USING DIA2-USUARIO.
           05 MES-MAX BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 53 PIC 9(2) USING MES2-USUARIO.
           05 ANO-MAX BLANK ZERO AUTO FOREGROUND-COLOR CYAN
               LINE 13 COL 56 PIC 9(4) USING ANO2-USUARIO.

       01 FILA-TRANSFERENCIA-PAR.

           05 TRF-DIA-PAR LINE LINEA-TRF-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-TRF-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 TRF-MES-PAR LINE LINEA-TRF-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-TRF-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 TRF-ANO-PAR LINE LINEA-TRF-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM TRF-ANO.
           05 TRF-HOR-PAR LINE LINEA-TRF-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-HOR.
           05 SEPARADOR-PAR-3 LINE LINEA-TRF-ACTUAL COL 15
               FOREGROUND-COLOR YELLOW PIC A FROM ":".
           05 TRF-MIN-PAR LINE LINEA-TRF-ACTUAL COL 16
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-MIN.
           05 SEPARADOR-PAR-4 LINE LINEA-TRF-ACTUAL COL 18
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRF-CONCEPTO-PAR LINE LINEA-TRF-ACTUAL COL 19
               FOREGROUND-COLOR YELLOW PIC X(35) FROM TRF-CONCEPTO.
           05 SEPARADOR-5-PAR LINE LINEA-TRF-ACTUAL COL 66
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRF-IMPORTE-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-TRF-ACTUAL COL 67
               FOREGROUND-COLOR YELLOW PIC -9(7) FROM TRF-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-TRF-ACTUAL COL 75
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 TRF-IMPORTE-DEC-PAR LINE LINEA-TRF-ACTUAL COL 76
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-IMPORTE-DEC.

       01 FILA-TRANSFERENCIA-IMPAR.
           05 TRF-DIA-IMPAR LINE LINEA-TRF-ACTUAL COL 02
               PIC 99 FROM TRF-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-TRF-ACTUAL COL 04
               PIC A FROM "-".
           05 TRF-MES-IMPAR LINE LINEA-TRF-ACTUAL COL 05
               PIC 99 FROM TRF-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-TRF-ACTUAL COL 07
               PIC A FROM "-".
           05 TRF-ANO-IMPAR LINE LINEA-TRF-ACTUAL COL 08
               PIC 9(4) FROM TRF-ANO.
           05 TRF-HOR-IMPAR LINE LINEA-TRF-ACTUAL COL 13
               PIC 99 FROM TRF-HOR.
           05 SEPARADOR-IMPAR-3 LINE LINEA-TRF-ACTUAL COL 15
               PIC A FROM ":".
           05 TRF-MIN-IMPAR LINE LINEA-TRF-ACTUAL COL 16
               PIC 99 FROM TRF-MIN.
           05 SEPARADOR-IMPAR-4 LINE LINEA-TRF-ACTUAL COL 18
               PIC A FROM "|".
           05 TRF-CONCEPTO-IMPAR LINE LINEA-TRF-ACTUAL COL 19
               PIC X(35) FROM TRF-CONCEPTO.
           05 SEPARADOR-5-IMPAR LINE LINEA-TRF-ACTUAL COL 66
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRF-IMPORTE-ENT-IMPAR SIGN IS LEADING SEPARATE
               LINE LINEA-TRF-ACTUAL COL 67
               FOREGROUND-COLOR YELLOW PIC -9(7) FROM TRF-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-TRF-ACTUAL COL 75
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 TRF-IMPORTE-DEC-IMPAR LINE LINEA-TRF-ACTUAL COL 76
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRF-IMPORTE-DEC.


       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

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

       PCONSULTA-TRF.

           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE DIA2-USUARIO.
           INITIALIZE MES2-USUARIO.
           INITIALIZE ANO2-USUARIO.

           DISPLAY "Se  mostraran las ultimas transferencias," LINE 8
               COLUMN 8.
           DISPLAY "de mas a menos recientes." LINE 8 COLUMN 47.

           DISPLAY "Alternativamente, indique un intervalo" LINE 10
               COLUMN 8.
           DISPLAY "de fechas." LINE 10 COLUMN 47.

           DISPLAY "Entre las fechas   /  /     y   /  /    " LINE 13
               COLUMN 20.


           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 01.
           DISPLAY "ESC - Cancelar" LINE 24 COLUMN 65.

           ACCEPT FILTRO-TRANSFERENCIAS ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO PCONSULTA-TRF.

           IF DIA2-USUARIO = 0
               IF MES2-USUARIO = 0
                   IF ANO2-USUARIO = 0
                       MOVE 99   TO DIA2-USUARIO
                       MOVE 99   TO MES2-USUARIO
                       MOVE 9999 TO ANO2-USUARIO.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           OPEN I-O F-TRANSFERENCIAS.
               IF FSTRF <> 00 AND 05
                   GO TO PSYS-ERR.

       POSICIONAR-FINAL.
           READ F-TRANSFERENCIAS NEXT RECORD AT END GO PLECTURA-TRF.
               GO TO POSICIONAR-FINAL.

       PLECTURA-TRF.
           DISPLAY "FECHA" LINE 7 COLUMN 8.
           DISPLAY "|" LINE 7 COLUMN 18.
           DISPLAY "CONCEPTO" LINE 7 COLUMN 35.
           DISPLAY "|" LINE 7 COLUMN 66.
           DISPLAY "IMPORTE" LINE 7 COLUMN 69.

           DISPLAY "Re. pag - Esp. anteriores" LINE 24 COLUMN 2.
           DISPLAY "ESC - Salir" LINE 24 COLUMN 33.
           DISPLAY "Av. pag - Esp. posteriores" LINE 24 COLUMN 54.

           MOVE 0 TO TRF-EN-PANTALLA.
           MOVE 7 TO LINEA-TRF-ACTUAL.


       LEER-PRIMEROS.
           READ F-TRANSFERENCIAS PREVIOUS RECORD AT END GO WAIT-ORDER.
               MOVE 1 TO TRF-VALIDO.

               PERFORM FILTRADO THRU FILTRADO.

               IF TRF-VALIDO = 1
                   ADD 1 TO LINEA-TRF-ACTUAL
                   ADD 1 TO TRF-EN-PANTALLA
                   MOVE TRF-NUM TO
                       REGISTROS-EN-PANTALLA(TRF-EN-PANTALLA)
                   MOVE 0 TO TRF-VALIDO
                   PERFORM MOSTRAR-TRANSFERENCIA 
                       THRU MOSTRAR-TRANSFERENCIA.

               IF TRF-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

               GO TO LEER-PRIMEROS.

       WAIT-ORDER.

           ACCEPT PRESSED-KEY LINE 24 COLUMN 80 ON EXCEPTION

              IF ESC-PRESSED THEN
                  CLOSE F-TRANSFERENCIAS
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
           MOVE REGISTROS-EN-PANTALLA(TRF-EN-PANTALLA) TO TRF-NUM.
           READ F-TRANSFERENCIAS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-VIEJO.

       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO TRF-NUM.
           READ F-TRANSFERENCIAS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-NUEVO.

       LEER-VIEJO.
           READ F-TRANSFERENCIAS PREVIOUS RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO TRF-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF TRF-VALIDO = 1
                   MOVE 2 TO TRF-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-VIEJO.

       LEER-NUEVO.
           READ F-TRANSFERENCIAS NEXT RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO TRF-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF TRF-VALIDO = 1
                   MOVE 3 TO TRF-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-NUEVO.

       CONTROL-PANTALLA.
           IF TRF-VALIDO = 2 THEN
               MOVE 0 TO TRF-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF TRF-VALIDO = 3 THEN
                   MOVE 0 TO TRF-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.

       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE TRF-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.

           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-TRF
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-TRF TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.

           MOVE TRF-NUM TO REGISTROS-EN-PANTALLA(TRF-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       REORDENAR-2.
           MOVE TRF-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE TRF-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.


           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-TRF
               ADD 1 TO CONTADOR
               MOVE COPIA-TRF TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.

           MOVE TRF-NUM TO REGISTROS-EN-PANTALLA(1).

           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 8 TO LINEA-TRF-ACTUAL.
           MOVE 1 TO CONTADOR.

           PERFORM TRF-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO TRF-NUM
               PERFORM READ-TRANSFERENCIA THRU READ-TRANSFERENCIA
               PERFORM MOSTRAR-TRANSFERENCIA THRU MOSTRAR-TRANSFERENCIA
               ADD 1 TO LINEA-TRF-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.

       READ-TRANSFERENCIA.
           READ F-TRANSFERENCIAS INVALID KEY GO TO PSYS-ERR.

       PSYS-ERR.

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


       FILTRADO.

           IF TNUM NOT = TRF-TARJETA
               MOVE 0 TO TRF-VALIDO.

           COMPUTE FECHA-MIN = (ANO1-USUARIO * 10000)
                               + (MES1-USUARIO * 100)
                               + DIA1-USUARIO.

           COMPUTE FECHA-TRF = (TRF-ANO * 10000)
                               + (TRF-MES * 100)
                               + TRF-DIA.

           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.

           IF FECHA-MIN > FECHA-TRF
               MOVE 0 TO TRF-VALIDO.
           IF FECHA-MAX < FECHA-TRF
               MOVE 0 TO TRF-VALIDO.

       MOSTRAR-TRANSFERENCIA.

           MOVE FUNCTION MOD(LINEA-TRF-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.

           IF MODULO-LIN-ACTUAL = 0
               DISPLAY FILA-TRANSFERENCIA-PAR
           ELSE
               DISPLAY FILA-TRANSFERENCIA-IMPAR.
