       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK8.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM-C
           FILE STATUS IS FST.


      
       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM-C      PIC 9(16).
           02 TPIN-C      PIC  9(4).
       
       WORKING-STORAGE SECTION.
       77 FST                       PIC   X(2).
      
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
           88 ESC-PRESSED         VALUE  2005.
      
       77 LAST-MOV-NUM             PIC  9(35).
       77 PRESSED-KEY              PIC   9(4).
       77 TACT                     PIC   9(4).
       77 PIN-NUEVO                PIC   9(4).
       77 PIN-NUEVO-REP            PIC   9(4).
       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.
       01 DATA-ACCEPT.
           05  BLANK ZERO SECURE LINE 11 COL 50
               PIC 9(4) USING TACT.
           05  BLANK ZERO SECURE LINE 12 COL 50
               PIC 9(4) USING PIN-NUEVO.
           05  BLANK ZERO SECURE LINE 13 COL 50
               PIC 9(4) USING PIN-NUEVO-REP.

       
       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
          
           INITIALIZE TACT.
           INITIALIZE PIN-NUEVO.
           INITIALIZE PIN-NUEVO-REP.
          
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

       P1.
       
           DISPLAY "Enter - Aceptar" LINE 24 COLUMN 1.
           DISPLAY "ESC - Cancelar" LINE 24 COLUMN 66.
           DISPLAY "Cambiar clave" LINE 8 COLUMN 30.
           DISPLAY "Introduzca clave actual:" LINE 11 COLUMN 15.
           DISPLAY "Introduzca clave actual:" LINE 12 COLUMN 15.
           DISPLAY "Repita nueva clave:" LINE 13 COLUMN 15.
       
    
       ENTER-VERIFICACION.
           ACCEPT DATA-ACCEPT ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO ENTER-VERIFICACION
           END-IF.  

       VERIFICACION-CTA-CORRECTA.
           OPEN I-O TARJETAS.
           IF FST <> 00 
              GO TO PSYS-ERR.

       IF PIN-NUEVO NOT = PIN-NUEVO-REP THEN
              GO TO PIN-NOT-EQUAL.


           MOVE TNUM TO TNUM-C.
           READ TARJETAS INVALID KEY GO TO PSYS-ERR.
           
           IF TACT NOT = TPIN-C THEN 
              GO TO USER-BAD.

           MOVE PIN-NUEVO TO TPIN-C.
           REWRITE TAJETAREG INVALID KEY GO TO PSYS-ERR.
           CLOSE TARJETAS.   
           
       PANTALLA-CLAVE-ACTUALIZADO.    
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Enter - Salir" LINE 24 COLUMN 33.
           DISPLAY "Cambiar clave" LINE 8 COLUMN 30.
           DISPLAY "La clave se ha actualizado" LINE 11 COL 20.
           GO TO EXIT-ENTER.
       
       USER-BAD.
           CLOSE TARJETAS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "El pin actual es incorrecto" LINE 9 COLUMN 22
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Salir" LINE 24 COLUMN 33.
           GO TO EXIT-ENTER.     

       PIN-NOT-EQUAL.
           CLOSE TARJETAS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La nueva clave no coincide." LINE 9 COLUMN 22
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Salir" LINE 24 COLUMN 33.
           GO TO EXIT-ENTER.     





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

    