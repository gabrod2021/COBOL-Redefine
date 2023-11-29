      ******************************************************************
      * Author: GABRIELA RODRIGUEZ
      * Date: 03/10/2023
      * Purpose: REDEFINES
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL17EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ARCH-ENT-DATOS
           ASSIGN TO '../DATOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-DATOS.

       SELECT ARCH-SAL-RESULTADO
           ASSIGN TO '../RESULTADO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-RESULTADO.

       SELECT ARCH-SAL-ERROR
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERROR.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ARCH-ENT-DATOS.
       01 ENT-PRODUCTOS.
          05 ENT-TIPO-REGISTRO              PIC X(01).
          05 ENT-ID-PRODUCTO                PIC 9(04).
          05 ENT-DATOS                      PIC X(20).
          05 ENT-REG-A                      REDEFINES ENT-DATOS.
             10 ENT-REG-A-PRODUCTO          PIC X(20).
          05 ENT-REG-B                      REDEFINES ENT-DATOS.
             10 ENT-REG-B-CATEGORIA         PIC X(20).
          05 ENT-REG-C                      REDEFINES ENT-DATOS.
             10 ENT-REG-C-IMP-SIN-IVA       PIC 9(05)V9(02).
             10 ENT-REG-C-IMp-CON-IVA       PIC 9(05)V9(02).
             10 FILLER                      PIC X(06).

       FD ARCH-SAL-RESULTADO.
       01 SAL-RESULTADO.
          05 SAL-ID-PRODUCTO                PIC 9(04).
          05 SAL-PRODUCTO                   PIC X(20).
          05 SAL-CATEGORIA                  PIC X(20).
          05 SAL-IMPORTE-SIN-IVA            PIC $ZZ.ZZ9,99.
          05 SAL-IMPORTE-CON-IVA            PIC $ZZ.ZZ9,99.

       FD ARCH-SAL-ERROR.
       01 SAL-ERROR                         PIC X(21).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-DATOS                       PIC X(02).
             88 FS-DATOS-OK                     VALUE '00'.
             88 FS-DATOS-EOF                    VALUE '10'.
             88 FS-DATOS-NFD                    VALUE '35'.
          05 FS-RESULTADO                   PIC X(02).
             88 FS-RESULTADO-OK                 VALUE '00'.
             88 FS-RESULTADO-EOF                VALUE '10'.
          05 FS-ERROR                       PIC X(02).
             88 FS-ERROR-OK                     VALUE '00'.
             88 FS-ERROR-EOF                    VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-DATOS              PIC 9(04) VALUE 0.
          05 WS-CONT-REG-RESULTADO          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

       01 WS-ENTRADA-VALIDA                 PIC X(01).
          88 WS-ENTRADA-VALIDA-SI               VALUE 'S'.
          88 WS-ENTRADA-VALIDA-NO               VALUE 'N'.

       77 WS-ID-PRODUCTO-ANT                PIC 9(04) VALUES ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-DATOS-OK AND FS-RESULTADO-OK AND FS-ERROR-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-DATOS-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-DATOS
              THRU 1100-ABRIR-DATOS-FIN.

           PERFORM 1200-ABRIR-RESULTADO
              THRU 1200-ABRIR-RESULTADO-FIN.

           PERFORM 1300-ABRIR-ERROR
              THRU 1300-ABRIR-ERROR-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-DATOS.

           OPEN INPUT ARCH-ENT-DATOS.

           EVALUATE TRUE
               WHEN FS-DATOS-OK
                    PERFORM 1110-LEER-DATOS
                       THRU 1110-LEER-DATOS-FIN
               WHEN FS-DATOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DATOS'
                    DISPLAY 'FILE STATUS: ' FS-DATOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DATOS'
                    DISPLAY 'FILE STATUS: ' FS-DATOS
           END-EVALUATE.

       1100-ABRIR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-DATOS.

           READ ARCH-ENT-DATOS.

           EVALUATE TRUE
               WHEN FS-DATOS-OK
                    ADD 1                   TO WS-CONT-REG-DATOS
               WHEN FS-DATOS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE DATOS'
                    DISPLAY 'FILE STATUS: ' FS-DATOS

           END-EVALUATE.

       1110-LEER-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-RESULTADO.

           OPEN OUTPUT ARCH-SAL-RESULTADO.

           EVALUATE TRUE
               WHEN FS-RESULTADO-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE RESULTADO'
                    DISPLAY 'FILE STATUS: ' FS-RESULTADO
           END-EVALUATE.

       1200-ABRIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1300-ABRIR-ERROR.

           OPEN OUTPUT ARCH-SAL-ERROR.

           EVALUATE TRUE
               WHEN FS-ERROR-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERROR
           END-EVALUATE.

       1300-ABRIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           INITIALIZE SAL-RESULTADO.

           MOVE ENT-ID-PRODUCTO             TO WS-ID-PRODUCTO-ANT

           MOVE ENT-ID-PRODUCTO             TO SAL-ID-PRODUCTO.

           PERFORM UNTIL ENT-ID-PRODUCTO NOT EQUAL WS-ID-PRODUCTO-ANT
                      OR FS-DATOS-EOF

              EVALUATE ENT-TIPO-REGISTRO
                  WHEN "A"
                       MOVE ENT-REG-A-PRODUCTO    TO SAL-PRODUCTO
                  WHEN "B"
                       MOVE ENT-REG-B-CATEGORIA   TO SAL-CATEGORIA
                  WHEN "C"
                       MOVE ENT-REG-C-IMP-SIN-IVA TO SAL-IMPORTE-SIN-IVA
                       MOVE ENT-REG-C-IMP-CON-IVA TO SAL-IMPORTE-CON-IVA
                 WHEN OTHER
                       DISPLAY "TIPO DE REGISTRO INVALIDO: "

                       PERFORM 2300-MOVER-ERROR
                       THRU 2300-MOVER-ERROR-FIN


              END-EVALUATE

              PERFORM 1110-LEER-DATOS
                 THRU 1110-LEER-DATOS-FIN


           END-PERFORM.

           PERFORM 2210-ESCRIBIR-RESULTADO
              THRU 2210-ESCRIBIR-RESULTADO-FIN.



       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2210-ESCRIBIR-RESULTADO.

           WRITE SAL-RESULTADO.

           IF FS-RESULTADO-OK
              ADD 1                         TO  WS-CONT-REG-RESULTADO
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR RESULTADO.TXT: ' FS-RESULTADO
           END-IF.

       2210-ESCRIBIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2300-MOVER-ERROR.

           MOVE ENT-PRODUCTOS TO SAL-ERROR.
           PERFORM 2310-ESCRIBIR-ERROR
              THRU 2310-ESCRIBIR-ERROR-FIN.

       2300-MOVER-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2310-ESCRIBIR-ERROR.


           WRITE SAL-ERROR.


           IF FS-ERROR-OK
              ADD 1                         TO  WS-CONT-REG-ERROR
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR ERROR.TXT: ' FS-ERROR
           END-IF.

       2310-ESCRIBIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY 'CANTIDAD DE REGISTROS DATOS       : '
                   WS-CONT-REG-DATOS.
           DISPLAY 'CANTIDAD DE REGISTROS RESULTADO   : '
                   WS-CONT-REG-RESULTADO.
           DISPLAY 'CANTIDAD DE REGISTROS CON ERROR   : '
                   WS-CONT-REG-ERROR.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ARCH-ENT-DATOS
                 ARCH-SAL-RESULTADO
                 ARCH-SAL-ERROR.

           IF NOT FS-DATOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS: ' FS-DATOS
           END-IF.

           IF NOT FS-RESULTADO-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO RESULTADO: ' FS-RESULTADO
           END-IF.

           IF NOT FS-ERROR-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ERROR: ' FS-ERROR
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL17EJ02.
