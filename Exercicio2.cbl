      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exercicio2.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRA002
           ASSIGN TO 'D:\ENTRA002.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SAIDA002
           ASSIGN TO 'D:\SAIDA002.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SAIDA003
           ASSIGN TO 'D:\SAIDA003.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ENTRA002.
       01  ENT02-REGISTRO                 PIC X(94).

       FD  SAIDA002.
       01  SAI02-REGISTRO                 PIC X(94).

       FD  SAIDA003.
       01  SAI03-REGISTRO                 PIC X(94).

       WORKING-STORAGE SECTION.
       01  WS-ARQ-ENTRA002.
           03  ARQ01-COD-CLIENTE          PIC 9(10).
           03  ARQ01-NOME-CLIENTE         PIC X(30).
           03  ARQ01-COD-MUNIC            PIC 9(06).
           03  ARQ01-DESC-MUNIC           PIC X(20).
           03  ARQ01-VAL-A-TRIB           PIC 9(13)V99.
           03  ARQ01-DAT-NOTA-SERV        PIC 9(08).


       01  WS-FIM-ENTRA002                PIC X(01)  VALUE 'N'.

       01  WS-QT-LIDOS-ENTRA002           PIC 9(10)  VALUE ZEROS.
       01  WS-QT-GRAVS-SAIDA002           PIC 9(10)  VALUE ZEROS.
       01  WS-QT-GRAVS-SAIDA003           PIC 9(10)  VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       100-00-PRINCIPAL  SECTION.

           PERFORM 200-00-INICIALIZA.

           PERFORM 500-00-PROCESSA
                   UNTIL  WS-FIM-ENTRA002 = 'S'.

           PERFORM 900-00-FINALIZA.

           DISPLAY 'FIM DO PROCESSAMENTO'

           STOP RUN.

       100-99-PRINCIPAL-FIM.
           EXIT.

       200-00-INICIALIZA  SECTION.

           OPEN INPUT  ENTRA002
                OUTPUT SAIDA002
                OUTPUT SAIDA003.

           PERFORM  300-00-LER-ENTRA002.

       200-99-INICIALIZA-FIM.
           EXIT.

       300-00-LER-ENTRA002  SECTION.

           READ  ENTRA002  INTO  WS-ARQ-ENTRA002
                   AT END
                       MOVE 'S'          TO  WS-FIM-ENTRA002

                   NOT END
                       ADD  1            TO  WS-QT-LIDOS-ENTRA002.

       300-99-LER-ENTRA002-FIM.
           EXIT.

       500-00-PROCESSA  SECTION.

           IF  ARQ01-COD-MUNIC = 000100  AND
               ARQ01-VAL-A-TRIB  >=  10
               PERFORM  600-00-GRAVA-SAIDA002
           END-IF.

           IF  ARQ01-COD-MUNIC = 000100  AND
               ARQ01-VAL-A-TRIB  <  10
               PERFORM  700-00-GRAVA-SAIDA003
           END-IF.


           PERFORM  300-00-LER-ENTRA002.

       500-99-PROCESSA-FIM.
           EXIT.

       600-00-GRAVA-SAIDA002  SECTION.

           MOVE  ENT02-REGISTRO          TO  SAI02-REGISTRO.

           WRITE SAI02-REGISTRO.

           ADD  1                        TO  WS-QT-GRAVS-SAIDA002.

       600-99-GRAVA-SAIDA002-FIM.
           EXIT.

       700-00-GRAVA-SAIDA003 SECTION.
           MOVE  ENT02-REGISTRO          TO  SAI03-REGISTRO.

           WRITE SAI03-REGISTRO.

           ADD  1                        TO  WS-QT-GRAVS-SAIDA003.

       700-99-GRAVA-SAIDA003-FIM.
           EXIT.

       900-00-FINALIZA  SECTION.

           DISPLAY  'LENDO ARQUIVOS DE ENTRADA '  WS-QT-LIDOS-ENTRA002
           DISPLAY  'GRAVANDO SAIDA2...... '  WS-QT-GRAVS-SAIDA002
           DISPLAY  'GRAVANDO SAIDA3.......' WS-QT-GRAVS-SAIDA003

           CLOSE  ENTRA002
                  SAIDA002
                  SAIDA003.

       900-99-FINALIZA-FIM.
           EXIT.
       END PROGRAM Exercicio2.
