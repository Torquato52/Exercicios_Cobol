      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exercicio1.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRA001
           ASSIGN TO 'D:\ENTRA001.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SAIDA001
           ASSIGN TO 'D:\SAIDA001.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ENTRA001.
       01  ENT01-REGISTRO                 PIC X(94).

       FD  SAIDA001.
       01  SAI01-REGISTRO                 PIC X(94).

       WORKING-STORAGE SECTION.
       01  WS-ARQ-ENTRA001.
           03  WS-ENT01-COD-MUNIC         PIC 9(06).
           03  WS-ENT01-DESC-MUNIC        PIC X(20).
           03  WS-ENT01-COD-SERV          PIC 9(04).
           03  WS-ENT01-DESC-SERV         PIC X(20).
           03  WS-ENT01-VLR-ALIQ          PIC 9(02)V99.
           03  WS-ENT01-DAT-INI-VIGEN     PIC 9(08).
           03  WS-ENT01-DAT-FIN-VIGEN     PIC 9(08).
           03  WS-ENT01-USU-MANUT         PIC X(08).
           03  WS-ENT01-DAT-MANUT         PIC 9(08).
           03  WS-ENT01-HOR-MANUT         PIC 9(08).

       01  WS-FIM-ENTRA001                PIC X(01)  VALUE 'N'.

       01  WS-QT-LIDOS-ENTRA001           PIC 9(10)  VALUE ZEROS.
       01  WS-QT-GRAVS-SAIDA001           PIC 9(10)  VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       100-00-PRINCIPAL  SECTION.

           PERFORM 200-00-INICIALIZA.

           PERFORM 500-00-PROCESSA
                   UNTIL  WS-FIM-ENTRA001 = 'S'.

           PERFORM 900-00-FINALIZA.

           DISPLAY 'FIM DO PROCESSAMENTO'

           STOP RUN.

       100-99-PRINCIPAL-FIM.
           EXIT.

       200-00-INICIALIZA  SECTION.

           OPEN INPUT  ENTRA001
                OUTPUT SAIDA001.

           PERFORM  300-00-LER-ENTRA001.

       200-99-INICIALIZA-FIM.
           EXIT.

       300-00-LER-ENTRA001  SECTION.

           READ  ENTRA001  INTO  WS-ARQ-ENTRA001
                   AT END
                       MOVE 'S'          TO  WS-FIM-ENTRA001

                   NOT END
                       ADD  1            TO  WS-QT-LIDOS-ENTRA001.

       300-99-LER-ENTRA001-FIM.
           EXIT.

       500-00-PROCESSA  SECTION.

           IF  WS-ENT01-DAT-FIN-VIGEN = 99999999  AND
               WS-ENT01-VLR-ALIQ  >  0
               PERFORM  600-00-GRAVA-SAIDA001
           END-IF.

           PERFORM  300-00-LER-ENTRA001.

       500-99-PROCESSA-FIM.
           EXIT.

       600-00-GRAVA-SAIDA001  SECTION.

           MOVE  ENT01-REGISTRO          TO  SAI01-REGISTRO.

           WRITE SAI01-REGISTRO.

           ADD  1                        TO  WS-QT-GRAVS-SAIDA001.

       600-99-GRAVA-SAIDA001-FIM.
           EXIT.

       900-00-FINALIZA  SECTION.

           DISPLAY  'LENDO ARQUIVOS DE ENTRADA '  WS-QT-LIDOS-ENTRA001
           DISPLAY  'GRAVANDO SAIDA...... '  WS-QT-GRAVS-SAIDA001

           CLOSE  ENTRA001
                  SAIDA001.

       900-99-FINALIZA-FIM.
           EXIT.
       END PROGRAM Exercicio1.
