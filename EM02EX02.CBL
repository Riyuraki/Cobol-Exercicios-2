       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX02.
       AUTHOR.          MIYAZAKI.
       INSTALLATION.    MARCELO-PC.
       DATE-WRITTEN.    20/04/2024.
       DATE-COMPILED.
       SECURITY.        SEGURANCA DE EM02.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  MARCELO-PC.
       OBJECT-COMPUTER.  FATEC-PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELCLI ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD CADCLI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
       01 REG-ENT.
           02 CPF      PIC 9(11).
           02 NOME     PIC X(30).
           02 ESTADO   PIC X(02).
           02 CIDADE   PIC X(30).
           02 TEL      PIC 9(08).
           02 REG-DATA.
            03 DIA    PIC 9(02).
            03 MES    PIC 9(02).
            03 ANO    PIC 9(04).
                   

       FD RELCLI
           LABEL RECORD IS OMITTED.
       01 REG-SOC       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 CT-LIN        PIC 9(02) VALUE 26.
       77 CT-PAG        PIC 9(02) VALUE ZEROES.
       77 SOMA-CLI      PIC 9(04) VALUE ZEROES.

       01 CAB-01.
           02 FILLER    PIC X(70) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC 99.
           02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(32) VALUE SPACES.
           02 FILLER    PIC X(17) VALUE "TURISMAR TURISMOS".
           02 FILLER    PIC X(31) VALUE SPACES.

       01 CAB-03.
           02 FILLER    PIC X(22) VALUE SPACES.
           02 FILLER    PIC X(24) VALUE "CLIENTES NO PERIODO DE: ".
           02 FILLER    PIC X(11) VALUE "2010 A 2011".
           02 FILLER    PIC X(23) VALUE SPACES.

       01 CAB-04.
           02 FILLER    PIC X(13) VALUE SPACES.
           02 FILLER    PIC X(4) VALUE "NOME".
           02 FILLER    PIC X(22) VALUE SPACES.
           02 FILLER    PIC X(6) VALUE "ESTADO".
           02 FILLER    PIC X(10) VALUE SPACES.
           02 FILLER    PIC X(8) VALUE "TELEFONE".
           02 FILLER    PIC X(17) VALUE SPACES.

       01 CAB-05.
           02 FILLER    PIC X(20) VALUE "--------------------".
           02 FILLER    PIC X(20) VALUE "--------------------".
           02 FILLER    PIC X(20) VALUE "--------------------".
           02 FILLER    PIC X(20) VALUE "--------------------".

       01 DETALHE.
           02 FILLER     PIC X(4) VALUE SPACES.
           02 NOME-SAI   PIC X(30).
           02 FILLER     PIC X(9) VALUE SPACES.
           02 ESTADO-SAI PIC X(2).
           02 FILLER     PIC X(11) VALUE SPACES.
           02 TEL-SAI    PIC 9(8).
           02 FILLER     PIC X(16) VALUE SPACES.

       01 CAB-06.
           02 FILLER       PIC X(4) VALUE SPACES.
           02 FILLER       PIC X(19) VALUE "TOTAL DE CLIENTES: ".
           02 SOMACLI-SAI  PIC 9999.                                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM RODAPE.
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT CADCLI
                OUTPUT RELCLI.
           PERFORM LEITURA.

       LEITURA.
           READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM IMPRESSAO.
           PERFORM LEITURA.

       IMPRESSAO.
           IF CT-LIN GREATER THAN 25
              PERFORM CABECALHO.
           PERFORM SELECAO.   

       SELECAO.
           IF ANO EQUAL 2011 OR EQUAL 2010
              PERFORM IMPDET.
       IMPDET.
           ADD 1 	   TO SOMA-CLI.
           MOVE NOME   TO NOME-SAI.
           MOVE ESTADO TO ESTADO-SAI.
           MOVE TEL    TO TEL-SAI.
           WRITE REG-SOC FROM DETALHE AFTER ADVANCING 2 LINES.
           ADD 1           TO CT-LIN.

       CABECALHO.
           ADD 1         TO CT-PAG.
           MOVE CT-PAG   TO VAR-PAG.
           MOVE SPACES   TO REG-SOC.
           WRITE REG-SOC AFTER ADVANCING PAGE.
           WRITE REG-SOC FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-SOC FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-SOC FROM CAB-03 AFTER ADVANCING 2 LINES.
           WRITE REG-SOC FROM CAB-04 AFTER ADVANCING 2 LINES.
           WRITE REG-SOC FROM CAB-05 AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO CT-LIN.

       RODAPE.
           MOVE SOMA-CLI   TO SOMACLI-SAI.
           MOVE SPACES	   TO REG-SOC.
           WRITE REG-SOC FROM CAB-06 AFTER ADVANCING 3 LINES.
   
       FIM.
           CLOSE CADCLI
                 RELCLI.      