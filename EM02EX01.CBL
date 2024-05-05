       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX01.
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
           SELECT CADSOC1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSOC2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSOCIO ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD CADSOC1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSOC1.DAT".
       01 REG-ENT.
           02 CODPAG    PIC 9(02).
           02 NUMSOC1   PIC 9(06).
           02 NOMESOC1  PIC X(30).
           02 VALPAG1   PIC 9(09)V9(02).
       FD CADSOC2
           LABEL RECORD ARE STANDARD                                                                                                                            
           VALUE OF FILE-ID "CADSOC2.DAT".
       01 REG-SAI.
           02 NUMSOC2   PIC 9(06).
           02 NOMESOC2  PIC X(30).
           02 VALPAG2   PIC 9(09)V9(02).
       FD RELSOCIO
           LABEL RECORD IS OMITTED.
       01 REG-SOC       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 CT-LIN        PIC 9(02) VALUE 31.
       77 CT-PAG        PIC 9(02) VALUE ZEROES.
       77 SOMAVALOR1	PIC 9(12)V9(02).
       77 SOMASOCIO     PIC 9(5).

       01 CAB-01.
           02 FILLER    PIC X(70) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC 99.
           02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(27) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "RELACAO DE SOCIOS ATRASADOS".
           02 FILLER    PIC X(26) VALUE SPACES.

       01 CAB-03.
           02 FILLER    PIC X(8) VALUE SPACES.
           02 FILLER    PIC X(15) VALUE "NUMERO DO SOCIO".
           02 FILLER    PIC X(8) VALUE SPACES.
           02 FILLER    PIC X(13) VALUE "NOME DO SOCIO".
           02 FILLER    PIC X(9) VALUE SPACES.
           02 FILLER    PIC X(18) VALUE "VALOR DE PAGAMENTO".
           02 FILLER    PIC X(9) VALUE SPACES.

       01 DETALHE.
           02 FILLER    PIC X(12) VALUE SPACES.
           02 NUMSOC    PIC 999.999.
           02 FILLER    PIC X(11) VALUE SPACES.
           02 NOME      PIC X(30).
           02 FILLER    PIC X(2) VALUE SPACES.
           02 VALOR     PIC ZZZ.ZZZ.ZZ9,99.

       01 CAB-04.
           02 FILLER    PIC X(8) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "TOTAL DE SOCIOS ATRASADOS: ".
           02 SOMASOC   PIC 99999.

       01 CAB-05.
           02 FILLER    PIC X(8) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "VALOR TOTAL ATRASADO     : ".
           02 SOMAVALOR PIC ZZZ.ZZZ.ZZZ.ZZ9,99.                                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM RODAPE.
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT CADSOC1
                OUTPUT CADSOC2
                OUTPUT RELSOCIO.
           PERFORM LEITURA.

       LEITURA.
           READ CADSOC1 AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           IF CODPAG EQUAL 1
                PERFORM GRAVACAO
           ELSE
                PERFORM IMPRESSAO.

       IMPRESSAO.
           IF CT-LIN GREATER THAN 30
              PERFORM CABECALHO.
           PERFORM IMPDET.
           

       IMPDET.
           ADD CODPAG 	 TO SOMASOCIO.
           ADD VALPAG1   TO SOMAVALOR1.
           MOVE NUMSOC1  TO NUMSOC.
           MOVE NOMESOC1 TO NOME.
           MOVE VALPAG1  TO VALOR.
           WRITE REG-SOC FROM DETALHE AFTER ADVANCING 2 LINES.
           ADD 1         TO CT-LIN.

       CABECALHO.
           ADD 1         TO CT-PAG.
           MOVE CT-PAG   TO VAR-PAG.
           MOVE SPACES   TO REG-SOC.
           WRITE REG-SOC AFTER ADVANCING PAGE.
           WRITE REG-SOC FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-SOC FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-SOC FROM CAB-03 AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO CT-LIN.

       RODAPE.
           DIVIDE 2 INTO SOMASOCIO.
           MOVE SOMASOCIO  TO SOMASOC.
           MOVE SOMAVALOR1 TO SOMAVALOR.
           MOVE SPACES	   TO REG-SOC.
           WRITE REG-SOC FROM CAB-04 AFTER ADVANCING 3 LINES.
           WRITE REG-SOC FROM CAB-05 AFTER ADVANCING 1 LINE.

       GRAVACAO.
           MOVE NUMSOC1  TO NUMSOC2.
           MOVE NOMESOC1 TO NOMESOC2.
           MOVE VALPAG1  TO VALPAG2.
           WRITE REG-SAI.
                
       FIM.
           CLOSE CADSOC1
                 CADSOC2
                 RELSOCIO.                                                                                                                                                                                                                                                                                                                                                                       





