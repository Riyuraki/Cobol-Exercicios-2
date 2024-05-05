       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX06.
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
           SELECT CADOK ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELOCOR ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD CADCLI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
       01 REGCLI.
           02 CODENT PIC 9(03).
           02 CPF    PIC 9(11).
           02 NOME   PIC X(30).
              88 NOME-INVALIDO VALUE SPACES.
           02 ESTADO PIC X(02).
              88 ESTADO-VALIDO VALUE "AC" "AL" "AP" "AM" 
                                     "BA" "CE" "DF" "ES"
                                     "GO" "MA" "MT" "MS"
                                     "MG" "PA" "PB" "PR" 
                                     "PE" "PI" "RJ" "RN" 
                                     "RS" "RO" "RR" "SC"
                                     "SP" "SE" "TO".
           02 CIDADE PIC X(30).
              88 CIDADE-INVALIDO VALUE SPACES.
           02 EMAIL  PIC X(30).
              88 EMAIL-INVALIDO VALUE SPACES.
           02 TEL    PIC 9(10).
       FD CADOK
           LABEL RECORD ARE STANDARD                                                                                                                            
           VALUE OF FILE-ID "CADOK.DAT".
       01 REGOK.
           02 CPF-SAI    PIC 9(11).
           02 NOME-SAI   PIC X(30).
           02 ESTADO-SAI PIC X(02).
           02 CIDADE-SAI PIC X(30).
           02 EMAIL-SAI  PIC X(30).
           02 TEL-SAI    PIC 9(10).
       FD RELOCOR
           LABEL RECORD IS OMITTED.
       01 REGRUIM       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 CH-ERRO	PIC X(03) VALUE "NAO".
       77 CT-LIN        PIC 9(02) VALUE 41.
       77 CT-PAG        PIC 9(02) VALUE ZEROES.  
 
       01 DV-CALC       PIC 9.
       01 DV-INFO       PIC 9.     
       01 RESTOCPF      PIC 9 VALUE ZERO.
       01 MULTICPF      PIC 9(2).
       01 SOMACPF	PIC 9(3) VALUE ZEROES.
       01 CONTA1        PIC 9(3) VALUE ZEROES.
       01 CONTA2        PIC 9(3) VALUE ZEROES.
       01 CONTA3        PIC 9(3) VALUE ZEROES.
 
       01 CAB-00.
           02 FILLER    PIC X(70) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC 99.
           02 FILLER    PIC X(03) VALUE SPACES.
       
       01 CAB-01.
           02 FILLER    PIC X(32) VALUE SPACES.
           02 FILLER    PIC X(17) VALUE "TURISMAR TURISMOS".
           02 FILLER    PIC X(31) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(18) VALUE SPACES.
           02 FILLER    PIC X(18) VALUE "RELATORIO DE DADOS".
           02 FILLER    PIC X(13) VALUE " DE CLIENTES ".
           02 FILLER    PIC X(14) VALUE "INCONSISTENTES".
           02 FILLER    PIC X(17) VALUE SPACES.

       01 CAB-03.
           02 FILLER	PIC X(20) VALUE "--------------------".
           02 FILLER	PIC X(20) VALUE "--------------------".
           02 FILLER	PIC X(20) VALUE "--------------------".
           02 FILLER	PIC X(20) VALUE "--------------------".

       01 CAB-04.
           02 FILLER    PIC X(2) VALUE SPACES.
           02 FILLER    PIC X(17) VALUE "CODIGO DO CLIENTE".
           02 FILLER    PIC X(10) VALUE SPACES.
           02 FILLER    PIC X(15) VALUE "DADOS INVALIDOS".
           02 FILLER    PIC X(34) VALUE SPACES.

       01 DETALHE.
           02 FILLER     PIC X(7) VALUE SPACES.
           02 CODSAI     PIC 9(03).
           02 FILLER     PIC X(12) VALUE SPACES.
           02 MENSAGEM   PIC X(58) VALUE SPACES.
       
       01 DETALHE2.
           02 FILLER	 PIC X(20) VALUE SPACES.
           02 MENSAGEM2  PIC X(60) VALUE SPACES. 
                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM CABECALHO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT CADCLI
                OUTPUT CADOK
                OUTPUT RELOCOR.
           PERFORM LEITURA.

       LEITURA.
           READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM VALIDACAO.
           PERFORM LEITURA.

       VALIDACAO.

           MOVE "NAO" TO CH-ERRO.
           PERFORM VAL-CPF.
           PERFORM VAL-NOME.
           PERFORM VAL-ESTADO.
           PERFORM VAL-CIDADE.
           PERFORM VAL-EMAIL.
           IF CH-ERRO EQUAL "SIM"
              PERFORM GRAVA-RUIM
           ELSE
              PERFORM GRAVA-OK.

       VAL-CPF.
        MOVE CPF(10:1) TO DV-INFO.
        MOVE ZERO TO SOMACPF.
        MOVE 2 TO MULTICPF.
        PERFORM UNTIL MULTICPF > 10
          MOVE CPF(MULTICPF:1) TO CONTA1
          MULTIPLY MULTICPF BY CONTA1 GIVING CONTA2
          COMPUTE CONTA3 = FUNCTION MOD(CONTA2, 11)            
          COMPUTE SOMACPF = SOMACPF + CONTA3
          ADD 1 TO MULTICPF.
        COMPUTE RESTOCPF = FUNCTION MOD(SOMACPF, 11).
        IF RESTOCPF < 2 THEN
              MOVE 0 TO DV-CALC
        ELSE
              COMPUTE DV-CALC = 11 - RESTOCPF
        END-IF.
           
        IF DV-CALC = DV-INFO
              	NEXT SENTENCE
        ELSE 
                MOVE "CPF INVALIDO " TO MENSAGEM
                MOVE "SIM" TO CH-ERRO
        END-IF.

       VAL-NOME.
           IF NOME-INVALIDO
               MOVE "NOME NAO INFORMADO " TO MENSAGEM(14:33)
               MOVE "SIM" TO CH-ERRO
           ELSE
              NEXT SENTENCE.
 
       VAL-ESTADO.
           IF ESTADO-VALIDO
               NEXT SENTENCE
           ELSE
              MOVE "ESTADO INVALIDO " TO MENSAGEM(34:50)
              MOVE "SIM" TO CH-ERRO.

       VAL-CIDADE.
           IF CIDADE-INVALIDO
               MOVE "CIDADE NAO INFORMADA " TO MENSAGEM2
               MOVE "SIM" TO CH-ERRO
           ELSE
              NEXT SENTENCE.

       VAL-EMAIL.
           IF EMAIL-INVALIDO
              MOVE "EMAIL SEM DESCRICAO"  TO MENSAGEM2(22:31)
              MOVE "SIM" TO CH-ERRO
           ELSE
              NEXT SENTENCE.

       GRAVA-RUIM.
           IF CT-LIN GREATER THAN 40
              PERFORM CABECALHO.
           PERFORM IMPDET.
           

       IMPDET.
           MOVE CODENT  TO CODSAI.  
           WRITE REGRUIM FROM DETALHE AFTER ADVANCING 1 LINE.
           WRITE REGRUIM FROM DETALHE2 AFTER ADVANCING 1 LINE.


       CABECALHO.

           ADD 1	 TO CT-PAG.
           MOVE CT-PAG	 TO VAR-PAG.
           MOVE SPACES   TO REGRUIM.
           WRITE REGRUIM AFTER ADVANCING PAGE.
           WRITE REGRUIM FROM CAB-00 AFTER ADVANCING 2 LINES.
           WRITE REGRUIM FROM CAB-01 AFTER ADVANCING 2 LINES.
           WRITE REGRUIM FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REGRUIM FROM CAB-03 AFTER ADVANCING 2 LINES.
           WRITE REGRUIM FROM CAB-04 AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO CT-LIN.

       GRAVA-OK.
           MOVE CPF     TO CPF-SAI.
           MOVE NOME    TO NOME-SAI.
           MOVE ESTADO  TO ESTADO-SAI.
           MOVE CIDADE	TO CIDADE-SAI.
           MOVE EMAIL   TO EMAIL-SAI.
           MOVE TEL	TO TEL-SAI.
           WRITE REGOK.
                
       FIM.
           CLOSE CADCLI
                 CADOK
                 RELOCOR.  