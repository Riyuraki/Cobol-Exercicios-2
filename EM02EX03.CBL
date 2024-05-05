       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX03.
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
           SELECT CADENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSAI ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
       01 REG-ENT.
           02 COD   	 PIC 9(07).
           02 NOME    	 PIC X(30).
           02 SALARIO    PIC 9(5)V9(2).
           02 SEXO	 PIC X(01).
       FD RELSAI
           LABEL RECORD IS OMITTED.
       01 REG-REL       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) 	VALUE "NAO".
       77 CT-LIN        PIC 9(02) 	VALUE 21.
       77 CT-PAG        PIC 9(02) 	VALUE ZEROES.
       77 SOMASAL	PIC 9(14)V9(2)  VALUE ZEROES.
       77 SOMACLI       PIC 9(5) 	VALUE ZEROES.

       01 CAB-01.
           02 FILLER    PIC X(70) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC ZZ9.
           02 FILLER    PIC X(02) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(21) VALUE SPACES.
           02 FILLER    PIC X(22) VALUE "LISTAGEM DOS CLIENTES ".
           02 FILLER	PIC X(16) VALUE "DO SEXO FEMININO".
           02 FILLER    PIC X(21) VALUE SPACES.

       01 CAB-03.
           02 FILLER    PIC X(23) VALUE SPACES.
           02 FILLER    PIC X(23) VALUE "COM SALARIOS SUPERIORES".
           02 FILLER    PIC X(11) VALUE " A 5.000,00".
           02 FILLER    PIC X(24) VALUE SPACES.

       01 CAB-04.
           02 FILLER	PIC X(12) VALUE SPACES.
           02 FILLER	PIC X(6) VALUE "CODIGO".
           02 FILLER    PIC X(10) VALUE SPACES.
           02 FILLER    PIC X(4) VALUE "NOME".
           02 FILLER	PIC X(35) VALUE SPACES.
           02 FILLER 	PIC X(7) VALUE "SALARIO".
           02 FILLER 	PIC X(6) VALUE SPACES.


       01 DETALHE.
           02 FILLER    PIC X(12) VALUE SPACES.
           02 COD-REL   PIC 9(7).
           02 FILLER    PIC X(5) VALUE SPACES.
           02 NOME-REL  PIC X(30).
           02 FILLER    PIC X(7) VALUE SPACES.
           02 SAL-REL   PIC ZZZ.ZZZ.ZZ9,99.
           02 FILLER	PIC X(5) VALUE SPACES.


       01 CAB-05.
           02 FILLER	PIC X(8) VALUE SPACES.
           02 FILLER	PIC X(28) VALUE "TOTAL DE CLIENTES IMPRESSOS:".
           02 FILLER	PIC X(6) VALUE SPACES.
           02 TOTALCLI  PIC ZZ.999.
            
       01 CAB-06.
           02 FILLER    PIC X(8) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "TOTAL DE SALARIO:".
           02 FILLER	PIC X(6) VALUE SPACES.
           02 TOTALSAL  PIC ZZZ.ZZZ.ZZ9,99.                                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM RODAPE.
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT CADENT
                OUTPUT RELSAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADENT AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           IF SEXO EQUAL "F" OR "f"  
              IF SALARIO GREATER THAN 5000 OR SALARIO EQUAL 5000 
                 PERFORM IMPRESSAO.

       IMPRESSAO.
           IF CT-LIN GREATER THAN 20
              PERFORM CABECALHO.
           PERFORM IMPDET.
           

       IMPDET.
           ADD 1 	 TO SOMACLI.
           ADD SALARIO   TO SOMASAL.
           MOVE COD  	 TO COD-REL.
           MOVE NOME	 TO NOME-REL.
           MOVE SALARIO  TO SAL-REL.
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 2 LINES.
           ADD 1         TO CT-LIN.

       CABECALHO.
           ADD 1         TO CT-PAG.
           MOVE CT-PAG   TO VAR-PAG.
           MOVE SPACES   TO REG-REL.
           WRITE REG-REL AFTER ADVANCING PAGE.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-04 AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO CT-LIN.

       RODAPE.
           MOVE SOMACLI  TO TOTALCLI.
           MOVE SOMASAL  TO TOTALSAL.
           MOVE SPACES	 TO REG-REL.
           WRITE REG-REL FROM CAB-05 AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-06 AFTER ADVANCING 1 LINE.

                
       FIM.
           CLOSE CADENT
                 RELSAI.
