       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX04.
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
           SELECT CADESTOQ ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELESTOQ ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD CADESTOQ
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADESTOQ.DAT".
       01 REG-ENT.
           02 COD   	 	PIC 9(05).
           02 NOME    	 	PIC X(15).
           02 QTDE-ESTOQUE    	PIC 9(5).
           02 PRECO-UNITARIO    PIC 9(6)V99.
       FD RELESTOQ
           LABEL RECORD IS OMITTED.
       01 REG-REL       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) 	VALUE "NAO".
       77 CT-LIN        PIC 9(02) 	VALUE 26.
       77 CT-PAG        PIC 9(02) 	VALUE ZEROES.
       77 CUSTOTT       PIC 9(7)V99	VALUE ZEROES.
       77 SOMAQTDE	PIC 9(7)  	VALUE ZEROES.
       77 SOMATOTAL     PIC 9(8)V99  	VALUE ZEROES.
       77 PRODUTOS	PIC 9(5)	VALUE ZEROES.
       77 SOMAPRECO     PIC 9(9)V99     VALUE ZEROES.
       77 PRECO-MED     PIC 9(6)V99     VALUE ZEROES.

       01 CAB-01.
           02 FILLER    PIC X(1)  VALUE SPACES.
           02 FILLER	PIC X(16)  VALUE "DATA: 99/99/99".
           02 FILLER	PIC X(4)  VALUE SPACES.
           02 FILLER    PIC X(10) VALUE "RELATORIO ".
           02 FILLER    PIC X(22) VALUE "DE MATERIAL EM ESTOQUE".
           02 FILLER 	PIC X(16) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC ZZ9.
           02 FILLER    PIC X(02) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(1) VALUE SPACES.
           02 FILLER    PIC X(6) VALUE "CODIGO".
           02 FILLER	PIC X(4) VALUE SPACES.
           02 FILLER	PIC X(4) VALUE "NOME".
           02 FILLER 	PIC X(9) VALUE SPACES.
           02 FILLER	PIC X(7) VALUE "QTDE DO".
           02 FILLER    PIC X(4) VALUE SPACES.
           02 FILLER	PIC X(14) VALUE "CUSTO UNITARIO".
           02 FILLER	PIC X(5) VALUE SPACES.
           02 FILLER	PIC X(11) VALUE "CUSTO TOTAL".
           02 FILLER	PIC X(15) VALUE SPACES.
       
       01 CAB-03.
           02 FILLER	PIC X(24) VALUE SPACES.
           02 FILLER 	PIC X(7)  VALUE "ESTOQUE".	

       01 DETALHE.
           02 FILLER    	PIC X(1) VALUE SPACES.
           02 COD-REL   	PIC 9(5).
           02 FILLER    	PIC X(3) VALUE SPACES.
           02 NOME-REL  	PIC X(15).
           02 FILLER    	PIC X(3) VALUE SPACES.
           02 QTDE-REL  	PIC ZZ.ZZ9.
           02 FILLER		PIC X(6) VALUE SPACES.
           02 CUSTO-REL 	PIC ZZZ.ZZ9,99.
           02 FILLER		PIC X(8) VALUE SPACES.
           02 CUSTOTT-REL  	PIC Z.ZZZ.ZZ9,99.


       01 CAB-04.
           02 FILLER	PIC X(1) VALUE SPACES.
           02 FILLER	PIC X(26) VALUE "QUANTIDADE TOTAL".
           02 FILLER	PIC X(7) VALUE SPACES.
           02 FILLER	PIC X(14) VALUE "PRECO MEDIO".
           02 FILLER 	PIC X(6)  VALUE SPACES.
           02 FILLER 	PIC X(11) VALUE "PRECO TOTAL".
            
       01 LAYOUT.
           02 FILLER	   	PIC X(4) VALUE SPACES.
           02 QTDETOTAL		PIC Z.ZZZ.ZZ9.
           02 FILLER	 	PIC X(20) VALUE SPACES.
           02 PREMED-REL 	PIC ZZZ.ZZ9,99.
           02 FILLER		PIC X(9) VALUE SPACES.
           02 PRECOTT		PIC ZZ.ZZZ.ZZ9,99.                                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM RODAPE.
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT CADESTOQ
                OUTPUT RELESTOQ.
           PERFORM LEITURA.

       LEITURA.
           READ CADESTOQ AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM IMPRESSAO.
           PERFORM LEITURA.

       IMPRESSAO.
           IF CT-LIN GREATER THAN 25
              PERFORM CABECALHO.
           PERFORM IMPDET.
           

       IMPDET.
           MULTIPLY QTDE-ESTOQUE BY PRECO-UNITARIO GIVING CUSTOTT.
           ADD PRECO-UNITARIO	TO SOMAPRECO.
           ADD QTDE-ESTOQUE 	TO SOMAQTDE.
           ADD CUSTOTT 		TO SOMATOTAL.
	   ADD 1 		TO PRODUTOS.
           MOVE COD  	 	TO COD-REL.
           MOVE NOME	 	TO NOME-REL.
           MOVE QTDE-ESTOQUE    TO QTDE-REL.
           MOVE PRECO-UNITARIO  TO CUSTO-REL.
           MOVE CUSTOTT		TO CUSTOTT-REL.
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD 1         TO CT-LIN.

       CABECALHO.
           ADD 1         TO CT-PAG.
           MOVE CT-PAG   TO VAR-PAG.
           MOVE SPACES   TO REG-REL.
           WRITE REG-REL AFTER ADVANCING PAGE.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 1 LINE.
           MOVE ZEROS TO CT-LIN.

       RODAPE.
           ADD 1	TO CT-PAG.
           MOVE CT-PAG  TO VAR-PAG.
           MOVE SPACES  TO REG-REL.
           DIVIDE SOMAPRECO BY PRODUTOS GIVING PRECO-MED.
           MOVE SOMAQTDE 	TO QTDETOTAL.
           MOVE PRECO-MED  	TO PREMED-REL.
           MOVE SOMATOTAL	TO PRECOTT.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-04 AFTER ADVANCING 2 LINES.
           WRITE REG-REL FROM LAYOUT AFTER ADVANCING 2 LINES.

       FIM.
           CLOSE CADESTOQ
                 RELESTOQ.