       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM02EX05.
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
           SELECT ARQALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQREP ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELAPROV ASSIGN TO DISK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

       DATA DIVISION.

       FILE SECTION.
       FD ARQALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "ARQALU.DAT".
       01 REG-ALU.
           02 MATRICULA    PIC 9(7).
           02 NOME   	   PIC X(30).
           02 NOTA1  	   PIC 9(2)V9(2).
           02 NOTA2  	   PIC 9(2)V9(2).           
           02 NOTA3        PIC 9(2)V9(2). 
           02 FALTAS       PIC 9(02).
           02 SEXO         PIC X(1).

       FD ARQREP
           LABEL RECORD ARE STANDARD                                                                                                                            
           VALUE OF FILE-ID "ARQREP.DAT".
       01 REG-REP.
           02 MATRICULA-REP    PIC 9(7).
           02 NOME-REP         PIC X(30).
           02 NOTA1-REP        PIC 9(2)V9(2).
           02 NOTA2-REP        PIC 9(2)V9(2).           
           02 NOTA3-REP        PIC 9(2)V9(2). 
           02 FALTAS-REP       PIC 9(02).
           02 SEXO-REP         PIC X(1).

       FD RELAPROV
           LABEL RECORD IS OMITTED.
       01 REG-APV       PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 CT-LIN        PIC 9(02) VALUE 31.
       77 CT-PAG        PIC 9(02) VALUE ZEROES.
       77 SOMAALU       PIC 99999 VALUE ZEROES.
       77 MEDIATT	PIC 999V99 VALUE ZEROES.
       77 SOMANOTA      PIC 99V99 VALUE ZEROES.
       77 MEDIAALU      PIC 99V99 VALUE ZEROES.
       77 MEDIATOTAL    PIC 99V99 VALUE ZEROES.
       77 ALUAPV        PIC 999 VALUE ZEROES.

       01 CAB-01.
           02 FILLER    PIC X(30) VALUE SPACES.
           02 FILLER	PIC X(27) VALUE "RELACAO DE ALUNOS APROVADOS".
           02 FILLER    PIC X(7)  VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG. ".
           02 VAR-PAG   PIC ZZ9.
           02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(7) VALUE SPACES.
           02 FILLER    PIC X(6) VALUE "NUMERO".
           02 FILLER    PIC X(15) VALUE SPACES.
           02 FILLER	PIC X(13) VALUE "NOME DO ALUNO".
           02 FILLER	PIC X(10) VALUE SPACES.
           02 FILLER	PIC X(5) VALUE "MEDIA".
           02 FILLER	PIC X(10) VALUE SPACES.
           02 FILLER 	PIC X(6) VALUE "FALTAS".

       01 DETALHE.
           02 FILLER        PIC X(7) VALUE SPACES.
           02 MATRICULA-REL PIC X(8).
           02 FILLER        PIC X(11) VALUE SPACES.
           02 NOME-REL      PIC X(30).
           02 FILLER        PIC X(5) VALUE SPACES.
           02 MEDIA-REL     PIC Z9,99.
           02 FILLER        PIC X(10) VALUE SPACES.
           02 FALTAS-REL    PIC 99.

       01 CAB-03.
           02 FILLER    PIC X(7) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "TOTAL DE ALUNOS APROVADOS: ".
           02 SOMAALU-REL   PIC 99999.

       01 CAB-04.
           02 FILLER    PIC X(7) VALUE SPACES.
           02 FILLER    PIC X(27) VALUE "MEDIA GERAL DA TURMA     : ".
           02 MEDIATT-REL PIC Z9,99.                                                                                                                                                                                                                                                                                                                                                                                                     

       PROCEDURE DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL 
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM RODAPE.
       PERFORM FIM.
       STOP RUN.

       INICIO.
           OPEN INPUT ARQALU
                OUTPUT ARQREP
                OUTPUT RELAPROV.
           PERFORM LEITURA.

       LEITURA.
           READ ARQALU AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CALCULO.
           PERFORM SELECAO.
           PERFORM LEITURA.

       CALCULO.
           ADD NOTA1 NOTA2 NOTA3 GIVING SOMANOTA.
           DIVIDE SOMANOTA BY 3 GIVING MEDIAALU.
           ADD 1 TO SOMAALU.
           ADD MEDIAALU TO MEDIATT.       

       SELECAO.
           IF MEDIAALU LESS THAN 7 OR FALTAS GREATER THAN 15
                PERFORM GRAVACAO
           ELSE
                ADD 1 TO ALUAPV
                PERFORM IMPRESSAO.

       IMPRESSAO.
           IF CT-LIN GREATER THAN 30
              PERFORM CABECALHO.
           PERFORM IMPDET.
           
       IMPDET.
           MOVE MATRICULA(1:6)   TO MATRICULA-REL(1:6).
           MOVE "-" TO MATRICULA-REL(7:1)
           MOVE MATRICULA(7:1)   TO MATRICULA-REL(8:1).
           MOVE NOME        TO NOME-REL.
           MOVE MEDIAALU    TO MEDIA-REL.
           MOVE FALTAS	    TO FALTAS-REL.
           WRITE REG-APV FROM DETALHE AFTER ADVANCING 2 LINES.
           ADD 1         TO CT-LIN.

       CABECALHO.
           ADD 1         TO CT-PAG.
           MOVE CT-PAG   TO VAR-PAG.
           MOVE SPACES   TO REG-APV.
           WRITE REG-APV AFTER ADVANCING PAGE.
           WRITE REG-APV FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-APV FROM CAB-02 AFTER ADVANCING 2 LINES.
           MOVE ZEROS TO CT-LIN.

       RODAPE.
           DIVIDE MEDIATT BY SOMAALU GIVING MEDIATOTAL.
           MOVE ALUAPV    TO SOMAALU-REL.
           MOVE MEDIATOTAL TO MEDIATT-REL.
           MOVE SPACES	   TO REG-APV.
           WRITE REG-APV FROM CAB-03 AFTER ADVANCING 4 LINES.
           WRITE REG-APV FROM CAB-04 AFTER ADVANCING 1 LINE.

       GRAVACAO.
           MOVE MATRICULA  TO MATRICULA-REP.
           MOVE NOME       TO NOME-REP.
           MOVE NOTA1      TO NOTA1-REP.
           MOVE NOTA2      TO NOTA2-REP.
           MOVE NOTA3      TO NOTA3-REP.
           MOVE FALTAS     TO FALTAS-REP.
           MOVE SEXO	   TO SEXO-REP.
           WRITE REG-REP.
                
       FIM.
           CLOSE ARQALU
                 ARQREP
                 RELAPROV.