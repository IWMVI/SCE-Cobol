       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE002.
      *************************************************************************
      * INSTALLATION.       CADASTRO DE CLIENTES                              *
      * AUTHOR.             Wallace Martins Vieira                            *
      * DATE-WRITTEN        00/00/0000                                        *
      *************************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCLI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CLI-CPF
                      ALTERNATE RECORD KEY IS CLI-NOME WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.

           SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA WITH DUPLICATES.

      *************************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".

       01 REGCLI.
        03 CHAVE1.
         05 CLI-CPF                PIC 9(14).
        03 CLI-CEP                 PIC 9(08).
        03 CLI-TELEFONE            PIC 9(11).
        03 CLI-NOME                PIC X(30).
        03 CLI-APELIDO             PIC X(30).
        03 CLI-LOGRADOURO          PIC X(24).
        03 CLI-NUMERO              PIC X(04).
        03 CLI-COMPLEMENTO         PIC X(24).
        03 CLI-BAIRRO              PIC X(40).
        03 CLI-CIDADE              PIC X(24).
        03 CLI-ESTADO              PIC X(02).
        03 CLI-EMAIL               PIC X(33).
        03 CLI-CONTATO             PIC X(32).

      *************************************************************************

       FD ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".

       01 REGCEP.
        03 CEP-NUMCEP        PIC 9(08).
        03 CEP-LOGRA         PIC X(30).
        03 CEP-BAIRRO        PIC X(20).
        03 CEP-CIDADE        PIC X(20).
        03 CEP-UF            PIC X(02).

      *************************************************************************

       WORKING-STORAGE SECTION.

       01 WS-OPCAO                   PIC X(01) VALUE SPACES.
       01 ST-ERRO                    PIC X(02) VALUE "00".
       01 DCLI-ESTADO                PIC X(15) VALUE SPACES.
       01 DCLI-TIPOCLIENTE           PIC X(20) VALUE SPACES.
       01 MENS                       PIC X(60) VALUE SPACES.
       01 WS-CONT                    PIC 9(06) VALUE ZEROS.
       01 WS-ACT                     PIC 9(02) VALUE ZEROS.
       01 WS-SEL                     PIC 9(01) VALUE ZEROS.

       01 TXUF.
        03 UF-SIGLA   PIC X(02) VALUE SPACES.
        03 UF-TEXTO   PIC X(20) VALUE SPACES.

       01 TBUF.
        03 FILLER     PIC X(25) VALUE "AC - ACRE".
        03 FILLER     PIC X(25) VALUE "AL - ALAGOAS".
        03 FILLER     PIC X(25) VALUE "AM - AMAZONAS".
        03 FILLER     PIC X(25) VALUE "AP - AMAPA".
        03 FILLER     PIC X(25) VALUE "BA - BAHIA".
        03 FILLER     PIC X(25) VALUE "CE - CEARA".
        03 FILLER     PIC X(25) VALUE "DF - DISTRITO FEDERAL".
        03 FILLER     PIC X(25) VALUE "ES - ESPIRITO SANTO".
        03 FILLER     PIC X(25) VALUE "GO - GOIAS".
        03 FILLER     PIC X(25) VALUE "MA - MARANHÃO".
        03 FILLER     PIC X(25) VALUE "MT - MATO GROSSO".
        03 FILLER     PIC X(25) VALUE "MS - MATO GROSSO DO SUL".
        03 FILLER     PIC X(25) VALUE "MG - MINAS GERAIS".
        03 FILLER     PIC X(25) VALUE "PA - PARA".
        03 FILLER     PIC X(25) VALUE "PB - PARAIBA".
        03 FILLER     PIC X(25) VALUE "PR - PARANÁ".
        03 FILLER     PIC X(25) VALUE "PE - PERNAMBUCO".
        03 FILLER     PIC X(25) VALUE "PI - PIAUI".
        03 FILLER     PIC X(25) VALUE "RJ - RIO DE JANEIRO".
        03 FILLER     PIC X(25) VALUE "RN - GRANDE DO NORTE".
        03 FILLER     PIC X(25) VALUE "RS - RIO GRANDE DO SUL".
        03 FILLER     PIC X(25) VALUE "RO - RONDONIA".
        03 FILLER     PIC X(25) VALUE "RR - RORAIMA".
        03 FILLER     PIC X(25) VALUE "SC - SANTA CATARINA".
        03 FILLER     PIC X(25) VALUE "SP - SAO PAULO".
        03 FILLER     PIC X(25) VALUE "SE - SERGIPE".
        03 FILLER     PIC X(25) VALUE "TO - TOCANTINS".

       01 TBUFR REDEFINES TBUF.
        03 TABUF        PIC X(25) OCCURS 27 TIMES.

       01 IND         PIC 9(02) VALUE ZEROS.


      *************************************************************************

       SCREEN SECTION.


       01  TELAPRINCIPAL.
           05  LINE 01  COLUMN 01
               VALUE  "========================================".
           05  LINE 01  COLUMN 41
               VALUE  "========================================".
           05  LINE 02  COLUMN 01
               VALUE  "                              CADASTRO D".
           05  LINE 02  COLUMN 41
               VALUE  "E CLIENTES".
           05  LINE 03  COLUMN 01
               VALUE  "========================================".
           05  LINE 03  COLUMN 41
               VALUE  "========================================".
           05  LINE 05  COLUMN 01
               VALUE  "  CPF / CNPJ   :                  NOME :".
           05  LINE 06  COLUMN 01
               VALUE  "  RAZAO SOCIAL :".
           05  LINE 08  COLUMN 01
               VALUE  "========================================".
           05  LINE 08  COLUMN 41
               VALUE  "========================================".
           05  LINE 10  COLUMN 01
               VALUE  "  CEP    :                  LOGRADOURO :".
           05  LINE 11  COLUMN 01
               VALUE  "  NUMERO :                  COMPLEMENTO:".
           05  LINE 12  COLUMN 01
               VALUE  "  BAIRRO :".
           05  LINE 13  COLUMN 01
               VALUE  "  CIDADE :                          UF :".
           05  LINE 13  COLUMN 41
               VALUE  "    -".
           05  LINE 15  COLUMN 01
               VALUE  "========================================".
           05  LINE 15  COLUMN 41
               VALUE  "========================================".
           05  LINE 17  COLUMN 01
               VALUE  "  TELEFONE :                 EMAIL :".
           05  LINE 18  COLUMN 01
               VALUE  "  CONTATO  :".
           05  LINE 20  COLUMN 01
               VALUE  "========================================".
           05  LINE 20  COLUMN 41
               VALUE  "========================================".
           05  LINE 22  COLUMN 01
               VALUE  "  AVISOS   :".
           05  LINE 24  COLUMN 01
               VALUE  "========================================".
           05  LINE 24  COLUMN 41
               VALUE  "========================================".
           05  TCLI-CPF
               LINE 05  COLUMN 18  PIC 9(14)
               USING  CLI-CPF.
           05  TCLI-NOME
               LINE 05  COLUMN 42  PIC X(30)
               USING  CLI-NOME.
           05  TCLI-APELIDO
               LINE 06  COLUMN 18  PIC X(30)
               USING  CLI-APELIDO.
           05  TCLI-CEP
               LINE 10  COLUMN 12  PIC 99999.999
               USING  CLI-CEP.
           05  TCLI-LOGRADOURO
               LINE 10  COLUMN 42  PIC X(30)
               USING  CEP-LOGRA.
           05  TCLI-NUMERO
               LINE 11  COLUMN 12  PIC 9(08)
               USING  CLI-NUMERO.
           05  TCLI-COMPLEMENTO
               LINE 11  COLUMN 42  PIC X(30)
               USING  CLI-COMPLEMENTO.
           05  TCLI-BAIRRO
               LINE 12  COLUMN 12  PIC X(25)
               USING  CEP-BAIRRO.
           05  TCLI-CIDADE
               LINE 13  COLUMN 12  PIC X(25)
               USING  CEP-CIDADE.
           05  TUF-TEXTO
               LINE 13  COLUMN 42  PIC X(02)
               USING  CEP-UF.
           05  TCLI-TELEFONE
               LINE 17  COLUMN 14  PIC 9(11)
               USING  CLI-TELEFONE.
           05  TCLI-EMAIL
               LINE 17  COLUMN 38  PIC X(30)
               USING  CLI-EMAIL.
           05  TCLI-CONTATO
               LINE 18  COLUMN 14  PIC X(30)
               USING  CLI-CONTATO.

      *************************************************************************

       PROCEDURE DIVISION.

      ************************ ABERTURA ARQUIVO *******************************
      
       INICIO.

       R0.
           OPEN I-O ARQCLI
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
               OPEN OUTPUT ARQCLI
               MOVE "CRIANDO ARQUIVO CADASTRO DE CLIENTES" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               CLOSE ARQCLI
               GO TO INICIO
           ELSE
             IF ST-ERRO = "95"
               MOVE "ISAM NAO CARREGADO " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
             ELSE
               MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTES" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQCEP
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
               MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
             ELSE
               MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.

      ******************** INCICIALIZAÇÃO VARIÁVEIS ***************************

       R0B.
           MOVE SPACES TO CLI-NOME CLI-APELIDO CLI-LOGRADOURO
                          CLI-COMPLEMENTO CLI-BAIRRO
                          CLI-CIDADE CLI-ESTADO CLI-EMAIL 
                          CLI-CONTATO

            MOVE SPACES TO CEP-LOGRA CEP-BAIRRO CEP-CIDADE CEP-UF
            MOVE ZEROS  TO CLI-CPF CLI-CEP CLI-TELEFONE CLI-NUMERO.

      ********************** VISUALIZAÇÃO DA TELA *****************************

           DISPLAY TELAPRINCIPAL.

      ************************ ENTRADA DE DADOS *******************************

       R1.
           ACCEPT TCLI-CPF.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
            CLOSE ARQCLI
            STOP RUN.
           IF CLI-CPF = SPACES
             MOVE "*** CPF NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.

       LER-ARQCLI.
           READ ARQCLI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
               DISPLAY TELAPRINCIPAL
               PERFORM R4A THRU R4B
               MOVE "*** CLIENTE CADASTRAD0 ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACE-001
             ELSE
              MOVE "*** ERRO NA LEITURA ARQUIVO DE CLIENTES ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
           ELSE
               MOVE "*** CLIENTE NAO CADASTRAD0 ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R2.
           ACCEPT TCLI-NOME.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R1.
           IF CLI-NOME = SPACES
             MOVE "*** NOME NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R2.

       R3.
           ACCEPT TCLI-APELIDO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R2.
           IF CLI-APELIDO = SPACES
             MOVE " *** APELIDO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R3.

       R4.
           ACCEPT TCLI-CEP
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             DISPLAY TELAPRINCIPAL
             GO TO R3.
           IF CLI-CEP = 0
             MOVE "*** CEP NÃO INFORMADO  ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R4.

       R4A.
           MOVE CLI-CEP TO CEP-NUMCEP.
           READ ARQCEP
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "23"
               MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R8
             ELSE
               MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
           ELSE
               MOVE 1 TO IND.
               DISPLAY TELAPRINCIPAL.

       R4B.
           MOVE 1 TO IND.

       R4C.
           MOVE TABUF (IND) TO TXUF
           IF CEP-UF NOT = UF-SIGLA
             ADD 1 TO IND
               IF IND > 27
                 MOVE "UF NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
              ELSE
                 GO TO R4C
              ELSE
                 DISPLAY TUF-TEXTO.

       R5.
           ACCEPT TCLI-NUMERO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R4.
           IF CLI-NUMERO = SPACES
             MOVE " *** NUMERO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R5.

       R6.
           ACCEPT TCLI-COMPLEMENTO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R5.
           IF CLI-COMPLEMENTO = SPACES
            MOVE " *** COMPLEMENTO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R6.

       R7.
           ACCEPT TCLI-TELEFONE.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R6.
           IF CLI-TELEFONE = SPACES
             MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R7.

       R8.
           ACCEPT TCLI-EMAIL.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R7.
           IF CLI-EMAIL = SPACES
             MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R8.

       R9.
           ACCEPT TCLI-CONTATO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R8.
           IF CLI-CONTATO = SPACES
             MOVE " *** CONTATO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R9.

           IF WS-SEL = 1
             GO TO ALT-OPC.

       INC-OPC.
           MOVE "S" TO WS-OPCAO
           DISPLAY (22, 14) "Deseja Salvar (S/N) : ".
           ACCEPT (22, 36) WS-OPCAO WITH UPDATE
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R1.
           IF WS-OPCAO = "N" OR "n"
             MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO INC-OPC.

       INC-WR1.
           WRITE REGCLI
           IF ST-ERRO = "00" OR ST-ERRO = "02"
             MOVE "*** DADOS GRAVADOS *** " TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* AMIGO JA EXISTE, DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
             MOVE "*** ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO ***"
                                                           TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ROT-FIM.


      *********** ROTINAS DE CONSULTA / ALTERAÇÃO / EXCLUSÃO ******************

       ACE-001.
           DISPLAY (22, 14)
           "N = NOVO REGISTRO | A = ALTERAR | E = EXCLUIR | S = SAIR:"
                ACCEPT (22, 72) WS-OPCAO
                IF WS-OPCAO NOT = "N" AND WS-OPCAO NOT = "A"
                AND WS-OPCAO NOT = "E" AND WS-OPCAO NOT = "S"
                  GO TO ACE-001.
                  MOVE SPACES TO MENS
                  DISPLAY (20, 18) MENS
                IF WS-OPCAO = "N"
                  GO TO R1
                ELSE
                  IF WS-OPCAO = "A"
                    MOVE 1 TO WS-SEL
                    GO TO R3
                ELSE
                  IF WS-OPCAO = "S"
                    MOVE 1 TO WS-SEL
                    GO TO ROT-FIM.

       EXC-OPC.
           DISPLAY (22, 14) "Deseja EXCLUIR? (S/N) : ".
           ACCEPT (22, 39) WS-OPCAO
           IF WS-OPCAO = "N" OR "n"
             MOVE "*** REGISTRO MANTIDO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "*** DIGITE APENAS S = SIM OU N = NAO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO EXC-OPC.

       EXC-DL1.
           DELETE ARQCLI RECORD
           IF ST-ERRO = "00" OR ST-ERRO = "02"
             MOVE "*** REGISTRO CLIENTE EXCLUIDO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

       ALT-OPC.
           DISPLAY (22, 14) "Deseja ALTERAR? (S/N) : ".
           ACCEPT (22, 39) WS-OPCAO
           ACCEPT WS-ACT FROM ESCAPE KEY

           IF WS-OPCAO = "N" OR "n"
             MOVE "*** INFORMACOES MANTIDAS *** " TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.

           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "*** DIGITE APENAS S = SIM OU N = NAO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ALT-OPC.

       ALT-RW1.
           REWRITE REGCLI
           IF ST-ERRO = "00" OR ST-ERRO = "02"
             MOVE "*** REGISTRO ALTERADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.

           MOVE "*** ERRO NA ALT DO REGISTRO DE CLIENTE ***" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

      ***************************************************************

       ROT-FIM.
           CLOSE ARQCLI ARQCEP.
           STOP RUN.

      *********************** ROTINA DE MENSAGEM ********************

       ROT-MENS.
           MOVE ZEROS TO WS-CONT.
       ROT-MENS1.
           DISPLAY (22, 14) MENS.
       ROT-MENS2.
           ADD 1 TO WS-CONT
           IF WS-CONT < 3000
             GO TO ROT-MENS2
           ELSE
             MOVE SPACES TO MENS
             DISPLAY (22, 14) MENS.
       ROT-MENS-FIM.
             EXIT.
       FIM-ROT-TEMPO.

      ***************************** STATUS ************************************

      *    00 = OPERAÇÃO REALIZADA COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NAO CARREGADO
      *    10 = FIM DA LEITURA ARQUIVO SEQUENCIAL

      *************************************************************************
