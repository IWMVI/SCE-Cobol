       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE004.
      *************************************************************************
      * INSTALLATION.       CADASTRO DE PRODUTOS                              *
      * AUTHOR.             Wallace Martins Vieira                            *
      * DATE-WRITTEN        00/00/0000                                        *
      *************************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
             
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT ARQPRO ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS PRO-CODIGO
                      ALTERNATE RECORD KEY IS PRO-DESCRICAO 
                                            WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
                      
           SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS FOR-CPFCNPJ
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS FOR-CODIGO
                                   WITH DUPLICATES.

      *************************************************************************
      
       DATA DIVISION.
       FILE SECTION.
       FD ARQPRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".

       01 REGPRO.
        03 CHAVE1.
         05 PRO-CODIGO             PIC 9(06).
        03 PRO-DESCRICAO           PIC X(30).
        03 PRO-UNIDADE             PIC X(02).
        03 PRO-TIPOPRODUTO         PIC 9(01).
        03 PRO-PRECO               PIC 9(06).
        03 PRO-DATAULTIMA          PIC 9(08).
        03 PRO-QTDESTOQUE          PIC 9(06).
        03 PRO-CPFCNPJ             PIC 9(14).

      *************************************************************************

       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".

       01 REGFOR.
        03 FOR-CODIGO              PIC 9(06).
        03 FOR-TIPOFORNECEDOR      PIC X(01).
        03 FOR-CPFCNPJ             PIC 9(14).
        03 FOR-NOME                PIC X(30).
        03 FOR-APELIDO             PIC X(30).
        03 FOR-CEP                 PIC 9(08).
        03 FOR-LOGRADOURO          PIC X(24).
        03 FOR-NUMERO              PIC X(11).
        03 FOR-COMPLEMENTO         PIC X(24).
        03 FOR-BAIRRO              PIC X(40).
        03 FOR-CIDADE              PIC X(24).
        03 FOR-ESTADO              PIC X(02).
        03 FOR-TELEFONE            PIC 9(11).
        03 FOR-EMAIL               PIC X(33).
        03 FOR-CONTATO             PIC X(32).

      *************************************************************************
      
       WORKING-STORAGE SECTION.
       
       01 WS-OPCAO            PIC X(01) VALUE SPACES.
       01 WS-ACT              PIC 9(02) VALUE ZEROS.
       01 ST-ERRO            PIC X(02) VALUE "00".
       01 DPRO-TIPOPRODUTO   PIC X(20) VALUE SPACES.
       01 DPRO-UNIDADE       PIC X(20) VALUE SPACES.
       01 WS-CONT             PIC 9(06) VALUE ZEROS.
       01 MENS               PIC X(50) VALUE SPACES.
       01 WS-SEL              PIC 9(01) VALUE ZEROS.
       01 IND                PIC 9(02) VALUE ZEROS.
       01 TABTIPOPRODUTO.
          03 TBTIPOPRODUTO   PIC X(30) OCCURS 10 TIMES.

      *************************************************************************

        SCREEN SECTION.
        
       01 TELAPRINCIPAL.
           05 LINE 01 COLUMN 01 
              VALUE "========================================".
           05 LINE 01 COLUMN 41 
              VALUE "========================================".
           05 LINE 02 COLUMN 01 
              VALUE "                              CADASTRO D".
           05 LINE 02 COLUMN 41 
              VALUE "E PRODUTOS".
           05 LINE 03 COLUMN 01 
              VALUE "========================================".
           05 LINE 03 COLUMN 41 
              VALUE "========================================".
           05 LINE 05 COLUMN 01 
              VALUE "  CODIGO             :".
           05 LINE 06 COLUMN 01 
              VALUE "  PRODUTO            :".
           05 LINE 07 COLUMN 01 
              VALUE "  MEDIDA             :    -".
           05 LINE 08 COLUMN 01 
              VALUE "  TIPO DE PRODUTO    :    -".
           05 LINE 09 COLUMN 01 
              VALUE "  ULTIMO PRECO       :".
           05 LINE 10 COLUMN 01 
              VALUE "  DATA ULTIMA COMPRA :".
           05 LINE 11 COLUMN 01 
              VALUE "  QUANTIDADE ESTOQUE :".
           05 LINE 13 COLUMN 01 
              VALUE "========================================".
           05 LINE 13 COLUMN 41 
              VALUE "========================================".
           05 LINE 14 COLUMN 01 
              VALUE "                            INFORMACOES".
           05 LINE 14 COLUMN 41 
              VALUE "FORNECEDOR".
           05 LINE 15 COLUMN 01 
              VALUE "========================================".
           05 LINE 15 COLUMN 41 
              VALUE "========================================".
           05 LINE 17 COLUMN 01 
              VALUE "  CPF / CNPJ :".
           05 LINE 17 COLUMN 41 
              VALUE "FN :".
           05 LINE 19 COLUMN 01 
              VALUE "========================================".
           05 LINE 19 COLUMN 41 
              VALUE "========================================".
           05 LINE 21 COLUMN 01 
              VALUE "  AVISOS :".
           05 LINE 23 COLUMN 01 
              VALUE "========================================".
           05 LINE 23 COLUMN 41 
              VALUE "========================================".

           05 LINE 05 COLUMN 24
              VALUE " :".
           05 LINE 06 COLUMN 24
              VALUE " :".
           05 LINE 07 COLUMN 24
              VALUE " :".
           05 LINE 08 COLUMN 24
              VALUE " :".
           05 LINE 09 COLUMN 24
              VALUE " :".
           05 LINE 10 COLUMN 24
              VALUE " :".
           05 LINE 11 COLUMN 24
              VALUE " :".
           05 LINE 17 COLUMN 54
              VALUE " :".

           05 TPRO-CODIGO
              LINE 05 COLUMN 25 PIC 9(06)
              USING PRO-CODIGO.
           05 TPRO-DESCRICAO
              LINE 06 COLUMN 25 PIC X(30)
              USING PRO-DESCRICAO.
           05 TPRO-UNIDADE
              LINE 07 COLUMN 25 PIC X(02)
              USING PRO-UNIDADE.
           05 TDPRO-UNIDADE
              LINE 07 COLUMN 28 PIC X(15)
              USING DPRO-UNIDADE.
           05 TPRO-TIPOPRODUTO
              LINE 08 COLUMN 25 PIC 9(01)
              USING PRO-TIPOPRODUTO.
           05 TDPRO-TIPOPRODUTO
              LINE 08 COLUMN 28 PIC X(15)
              USING DPRO-TIPOPRODUTO.
           05 TPRO-PRECO
              LINE 09 COLUMN 25 PIC 9(6)V99
              USING PRO-PRECO.
           05 TPRO-DATAULTIMA
              LINE 10 COLUMN 25 PIC XX/XX/XXXX
              USING PRO-DATAULTIMA.
           05 TPRO-QTDESTOQUE
              LINE 11 COLUMN 25 PIC 9(06)
              USING PRO-QTDESTOQUE.
           05 TPRO-CPFCNPJ
              LINE 17 COLUMN 15 PIC 9(14)
              USING PRO-CPFCNPJ.
           05 TFOR-NOME
              LINE 17 COLUMN 45 PIC X(20)
              USING FOR-NOME.


        01 TELATIPOPRODUTO.
        
           05 LINE 20 COLUMN 01
              VALUE "          1-SAUDE                     2-".
           05 LINE 20 COLUMN 41
              VALUE "ALIMENTO          3-AGROPECUARIA        ".
           05 LINE 21 COLUMN 01
              VALUE "          4-PET                       5-".
           05 LINE 21 COLUMN 41
              VALUE "AAAAAAAA          6-BBBBBBBBBBBB       ".
           05 LINE 22 COLUMN 01
              VALUE "          7-CCCCCCCC                  8-".
           05 LINE 22 COLUMN 41
              VALUE "DDDDDDDD          9-FFFFFFFFFFFF       ".
           05 LINE 23 COLUMN 41
              VALUE "                                       ".
           05 LINE 24 COLUMN 01
              VALUE "========================================".
           05 LINE 24 COLUMN 41
              VALUE "=========================================".

        01 TELAUNIDADE.

           05 LINE 18 COLUMN 01
              VALUE "|".
           05 LINE 19 COLUMN 01
              VALUE "|".
           05 LINE 19 COLUMN 41
              VALUE "                           |".
           05 LINE 20 COLUMN 01
              VALUE "|         1-CX                   2-".
           05 LINE 20 COLUMN 41
              VALUE "UN                   3-FD                |".
           05 LINE 21 COLUMN 01
              VALUE "|         4-PCT                  5-".
           05 LINE 21 COLUMN 41
              VALUE "LT                   6-MT                |".
           05 LINE 22 COLUMN 01
              VALUE "|         7-GR                   8-".
           05 LINE 22 COLUMN 41
              VALUE "KG                   9-ML                |".
           05 LINE 23 COLUMN 01
              VALUE "|".
           05 LINE 23 COLUMN 41
              VALUE "                           |".
           05 LINE 24 COLUMN 01
              VALUE "|---------------------------------------".
           05 LINE 24 COLUMN 41
              VALUE "---------------------------------------|".


       01  TELAUNIDADE.

           05  LINE 18  COLUMN 01
               VALUE  "|".
           05  LINE 19  COLUMN 01
               VALUE  "|".
           05  LINE 19  COLUMN 41
               VALUE  "                                       |".
           05  LINE 20  COLUMN 01
               VALUE  "|                                 PC-PEC".
           05  LINE 20  COLUMN 41
               VALUE  "A                                      |".
           05  LINE 21  COLUMN 01
               VALUE  "|                                 LT-LIT".
           05  LINE 21  COLUMN 41
               VALUE  "RO                                     |".
           05  LINE 22  COLUMN 01
               VALUE  "|                                 KL-KIL".
           05  LINE 22  COLUMN 41
               VALUE  "O                                      |".
           05  LINE 23  COLUMN 01
               VALUE  "|                                 TN-TON".
           05  LINE 23  COLUMN 41
               VALUE  "ELADA                                  |".
           05  LINE 24  COLUMN 01
               VALUE  "|---------------------------------------".
           05  LINE 24  COLUMN 41
               VALUE  "---------------------------------------|".

       01  TELALIMPAR.
           05  LINE 19  COLUMN 01
               VALUE  "                                              ".
           05  LINE 19  COLUMN 41
               VALUE  "                                              ".
           05  LINE 20  COLUMN 01
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41
               VALUE  "                                               ".
           05  LINE 21  COLUMN 01
               VALUE  "                                               ".
           05  LINE 21  COLUMN 41
               VALUE  "                                              ".
           05  LINE 22  COLUMN 01
               VALUE  "                                               ".
           05  LINE 22  COLUMN 41
               VALUE  "                                               ".
           05  LINE 23  COLUMN 01
               VALUE  "                                               ".
           05  LINE 23  COLUMN 41
               VALUE  "                                               ".
           05  LINE 24  COLUMN 01
               VALUE  "                                               ".
           05  LINE 24  COLUMN 41
               VALUE  "                                               ".

      *************************************************************************
      
       PROCEDURE DIVISION.

       INICIO.
            MOVE "SAUDE"                         TO TBTIPOPRODUTO(1)
            MOVE "ALIMENTO"                      TO TBTIPOPRODUTO(2)
            MOVE "VESTUARIO"                     TO TBTIPOPRODUTO(3)
            MOVE "AGROPECUARIA"                  TO TBTIPOPRODUTO(4)
            MOVE "PET"                           TO TBTIPOPRODUTO(5)
            MOVE "AAAAAAAAAAAA"                  TO TBTIPOPRODUTO(6)
            MOVE "BBBBBBBBBBBB"                  TO TBTIPOPRODUTO(7)
            MOVE "CCCCCCCCCCCC"                  TO TBTIPOPRODUTO(8)
            MOVE "DDDDDDDDDDDD"                  TO TBTIPOPRODUTO(9)
            MOVE "EEEEEEEEEEEE"                  TO TBTIPOPRODUTO(10).
            
      ************************ ABERTURA ARQUIVO *******************************
       
       R0.    
           OPEN I-O ARQPRO
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
               OPEN OUTPUT ARQPRO
               MOVE "CRIANDO ARQUIVO CADASTRO DE FUNCIONARIOS " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               CLOSE ARQPRO
               GO TO INICIO
             ELSE
               IF ST-ERRO = "95"
                 MOVE "ISAM NAO CARREGADO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
             MOVE "ERRO NA ABERTURA DO ARQUIVO CDAMIGOS" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQFOR
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
               MOVE "*** ARQUIVO DE FORNECEDORES NAO ENCONTRADO **" 
                                                            TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
             ELSE
               MOVE "ERRO NA ABERTURA DO ARQUIVO DE FORNECEDORES " 
                                                           TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.

      ******************** INCICIALIZAÇÃO VARIÁVEIS ***************************

       R1.
           MOVE SPACES TO PRO-DESCRICAO PRO-UNIDADE DPRO-TIPOPRODUTO
                          DPRO-UNIDADE

           MOVE ZEROS  TO PRO-CODIGO PRO-TIPOPRODUTO PRO-PRECO
                          PRO-DATAULTIMA PRO-QTDESTOQUE PRO-CPFCNPJ

           MOVE ZEROS  TO FOR-CPFCNPJ FOR-TELEFONE FOR-CEP FOR-CODIGO

           MOVE SPACES TO FOR-TIPOFORNECEDOR FOR-NOME FOR-APELIDO
                          FOR-LOGRADOURO FOR-NUMERO FOR-COMPLEMENTO
                          FOR-BAIRRO FOR-CIDADE FOR-ESTADO 
                          FOR-EMAIL FOR-CONTATO.


      ********************** VISUALIZAÇÃO DA TELA *****************************

           DISPLAY TELAPRINCIPAL.

      ************************ ENTRADA DE DADOS *******************************

       R2.
           ACCEPT TPRO-CODIGO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             CLOSE ARQPRO
             STOP RUN.
           IF PRO-CODIGO = SPACES
             MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R2.
       
       LER-ARQPRO.
           READ ARQPRO
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
               PERFORM R5A
               PERFORM R4A
               PERFORM R9A THRU R9B
               MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ACE-001
             ELSE
               MOVE "ERRO NA LEITURA ARQUIVO PRODUTOS" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
           ELSE
             MOVE "*** PRODUTO NAO CADASTRAD0 ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM.
       
       R3.
           ACCEPT TPRO-DESCRICAO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R2.
           IF PRO-DESCRICAO = SPACES
             MOVE "DESCRICAO NAO PODE FICAR EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R3.
       
       R4.
           DISPLAY TELAUNIDADE.
           ACCEPT TPRO-UNIDADE.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R3.
           IF PRO-UNIDADE = SPACES
             MOVE "UNIDADE NAO PODE FICAR EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R4.
       
       R4A.
           IF PRO-UNIDADE = "kl" OR "KL" OR "Kl" OR "kL"
             MOVE "KILO" TO DPRO-UNIDADE
             MOVE "KL" TO PRO-UNIDADE
           ELSE
             IF PRO-UNIDADE = "PC" OR "pc" OR "Pc" OR "pC"
               MOVE "PECA" TO DPRO-UNIDADE
               MOVE "PC" TO PRO-UNIDADE
             ELSE
       
           IF PRO-UNIDADE = "LT" OR "lt" OR "Lt" OR "lT"
             MOVE "LITRO" TO DPRO-UNIDADE
             MOVE "LT" TO PRO-UNIDADE
           ELSE
             IF PRO-UNIDADE = "TN" OR "tN" OR "tn" OR "Tn"
               MOVE "TONELADA" TO DPRO-UNIDADE
               MOVE "TN" TO PRO-UNIDADE
             ELSE
               MOVE " Informe uma Opcao Valida" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R4.
      
           DISPLAY TDPRO-UNIDADE.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.

       R5.
           DISPLAY TELATIPOPRODUTO
           ACCEPT TPRO-TIPOPRODUTO.
           IF WS-ACT = 01
             GO TO R4
           IF PRO-TIPOPRODUTO = 0 OR > 10
             MOVE "*** DIGITE APENAS DE 1 ATE 10 ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R5.
           MOVE TBTIPOPRODUTO(PRO-TIPOPRODUTO) TO DPRO-TIPOPRODUTO
           DISPLAY TDPRO-TIPOPRODUTO.

       R5A.
           MOVE TBTIPOPRODUTO(PRO-TIPOPRODUTO) TO DPRO-TIPOPRODUTO
           DISPLAY TDPRO-TIPOPRODUTO.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.

       R6.
           ACCEPT TPRO-PRECO.
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R5.
           IF PRO-PRECO = SPACES
             MOVE " *** PRECO NAO PODE FICAR EM BRANCO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R6.

       R7.
           ACCEPT TPRO-DATAULTIMA
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             GO TO R6.
           IF PRO-DATAULTIMA = 0
             MOVE "*** DATA NAO PODE FICAR EM BRANCO***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R7.

       R8.
           ACCEPT TPRO-QTDESTOQUE
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             DISPLAY TELAPRINCIPAL
             GO TO R7.
           IF PRO-QTDESTOQUE = 0
             MOVE "*** QTD ESTOQUE NAO INFORMADA ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R8.
       
       R9.
           ACCEPT TPRO-CPFCNPJ
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01
             DISPLAY TELAPRINCIPAL
             GO TO R7.
           IF PRO-CPFCNPJ = 0
             MOVE "*** CPF/CNPJ FORNECEDOR NAO INFORMADA ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R8.

       R9A.
           MOVE PRO-CPFCNPJ TO FOR-CPFCNPJ.
           READ ARQFOR
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "23"
               MOVE "*** FORNECEDOR DIGITADO NAO ENCONTRADO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R8
             ELSE
               MOVE "ERRO NA LEITURA ARQUIVO FORNECEDORES" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM
           ELSE
             MOVE 1 TO IND.
             DISPLAY TELAPRINCIPAL.
       
       R9B.
           MOVE 1 TO IND.
           IF WS-SEL = 1
           GO TO ALT-OPC.
     
       INC-OPC.
           MOVE "S" TO WS-OPCAO
           DISPLAY (21, 30) "Deseja Salvar (S/N) : ".
           ACCEPT (21, 55) WS-OPCAO WITH UPDATE
           ACCEPT WS-ACT FROM ESCAPE KEY
           IF WS-ACT = 01 GO TO R1.
           IF WS-OPCAO = "N" OR "n"
             MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO INC-OPC.
      
       INC-WR1.
           WRITE REGPRO
           IF ST-ERRO = "00" OR "02"
             MOVE "*** DADOS GRAVADOS *** " TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* PRODUTO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
             MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ROT-FIM.

      *********** ROTINAS DE CONSULTA / ALTERAÇÃO / EXCLUSÃO ******************
      
       ACE-001.
           DISPLAY (21, 18)
                     "N=NOVO REGISTRO | A=ALTERAR | E=EXCLUIR | S=SAIR:"
           ACCEPT (21, 67) WS-OPCAO
           IF WS-OPCAO NOT = "N" AND WS-OPCAO NOT = "A"
           AND WS-OPCAO NOT = "E" AND WS-OPCAO NOT = "S"
           GO TO ACE-001.
           MOVE SPACES TO MENS
           DISPLAY (21, 18) MENS
                
           IF WS-OPCAO = "N" OR "n"
             GO TO R1
           ELSE
             IF WS-OPCAO = "A" OR "a"
               MOVE 1 TO WS-SEL
               GO TO R3
             ELSE
               IF WS-OPCAO = "S" OR "s"
                 MOVE 1 TO WS-SEL
                 GO TO ROT-FIM.

       EXC-OPC.
           DISPLAY (21, 30) "Deseja EXCLUIR   (S/N) : ".
           ACCEPT (21, 55) WS-OPCAO
           IF WS-OPCAO = "N" OR "n"
             MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO EXC-OPC.
      
       EXC-DL1.
           DELETE ARQPRO RECORD
           IF ST-ERRO = "00"
             MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

       ALT-OPC.
           DISPLAY (21, 30) "Deseja ALTERAR  (S/N) : ".
           ACCEPT (21, 55) WS-OPCAO
           ACCEPT WS-ACT FROM ESCAPE KEY

           IF WS-OPCAO = "N" OR "n"
             MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.

           IF WS-OPCAO NOT = "S" AND "s"
             MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ALT-OPC.

       ALT-RW1.
           REWRITE REGPRO
           IF ST-ERRO = "00" OR "02"
             MOVE "*** REGISTRO ALTERADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1.
             DISPLAY TELALIMPAR.
           
           MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

      ***************************************************************
      
       ROT-FIM.
           CLOSE ARQPRO ARQFOR.
           STOP RUN.

      *********************** ROTINA DE MENSAGEM ********************
      
       ROT-MENS.
           MOVE ZEROS TO WS-CONT.
       ROT-MENS1.
           DISPLAY (21, 14) MENS.
       ROT-MENS2.
           ADD 1 TO WS-CONT
           IF WS-CONT < 3000
             GO TO ROT-MENS2
           ELSE
             MOVE SPACES TO MENS
             DISPLAY (21, 14) MENS.
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
