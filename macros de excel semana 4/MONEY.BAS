DECLARE SUB Creceber ()
DECLARE SUB Fluxo ()
DECLARE SUB Cpagar ()
DECLARE SUB DigTrans ()
DEFINT A-Z
DECLARE SUB Empenho () : DECLARE SUB ScrolDown ()
DECLARE SUB ScrolUp () : DECLARE SUB AlterCad ()
DECLARE SUB CadClie () : DECLARE SUB ListClie ()
DECLARE SUB LCenter (text$) : DECLARE SUB ScrollUp ()
DECLARE SUB ScrollDown () : DECLARE SUB Initialize ()
DECLARE SUB Intro () : DECLARE SUB SparklePause ()
DECLARE SUB Center (row%, text$) : DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB LoadState () : DECLARE SUB SaveState ()
DECLARE SUB MenuSystem () : DECLARE SUB Box (Row1%, Col1%, Row2%, Col2%)
DECLARE SUB EditAccounts () : DECLARE SUB PrintHelpLine (help$)
DECLARE FUNCTION Menu% (CurrChoiceX%, MaxChoice%, choice$(), ItemRow%(), ItemCol%(), help$(), BarMode%)
DECLARE FUNCTION GetString$ (row%, Col%, Start$, End$, Vis%, Max%)
DECLARE FUNCTION Trim$ (X$)
DECLARE FUNCTION OrdemData$ (Mens$)
COMMON SHARED LIM%, KeyFlags, Clie$, Din$, Dfi$, Menor$, Maior$
'Constants
CONST True = -1
CONST False = NOT True
'User-defined types
TYPE Controle
 Vl AS STRING * 11
 Mx AS STRING * 5
 Iob AS DOUBLE
END TYPE
TYPE Ckval
 Vl AS STRING * 11
 Mx AS INTEGER
 Ex AS INTEGER
END TYPE
TYPE FORNECEDOR
  N AS STRING * 50
  E AS STRING * 50
  B AS STRING * 20
  F AS STRING * 13
  c AS STRING * 30
  P AS STRING * 10
  u AS STRING * 2
  G AS STRING * 20
  I AS STRING * 15
  K AS STRING * 15
  o AS STRING * 13
  T AS STRING * 1
  R AS STRING * 5
END TYPE
TYPE recordType
 Rg    AS STRING * 5
 Dt     AS STRING * 8
 Rf     AS STRING * 10
 Dsc    AS STRING * 50
 Fg1    AS DOUBLE
 Fg2    AS DOUBLE
END TYPE
DIM SHARED Fo AS FORNECEDOR, Kword AS recordType, limm AS Ckval
DIM SHARED Crg AS Controle
DIM SHARED ColorPref                            'Color Preference
DIM SHARED colors(0 TO 20, 1 TO 4)              'Different Colors
DIM SHARED PrintErr AS INTEGER                  'Printer error flag
DIM SHARED Linha%(1 TO 13)
DIM SHARED ScrollUpAsm(1 TO 7)                  'Assembly Language Routines
DIM SHARED ScrollDownAsm(1 TO 7)
DIM SHARED ScrolUpAsm(1 TO 7)                  'Assembly Language Routines
DIM SHARED ScrolDownAsm(1 TO 7)
ON ERROR GOTO ErrorTrap
OPEN "money.dat" FOR INPUT AS #1
'LPRINT CHR$(27); "t"; CHR$(1);
'LPRINT CHR$(27); CHR$(14);
CLOSE
ON ERROR GOTO 0     'Reset error handler
 Linha%(1) = 15: Linha%(2) = 15: Linha%(13) = 15
 Linha%(3) = 16: Linha%(4) = 17: Linha%(5) = 18
 Linha%(6) = 19: Linha%(7) = 20: Linha%(8) = 20: Linha%(9) = 21
 Linha%(10) = 22: Linha%(11) = 23: Linha%(12) = 24
 Initialize          'Initialize program
 Intro               'Display introduction screen
 MenuSystem          'This is the main program
 COLOR 7, 0          'Clear screen and end
 CLS
 CLOSE
 SYSTEM
 ' Error handler for program
 ' If data file not found, create and initialize a new one.
ErrorTrap:
 SELECT CASE ERR
  ' If data file not found, create and initialize a new one.
  CASE 53
   CLOSE
   ColorPref = 1
   SaveState
   RESUME
  CASE 24, 25
   PrintErr = True
   Box 8, 13, 14, 69
   Center 11, "Printer not responding ... Press Space to continue"
   WHILE INKEY$ <> "": WEND
    WHILE INKEY$ <> " ": WEND
     RESUME NEXT
  CASE ELSE
 END SELECT
 RESUME NEXT
'The following data defines the color schemes available via the main menu.
'
'    scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0
'The following data is actually a machine language program to
'scroll the screen up or down very fast using a BIOS call.
DATA &HB8,&H01,&H06,&HB9,&H0A,&H03,&HBA,&H3C,&H0C,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H0A,&H03,&HBA,&H3C,&H0C,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H06,&HB9,&H01,&H04,&HBA,&H4E,&H15,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H01,&H04,&HBA,&H4E,&H15,&HB7,&H00,&HCD,&H10,&HCB

SUB AlterCad
 REDIM help$(13), Col(13), Vis(13), Max(13), Edit$(13)
 OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
 GET #1, 1, limm
 IF limm.Vl = "THISISVALID" THEN
  MaxRecord = limm.Mx: Ctrl = limm.Ex
  COLOR colors(7, ColorPref), colors(4, ColorPref)
  CLS
  Box 1, 10, 14, 61  'Monta as bordas da tela
  Box 14, 65, 16, 67 'Monta a caixa de da Letra Do Tipo De Cadastro
  Box 14, 68, 16, 74 'Monta a Caixa de C¢digo
  Box 2, 62, 4, 69   'Monta a Caixa de instru‡Æo 1 CHR$(24); CHR$(25)
  Box 17, 65, 19, 72 'Monta Caixa de instru‡Æo 2 CHR$(27); CHR$(26)
  LOCATE 3, 63: PRINT "Use:"; CHR$(24); CHR$(25)
  LOCATE 18, 66: PRINT "Use:"; CHR$(27); CHR$(26)
  dgto13$ = "543298765432"
  dgto14$ = "6543298765432"
  COLOR colors(7, ColorPref), colors(4, ColorPref)
  LOCATE 2, 10: PRINT "³  Alterar Cadastro                                ³"
  LOCATE 3, 10: PRINT "³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³"
                 u$ = "³\                                                \³"
                u1$ = "³                                                  ³"
               U1x$ = "³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³"
  LOCATE 15, 2: PRINT "Nome:"; STRING$(50, "_")
  LOCATE 16, 2: PRINT "Endereco:"; STRING$(50, "_")
  LOCATE 17, 2: PRINT "Bairro:____________________"
  LOCATE 18, 2: PRINT "Telefone:_____________"
  LOCATE 19, 2: PRINT "Cidade:______________________________"
  LOCATE 20, 2: PRINT "Cep:__________ Uf:__"
  LOCATE 21, 2: PRINT "Cgc:____________________"
  LOCATE 22, 2: PRINT "Insc.Est.:_______________"
  LOCATE 23, 2: PRINT "Contato:_______________"
  LOCATE 24, 2: PRINT "Telefone:_____________"
  'Initialize variables
  help$(1) = "  Nome do cliente ": help$(2) = " (C)liente - (F)ornecedor - (R)epres. "
  help$(13) = " C¢digo "
  help$(3) = "  Endereco ": help$(4) = "  Bairro ": help$(5) = "  Telefone "
  help$(6) = "  Cidade ": help$(7) = "  Cep    ": help$(8) = "  Unidade Federal "
  help$(9) = " Cgc/Cnpj  ": help$(10) = "  Inscri‡Æo Estadual ": help$(11) = "  Contato    "
  help$(12) = "  Telefone do Contato "
  Col(1) = 7: Col(2) = 66: Col(13) = 69
  Col(3) = 11: Col(4) = 9: Col(5) = 11: Col(6) = 9
  Col(7) = 6: Col(8) = 20: Col(9) = 6: Col(10) = 12: Col(11) = 10
  Col(12) = 11
  Vis(1) = 50: Vis(2) = 1: Vis(13) = 5
  Vis(3) = 50: Vis(4) = 20: Vis(5) = 13: Vis(6) = 30: Vis(7) = 10: Vis(8) = 2
  Vis(9) = 20: Vis(10) = 15: Vis(11) = 15: Vis(12) = 13
  FOR Wsx = 1 TO 13: Max(Wsx) = Vis(Wsx): NEXT Wsx
  FOR Y% = 1 TO 13: LOCATE Linha%(Y%), Col(Y%): PRINT Edit$(Y%): NEXT Y%
  Finished = False: CROW = 1: CurrCol = CROW
  CurrTopLine = 1: CurrRow = 1
  GOSUB AditTransPrintWholeScreen
  CurrTopLine = 1: CurrRow = 1
  PrintHelpLine help$(CROW)
  GOSUB AditTransGetLine
  DO
   GOSUB AditTransShowCursor                              'Show Cursor
   DO                                      'Wait for key
    Kbd$ = INKEY$
   LOOP UNTIL Kbd$ <> ""
   IF Kbd$ >= " " AND Kbd$ < "þ" THEN      'If legal, edit item
    GOSUB AditTransEditItem
   END IF
   GOSUB AditTransHideCursor                              'Hide Cursor so it can move
   SELECT CASE Kbd$
    CASE CHR$(0) + "H"                              'Up Arrow
     GOSUB AditTransMoveUp
    CASE CHR$(0) + "P"                               'Down Arrow
     GOSUB AditTransMoveDown
    CASE CHR$(0) + "K"                              'Up Arrow
     CurrCol = (CurrCol + 10) MOD 12 + 1
     PrintHelpLine help$(CurrCol)
    CASE CHR$(0) + "M"                               'Down Arrow
     CurrCol = (CurrCol MOD 12) + 1
     PrintHelpLine help$(CurrCol)
    CASE CHR$(0) + "I"                      'Page Up
     CurrRow = 1
     CurrTopLine = CurrTopLine - 10
     IF CurrTopLine < 1 THEN
      CurrTopLine = 1
     END IF
     GOSUB AditTransPrintWholeScreen
     GOSUB AditTransGetLine
     GOSUB AditTransPrintLine
    CASE CHR$(0) + "Q"                      'Page Down
     CurrRow = 1
     CurrTopLine = CurrTopLine + 10
     IF CurrTopLine > MaxRecord THEN
      CurrTopLine = MaxRecord
     END IF
     GOSUB AditTransPrintWholeScreen
     GOSUB AditTransGetLine
     GOSUB AditTransPrintLine
    CASE CHR$(0) + "S"
     GOSUB AditTransDeleteRecord
    CASE CHR$(27)
     Finished = True
    CASE ELSE
     BEEP
   END SELECT
  LOOP UNTIL Finished
 END IF
 CLOSE #1
EXIT SUB
AditTransShowCursor:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$(1), Vis(1));
 LOCATE Linha%(CurrCol), Col(CurrCol)
 PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
 RETURN
AditTransEditItem:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 ok = False
 Start$ = Kbd$
 DO
  Kbd$ = GetString$(Linha%(CurrCol), Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
  Edit$(CurrCol) = LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol))
  Start$ = ""
  ok = True
  IF CurrCol = 9 THEN
   soma13% = 0
   FOR Ps% = 1 TO 12
    soma13% = soma13% + (VAL(MID$(Edit$(CurrCol), Ps%, 1)) * VAL(MID$(dgto13$, Ps%, 1)))
   NEXT Ps%
   resto13% = soma13% MOD 11
   IF resto13% = 0 OR resto13% = 1 THEN
    dgto13% = 0
   ELSE
    dgto13% = 11 - resto13%
   END IF
   conf13% = VAL(MID$(Edit$(CurrCol), 13, 1))
   soma14% = 0
   FOR Ps% = 1 TO 13
    soma14% = soma14% + (VAL(MID$(Edit$(CurrCol), Ps%, 1)) * VAL(MID$(dgto14$, Ps%, 1)))
   NEXT Ps%
   resto14% = soma14% MOD 11
   IF resto14% = 0 OR resto14% = 1 THEN
    dgto14% = 0
   ELSE
    dgto14% = 11 - resto14%
   END IF
   conf14% = VAL(MID$(Edit$(CurrCol), 14, 1))
   IF conf13% <> dgto13% OR conf14% <> dgto14% THEN
    BEEP: BEEP: BEEP
    PrintHelpLine "CGC Inconsistente - Tecle QQTecla"
    sim$ = INPUT$(1)
   ELSE
    Cgc$ = MID$(Edit$(CurrCol), 1, 2) + "." + MID$(Edit$(CurrCol), 3, 3) + "." + MID$(Edit$(CurrCol), 6, 3) + "/"
    Cgc$ = Cgc$ + MID$(Edit$(CurrCol), 9, 4) + "-" + MID$(Edit$(CurrCol), 13, 2)
    Edit$(CurrCol) = Cgc$
   END IF
  END IF
 LOOP UNTIL ok
 GOSUB AditTransPutLine
 GOSUB AditTransGetLine
 GOSUB AditTransPrintLine
 RETURN
AditTransHideCursor:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$(1), Vis(1));
 LOCATE Linha%(CurrCol), Col(CurrCol)
 PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
 RETURN
AditTransDeleteRecord:
 IF MaxRecord = 1 THEN
  BEEP
 ELSE
  CurrRecord = CurrTopLine + CurrRow - 1
  MaxRecord = MaxRecord - 1
  a = CurrRecord
  WHILE a <= MaxRecord
   GET #1, a + 2, Fo
   PUT #1, a + 1, Fo
   a = a + 1
  WEND
  limm.Vl = "THISISVALID"
  limm.Mx = MaxRecord: limm.Ex = Ctrl
  PUT #1, 1, limm
  GOSUB AditTransPrintWholeScreen
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord > MaxRecord THEN
   GOSUB AditTransMoveUp
  END IF
  GOSUB AditTransGetLine
 END IF
 RETURN
AditTransMoveUp:
 IF CurrRow = 1 THEN
  IF CurrTopLine = 1 THEN
   BEEP
  ELSE
   ScrollDown
   CurrTopLine = CurrTopLine - 1
   GOSUB AditTransGetLine
   GOSUB AditTransPrintLine
  END IF
 ELSE
  CurrRow = CurrRow - 1
  GOSUB AditTransGetLine
  GOSUB AditTransPrintLine
 END IF
 RETURN
AditTransMoveDown:
 IF (CurrRow + CurrTopLine - 1) >= MaxRecord THEN
  BEEP
 ELSE
  IF CurrRow = 10 THEN
   ScrollUp
   CurrTopLine = CurrTopLine + 1
   GOSUB AditTransGetLine
   GOSUB AditTransPrintLine
  ELSE
   CurrRow = CurrRow + 1
   GOSUB AditTransGetLine
   GOSUB AditTransPrintLine
  END IF
 END IF
 RETURN
AditTransPrintLine:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 CurrRecord = CurrTopLine + CurrRow - 1
 LOCATE CurrRow + 3, 10
 IF CurrRecord = MaxRecord + 1 THEN
  PRINT U1x$;
 ELSE
  IF CurrRecord > MaxRecord THEN
   PRINT u1$;
  ELSE
   PRINT USING u$; Edit$(1)
   FOR Y% = 1 TO 13: LOCATE Linha%(Y%), Col(Y%): PRINT Edit$(Y%): NEXT Y%
  END IF
 END IF
RETURN
AditTransPrintWholeScreen:
 temp = CurrRow
 FOR CurrRow = 1 TO 10
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord <= MaxRecord THEN
   GOSUB AditTransGetLine
  END IF
  GOSUB AditTransPrintLine
 NEXT CurrRow
 CurrRow = temp
 RETURN
AditTransPutLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 FOR Ws = 1 TO 13
  Edit$(Ws) = UCASE$(Edit$(Ws))
  Edit$(Ws) = LTRIM$(RTRIM$(Edit$(Ws)))
 NEXT Ws
 IF cfer$ <> Edit$(1) THEN
  IF MaxRecord = 1 THEN
   Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
   Fo.E = Edit$(3): Fo.B = Edit$(4)
   Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
   Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
   Fo.K = Edit$(11): Fo.o = Edit$(12)
   PUT #1, CurrRecord + 1, Fo
  ELSE
   MaxRecord = MaxRecord - 1
   a = CurrRecord
   WHILE a <= MaxRecord
    GET #1, a + 2, Fo
    PUT #1, a + 1, Fo
    a = a + 1
   WEND
   limm.Vl = "THISISVALID"
   limm.Mx = MaxRecord: limm.Ex = Ctrl
   PUT #1, 1, limm
   'REORGANIZAR O ARQUIVO
   Verif$ = Edit$(1): NewPoint = 0: Wsv = 0
   DO
    Wsv = Wsv + 1
    GET #1, Wsv + 1, Fo
    Clasf$ = LTRIM$(RTRIM$(Fo.N))
    IF LEN(Clasf$) <> 0 THEN
     IF LEN(Clasf$) < LEN(Verif$) THEN Menor = LEN(Clasf$) ELSE Menor = LEN(Verif$)
     FOR Xc = 1 TO Menor
      Ps1 = ASC(MID$(Clasf$, Xc, 1)): Ps2 = ASC(MID$(Verif$, Xc, 1))
      IF Ps1 <> Ps2 THEN
       IF Ps2 < Ps1 THEN
        NewPoint = Wsv: Xc = Menor
       ELSE
        Xc = Menor
       END IF
      END IF
     NEXT Xc
    END IF
   LOOP UNTIL NewPoint <> 0 OR Wsv >= MaxRecord
   IF NewPoint <> 0 THEN
    a = MaxRecord
    WHILE a >= NewPoint
     GET #1, a + 1, Fo
     PUT #1, a + 2, Fo
     a = a - 1
    WEND
    MaxRecord = MaxRecord + 1
    Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
    Fo.E = Edit$(3): Fo.B = Edit$(4)
    Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
    Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
    Fo.K = Edit$(11): Fo.o = Edit$(12)
    PUT #1, NewPoint + 1, Fo
    limm.Vl = "THISISVALID": limm.Mx = MaxRecord: limm.Ex = Ctrl
    PUT #1, 1, limm
    CurrRow = ((NewPoint - 1) MOD 10) + 1
    CurrTopLine = NewPoint - CurrRow + 1
   ELSE
    CurrRecord = MaxRecord
    MaxRecord = MaxRecord + 1
    Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
    Fo.E = Edit$(3): Fo.B = Edit$(4)
    Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
    Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
    Fo.K = Edit$(11): Fo.o = Edit$(12)
    PUT #1, MaxRecord + 1, Fo
    limm.Vl = "THISISVALID": limm.Mx = MaxRecord: limm.Ex = Ctrl
    PUT #1, 1, limm
    CurrRow = (CurrRecord MOD 10) + 1
    CurrTopLine = MaxRecord - CurrRow + 1
   END IF
   GOSUB AditTransPrintWholeScreen
  END IF
 ELSE
  Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
  Fo.E = Edit$(3): Fo.B = Edit$(4)
  Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
  Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
  Fo.K = Edit$(11): Fo.o = Edit$(12)
  PUT #1, CurrRecord + 1, Fo
 END IF
 RETURN
AditTransGetLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 GET #1, CurrRecord + 1, Fo
 Edit$(1) = Fo.N
 Edit$(2) = Fo.T: Edit$(13) = Fo.R
 Edit$(3) = Fo.E: Edit$(4) = Fo.B
 Edit$(5) = Fo.F: Edit$(6) = Fo.c: Edit$(7) = Fo.P
 Edit$(8) = Fo.u: Edit$(9) = Fo.G: Edit$(10) = Fo.I
 Edit$(11) = Fo.K: Edit$(12) = Fo.o
 cfer$ = UCASE$(LTRIM$(RTRIM$(Edit$(1))))
RETURN
END SUB

'Box:
'  Draw a box on the screen between the given coordinates.
SUB Box (Row1, Col1, Row2, Col2) STATIC

    BoxWidth = Col2 - Col1 + 1

    LOCATE Row1, Col1
    PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿";

    FOR a = Row1 + 1 TO Row2 - 1
        LOCATE a, Col1
        PRINT "³"; SPACE$(BoxWidth - 2); "³";
    NEXT a

    LOCATE Row2, Col1
    PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù";

END SUB

SUB CadClie
    'Information about each column
    REDIM help$(13), Col(13), Vis(13), Max(13), Edit$(13), Ps(13)
    COLOR colors(7, ColorPref), colors(4, ColorPref)
    Box 2, 1, 24, 80 'Monta as bordas da tela
    Box 3, 2, 15, 64 'Monta as bordas do formulario do fornecedor
    Box 3, 65, 5, 67
    Box 3, 68, 5, 74
    Box 6, 65, 8, 72
    LOCATE 7, 66: PRINT "Use:"; CHR$(24); CHR$(25)
    COLOR colors(5, ColorPref), colors(4, ColorPref)
    LOCATE 1, 1: PRINT SPACE$(80)
    LOCATE 1, 4: PRINT "Editar cliente";
    COLOR colors(7, ColorPref), colors(4, ColorPref)
    LOCATE 4, 3: PRINT "Cliente:"; STRING$(50, "_")
    LOCATE 5, 3: PRINT "Endereco:"; STRING$(50, "_")
    LOCATE 6, 3: PRINT "Bairro:"; STRING$(20, "_")
    LOCATE 7, 3: PRINT "Telefone:"; STRING$(13, "_")
    LOCATE 8, 3: PRINT "Cidade:"; STRING$(30, "_")
    LOCATE 9, 3: PRINT "Cep:"; STRING$(10, "_")
    LOCATE 10, 3: PRINT "Uf:__"
    LOCATE 11, 3: PRINT "Cgc:"; STRING$(20, "_")
    LOCATE 12, 3: PRINT "Insc.Est.:"; STRING$(14, "_")
    LOCATE 13, 3: PRINT "Representante:"; STRING$(15, "_")
    LOCATE 14, 3: PRINT "Telefone:"; STRING$(13, "_")
    help$(1) = " Entre com o registro do cliente "
    PrintHelpLine help$(1)
    'Initialize variables
    help$(1) = "  Nome do cliente                          | <F2=Salva e Volta> <Esc=Volta>   "
    help$(2) = "  (C)liente - (F)ornecedor - (R)epres.     | <F2=Salva e Volta> <Esc=Volta>   "
    help$(13) = "  C¢digo                                   | <F2=Salva e Volta> <Esc=Volta>   "
    help$(3) = "  Endereco/No/Bairro                       | <F2=Salva e Volta> <Esc=Volta>   "
    help$(4) = "  Bairro                                   | <F2=Salva e Volta> <Esc=Volta>   "
    help$(5) = "  Telefone (NNN) NNNNNNN                   | <F2=Salva e Volta> <Esc=Volta>   "
    help$(6) = "  Cidade                                   | <F2=Salva e Volta> <Esc=Volta>   "
    help$(7) = "  Codigo de enderecamento postal (cep)     | <F2=Salva e Volta> <Esc=Volta>   "
    help$(8) = "  Unidade Federal - UF - Estado            | <F2=Salva e Volta> <Esc=Volta>   "
    help$(9) = "  CGC do fornecedor                        | <F2=Salva e Volta> <Esc=Volta>   "
    help$(10) = "  Inscricao Estadual                       | <F2=Salva e Volta> <Esc=Volta>   "
    help$(11) = "  Representante ou Contado                 | <F2=Salva e Volta> <Esc=Volta>   "
    help$(12) = "  Telefone do Representante ou do Contato  | <F2=Salva e Volta> <Esc=Volta>   "
    Col(1) = 11: Col(2) = 66: Col(3) = 12: Col(4) = 10: Col(5) = 12
    Col(6) = 10: Col(7) = 7: Col(8) = 6: Col(9) = 7: Col(10) = 12
    Col(11) = 17: Col(12) = 12: Col(13) = 69
    Vis(1) = 50: Vis(2) = 1:  Vis(3) = 50: Vis(4) = 20: Vis(5) = 13
    Vis(6) = 30: Vis(7) = 10: Vis(8) = 2: Vis(9) = 20: Vis(10) = 15
    Vis(11) = 15: Vis(12) = 13: Vis(13) = 5
    FOR Ws = 1 TO 13
     Max(Ws) = Vis(Ws)
     Edit$(Ws) = STRING$(Max(Ws), " ")
    NEXT Ws
    Ps(1) = 4: Ps(2) = 4:  Ps(3) = 5: Ps(4) = 6: Ps(5) = 7: Ps(6) = 8
    Ps(7) = 9: Ps(8) = 10: Ps(9) = 11: Ps(10) = 12: Ps(11) = 13
    Ps(12) = 14: Ps(13) = 4: dgto13$ = "543298765432": dgto14$ = "6543298765432"
RG2:
    FOR Y% = 1 TO 13: LOCATE Ps(Y%), Col(Y%): PRINT Edit$(Y%): NEXT Y%
    Finished = False: CurrRow = 4
    CurrCol = 1
    PrintHelpLine help$(CurrRow - 3)
    'Loop until F2 or <ESC> is pressed
    DO
     GOSUB Show                              'Show Cursor
     DO                                      'Wait for key
      Kbd$ = INKEY$
     LOOP UNTIL Kbd$ <> ""
     IF Kbd$ >= " " AND Kbd$ < "~" THEN      'If legal, edit item
      GOSUB Edit
     END IF
     GOSUB Hide
     SELECT CASE Kbd$
      CASE CHR$(0) + "H"                              'Up Arrow
       CurrCol = (CurrCol + 10) MOD 12 + 1
       PrintHelpLine help$(CurrCol)
      CASE CHR$(0) + "P"                              'Down Arrow
       CurrCol = (CurrCol MOD 12) + 1
       PrintHelpLine help$(CurrCol)
      CASE CHR$(0) + "<"                              'F2
       Finished = True
       save = True
      CASE CHR$(27)                                   'Esc
       Finished = True
       save = False
      CASE CHR$(13)                                   'Return
      CASE ELSE
       BEEP
     END SELECT
    LOOP UNTIL Finished
    IF save THEN
     GOSUB SaveData
     GOTO RG2
    END IF
   EXIT SUB
Show:
    COLOR colors(8, ColorPref), colors(9, ColorPref)
    LOCATE Ps(CurrCol), Col(CurrCol)
    PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
    RETURN
Edit:
    COLOR colors(8, ColorPref), colors(9, ColorPref)
    ok = False
    Start$ = Kbd$
    DO
     Kbd$ = GetString$(Ps(CurrCol), Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
     Edit$(CurrCol) = LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol))
     Start$ = ""
     ok = True
     IF CurrCol = 9 THEN
      soma13 = 0
      FOR Pw = 1 TO 12
       soma13 = soma13 + (VAL(MID$(Edit$(CurrCol), Pw, 1)) * VAL(MID$(dgto13$, Pw, 1)))
      NEXT Pw
      resto13 = soma13 MOD 11
      IF resto13 = 0 OR resto13 = 1 THEN
      dgto13 = 0
     ELSE
      dgto13 = 11 - resto13
     END IF
     conf13 = VAL(MID$(Edit$(CurrCol), 13, 1))
     soma14 = 0
     FOR Pw = 1 TO 13
      soma14 = soma14 + (VAL(MID$(Edit$(CurrCol), Pw, 1)) * VAL(MID$(dgto14$, Pw, 1)))
     NEXT Pw
     resto14 = soma14 MOD 11
     IF resto14 = 0 OR resto14 = 1 THEN dgto14 = 0 ELSE dgto14 = 11 - resto14
     conf14 = VAL(MID$(Edit$(CurrCol), 14, 1))
     IF conf13 <> dgto13 OR conf14 <> dgto14 THEN
      BEEP: BEEP: BEEP
      PrintHelpLine "CGC Inconsistente - Tecle QQTecla"
      sim$ = INPUT$(1)
     ELSE
      Cgc$ = MID$(Edit$(CurrCol), 1, 2) + "." + MID$(Edit$(CurrCol), 3, 3) + "." + MID$(Edit$(CurrCol), 6, 3) + "/"
      Cgc$ = Cgc$ + MID$(Edit$(CurrCol), 9, 4) + "-" + MID$(Edit$(CurrCol), 13, 2)
      Edit$(CurrCol) = Cgc$
     END IF
    END IF
    LOOP UNTIL ok
    RETURN
Hide:
    COLOR colors(7, ColorPref), colors(4, ColorPref)
    LOCATE Ps(CurrCol), Col(CurrCol)
    PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
    RETURN
SaveData:
 FOR X = 1 TO 13
  Edit$(X) = UCASE$(Edit$(X))
  Edit$(X) = LTRIM$(RTRIM$(Edit$(X)))
 NEXT X
 IF Edit$(1) <> "" THEN
  OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
  GET #1, 1, limm
  IF limm.Vl <> "THISISVALID" THEN
   Edit$(13) = "00001"
   Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
   Fo.E = Edit$(3): Fo.B = Edit$(4)
   Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
   Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
   Fo.K = Edit$(11): Fo.o = Edit$(12)
   PUT #1, 2, Fo
   limm.Vl = "THISISVALID"
   limm.Mx = 1: limm.Ex = 1
   PUT #1, 1, limm
  ELSE
   MaxRecord = limm.Mx: Ctrl = limm.Ex
   Ps = 0: NCL$ = Edit$(1)
   FOR X = 1 TO MaxRecord
    GET #1, X + 1, Fo
    CLL$ = Fo.N: CLL$ = LTRIM$(RTRIM$(CLL$))
    IF NCL$ = CLL$ THEN
     Ps = X: X = MaxRecord
    ELSE
     Qt = LEN(NCL$)
     IF LEN(CLL$) < Qt THEN Qt = LEN(CLL$)
     FOR Xy = 1 TO Qt
      z = ASC(MID$(NCL$, Xy, 1)): c = ASC(MID$(CLL$, Xy, 1))
      IF z <> c THEN
       IF z < c THEN
        Ps = X: Xy = Qt: X = MaxRecord
       ELSE
        Xy = Qt
       END IF
      END IF
     NEXT Xy
    END IF
   NEXT X
   IF Ps <> 0 THEN
    a = MaxRecord
    WHILE a >= Ps
     GET #1, a + 1, Fo
     PUT #1, a + 2, Fo
     a = a - 1
    WEND
    MaxRecord = MaxRecord + 1: Ctrl = Ctrl + 1
    RgAnal$ = STR$(Ctrl): RgAnal$ = LTRIM$(RTRIM$(RgAnal$))
    nal = LEN(RgAnal$)
    IF nal <= 4 THEN RgAnal$ = STRING$(5 - nal, "0") + RgAnal$
    Edit$(13) = RgAnal$
    Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
    Fo.E = Edit$(3): Fo.B = Edit$(4)
    Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
    Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
    Fo.K = Edit$(11): Fo.o = Edit$(12)
    PUT #1, Ps + 1, Fo
    limm.Vl = "THISISVALID": limm.Mx = MaxRecord
    limm.Ex = Ctrl
    PUT #1, 1, limm
   ELSE
    MaxRecord = MaxRecord + 1: Ctrl = Ctrl + 1: Ps = MaxRecord
    RgAnal$ = STR$(Ctrl): RgAnal$ = LTRIM$(RTRIM$(RgAnal$))
    nal = LEN(RgAnal$)
    IF nal <= 4 THEN RgAnal$ = STRING$(5 - nal, "0") + RgAnal$
    Edit$(13) = RgAnal$
    Fo.N = Edit$(1): Fo.T = Edit$(2): Fo.R = Edit$(13)
    Fo.E = Edit$(3): Fo.B = Edit$(4)
    Fo.F = Edit$(5): Fo.c = Edit$(6): Fo.P = Edit$(7)
    Fo.u = Edit$(8): Fo.G = Edit$(9): Fo.I = Edit$(10)
    Fo.K = Edit$(11): Fo.o = Edit$(12)
    PUT #1, Ps + 1, Fo
    limm.Vl = "THISISVALID": limm.Mx = MaxRecord
    limm.Ex = Ctrl
    PUT #1, 1, limm
   END IF
   FOR Ws = 1 TO 13
    Edit$(Ws) = STRING$(Max(Ws), " ")
   NEXT Ws
  END IF
  CLOSE #1
 END IF
 RETURN
END SUB

'Center:
'  Center text on the given row.
SUB Center (row, text$)
    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;
END SUB

SUB Cpagar
 Menor$ = OrdemData(Din$)
 Maior$ = OrdemData(Dfi$)
 OPEN "Empenho.Dat" FOR RANDOM AS #2 LEN = LEN(Kword)
 GET #2, 1, Crg
 IF Crg.Vl = "THISISVALID" THEN
  Mx = VAL(Crg.Mx)
   Finished = True: Contador = 0: Total# = 0
   Ca$ = "  Relat¢rio Das Contas … Pagar No Per¡odo: \      \ … \      \"
   Cb$ = "  Dia   ³ Fornecedor      ³ Descricao                   ³ Valor $  ³ Saldo $"
    u$ = "\      \³\               \³\                           \³ ##,###.##³ ###,###.##"
  U1x$ = "ßßßßßßßß³ßßßßßßßßßßßßßßßßß³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßßß"
  FOR Ws = 1 TO Mx
   GET #2, Ws + 1, Kword
   'Classificar a Data
   Dado$ = OrdemData(Kword.Dt)
   IF Kword.Fg2 <> 0 THEN
    IF VAL(Dado$) = VAL(Menor$) OR VAL(Dado$) > VAL(Menor$) THEN
     IF VAL(Dado$) = VAL(Maior$) OR VAL(Dado$) < VAL(Maior$) THEN
      'Buscar o Cliente$
      Cliente$ = Kword.Rg
      OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
      GET #1, 1, limm
      IF limm.Vl = "THISISVALID" THEN
       MaxRecord = limm.Mx
       FOR Rg = 1 TO MaxRecord
        GET #1, Rg + 1, Fo
        IF Fo.R = Cliente$ THEN Cliente$ = Fo.N: Rg = MaxRecord
       NEXT Rg
      END IF
      CLOSE #1
      IF Finished THEN
       LPRINT USING Ca$; Din$; Dfi$
       LPRINT Cb$
       LPRINT U1x$
       Finished = False
      END IF
      Contador = Contador + 1
      Total# = Total# + Kword.Fg2
      Cb$ = "  Dia   ³ Fornecedor      ³ Descricao                   ³ Valor $  ³ Saldo $"
      LPRINT USING u$; Kword.Dt; Cliente$; Kword.Dsc; Kword.Fg2; Total#
      IF Contador = 55 THEN Contador = 0: Finished = True
     END IF
    END IF
   END IF
  NEXT Ws
 END IF
 CLOSE #2
END SUB

SUB Creceber
 Menor$ = OrdemData(Din$)
 Maior$ = OrdemData(Dfi$)
 OPEN "Empenho.Dat" FOR RANDOM AS #2 LEN = LEN(Kword)
 GET #2, 1, Crg
 IF Crg.Vl = "THISISVALID" THEN
  Mx = VAL(Crg.Mx)
   Finished = True: Contador = 0: Total# = 0
   Ca$ = "  Relat¢rio Das Contas … Receber De: \      \ … \      \"
   Cb$ = "  Dia   ³ Cliente         ³ Descricao                   ³ Valor $  ³ Saldo $"
    u$ = "\      \³\               \³\                           \³ ##,###.##³ ###,###.##"
  U1x$ = "ßßßßßßßß³ßßßßßßßßßßßßßßßßß³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßßß"
  FOR Ws = 1 TO Mx
   GET #2, Ws + 1, Kword
   'Classificar a Data
   Dado$ = OrdemData(Kword.Dt)
   IF Kword.Fg1 <> 0 THEN
    IF VAL(Dado$) = VAL(Menor$) OR VAL(Dado$) > VAL(Menor$) THEN
     IF VAL(Dado$) = VAL(Maior$) OR VAL(Dado$) < VAL(Maior$) THEN
      'Buscar o Cliente$
      Cliente$ = Kword.Rg
      OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
      GET #1, 1, limm
      IF limm.Vl = "THISISVALID" THEN
       MaxRecord = limm.Mx
       FOR Rg = 1 TO MaxRecord
        GET #1, Rg + 1, Fo
        IF Fo.R = Cliente$ THEN Cliente$ = Fo.N: Rg = MaxRecord
       NEXT Rg
      END IF
      CLOSE #1
      IF Finished THEN
       LPRINT USING Ca$; Din$; Dfi$
       LPRINT Cb$
       LPRINT U1x$
       Finished = False
      END IF
      Contador = Contador + 1
      Total# = Total# + Kword.Fg1
      LPRINT USING u$; Kword.Dt; Cliente$; Kword.Dsc; Kword.Fg1; Total#
      IF Contador = 55 THEN Contador = 0: Finished = True
     END IF
    END IF
   END IF
  NEXT Ws
 END IF
 CLOSE #2
END SUB

SUB DigTrans
 REDIM help$(6), Col(6), Vis(6), Max(6), CurrString$(3), CurrFig#(5)
 REDIM Balance#(7000)
 OPEN "Empenho.Dat" FOR RANDOM AS #2 LEN = LEN(Kword)
 CurrString$(1) = "": CurrString$(2) = "": CurrString$(3) = ""
 CurrFig#(4) = 0: CurrFig#(5) = 0
 GET #2, 1, Crg
 IF Crg.Vl = "THISISVALID" THEN
  MaxRecord = VAL(Crg.Mx)
  Balance#(0) = 0
  a = 1
  WHILE a <= MaxRecord
   GET #2, a + 1, Kword
   Balance#(a) = Balance#(a - 1) + Kword.Fg1 - Kword.Fg2
   a = a + 1
  WEND
  help$(1) = "(dd/mm/aa) "
  help$(2) = "Codigo "
  help$(3) = "Descricao "
  help$(4) = "Entrada no Caixa "
  help$(5) = "Saida do Caixa ": Mns$ = ""
  Col(1) = 2: Col(2) = 11: Col(3) = 18: Col(4) = 44: Col(5) = 55
  Vis(1) = 8: Vis(2) = 6: Vis(3) = 25: Vis(4) = 10: Vis(5) = 10
  Max(1) = 8: Max(2) = 6: Max(3) = 50: Max(4) = 10: Max(5) = 10
  COLOR colors(7, ColorPref), colors(4, ColorPref)
  Box 2, 1, 23, 80
  COLOR colors(5, ColorPref), colors(4, ColorPref)
  LOCATE 1, 1: PRINT SPACE$(80);
  LOCATE 1, 2: PRINT " Caixa Use: <Ins> <Page Down> <Page Up> <Delete> "; CHR$(24); CHR$(25); CHR$(27); CHR$(26)
  COLOR colors(7, ColorPref), colors(4, ColorPref)
  LOCATE 3, 2: PRINT "  Dia   ³ Ref# ³ Descricao               ³ Entrada  ³ Saida    ³   Saldo      "
  LOCATE 4, 2: PRINT "ÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
  u$ = "\      \³\    \³\                       \³"
  u1$ = "        ³      ³                         ³          ³          ³              "
  U1x$ = "ßßßßßßßß³ßßßßßß³ßßßßßßßßßßßßßßßßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßßßßßß"
  u2$ = "###,###.##": u3$ = "###,###,###.##": u4$ = "          "
  CurrTopLine = 1
  GOSUB DigTransPrintWholeScreen
  CurrRow = 1: CurrCol = 1
  PrintHelpLine help$(CurrCol)
  GOSUB DigTransGetLine
  Finished = False
  'Loop until <F2> is pressed
  DO
   LOCATE 1, 65: PRINT USING "#### #### ####"; RGI%; RGF%; CurrRecord
   GOSUB DigTransShowCursor                   'Show Cursor, Wait for key
   DO: Kbd$ = INKEY$: LOOP UNTIL Kbd$ <> ""
   GOSUB DigTransHideCursor
   IF Kbd$ >= " " AND Kbd$ < "~" OR Kbd$ = CHR$(8) THEN        'If legal key, edit item
    GOSUB DigTransEditItem
   END IF
   SELECT CASE Kbd$                            'Handle Special keys
    CASE CHR$(0) + "H"                      'up arrow
     GOSUB DigTransMoveUp
    CASE CHR$(0) + "P"                      'Down arrow
     GOSUB DigTransMoveDown
    CASE CHR$(0) + "K", CHR$(0) + CHR$(15)  'Left Arrow,BackTab
     CurrCol = (CurrCol + 3) MOD 5 + 1
     PrintHelpLine help$(CurrCol) + Mns$
    CASE CHR$(0) + "M"                     'Right Arrow
     CurrCol = (CurrCol) MOD 5 + 1
     PrintHelpLine help$(CurrCol) + Mns$
    CASE CHR$(0) + "I"                      'Page Up
     CurrRow = 1
     CurrTopLine = CurrTopLine - 18
     IF CurrTopLine < 1 THEN CurrTopLine = 1
     GOSUB DigTransPrintWholeScreen
     GOSUB DigTransGetLine
    CASE CHR$(0) + "Q"                      'Page Down
     CurrRow = 1
     CurrTopLine = CurrTopLine + 18
     IF CurrTopLine > MaxRecord THEN CurrTopLine = MaxRecord
     GOSUB DigTransPrintWholeScreen
     GOSUB DigTransGetLine
    CASE CHR$(0) + "<", CHR$(27)            'F2
     Finished = True
    CASE CHR$(0) + "S"                      'F10
     GOSUB DigTransDeleteRecord
    CASE CHR$(9)
     IF RGI% = 0 THEN
      RGI% = CurrRecord
     ELSE
      IF RGI% = CurrRecord THEN RGI% = 0
     END IF
     IF RGF% = 0 AND (RGI% <> CurrRecord AND RGI% <> 0) THEN
      RGF% = CurrRecord
     ELSE
      IF RGF% = CurrRecord THEN RGF% = 0
     END IF
    CASE ELSE
     BEEP
    END SELECT
   LOOP UNTIL Finished
   CLOSE #2
  END IF
  EXIT SUB
DigTransShowCursor:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 LOCATE CurrRow + 4, Col(CurrCol)
 SELECT CASE CurrCol
  CASE 1, 2, 3
   PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
  CASE 4
   IF CurrFig#(4) <> 0 THEN
    PRINT USING u2$; CurrFig#(4);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  CASE 5
   IF CurrFig#(5) <> 0 THEN
    PRINT USING u2$; CurrFig#(5);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  END SELECT
  RETURN
DigTransHideCursor:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE CurrRow + 4, Col(CurrCol)
 SELECT CASE CurrCol
  CASE 1, 2, 3
   PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
  CASE 4
   IF CurrFig#(4) <> 0 THEN
    PRINT USING u2$; CurrFig#(4);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  CASE 5
   IF CurrFig#(5) <> 0 THEN
    PRINT USING u2$; CurrFig#(5);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  END SELECT
 RETURN
DigTransEditItem:
 CurrRecord = CurrTopLine + CurrRow - 1
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 SELECT CASE CurrCol
  CASE 1 TO 3
   Start$ = Kbd$
   Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
   CurrString$(CurrCol) = LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol))
  CASE 4 TO 5
   Start$ = Kbd$
   DO
    Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
    new4# = VAL(LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol)))
    Start$ = ""
   LOOP WHILE new4# >= 999999.99# OR new4# < 0
   a = CurrRecord
   IF CurrCol = 4 THEN
    WHILE a <= MaxRecord
     Balance#(a) = Balance#(a) + new4# - CurrFig#(4) + CurrFig#(5)
     a = a + 1
    WEND
    CurrFig#(4) = new4#
    CurrFig#(5) = 0
   ELSE
    WHILE a <= MaxRecord
     Balance#(a) = Balance#(a) - new5# + CurrFig#(5) - CurrFig#(4)
     a = a + 1
    WEND
    CurrFig#(4) = 0
    CurrFig#(5) = new5#
   END IF
  CASE ELSE
 END SELECT
 GOSUB DigTransPutLine
 GOSUB DigTransGetLine
 GOSUB DigTransPrintBalances
 GOSUB DigTransPrintLine
 RETURN
DigTransMoveUp:
 IF CurrRow = 1 THEN
  IF CurrTopLine = 1 THEN
   BEEP
  ELSE
   ScrolDown
   CurrTopLine = CurrTopLine - 1
   GOSUB DigTransGetLine
   GOSUB DigTransPrintLine
  END IF
 ELSE
  CurrRow = CurrRow - 1
  GOSUB DigTransGetLine
 END IF
 RETURN
DigTransMoveDown:
 IF (CurrRow + CurrTopLine - 1) >= MaxRecord THEN
  BEEP
 ELSE
  IF CurrRow = 18 THEN
   ScrolUp
   CurrTopLine = CurrTopLine + 1
   GOSUB DigTransGetLine
   GOSUB DigTransPrintLine
  ELSE
   CurrRow = CurrRow + 1
   GOSUB DigTransGetLine
  END IF
 END IF
 RETURN
DigTransPrintLine:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 CurrRecord = CurrTopLine + CurrRow - 1
 LOCATE CurrRow + 4, 2
 IF CurrRecord = MaxRecord + 1 THEN
  PRINT U1x$;
 ELSEIF CurrRecord > MaxRecord THEN
  PRINT u1$;
 ELSE
  PRINT USING u$; CurrString$(1); CurrString$(2); CurrString$(3);
  IF CurrFig#(4) = 0 AND CurrFig#(5) = 0 THEN
   PRINT USING u4$ + "³" + u4$ + "³" + u3$; Balance#(CurrRecord)
  ELSEIF CurrFig#(5) = 0 THEN
   PRINT USING u2$ + "³" + u4$ + "³" + u3$; CurrFig#(4); Balance#(CurrRecord)
  ELSE
   PRINT USING u4$ + "³" + u2$ + "³" + u3$; CurrFig#(5); Balance#(CurrRecord)
  END IF
 END IF
 RETURN
DigTransPrintBalances:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 FOR a = 1 TO 18
  CurrRecord = CurrTopLine + a - 1
  IF CurrRecord <= MaxRecord THEN
   LOCATE 4 + a, 66
   PRINT USING u3$; Balance#(CurrTopLine + a - 1);
  END IF
 NEXT a
 RETURN
DigTransDeleteRecord:
 IF MaxRecord = 1 THEN
  BEEP
 ELSE
  CurrRecord = CurrTopLine + CurrRow - 1
  MaxRecord = MaxRecord - 1
  a = CurrRecord
  WHILE a <= MaxRecord
   GET #2, a + 2, Kword
   PUT #2, a + 1, Kword
   Balance#(a) = Balance#(a + 1) - Kword.Fg1 + Kword.Fg2
   a = a + 1
  WEND
  Crg.Vl = "THISISVALID"
  Crg.Mx = LTRIM$(RTRIM$(STR$(MaxRecord)))
  Crg.Iob = 0
  PUT #2, 1, Crg
  GOSUB DigTransPrintWholeScreen
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord > MaxRecord THEN GOSUB DigTransMoveUp
  GOSUB DigTransGetLine
 END IF
 RETURN
DigTransPrintWholeScreen:
 temp = CurrRow
 FOR CurrRow = 1 TO 18
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord <= MaxRecord THEN GOSUB DigTransGetLine
  GOSUB DigTransPrintLine
 NEXT CurrRow
 CurrRow = temp
 RETURN
DigTransPutLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 Kword.Dt = CurrString$(1)
 Kword.Rf = CurrString$(2)
 Kword.Dsc = CurrString$(3)
 Kword.Fg1 = CurrFig#(4)
 Kword.Fg2 = CurrFig#(5): Kword.Rg = Cliente$
 PUT #2, CurrRecord + 1, Kword
 IF MaxRecord <> 1 AND LTRIM$(RTRIM$(cfer$)) <> LTRIM$(RTRIM$(CurrString$(1))) THEN GOSUB DigPoeOrdem
 RETURN
DigTransGetLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 GET #2, CurrRecord + 1, Kword
 CurrString$(1) = Kword.Dt
 CurrString$(2) = Kword.Rf
 CurrString$(3) = Kword.Dsc
 CurrFig#(4) = Kword.Fg1
 CurrFig#(5) = Kword.Fg2
 Cliente$ = Kword.Rg
 cfer$ = Kword.Dt: Mns$ = ""
RETURN
DigPoeOrdem:
 'deletar Registro
 a = CurrRecord
 MaxRecord = MaxRecord - 1
 WHILE a <= MaxRecord
  GET #2, a + 2, Kword
  PUT #2, a + 1, Kword
  a = a + 1
 WEND
 'Reordenando
 Mens$ = OrdemData(CurrString$(1))
 Dado$ = Mens$
 NovoPos = 0: txt = 0
 DO
  txt = txt + 1
  GET #2, txt + 1, Kword
  Mens$ = OrdemData(Kword.Dt)
  E$ = Mens$
  IF VAL(E$) > VAL(Dado$) THEN NovoPos = txt
 LOOP UNTIL NovoPos <> 0 OR txt = MaxRecord
 IF NovoPos = 0 THEN NovoPos = MaxRecord + 1
 a = MaxRecord
 WHILE a >= NovoPos
  GET #2, a + 1, Kword
  PUT #2, a + 2, Kword
  a = a - 1
 WEND
 MaxRecord = MaxRecord + 1
 Kword.Dt = CurrString$(1)
 Kword.Rf = CurrString$(2)
 Kword.Dsc = CurrString$(3)
 Kword.Fg1 = CurrFig#(4)
 Kword.Fg2 = CurrFig#(5): Kword.Rg = Cliente$
 PUT #2, NovoPos + 1, Kword
 FOR Ws = 1 TO MaxRecord
  IF Ws = NovoPos THEN
   CurrRow = ((Ws - 1) MOD 18) + 1
   CurrTopLine = Ws - CurrRow + 1
   CurrRecord = CurrTopLine + CurrRow - 1
   Ws = MaxRecord
  END IF
 NEXT Ws
 Balance#(0) = 0
 a = 1
 WHILE a <= MaxRecord
  GET #2, a + 1, Kword
  Balance#(a) = Balance#(a - 1) + Kword.Fg1 - Kword.Fg2
  a = a + 1
 WEND
 GOSUB DigTransPrintWholeScreen
RETURN
END SUB

SUB EditAccounts
 Clie$ = "00001"
 IF LEN(Clie$) = 0 THEN GOTO FimSub
 REDIM help$(6), Col(6), Vis(6), Max(6), CurrString$(3), CurrFig#(5), Reg(1)
 REDIM Balance#(1)
 OPEN "Empenho.dat" FOR RANDOM AS #2 LEN = LEN(Kword)
 CurrString$(1) = "": CurrString$(2) = "": CurrString$(3) = ""
 CurrFig#(4) = 0: CurrFig#(5) = 0
 GET #2, 1, Crg
 IF Crg.Vl <> "THISISVALID" THEN
  Kword.Rg = Clie$
  Kword.Dt = "": Kword.Rf = "": Kword.Dsc = ""
  Kword.Fg1 = 0: Kword.Fg2 = 0
  PUT #2, 2, Kword
  Crg.Vl = "THISISVALID"
  Crg.Mx = "1"
  Crg.Iob = 0
  PUT #2, 1, Crg
 END IF
 MaxReg = VAL(Crg.Mx)
 GOSUB CalcMatriz
 Balance#(0) = 0
 a = 1
 WHILE a <= Maximus
  GET #2, Reg(a) + 1, Kword
  Balance#(a) = Balance#(a - 1) + Kword.Fg1 - Kword.Fg2
  a = a + 1
 WEND
 help$(1) = "(dd/mm/aa) "
 help$(2) = "Codigo "
 help$(3) = "Descricao "
 help$(4) = "Entrada no Caixa "
 help$(5) = "Saida do Caixa ": Mns$ = ""
 Col(1) = 2: Col(2) = 11: Col(3) = 18: Col(4) = 44: Col(5) = 55
 Vis(1) = 8: Vis(2) = 6: Vis(3) = 25: Vis(4) = 10: Vis(5) = 10
 Max(1) = 8: Max(2) = 6: Max(3) = 50: Max(4) = 10: Max(5) = 10
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 Box 2, 1, 23, 80
 COLOR colors(5, ColorPref), colors(4, ColorPref)
 LOCATE 1, 1: PRINT SPACE$(80);
 LOCATE 1, 2: PRINT " Caixa Use: <Ins> <Page Down> <Page Up> <Delete> "; CHR$(24); CHR$(25); CHR$(27); CHR$(26)
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE 3, 2: PRINT "  Dia   ³ Ref# ³ Descricao               ³ Entrada  ³ Saida    ³   Saldo      "
 LOCATE 4, 2: PRINT "ÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
 u$ = "\      \³\    \³\                       \³"
 u1$ = "        ³      ³                         ³          ³          ³              "
 U1x$ = "ßßßßßßßß³ßßßßßß³ßßßßßßßßßßßßßßßßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßßßßßß"
 u2$ = "###,###.##": u3$ = "###,###,###.##": u4$ = "          "
 CurrTopLine = 1
 GOSUB EditTransPrintWholeScreen
 CurrRow = 1: CurrCol = 1
 PrintHelpLine help$(CurrCol)
 GOSUB EditTransGetLine
 Finished = False
 'Loop until <F2> is pressed
 DO
  LOCATE 1, 65: PRINT USING "#### #### ####"; RGI%; RGF%; CurrRecord
  GOSUB EditTransShowCursor                   'Show Cursor, Wait for key
  DO: Kbd$ = INKEY$: LOOP UNTIL Kbd$ <> ""
  GOSUB EditTransHideCursor
  IF Kbd$ >= " " AND Kbd$ < "~" OR Kbd$ = CHR$(8) THEN        'If legal key, edit item
   GOSUB EditTransEditItem
  END IF
  SELECT CASE Kbd$                            'Handle Special keys
   CASE CHR$(0) + "H"                      'up arrow
    GOSUB EditTransMoveUp
   CASE CHR$(0) + "P"                      'Down arrow
    GOSUB EditTransMoveDown
   CASE CHR$(0) + "K", CHR$(0) + CHR$(15)  'Left Arrow,BackTab
    CurrCol = (CurrCol + 3) MOD 5 + 1
    PrintHelpLine help$(CurrCol) + Mns$
   CASE CHR$(0) + "M"                     'Right Arrow
    CurrCol = (CurrCol) MOD 5 + 1
    PrintHelpLine help$(CurrCol) + Mns$
   CASE CHR$(0) + "I"                      'Page Up
    CurrRow = 1: CurrTopLine = CurrTopLine - 18
    IF CurrTopLine < 1 THEN CurrTopLine = 1
    GOSUB EditTransPrintWholeScreen
    GOSUB EditTransGetLine
   CASE CHR$(0) + "Q"                      'Page Down
    CurrRow = 1: CurrTopLine = CurrTopLine + 18
    IF CurrTopLine > Maximus THEN CurrTopLine = Maximus
    GOSUB EditTransPrintWholeScreen
    GOSUB EditTransGetLine
   CASE CHR$(0) + "<", CHR$(27)            'F2
    Finished = True
   CASE CHR$(0) + "R"                      'F9
    GOSUB EditTransAddRecord
    CurrCol = 1
    GOSUB EditTransMoveDown
   CASE CHR$(0) + "S"                      'F10
    GOSUB EditTransDeleteRecord
   CASE CHR$(9)
    IF RGI% = 0 THEN
     RGI% = CurrRecord
    ELSE
     IF RGI% = CurrRecord THEN RGI% = 0
    END IF
    IF RGF% = 0 AND (RGI% <> CurrRecord AND RGI% <> 0) THEN
     RGF% = CurrRecord
    ELSE
     IF RGF% = CurrRecord THEN RGF% = 0
    END IF
   CASE ELSE
    BEEP
  END SELECT
 LOOP UNTIL Finished
 CLOSE #2
 EXIT SUB
EditTransShowCursor:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 LOCATE CurrRow + 4, Col(CurrCol)
 SELECT CASE CurrCol
  CASE 1, 2, 3
   PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
  CASE 4
   IF CurrFig#(4) <> 0 THEN
    PRINT USING u2$; CurrFig#(4);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  CASE 5
   IF CurrFig#(5) <> 0 THEN
    PRINT USING u2$; CurrFig#(5);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  END SELECT
  RETURN
EditTransHideCursor:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE CurrRow + 4, Col(CurrCol)
 SELECT CASE CurrCol
  CASE 1, 2, 3
   PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
  CASE 4
   IF CurrFig#(4) <> 0 THEN
    PRINT USING u2$; CurrFig#(4);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  CASE 5
   IF CurrFig#(5) <> 0 THEN
    PRINT USING u2$; CurrFig#(5);
   ELSE
    PRINT SPACE$(Vis(CurrCol));
   END IF
  END SELECT
  RETURN
EditTransEditItem:
 CurrRecord = CurrTopLine + CurrRow - 1
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 SELECT CASE CurrCol
  CASE 1 TO 3
   Start$ = Kbd$
   Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
   CurrString$(CurrCol) = LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol))
  CASE 4
   Start$ = Kbd$
   DO
    Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
    new4# = VAL(LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol)))
    Start$ = ""
   LOOP WHILE new4# >= 999999.99# OR new4# < 0
   a = CurrRecord
   WHILE a <= Maximus
    Balance#(a) = Balance#(a) + new4# - CurrFig#(4) + CurrFig#(5)
    a = a + 1
   WEND
   CurrFig#(4) = new4#
   CurrFig#(5) = 0
  CASE 5
   Start$ = Kbd$
   DO
    Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Start$, End$, Vis(CurrCol), Max(CurrCol))
    new5# = VAL(LEFT$(End$ + SPACE$(Max(CurrCol)), Max(CurrCol)))
    Start$ = ""
   LOOP WHILE new5# >= 999999.99# OR new5# < 0
   a = CurrRecord
   WHILE a <= Maximus
    Balance#(a) = Balance#(a) - new5# + CurrFig#(5) - CurrFig#(4)
    a = a + 1
   WEND
   CurrFig#(4) = 0
   CurrFig#(5) = new5#
  CASE ELSE
 END SELECT
 GOSUB EditTransPutLine
 GOSUB EditTransGetLine
 GOSUB EditTransPrintBalances
 GOSUB EditTransPrintLine
 RETURN
EditTransMoveUp:
 IF CurrRow = 1 THEN
  IF CurrTopLine = 1 THEN
   BEEP
  ELSE
   ScrolDown
   CurrTopLine = CurrTopLine - 1
   GOSUB EditTransGetLine
   GOSUB EditTransPrintLine
  END IF
 ELSE
  CurrRow = CurrRow - 1
  GOSUB EditTransGetLine
 END IF
 RETURN
EditTransMoveDown:
 IF (CurrRow + CurrTopLine - 1) >= Maximus THEN
  BEEP
 ELSE
  IF CurrRow = 18 THEN
   ScrolUp
   CurrTopLine = CurrTopLine + 1
   GOSUB EditTransGetLine
   GOSUB EditTransPrintLine
  ELSE
   CurrRow = CurrRow + 1
   GOSUB EditTransGetLine
  END IF
 END IF
 RETURN
EditTransPrintLine:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 CurrRecord = CurrTopLine + CurrRow - 1
 LOCATE CurrRow + 4, 2
 IF CurrRecord = Maximus + 1 THEN
  PRINT U1x$;
 ELSE
  IF CurrRecord > Maximus THEN
   PRINT u1$;
  ELSE
   PRINT USING u$; CurrString$(1); CurrString$(2); CurrString$(3);
   IF CurrFig#(4) = 0 AND CurrFig#(5) = 0 THEN
    PRINT USING u4$ + "³" + u4$ + "³" + u3$; Balance#(CurrRecord)
   ELSE
    IF CurrFig#(5) = 0 THEN
     PRINT USING u2$ + "³" + u4$ + "³" + u3$; CurrFig#(4); Balance#(CurrRecord)
    ELSE
     PRINT USING u4$ + "³" + u2$ + "³" + u3$; CurrFig#(5); Balance#(CurrRecord)
    END IF
   END IF
  END IF
 END IF
 RETURN
EditTransPrintBalances:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 FOR a = 1 TO 18
  CurrRecord = CurrTopLine + a - 1
  IF CurrRecord <= Maximus THEN
   LOCATE 4 + a, 66
   PRINT USING u3$; Balance#(CurrTopLine + a - 1);
  END IF
 NEXT a
 RETURN
EditTransDeleteRecord:
 IF Maximus = 1 THEN
  BEEP
 ELSE
  CurrRecord = CurrTopLine + CurrRow - 1
  MaxReg = MaxReg - 1
  a = Reg(CurrRecord)
  WHILE a <= MaxReg
   GET #2, a + 2, Kword
   PUT #2, a + 1, Kword
   a = a + 1
  WEND
  Crg.Vl = "THISISVALID"
  Crg.Mx = LTRIM$(RTRIM$(STR$(MaxReg)))
  Crg.Iob = 0
  PUT #2, 1, Crg
  GOSUB CalcMatriz
  GOSUB EditTransPrintWholeScreen
  GOSUB EditTransGetLine
 END IF
 RETURN
EditTransAddRecord:
 CurrRecord = MaxReg
 CurrRow = ((Maximus - 1) MOD 18) + 1
 CurrTopLine = Maximus - CurrRow + 1
 MaxReg = MaxReg + 1
 Kword.Rg = Clie$
 Kword.Dt = "": Kword.Rf = "": Kword.Dsc = ""
 Kword.Fg1 = 0: Kword.Fg2 = 0
 PUT #2, CurrRecord + 2, Kword
 Crg.Vl = "THISISVALID": Crg.Mx = LTRIM$(RTRIM$(STR$(MaxReg)))
 Crg.Iob = 0
 PUT #2, 1, Crg
 GOSUB CalcMatriz
 GOSUB EditTransPrintWholeScreen
 GOSUB EditTransGetLine
 RETURN
EditTransPrintWholeScreen:
 temp = CurrRow
 FOR CurrRow = 1 TO 18
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord <= Maximus THEN GOSUB EditTransGetLine
  GOSUB EditTransPrintLine
 NEXT CurrRow
 CurrRow = temp
 RETURN
EditTransPutLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 Kword.Rg = Clie$
 Kword.Dt = CurrString$(1): Kword.Rf = CurrString$(2)
 Kword.Dsc = CurrString$(3): Kword.Fg1 = CurrFig#(4)
 Kword.Fg2 = CurrFig#(5)
 PUT #2, Reg(CurrRecord) + 1, Kword
 IF MaxReg <> 1 AND cfer$ <> LTRIM$(RTRIM$(CurrString$(1))) THEN GOSUB PoeOrdem
 RETURN
EditTransGetLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 GET #2, Reg(CurrRecord) + 1, Kword
 CurrString$(1) = Kword.Dt
 CurrString$(2) = Kword.Rf
 CurrString$(3) = Kword.Dsc
 CurrFig#(4) = Kword.Fg1
 CurrFig#(5) = Kword.Fg2
 cfer$ = Kword.Dt: cfer$ = LTRIM$(RTRIM$(cfer$))
 RETURN
PoeOrdem:
 'Deletando o Registro Reg(CurrRecord)
 a = Reg(CurrRecord)
 MaxReg = MaxReg - 1
 WHILE a <= MaxReg
  GET #2, a + 2, Kword
  PUT #2, a + 1, Kword
  a = a + 1
 WEND
 'Reordenando
 Mens$ = OrdemData(CurrString$(1))
 Dado$ = Mens$
 NovoPos = 0: txt = 0
 DO
  txt = txt + 1
  GET #2, txt + 1, Kword
  Mens$ = OrdemData(Kword.Dt)
  E$ = Mens$
  IF VAL(E$) > VAL(Dado$) THEN NovoPos = txt
 LOOP UNTIL NovoPos <> 0 OR txt = MaxReg
 'inserindo o Registro na nova Posi‡…o
 IF NovoPos = 0 THEN NovoPos = MaxReg + 1
 a = MaxReg
 WHILE a >= NovoPos
  GET #2, a + 1, Kword
  PUT #2, a + 2, Kword
  a = a - 1
 WEND
 MaxReg = MaxReg + 1
 Kword.Dt = CurrString$(1): Kword.Rf = CurrString$(2)
 Kword.Dsc = CurrString$(3): Kword.Fg1 = CurrFig#(4)
 Kword.Fg2 = CurrFig#(5): Kword.Rg = Clie$
 PUT #2, NovoPos + 1, Kword
 GOSUB CalcMatriz
 FOR Ws = 1 TO Maximus
  IF Reg(Ws) = NovoPos THEN
   CurrRow = ((Ws - 1) MOD 18) + 1
   CurrTopLine = Ws - CurrRow + 1
   CurrRecord = CurrTopLine + CurrRow - 1
   Ws = Maximus
  END IF
 NEXT Ws
 GOSUB EditTransPrintWholeScreen
 GOSUB EditTransGetLine
 GOSUB EditTransPrintBalances
 GOSUB EditTransPrintLine
RETURN
CalcMatriz:
 Ct = 0
 FOR Qt = 1 TO MaxReg
  GET #2, Qt + 1, Kword
  IF Kword.Rg = Clie$ THEN Ct = Ct + 1
 NEXT Qt
 IF Ct = 0 THEN
  MaxReg = MaxReg + 1
  Kword.Rg = Clie$
  Kword.Dt = "": Kword.Rf = "": Kword.Dsc = ""
  Kword.Fg1 = 0: Kword.Fg2 = 0
  PUT #2, MaxReg + 1, Kword
  Crg.Vl = "THISISVALID"
  Crg.Mx = LTRIM$(RTRIM$(STR$(MaxReg)))
  Crg.Iob = 0
  PUT #2, 1, Crg
  Ct = 1
 END IF
 REDIM Reg(Ct), Balance#(Ct)
 Maximus = 0: Balance#(0) = 0
 FOR Qt = 1 TO MaxReg
  GET #2, Qt + 1, Kword
  IF Kword.Rg = Clie$ THEN
   Maximus = Maximus + 1
   Reg(Maximus) = Qt
   Balance#(Maximus) = 0
  END IF
 NEXT Qt
 a = 1
 WHILE a <= Maximus
  GET #2, Reg(a) + 1, Kword
  Balance#(a) = Balance#(a - 1) + Kword.Fg1 - Kword.Fg2
  a = a + 1
 WEND
RETURN
FimSub:
END SUB

SUB Empenho
Emp:
 CLS
 Xx = 1
 c$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 Box 2, 40, 8, 40 + (LEN(c$) + 3) 'Monta as bordas da tela
 Box 5, 41, 7, 41 + (LEN(c$) + 1) 'Caixa de selecao da Letra pesquisada
 LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 3, 41: PRINT "Lan‡ar Contas Ou Receitas No Cadastro"
 LOCATE 4, 41: PRINT "Use:"; CHR$(27); CHR$(26); " <Enter> <Esc> "
 'CHR$(27); CHR$(26) direita e esquerda
 'CHR$(24); CHR$(25) Sobe e Desce
 Ps% = 1
 c$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ": L$ = ""
 LOCATE 6, 42: PRINT c$: Kbd$ = ""
 DO WHILE L$ = ""
  Kbd$ = INKEY$
  COLOR 15, 0
  LOCATE 6, 41 + Ps%: PRINT MID$(c$, Ps%, 1)
  SELECT CASE Kbd$
   CASE CHR$(0) + "M"
    COLOR 0, 15
    LOCATE 6, 41 + Ps%: PRINT MID$(c$, Ps%, 1)
    Ps% = Ps% + 1: IF Ps% = LEN(c$) + 1 THEN Ps% = 1
   CASE CHR$(0) + "K"
    COLOR 0, 15
    LOCATE 6, 41 + Ps%: PRINT MID$(c$, Ps%, 1)
    Ps% = Ps% - 1: IF Ps% = 0 THEN Ps% = LEN(c$)
   CASE CHR$(13)
    Xy% = 1
    L$ = MID$(c$, Ps%, 1)
    GOSUB MCadastro
   CASE CHR$(27)
    EXIT SUB
  END SELECT
 LOOP
GOTO Emp
MCadastro:
 OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
 GET #1, 1, limm
 IF limm.Vl = "THISISVALID" THEN
  Mx = 0: MaxRecord = limm.Mx: Ctrl = limm.Ex
  FOR Ws = 1 TO MaxRecord
   GET #1, Ws + 1, Fo
   IF MID$(Fo.N, 1, 1) = L$ THEN Mx = Mx + 1
  NEXT Ws
  IF Mx <> 0 THEN
   REDIM Rg(Mx)
   Ct = 0
   FOR Ws = 1 TO MaxRecord
    GET #1, Ws + 1, Fo
    IF MID$(Fo.N, 1, 1) = L$ THEN
     Ct = Ct + 1: Rg(Ct) = Ws
    END IF
   NEXT Ws
   COLOR colors(7, ColorPref), colors(4, ColorPref)
   CLS
   Box 1, 10, 23, 61  'Monta as bordas da tela
   COLOR colors(7, ColorPref), colors(4, ColorPref)
   LOCATE 2, 10: PRINT "³  Selecione O Devedor Ou O Credor <Home> ou <Esc> ³"
   LOCATE 3, 10: PRINT "³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³"
                  u$ = "³\                                                \³"
                 u1$ = "³                                                  ³"
                U1x$ = "³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³"
   Finished = False:  CurrCol = 1
   CurrTopLine = 1: CurrRow = 1
   GOSUB EmpTransPrintWholeScreen
   CurrTopLine = 1: CurrRow = 1
   'PrintHelpLine help$(CurrCol)
   GOSUB EmpTransGetLine
   DO
    GOSUB EmpTransShowCursor                              'Show Cursor
    DO                                      'Wait for key
     Kbd$ = INKEY$
    LOOP UNTIL Kbd$ <> ""
    GOSUB EmpTransHideCursor                              'Hide Cursor so it can move
    SELECT CASE Kbd$
     CASE CHR$(0) + "H"                              'Up Arrow
      GOSUB EmpTransMoveUp
     CASE CHR$(0) + "P"                               'Down Arrow
      GOSUB EmpTransMoveDown
     CASE CHR$(0) + "K"                              'Up Arrow
     CASE CHR$(0) + "M"                               'Down Arrow
     CASE CHR$(0) + "I"                      'Page Up
      CurrRow = 1
      CurrTopLine = CurrTopLine - 18
      IF CurrTopLine < 1 THEN
       CurrTopLine = 1
      END IF
      GOSUB EmpTransPrintWholeScreen
      GOSUB EmpTransGetLine
      GOSUB EmpTransPrintLine
     CASE CHR$(0) + "Q"                      'Page Down
      CurrRow = 1
      CurrTopLine = CurrTopLine + 18
      IF CurrTopLine > Mx THEN
       CurrTopLine = Mx
      END IF
      GOSUB EmpTransPrintWholeScreen
      GOSUB EmpTransGetLine
      GOSUB EmpTransPrintLine
     CASE CHR$(0) + "G"
      EditAccounts
      CLS
      Finished = True
     CASE CHR$(27)
      Finished = True
     CASE ELSE
      BEEP
    END SELECT
   LOOP UNTIL Finished
  END IF
 END IF
 CLOSE #1
RETURN
EmpTransShowCursor:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$, 50);
 RETURN
EmpTransHideCursor:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$, 50);
 RETURN
EmpTransMoveUp:
 IF CurrRow = 1 THEN
  IF CurrTopLine = 1 THEN
   BEEP
  ELSE
   ScrolDown
   CurrTopLine = CurrTopLine - 1
   GOSUB EmpTransGetLine
   GOSUB EmpTransPrintLine
  END IF
 ELSE
  CurrRow = CurrRow - 1
  GOSUB EmpTransGetLine
  GOSUB EmpTransPrintLine
 END IF
 RETURN
EmpTransMoveDown:
 IF (CurrRow + CurrTopLine - 1) >= Mx THEN
  BEEP
 ELSE
  IF CurrRow = 18 THEN
   ScrolUp
   CurrTopLine = CurrTopLine + 1
   GOSUB EmpTransGetLine
   GOSUB EmpTransPrintLine
  ELSE
   CurrRow = CurrRow + 1
   GOSUB EmpTransGetLine
   GOSUB EmpTransPrintLine
  END IF
 END IF
 RETURN
EmpTransPrintLine:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 CurrRecord = CurrTopLine + CurrRow - 1
 LOCATE CurrRow + 3, 10
 IF CurrRecord = Mx + 1 THEN
  PRINT U1x$;
 ELSE
  IF CurrRecord > Mx THEN
   PRINT u1$;
  ELSE
   PRINT USING u$; Edit$
  END IF
 END IF
RETURN
EmpTransPrintWholeScreen:
 temp = CurrRow
 FOR CurrRow = 1 TO 18
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord <= Mx THEN
   GOSUB EmpTransGetLine
  END IF
  GOSUB EmpTransPrintLine
 NEXT CurrRow
 CurrRow = temp
 RETURN
EmpTransGetLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 GET #1, Rg(CurrRecord) + 1, Fo
 Edit$ = Fo.N: Clie$ = Fo.R
RETURN
END SUB

'FancyCls:
'  Clears screen in the right color, and draws nice dots.
SUB FancyCls (dots, Background)

    VIEW PRINT 2 TO 24
    COLOR dots, Background
    CLS 2

    FOR a = 95 TO 1820 STEP 45
        row = a / 80 + 1
        Col = a MOD 80 + 1
        LOCATE row, Col
        PRINT CHR$(250);
    NEXT a

    VIEW PRINT

END SUB

SUB Fluxo
 Menor$ = OrdemData(Din$)
 Maior$ = OrdemData(Dfi$)
 OPEN "Empenho.Dat" FOR RANDOM AS #2 LEN = LEN(Kword)
 GET #2, 1, Crg
 IF Crg.Vl = "THISISVALID" THEN
  Mx = VAL(Crg.Mx)
   Finished = True: Contador = 0: Total# = 0
   Ca$ = "  Relat¢rio De Fluxo No Per¡odo: \      \ … \      \"
   Cb$ = "  Dia   ³ Nome            ³ Descricao                   ³ Valor $  ³ Saldo $"
    u$ = "\      \³\               \³\                           \³ ##,###.##³ ###,###.##"
  U1x$ = "ßßßßßßßß³ßßßßßßßßßßßßßßßßß³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³ßßßßßßßßßß³ßßßßßßßßßßß"
  FOR Ws = 1 TO Mx
   GET #2, Ws + 1, Kword
   'Classificar a Data
   Dado$ = OrdemData(Kword.Dt)
   IF VAL(Dado$) = VAL(Menor$) OR VAL(Dado$) > VAL(Menor$) THEN
    IF VAL(Dado$) = VAL(Maior$) OR VAL(Dado$) < VAL(Maior$) THEN
     'Buscar o Cliente$
     Cliente$ = Kword.Rg
     OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
     GET #1, 1, limm
     IF limm.Vl = "THISISVALID" THEN
      MaxRecord = limm.Mx
      FOR Rg = 1 TO MaxRecord
       GET #1, Rg + 1, Fo
       IF Fo.R = Cliente$ THEN Cliente$ = Fo.N: Rg = MaxRecord
      NEXT Rg
     END IF
     CLOSE #1
     IF Finished THEN
      LPRINT USING Ca$; Din$; Dfi$
      LPRINT Cb$
      LPRINT U1x$
      Finished = False
     END IF
     Contador = Contador + 1
     IF Kword.Fg2 = 0 THEN Valor# = Kword.Fg1 ELSE Valor# = Kword.Fg2 * -1
     Total# = Total# + Valor#
     LPRINT USING u$; Kword.Dt; Cliente$; Kword.Dsc; Valor#; Total#
     IF Contador = 55 THEN Contador = 0: Finished = True
    END IF
   END IF
  NEXT Ws
 END IF
 CLOSE #2
END SUB

'GetString$:
'  Given a row and col, and an initial string, edit a string
'  VIS is the length of the visible field of entry
'  MAX is the maximum number of characters allowed in the string
FUNCTION GetString$ (row, Col, Start$, End$, Vis, Max)
    curr$ = Trim$(LEFT$(Start$, Max))
    IF curr$ = CHR$(8) THEN curr$ = ""

    LOCATE , , 1

    Finished = False
    DO
        GOSUB GetStringShowText
        GOSUB GetStringGetKey

        IF LEN(Kbd$) > 1 THEN
            Finished = True
            GetString$ = Kbd$
        ELSE
            SELECT CASE Kbd$
                CASE CHR$(13), CHR$(27), CHR$(9)
                    Finished = True
                    GetString$ = Kbd$
                
                CASE CHR$(8)
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                    END IF

                CASE " " TO "}"
                    IF LEN(curr$) < Max THEN
                        curr$ = curr$ + Kbd$
                    ELSE
                        BEEP
                    END IF

                CASE ELSE
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL Finished

    End$ = curr$
    LOCATE , , 0
    EXIT FUNCTION
    

GetStringShowText:
    LOCATE row, Col
    IF LEN(curr$) > Vis THEN
        PRINT RIGHT$(curr$, Vis);
    ELSE
        PRINT curr$; SPACE$(Vis - LEN(curr$));
        LOCATE row, Col + LEN(curr$)
    END IF
    RETURN

GetStringGetKey:
    Kbd$ = ""
    WHILE Kbd$ = ""
        Kbd$ = INKEY$
    WEND
    RETURN
END FUNCTION

'Initialize:
'  Read colors in and set up assembly routines
SUB Initialize
 WIDTH , 25
 VIEW PRINT
 FOR ColorSet = 1 TO 4
  FOR X = 1 TO 10
   READ colors(X, ColorSet)
  NEXT X
 NEXT ColorSet
 LoadState
 P = VARPTR(ScrollUpAsm(1))
 DEF SEG = VARSEG(ScrollUpAsm(1))
 FOR I = 0 TO 13
  READ J
  POKE (P + I), J
 NEXT I
 P = VARPTR(ScrollDownAsm(1))
 DEF SEG = VARSEG(ScrollDownAsm(1))
 FOR I = 0 TO 13
  READ J
  POKE (P + I), J
 NEXT I
 DEF SEG
 P = VARPTR(ScrolUpAsm(1))
 DEF SEG = VARSEG(ScrolUpAsm(1))
 FOR I = 0 TO 13
  READ J
  POKE (P + I), J
 NEXT I
 P = VARPTR(ScrolDownAsm(1))
 DEF SEG = VARSEG(ScrolDownAsm(1))
 FOR I = 0 TO 13
  READ J
  POKE (P + I), J
 NEXT I
 DEF SEG
END SUB

'Intro:
'  Display introduction screen.
SUB Intro
    SCREEN 0
    WIDTH 80, 25
    COLOR 7, 0
    CLS
    Center 3, "Q B a s i c"
    COLOR 15
    Center 5, "Controle de Contas e Receitas "
    Center 12, "MS-DOS QBasic"
    Center 15, "Pressione qualquer tecla para continuar"
    SparklePause
END SUB

SUB ListClie
ClieIne:
 CLS
 Xx = 1
 c$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 Box 2, 1, 24, 80               'Monta as bordas da tela
 Box 3, 2, 5, 2 + (LEN(c$) + 1) 'Caixa de selecao da Letra pesquisada
 LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 3, 41: PRINT "SELECAO DO CADASTRO EM OD. ALFAB."
 LOCATE 4, 41: PRINT "Use:"; CHR$(27); CHR$(26); " <Enter> <Esc> "
 'LOCATE 5, 41: PRINT "Use:"; CHR$(24); CHR$(25); CHR$(27); CHR$(26); " <Enter> <Esc> "
 Ps% = 1
 c$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ": L$ = ""
 LOCATE 4, 3: PRINT c$: Kbd$ = ""
 DO WHILE L$ = ""
  Kbd$ = INKEY$
  COLOR 15, 0
  LOCATE 4, 2 + Ps%: PRINT MID$(c$, Ps%, 1)
  SELECT CASE Kbd$
   CASE CHR$(0) + "M"
    COLOR 0, 15
    LOCATE 4, 2 + Ps%: PRINT MID$(c$, Ps%, 1)
    Ps% = Ps% + 1: IF Ps% = LEN(c$) + 1 THEN Ps% = 1
   CASE CHR$(0) + "K"
    COLOR 0, 15
    LOCATE 4, 2 + Ps%: PRINT MID$(c$, Ps%, 1)
    Ps% = Ps% - 1: IF Ps% = 0 THEN Ps% = LEN(c$)
   CASE CHR$(13)
    Xy% = 1
    L$ = MID$(c$, Ps%, 1)
    GOSUB Reordenar
   CASE CHR$(27)
    EXIT SUB
  END SELECT
 LOOP
GOTO ClieIne
Reordenar:
'Organizar Matriz dos Clientes Com a Letra Determinada
 REDIM help$(13), Col(13), Vis(13), Max(13), Edit$(13)
 OPEN "CLIENTE.DBF" FOR RANDOM SHARED AS #1 LEN = LEN(Fo)
 GET #1, 1, limm
 IF limm.Vl = "THISISVALID" THEN
  Mx = 0: MaxRecord = limm.Mx: Ctrl = limm.Ex
  FOR Ws = 1 TO MaxRecord
   GET #1, Ws + 1, Fo
   IF MID$(Fo.N, 1, 1) = L$ THEN Mx = Mx + 1
  NEXT Ws
  IF Mx <> 0 THEN
   REDIM Reg(Mx)
   Ct = 0
   FOR Ws = 1 TO MaxRecord
    GET #1, Ws + 1, Fo
    IF MID$(Fo.N, 1, 1) = L$ THEN
     Ct = Ct + 1: Reg(Ct) = Ws
    END IF
   NEXT Ws
   COLOR colors(7, ColorPref), colors(4, ColorPref)
   CLS
   Box 1, 10, 14, 61  'Monta as bordas da tela
   Box 14, 65, 16, 67 'Monta a caixa de codigo do fornecedor
   Box 14, 68, 16, 74
   dgto13$ = "543298765432"
   dgto14$ = "6543298765432"
   COLOR colors(7, ColorPref), colors(4, ColorPref)
   LOCATE 2, 65: PRINT "Use:"; CHR$(24); CHR$(25); "<Esc> "
   LOCATE 18, 65: PRINT "Use:"; CHR$(27); CHR$(26); "<Esc> "
   LOCATE 2, 10: PRINT "³  Consulta Ao Cadastro                            ³"
   LOCATE 3, 10: PRINT "³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³"
                  u$ = "³\                                                \³"
                 u1$ = "³                                                  ³"
                U1x$ = "³ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß³"
   LOCATE 15, 2: PRINT "Nome:"; STRING$(50, "_")
   LOCATE 16, 2: PRINT "Endereco:"; STRING$(50, "_")
   LOCATE 17, 2: PRINT "Bairro:____________________"
   LOCATE 18, 2: PRINT "Telefone:_____________"
   LOCATE 19, 2: PRINT "Cidade:______________________________"
   LOCATE 20, 2: PRINT "Cep:__________ Uf:__"
   LOCATE 21, 2: PRINT "Cgc:____________________"
   LOCATE 22, 2: PRINT "Insc.Est.:_______________"
   LOCATE 23, 2: PRINT "Contato:_______________"
   LOCATE 24, 2: PRINT "Telefone:_____________"
   'Initialize variables
   help$(1) = "  Nome do cliente ": help$(2) = " (C)liente - (R)epres..."
   help$(13) = " C¢digo ": help$(3) = "  Endereco "
   help$(4) = "  Bairro ": help$(5) = "  Telefone ": help$(6) = "  Cidade "
   help$(7) = "  Cep    ": help$(8) = "  Unidade Federal ": help$(9) = " Cgc/Cnpj  "
   help$(10) = "  Inscri‡Æo Estadual ": help$(11) = "  Contato    "
   help$(12) = "  Telefone do Contato "
   Col(1) = 7: Col(2) = 66: Col(13) = 69
   Col(3) = 11: Col(4) = 9: Col(5) = 11: Col(6) = 9
   Col(7) = 6: Col(8) = 20: Col(9) = 6: Col(10) = 12: Col(11) = 10
   Col(12) = 11
   Vis(1) = 50: Vis(2) = 1: Vis(13) = 5
   Vis(3) = 50: Vis(4) = 20: Vis(5) = 13: Vis(6) = 30: Vis(7) = 10: Vis(8) = 2
   Vis(9) = 20: Vis(10) = 15: Vis(11) = 15: Vis(12) = 13
   FOR Wsx = 1 TO 13: Max(Wsx) = Vis(Wsx): NEXT Wsx
   FOR Y% = 1 TO 13: LOCATE Linha%(Y%), Col(Y%): PRINT Edit$(Y%): NEXT Y%
   Finished = False:  CurrCol = 1
   CurrTopLine = 1: CurrRow = 1
   GOSUB EdtTransPrintWholeScreen
   CurrTopLine = 1: CurrRow = 1
   PrintHelpLine help$(CurrCol)
   GOSUB EdtTransGetLine
   DO
    GOSUB EdtTransShowCursor                              'Show Cursor
    DO                                      'Wait for key
     Kbd$ = INKEY$
    LOOP UNTIL Kbd$ <> ""
    IF Kbd$ >= " " AND Kbd$ < "þ" THEN      'If legal, edit item
     REM
    END IF
    GOSUB EdtTransHideCursor                              'Hide Cursor so it can move
    SELECT CASE Kbd$
     CASE CHR$(0) + "H"                              'Up Arrow
      GOSUB EdtTransMoveUp
     CASE CHR$(0) + "P"                               'Down Arrow
      GOSUB EdtTransMoveDown
     CASE CHR$(0) + "K"                              'Up Arrow
      CurrCol = (CurrCol + 10) MOD 12 + 1
      PrintHelpLine help$(CurrCol)
     CASE CHR$(0) + "M"                               'Down Arrow
      CurrCol = (CurrCol MOD 12) + 1
      PrintHelpLine help$(CurrCol)
     CASE CHR$(0) + "I"                      'Page Up
      CurrRow = 1
      CurrTopLine = CurrTopLine - 10
      IF CurrTopLine < 1 THEN
       CurrTopLine = 1
      END IF
      GOSUB EdtTransPrintWholeScreen
      GOSUB EdtTransGetLine
      GOSUB EdtTransPrintLine
     CASE CHR$(0) + "Q"                      'Page Down
      CurrRow = 1
      CurrTopLine = CurrTopLine + 10
      IF CurrTopLine > Mx THEN
       CurrTopLine = Mx
      END IF
      GOSUB EdtTransPrintWholeScreen
      GOSUB EdtTransGetLine
      GOSUB EdtTransPrintLine
     CASE CHR$(27)
      Finished = True
     CASE ELSE
      BEEP
    END SELECT
   LOOP UNTIL Finished
  END IF
 END IF
 CLOSE #1
RETURN
EdtTransShowCursor:
 COLOR colors(8, ColorPref), colors(9, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$(1), Vis(1));
 LOCATE Linha%(CurrCol), Col(CurrCol)
 PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
 RETURN
EdtTransHideCursor:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 LOCATE CurrRow + 3, 11
 PRINT LEFT$(Edit$(1), Vis(1));
 LOCATE Linha%(CurrCol), Col(CurrCol)
 PRINT LEFT$(Edit$(CurrCol), Vis(CurrCol));
 RETURN
EdtTransMoveUp:
 IF CurrRow = 1 THEN
  IF CurrTopLine = 1 THEN
   BEEP
  ELSE
   ScrollDown
   CurrTopLine = CurrTopLine - 1
   GOSUB EdtTransGetLine
   GOSUB EdtTransPrintLine
  END IF
 ELSE
  CurrRow = CurrRow - 1
  GOSUB EdtTransGetLine
  GOSUB EdtTransPrintLine
 END IF
 RETURN
EdtTransMoveDown:
 IF (CurrRow + CurrTopLine - 1) >= Mx THEN
  BEEP
 ELSE
  IF CurrRow = 10 THEN
   ScrollUp
   CurrTopLine = CurrTopLine + 1
   GOSUB EdtTransGetLine
   GOSUB EdtTransPrintLine
  ELSE
   CurrRow = CurrRow + 1
   GOSUB EdtTransGetLine
   GOSUB EdtTransPrintLine
  END IF
 END IF
 RETURN
EdtTransPrintLine:
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 CurrRecord = CurrTopLine + CurrRow - 1
 LOCATE CurrRow + 3, 10
 IF CurrRecord = Mx + 1 THEN
  PRINT U1x$;
 ELSE
  IF CurrRecord > Mx THEN
   PRINT u1$;
  ELSE
   PRINT USING u$; Edit$(1)
   FOR Y% = 1 TO 13: LOCATE Linha%(Y%), Col(Y%): PRINT Edit$(Y%): NEXT Y%
  END IF
 END IF
RETURN
EdtTransPrintWholeScreen:
 temp = CurrRow
 FOR CurrRow = 1 TO 10
  CurrRecord = CurrTopLine + CurrRow - 1
  IF CurrRecord <= Mx THEN
   GOSUB EdtTransGetLine
  END IF
  GOSUB EdtTransPrintLine
 NEXT CurrRow
 CurrRow = temp
 RETURN
EdtTransGetLine:
 CurrRecord = CurrTopLine + CurrRow - 1
 GET #1, Reg(CurrRecord) + 1, Fo
 Edit$(1) = Fo.N: Edit$(2) = Fo.T: Edit$(3) = Fo.E: Edit$(4) = Fo.B
 Edit$(5) = Fo.F: Edit$(6) = Fo.c: Edit$(7) = Fo.P
 Edit$(8) = Fo.u: Edit$(9) = Fo.G: Edit$(10) = Fo.I
 Edit$(11) = Fo.K: Edit$(12) = Fo.o: Edit$(13) = Fo.R
RETURN
END SUB

SUB LoadState
 OPEN "money.dat" FOR INPUT AS #3
 INPUT #3, ColorPref
 CLOSE #3
END SUB

'Menu:
'  Handles Menu Selection for a single menu (either sub menu, or menu bar)
'  currChoiceX  :  Number of current choice
'  maxChoice    :  Number of choices in the list
'  choice$()    :  Array with the text of the choices
'  itemRow()    :  Array with the row of the choices
'  itemCol()    :  Array with the col of the choices
'  help$()      :  Array with the help text for each choice
'  barMode      :  Boolean:  TRUE = menu bar style, FALSE = drop down style
'
'  Returns the number of the choice that was made by changing currChoiceX
'  and returns the scan code of the key that was pressed to exit
'
FUNCTION Menu (CurrChoiceX, MaxChoice, choice$(), ItemRow(), ItemCol(), help$(), BarMode)
   
    currChoice = CurrChoiceX

    'if in bar mode, color in menu bar, else color box/shadow
    'bar mode means you are currently in the menu bar, not a sub menu
    IF BarMode THEN
        COLOR colors(7, ColorPref), colors(4, ColorPref)
        LOCATE 1, 1
        PRINT SPACE$(80);
    ELSE
        FancyCls colors(2, ColorPref), colors(1, ColorPref)
        COLOR colors(7, ColorPref), colors(4, ColorPref)
        Box ItemRow(1) - 1, ItemCol(1) - 1, ItemRow(MaxChoice) + 1, ItemCol(1) + LEN(choice$(1)) + 1
        
        COLOR colors(10, ColorPref), colors(6, ColorPref)
        FOR a = 1 TO MaxChoice + 1
            LOCATE ItemRow(1) + a - 1, ItemCol(1) + LEN(choice$(1)) + 2
            PRINT CHR$(178); CHR$(178);
        NEXT a
        LOCATE ItemRow(MaxChoice) + 2, ItemCol(MaxChoice) + 2
        PRINT STRING$(LEN(choice$(MaxChoice)) + 2, 178);
    END IF
    
    'print the choices
    COLOR colors(7, ColorPref), colors(4, ColorPref)
    FOR a = 1 TO MaxChoice
        LOCATE ItemRow(a), ItemCol(a)
        PRINT choice$(a);
    NEXT a

    Finished = False

    WHILE NOT Finished
        
        GOSUB MenuShowCursor
        GOSUB MenuGetKey
        GOSUB MenuHideCursor

        SELECT CASE Kbd$
            CASE CHR$(0) + "H": GOSUB MenuUp
            CASE CHR$(0) + "P": GOSUB MenuDown
            CASE CHR$(0) + "K": GOSUB MenuLeft
            CASE CHR$(0) + "M": GOSUB MenuRight
            CASE CHR$(13): GOSUB MenuEnter
            CASE CHR$(27): GOSUB MenuEscape
            CASE ELSE:  BEEP
        END SELECT
    WEND

    Menu = currChoice

    EXIT FUNCTION


MenuEnter:
    Finished = True
    RETURN

MenuEscape:
    currChoice = 0
    Finished = True
    RETURN

MenuUp:
    IF BarMode THEN
        BEEP
    ELSE
        currChoice = (currChoice + MaxChoice - 2) MOD MaxChoice + 1
    END IF
    RETURN

MenuLeft:
    IF BarMode THEN
        currChoice = (currChoice + MaxChoice - 2) MOD MaxChoice + 1
    ELSE
        currChoice = -2
        Finished = True
    END IF
    RETURN

MenuRight:
    IF BarMode THEN
        currChoice = (currChoice) MOD MaxChoice + 1
    ELSE
        currChoice = -3
        Finished = True
    END IF
    RETURN

MenuDown:
    IF BarMode THEN
        Finished = True
    ELSE
        currChoice = (currChoice) MOD MaxChoice + 1
    END IF
    RETURN

MenuShowCursor:
    COLOR colors(8, ColorPref), colors(9, ColorPref)
    LOCATE ItemRow(currChoice), ItemCol(currChoice)
    PRINT choice$(currChoice);
    PrintHelpLine help$(currChoice)
    RETURN

MenuGetKey:
    Kbd$ = ""
    WHILE Kbd$ = ""
        Kbd$ = INKEY$
    WEND
    RETURN

MenuHideCursor:
    COLOR colors(7, ColorPref), colors(4, ColorPref)
    LOCATE ItemRow(currChoice), ItemCol(currChoice)
    PRINT choice$(currChoice);
    RETURN


END FUNCTION

'MenuSystem:
'  Main routine that controls the program.  Uses the MENU function
'  to implement menu system and calls the appropriate function to handle
'  the user's selection
SUB MenuSystem
 DIM choice$(20), menuRow(20), menuCol(20), help$(20)
 LOCATE , , 0
 choice = 1
 Finished = False
 WHILE NOT Finished
  GOSUB MenuSystemMain
  subchoice = -1
  WHILE subchoice < 0
   SELECT CASE choice
    CASE 1: GOSUB MenuSystemFile
    CASE 2: GOSUB MenuSystemCount
    CASE 3: GOSUB MenuSystemColors
    '    CASE 6: GOSUB MenuSystemDir
   END SELECT
   FancyCls colors(2, ColorPref), colors(1, ColorPref)
   SELECT CASE subchoice
    CASE -2: choice = (choice + 3) MOD 5 + 1
    CASE -3: choice = (choice) MOD 3 + 1
   END SELECT
  WEND
 WEND
 EXIT SUB
MenuSystemMain:
 FancyCls colors(2, ColorPref), colors(1, ColorPref)
 COLOR colors(7, ColorPref), colors(4, ColorPref)
 Box 9, 13, 14, 69
 Center 11, "Use as teclas de direcao para navegar no Menu Principal"
 Center 12, "Pressione <Enter> para selecionar o Menu de itens      "
 choice$(1) = " Cadastro   "
 choice$(2) = " Movimentos "
 choice$(3) = " Cores      "
 'choice$(6) = ""
 menuRow(1) = 1: menuCol(1) = 2
 menuRow(2) = 1: menuCol(2) = 20
 menuRow(3) = 1: menuCol(3) = 41
 help$(1) = "Manipular representante"
 help$(2) = "Lan‡a Contas ou Receitas"
 help$(3) = "Manipular cor da tela"
 'help$(6) = ""
 DO
  NewChoice = Menu((choice), 3, choice$(), menuRow(), menuCol(), help$(), True)
 LOOP WHILE NewChoice = 0
 choice = NewChoice
RETURN
MenuSystemFile:
 choice$(2) = " Consultas      "
 choice$(1) = " Cadastro       "
 choice$(3) = " Alterar Cad.   "
 choice$(4) = " Exit           "
 menuRow(1) = 3: menuCol(1) = 2
 menuRow(2) = 4: menuCol(2) = 2
 menuRow(3) = 5: menuCol(3) = 2
 menuRow(4) = 6: menuCol(4) = 2
 help$(2) = "Cadastra representantes"
 help$(1) = "Cadastra cadastro "
 help$(3) = "Alterar Cadastro "
 help$(4) = "Encerrar programa"
 subchoice = Menu(1, 4, choice$(), menuRow(), menuCol(), help$(), False)
 SELECT CASE subchoice
  CASE 1: CadClie
  CASE 2: ListClie
  CASE 3: AlterCad
  CASE 4: Finished = True
  CASE ELSE
 END SELECT
RETURN
MenuSystemCount:
 choice$(1) = " Lan‡ar Contas Ou Receitas     "
 choice$(2) = " Mostrar Todos Os Lan‡amentos  "
 choice$(3) = " Relat¢rio De Contas · Receber "
 choice$(4) = " Relat¢rio De Contas · Pagar   "
 choice$(5) = " Relat¢rio De Posi‡Æo          "
 menuRow(1) = 3: menuCol(1) = 20
 menuRow(2) = 4: menuCol(2) = 20
 menuRow(3) = 5: menuCol(3) = 20
 menuRow(4) = 6: menuCol(4) = 20
 menuRow(5) = 7: menuCol(5) = 20
 help$(1) = " Lan‡ar contas ou receitas "
 help$(2) = " Empenho De Todas as Contas "
 help$(3) = " Relat¢rio de Contas a Receber "
 help$(4) = " Relat¢rio de Contas a Pagar "
 help$(5) = " Relat¢rio de Contas a Receber menos as Contas … Pagar "
 subchoice = Menu(1, 5, choice$(), menuRow(), menuCol(), help$(), False)
 SELECT CASE subchoice
  CASE 1
   Empenho
   'EditAccounts
  CASE 2
   DigTrans
  CASE 3
   Box 11, 20, 14, 49 ' 21234567890123456789012345678
   LOCATE 12, 21: PRINT "Contas · Receber No Per¡odo:"
   LOCATE 13, 21: PRINT "De:__/__/__ · __/__/__      "
   Din$ = "": Dfi$ = ""
   Start$ = ""
   Kbd$ = GetString$(13, 24, Start$, End$, 8, 8)
   Din$ = End$
   Start$ = ""
   Kbd$ = GetString$(13, 35, Start$, End$, 8, 8)
   Dfi$ = End$
   IF Din$ <> "" AND Dfi$ <> "" THEN Creceber
  CASE 4
   Box 11, 20, 14, 49 ' 21234567890123456789012345678
   LOCATE 12, 21: PRINT "Contas · Pagar No Per¡odo:  "
   LOCATE 13, 21: PRINT "De:__/__/__ · __/__/__      "
   Din$ = "": Dfi$ = ""
   Start$ = ""
   Kbd$ = GetString$(13, 24, Start$, End$, 8, 8)
   Din$ = End$
   Start$ = ""
   Kbd$ = GetString$(13, 35, Start$, End$, 8, 8)
   Dfi$ = End$
   IF Din$ <> "" AND Dfi$ <> "" THEN Cpagar
  CASE 5
   Box 11, 20, 14, 49 ' 21234567890123456789012345678
   LOCATE 12, 21: PRINT "Fluxo De Posi‡Æo No Per¡odo:"
   LOCATE 13, 21: PRINT "De:__/__/__ · __/__/__      "
   Din$ = "": Dfi$ = ""
   Start$ = ""
   Kbd$ = GetString$(13, 24, Start$, End$, 8, 8)
   Din$ = End$
   Start$ = ""
   Kbd$ = GetString$(13, 35, Start$, End$, 8, 8)
   Dfi$ = End$
   IF Din$ <> "" AND Dfi$ <> "" THEN Fluxo
  CASE ELSE
 END SELECT
RETURN
MenuSystemReport:
 choice$(1) = ""
 menuRow(1) = 3: menuCol(1) = 32
 help$(1) = ""
 subchoice = Menu(1, 1, choice$(), menuRow(), menuCol(), help$(), False)
 SELECT CASE subchoice
  CASE 1
  CASE ELSE
 END SELECT
 RETURN
MenuSystemColors:
 choice$(1) = " Monochrome Scheme "
 choice$(2) = " Cyan/Blue Scheme  "
 choice$(3) = " Blue/Cyan Scheme  "
 choice$(4) = " Red/Grey Scheme   "
 menuRow(1) = 3: menuCol(1) = 41
 menuRow(2) = 4: menuCol(2) = 41
 menuRow(3) = 5: menuCol(3) = 41
 menuRow(4) = 6: menuCol(4) = 41
 help$(1) = "Color scheme for monochrome and LCD displays"
 help$(2) = "Color scheme featuring cyan"
 help$(3) = "Color scheme featuring blue"
 help$(4) = "Color scheme featuring red"
 subchoice = Menu(1, 4, choice$(), menuRow(), menuCol(), help$(), False)
 SELECT CASE subchoice
  CASE 1 TO 4
   ColorPref = subchoice
  CASE ELSE
 END SELECT
RETURN
END SUB

FUNCTION OrdemData$ (Mens$)
 P% = INSTR(1, Mens$, "/")
 IF P% = 0 THEN P% = INSTR(1, Mens$, ".")
 IF P% = 0 THEN P% = INSTR(1, Mens$, "-")
 IF P% <> 0 THEN
  DIA$ = LTRIM$(MID$(Mens$, 1, P% - 1))
  DIA$ = RTRIM$(DIA$)
  P1% = INSTR(P% + 1, Mens$, "/")
  IF P1% = 0 THEN P1% = INSTR(P% + 1, Mens$, ".")
  IF P1% = 0 THEN P1% = INSTR(P% + 1, Mens$, "-")
  ANO$ = ""
  IF P1% <> 0 THEN
   MES$ = LTRIM$(MID$(Mens$, P% + 1, (P1% - P%) - 1))
   MES$ = RTRIM$(MES$)
   ANO$ = LTRIM$(MID$(Mens$, P1% + 1, 4))
  ELSE
   MES$ = LTRIM$(MID$(Mens$, P% + 1, 2))
   MES$ = RTRIM$(MES$)
  END IF
  IF VAL(DIA$) <= 9 AND LEN(DIA$) = 1 THEN DIA$ = "0" + DIA$
  IF VAL(MES$) <= 9 AND LEN(MES$) = 1 THEN MES$ = "0" + MES$
  OrdemData$ = ANO$ + MES$ + DIA$
 END IF
END FUNCTION

'PrintHelpLine:
'  Prints help text on the bottom row in the proper color
SUB PrintHelpLine (help$)
    COLOR colors(5, ColorPref), colors(4, ColorPref)
    LOCATE 25, 1
    PRINT SPACE$(80);
    Center 25, help$
    'LOCATE 1, 65: PRINT "Periodo:"; ATUAL$
END SUB

SUB SaveState
 OPEN "money.dat" FOR OUTPUT AS #3
 PRINT #3, ColorPref
 CLOSE #3
END SUB

SUB ScrolDown
 DEF SEG = VARSEG(ScrolDownAsm(1))
 CALL Absolute(VARPTR(ScrolDownAsm(1)))
 DEF SEG
END SUB

SUB ScrollDown
 DEF SEG = VARSEG(ScrollDownAsm(1))
 CALL Absolute(VARPTR(ScrollDownAsm(1)))
 DEF SEG
END SUB

SUB ScrollUp
 DEF SEG = VARSEG(ScrollUpAsm(1))
 CALL Absolute(VARPTR(ScrollUpAsm(1)))
 DEF SEG
END SUB

SUB ScrolUp
 DEF SEG = VARSEG(ScrolUpAsm(1))
 CALL Absolute(VARPTR(ScrolUpAsm(1)))
 DEF SEG
END SUB

'SparklePause:
'  Creates flashing border for intro screen
SUB SparklePause

    COLOR 4, 0
    a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
    WHILE INKEY$ <> "": WEND 'Clear keyboard buffer

    WHILE INKEY$ = ""
        FOR a = 1 TO 5
            LOCATE 1, 1                             'print horizontal sparkles
            PRINT MID$(a$, a, 80);
            LOCATE 22, 1
            PRINT MID$(a$, 6 - a, 80);

            FOR B = 2 TO 21                         'Print Vertical sparkles
                c = (a + B) MOD 5
                IF c = 1 THEN
                    LOCATE B, 80
                    PRINT "*";
                    LOCATE 23 - B, 1
                    PRINT "*";
                ELSE
                    LOCATE B, 80
                    PRINT " ";
                    LOCATE 23 - B, 1
                    PRINT " ";
                END IF
            NEXT B
        NEXT a
    WEND
END SUB

'Trin$:
'  Remove null and spaces from the end of a string.
FUNCTION Trim$ (X$)

    IF X$ = "" THEN
        Trim$ = ""
    ELSE
        lastChar = 0
        FOR a = 1 TO LEN(X$)
            Y$ = MID$(X$, a, 1)
            IF Y$ <> CHR$(0) AND Y$ <> " " THEN
                lastChar = a
            END IF
        NEXT a
        Trim$ = LEFT$(X$, lastChar)
    END IF
    
END FUNCTION

