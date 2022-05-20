%%
%name While
%term PROGRAM
    | DCOLON
    | VAR
    | COLON
    | SEMICOLON
    | INT
    | BOOL
    | COMMA
    | LCURLY
    | RCURLY
    | ASSIGN
    | READ
    | WRITE
    | IF
    | THEN
    | ELSE
    | ENDIF
    | WHILE
    | DO
    | ENDWH
    | LPAREN
    | RPAREN
    | OR
    | AND
    | TT
    | FF
    | LT
    | LE
    | GT
    | GE
    | EQ
    | NE
    | PLUS
    | MINUS
    | MUL
    | DIV
    | MOD
    | NEG
    | NOT
    | EOF
    | BADCH
    | NUMERAL of int
    | IDENTIFIER of string
    
%nonterm Program of AST.AST
       | Block of (AST.AST list * AST.AST) list * AST.AST list
       | DeclarationSeq of (AST.AST list * AST.AST) list
       | CommandSeq of AST.AST list
       | Declaration of AST.AST list * AST.AST
       | VariableList of AST.AST list
       | Type of AST.AST
       | Variable of AST.AST
       | Commands of AST.AST list
       | Command of AST.AST
       | IntExpression of AST.AST
       | BoolExpression of AST.AST
       | IntTerm of AST.AST
       | IntFactor of AST.AST
       | BoolTerm of AST.AST
       | BoolFactor of AST.AST
       | Comparison of AST.AST

%start Program
%pos int
%eop EOF
%noshift EOF
%verbose
%keyword PROGRAM VAR INT BOOL READ WRITE IF THEN ELSE ENDIF WHILE DO ENDWH TT FF 
%arg (fileName) : string

%%
Program: PROGRAM IDENTIFIER DCOLON Block ((AST.Program(IDENTIFIER, Block)))
Block: DeclarationSeq CommandSeq ((DeclarationSeq, CommandSeq))
DeclarationSeq: Declaration DeclarationSeq ((Declaration::DeclarationSeq))
            | (([]))
Declaration: VAR VariableList COLON Type SEMICOLON ((VariableList, Type))
Type: INT ((AST.INT))
   | BOOL ((AST.BOOL))
VariableList: Variable COMMA VariableList ((Variable::VariableList))
            | Variable (([Variable]))
CommandSeq: LCURLY Commands RCURLY ((Commands))
Commands: Command SEMICOLON Commands ((Command::Commands))
      | (([]))
Command: Variable ASSIGN BoolExpression ((AST.AssignBool(Variable, BoolExpression)))
      | Variable ASSIGN IntExpression ((AST.AssignInt(Variable, IntExpression)))
      | READ Variable ((AST.Read(Variable)))
      | WRITE IntExpression ((AST.Write(IntExpression)))
      | IF BoolExpression THEN CommandSeq ELSE CommandSeq ENDIF ((AST.IfThenElse(BoolExpression, CommandSeq, CommandSeq)))
      | WHILE BoolExpression DO CommandSeq ENDWH ((AST.WhileDo(BoolExpression, CommandSeq)))
IntExpression: IntExpression PLUS IntTerm ((AST.Add(IntExpression, IntTerm)))
            | IntExpression MINUS IntTerm ((AST.Sub(IntExpression, IntTerm)))
            | IntTerm   ((IntTerm))
IntTerm: IntTerm MUL IntFactor ((AST.Multiply(IntTerm, IntFactor)))
      | IntTerm DIV IntFactor ((AST.Divide(IntTerm, IntFactor)))
      | IntTerm MOD IntFactor ((AST.Modulo(IntTerm, IntFactor)))
      | IntFactor ((IntFactor))
IntFactor: NUMERAL ((AST.Num(NUMERAL)))
      | Variable ((Variable))
      | LPAREN IntExpression RPAREN ((AST.IntExpInPar(IntExpression)))
      | NEG IntFactor ((AST.Neg(IntFactor)))
      | PLUS IntFactor ((IntFactor))
BoolExpression: BoolExpression OR BoolTerm ((AST.Or(BoolExpression, BoolTerm)))
            | BoolTerm ((BoolTerm))
BoolTerm: BoolTerm AND BoolFactor ((AST.And(BoolTerm, BoolFactor)))
      | BoolFactor ((BoolFactor))    
BoolFactor: TT ((AST.TT))
      | FF ((AST.FF))
      | Variable ((Variable))
      | Comparison ((Comparison))
      | LPAREN BoolExpression RPAREN ((AST.BoolExpInPar(BoolExpression)))
      | NOT BoolFactor ((AST.Not(BoolFactor)))
Comparison: IntExpression LT IntExpression ((AST.LessThan(IntExpression1, IntExpression2)))
      | IntExpression LE IntExpression ((AST.LessThanEq(IntExpression1, IntExpression2)))
      | IntExpression EQ IntExpression ((AST.Equals(IntExpression1, IntExpression2)))
      | IntExpression NE IntExpression ((AST.NotEqual(IntExpression1, IntExpression2)))
      | IntExpression GT IntExpression ((AST.GreaterThan(IntExpression1, IntExpression2)))
      | IntExpression GE IntExpression ((AST.GreaterThanEq(IntExpression1, IntExpression2)))
Variable: IDENTIFIER ((AST.Variable(IDENTIFIER)))
