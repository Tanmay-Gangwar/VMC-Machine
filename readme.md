
## Important files:
 `While.lex` : It contains definitions, declarations and rules for creating lexer for while language
 `While.yacc`: It contains definitions, declarations and rules for creating parser for while language
 `compiler.sml`: It contains structure to compile While programs
 `glue.sml`: It combines `While.lex` and `While.yacc`
 `DataTypes.sml`: It defines Abstract Syntax Tree Data Structure
 `MakeFile`: It runs lexer and parser and creates compiler which outputs Abstract Syntax Tree
 `While.cm`: It create a structure for While language and provides basis library, ml-lex and ml-yacc library

## How to run parser:
- Just type `make` in command line - terminal.

## CONTEXT-FREE GRAMMAR
- Terminals
```sh
    PROGRAM
    DCOLON
    VAR
    COLON
    SEMICOLON
    INT
    BOOL
    COMMA
    LCURLY
    RCURLY
    ASSIGN
    READ
    WRITE
    IF
    THEN
    ELSE
    ENDIF
    WHILE
    DO
    ENDWH
    LPAREN
    RPAREN
    OR
    AND
    TT
    FF
    LT
    LE
    GT
    GE
    EQ
    NE
    PLUS
    MINUS
    MUL
    DIV
    MOD
    NEG
    NOT
    EOF
    NUMERAL of int
    IDENTIFIER of string
```
- NonTerminals
```
    Program of AST.Program
    Block of AST.Block
    DeclarationSeq of AST.DeclarationSeq
    CommandSeq of AST.CommandSeq
    Declaration of AST.Declaration
    VariableList of AST.VariableList
    Type of AST.Type
    Variable of AST.Variable
    Commands of AST.Commands
    Command of AST.Command
    Expression of AST.Expression
    IntExpression of AST.IntExpression
    BoolExpression of AST.BoolExpression
    AddOp of AST.AddOp
    IntTerm of AST.IntTerm
    MultOp of AST.MultOp
    IntFactor of AST.IntFactor
    BoolTerm of AST.BoolTerm
    BoolFactor of AST.BoolFactor
    Comparison of AST.Comparison
    RelOp of AST.RelOp
    Identifier of AST.IDENTIFIER
    Numeral of AST.NUMERAL
 ```
- Start Symbol `Program`

- Production Rules:
```
Program           ::= “program” Identifier “::”Block .
Block             ::= DeclarationSeq CommandSeq .
DeclarationSeq    ::= {Declaration} .
Declaration       ::= “var” V ariableList“:”T ype“;” .
Type              ::= “int” | “bool” .
VariableList      ::= V ariable{“,” V ariable} .
CommandSeq        ::= “{”{Command“;”}“}” .
Command           ::= Variable“:=”Expression
                   | “read” Variable
                   | “write” IntExpression
                   | “if” BoolExpression “then” CommandSeq “else” CommandSeq “endif” 
                   | “while” BoolExpression “do” CommandSeq “endwh” .
Expression        ::= IntExpression | BoolExpression .
IntExpression     ::= IntExpression AddOp IntT erm | IntTerm .
IntTerm           ::= IntTerm MultOp IntFactor | IntFactor .
IntFactor         ::= Numeral | Variable | “(”IntExpression“)” | “˜”IntFactor .
BoolExpression    ::= BoolExpression “||” BoolTerm | BoolTerm .
BoolTerm          ::= BoolTerm “&&” BoolFactor | BoolFactor .
BoolFactor        ::= “tt” | “ff” | Variable | Comparison | “(”BoolExpression“)” | “!”BoolF actor .
Comparison        ::= IntExpression RelOp IntExpression .
Variable          ::= Identifier .
RelOp             ::= “<” | “<=” | “=” | “>” | “>=” | “<>” .
AddOp             ::= “+” | “−” .
MultOp            ::= “∗” | “/” | “%” .
Identifier        ::= Letter{Letter | Digit} .
Numeral           ::= [“+” | “˜”]Digit{Digit} .
```

- Syntax-Directed Translation:
```
Program: PROGRAM Identifier DCOLON Block (AST.Program(AST.PROGRAM, Identifier, AST.DCOLON, Block))
Block: DeclarationSeq CommandSeq (AST.Block0(DeclarationSeq, CommandSeq))
      | CommandSeq (AST.Block1(CommandSeq))
DeclarationSeq: Declaration (AST.DeclarationSeq0(Declaration))
              | Declaration DeclarationSeq (AST.DeclarationSeq1(Declaration, DeclarationSeq))
Declaration: VAR VariableList COLON Type SEMICOLON (AST.Declaration(AST.VAR, VariableList, AST.COLON, Type, AST.SEMICOLON))
Type: INT (AST.INT)
    | BOOL (AST.BOOL)
VariableList: Variable (AST.VariableList0(Variable))
            | Variable COMMA VariableList (AST.VariableList1(Variable, AST.COMMA, VariableList))
CommandSeq: LCURLY Commands RCURLY (AST.CommandSeq(AST.LCURLY, Commands, AST.RCURLY))
Commands: Command (AST.Commands0(Command))
        | Command SEMICOLON Commands (AST.Commands1(Command, AST.SEMICOLON, Commands))
Command: Variable ASSIGN Expression (AST.Command0(Variable, AST.ASSIGN, Expression))
       | READ Variable (AST.Command1(AST.READ, Variable))
       | WRITE IntExpression (AST.Command2(AST.WRITE, IntExpression))
       | IF BoolExpression THEN CommandSeq ELSE CommandSeq ENDIF (AST.Command3(AST.IF, BoolExpression, AST.THEN, CommandSeq, AST.ELSE, CommandSeq, AST.ENDIF))
       | WHILE BoolExpression DO CommandSeq ENDWH (AST.Command4(AST.WHILE, BoolExpression, AST.DO, CommandSeq, AST.ENDWH))
Expression: IntExpression (AST.Expression0(IntExpression))
        | BoolExpression (AST.Expression1(BoolExpression))
IntExpression: IntExpression AddOp IntTerm (AST.IntExpression0(IntExpression, AddOp, IntTerm))
            | IntTerm (AST.IntExpression1(IntTerm))
IntTerm: IntTerm MultOp IntFactor (AST.IntTerm0(IntTerm, MultOp, IntFactor))
      | IntFactor (AST.IntTerm1(IntFactor))
IntFactor: Numeral (AST.IntFactor0(Numeral))
        | Variable (AST.IntFactor1(Variable))
        | LPAREN IntExpression RPAREN (AST.IntFactor2(AST.LPAREN, IntExpression, AST.RPAREN))
BoolExpression: BoolExpression OR BoolTerm (AST.BoolExpression0(BoolExpression, AST.OR, BoolTerm))
            | BoolTerm (AST.BoolExpression1(BoolTerm))
BoolTerm: BoolTerm AND BoolFactor (AST.BoolTerm0(BoolTerm, AST.AND, BoolFactor))
      | BoolFactor (AST.BoolTerm1(BoolFactor))
BoolFactor: TT (AST.BoolFactor0(AST.TT))
          | FF (AST.BoolFactor0(AST.FF))
          | Variable (AST.BoolFactor1(Variable))
          | Comparison (AST.BoolFactor2(Comparison))
          | LPAREN BoolExpression RPAREN (AST.BoolFactor3(AST.LPAREN, BoolExpression, AST.RPAREN))
          | NOT BoolFactor (AST.BoolFactor4(AST.NOT, BoolFactor))
Comparison: IntExpression RelOp IntExpression (AST.Comparison(IntExpression, RelOp, IntExpression))
Variable: Identifier (AST.Variable(Identifier))
RelOp: LT (AST.LT)
    | LE (AST.LE)
    | GT (AST.GT)
    | GE (AST.GE)
    | EQ (AST.EQ)
    | NE (AST.NE)
AddOp: PLUS (AST.PLUS)
      | MINUS (AST.MINUS)
MultOp: MUL (AST.MUL)
      | DIV (AST.DIV)
      | MOD (AST.MOD)
```
## DATATYPES
```
    type IDENTIFIER = string
    type NUMERAL = int
    datatype Program = Program of Keyword * IDENTIFIER * Symbol * Block
    and      Block = Block0 of DeclarationSeq * CommandSeq
                  | Block1 of CommandSeq
    and      DeclarationSeq = DeclarationSeq0 of Declaration
                          | DeclarationSeq1 of Declaration * DeclarationSeq
    and      Declaration = Declaration of Keyword * VariableList * Symbol * Type * Symbol
    and      Type = INT
                  | BOOL
    and      VariableList  = VariableList1 of Variable * Symbol * VariableList
                        | VariableList0 of Variable
    and      CommandSeq = CommandSeq of Symbol * Commands * Symbol
    and      Commands = Commands0 of Command
                    | Commands1 of Command * Symbol * Commands
    and      Command = Command0 of Variable * Symbol * Expression
                    | Command1 of Keyword * Variable
                    | Command2 of Keyword * IntExpression
                    | Command3 of Keyword * BoolExpression * Keyword * CommandSeq * Keyword * CommandSeq * Keyword
                    | Command4 of Keyword * BoolExpression * Keyword * CommandSeq * Keyword
    and      Expression = Expression0 of IntExpression
                      | Expression1 of BoolExpression
    and      IntExpression = IntExpression0 of IntExpression * AddOp * IntTerm
                          | IntExpression1 of IntTerm
    and      IntTerm = IntTerm0 of IntTerm * MultOp * IntFactor
                    | IntTerm1 of IntFactor
    and      IntFactor = IntFactor0 of NUMERAL
                      | IntFactor1 of Variable
                      | IntFactor2 of Symbol * IntExpression * Symbol
    and      BoolExpression = BoolExpression0 of BoolExpression * Symbol * BoolTerm
                          | BoolExpression1 of BoolTerm
    and      BoolTerm = BoolTerm0 of BoolTerm * Symbol * BoolFactor
                    | BoolTerm1 of BoolFactor
    and      BoolFactor = BoolFactor0 of Keyword
                      | BoolFactor1 of Variable
                      | BoolFactor2 of Comparison
                      | BoolFactor3 of Symbol * BoolExpression * Symbol
                      | BoolFactor4 of Symbol * BoolFactor
    and Comparison = Comparison of IntExpression * RelOp * IntExpression
    and Variable = Variable of IDENTIFIER
    and RelOp = LT
              | LE
              | GT
              | GE
              | EQ
              | NE
    and AddOp = PLUS
              | MINUS
    and MultOp = MUL
              | DIV
              | MOD
    and Symbol = DCOLON 
              | COMMA 
              | LCURLY 
              | RCURLY 
              | COLON
              | SEMICOLON 
              | ASSIGN 
              | LPAREN 
              | RPAREN 
              | OR
              | AND
              | NOT
              | NEG
    and Keyword = PROGRAM 
              | VAR 
              | READ
              | WRITE
              | IF
              | THEN
              | ELSE
              | ENDIF
              | WHILE
              | DO
              | ENDWH
              | TT
              | FF
```
## Acknowledgements 
- Mainly for compiler.sml, glue.sml and examples related to .lex and .yacc
- UG.pdf    (http://rogerprice.org/ug/ug.pdf)
- Hypernotes
- https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html#section23
- https://www.smlnj.org/doc/ML-Lex/manual.html
- http://www.smlnj.org/doc/ML-Yacc/

