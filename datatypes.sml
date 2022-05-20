(* structure AST =
struct
    datatype Program = Program of string * Block
    and      Block = Block of Declaration list * Command list
    and      DeclarationSeq = DeclarationSeq of Declaration list
    and      Declaration = Declaration of Variable list * Type
    and      Type = INT
                  | BOOL
    and      VariableList  = VariableList of Variable list
    and      CommandSeq = CommandSeq of Command list
    and      Commands = Commands of Command list
    and      Command = Assign of Variable * Expression
                    | Read of Variable
                    | Write of IntExpression
                    | IfThenElse of BoolExpression * Command list * Command list
                    | WhileDo of BoolExpression * Command list
    and      Expression = IntExp of IntExpression
                      | BoolExp of BoolExpression
    and      IntExpression = Plus of IntExpression * IntTerm
                        | Minus of IntExpression * IntTerm
                        | IntExpression of IntTerm
    and      IntTerm = Multiply of IntTerm * IntFactor
                    | Divide of IntTerm * IntFactor
                    | Modulo of IntTerm * IntFactor
                    | IntTe of IntFactor
    and      IntFactor = Number of int
                      | Var of Variable
                      | Exp of IntExpression
    and      BoolExpression = Or of BoolExpression * BoolTerm
                          | BoolExpression of BoolTerm
    and      BoolTerm = And of BoolTerm * BoolFactor
                    | BoolTerm of BoolFactor
    and      BoolFactor = TT
                      | FF
                      | BoolVar of Variable
                      | Cmp of Comparison
                      | BoolFact of BoolExpression
                      | Not of BoolFactor
    and Comparison = Comparison of IntExpression * RelOp * IntExpression
    and Variable = Variab of string
    and ID = ID of string
    and NUMERAL = NUMERAL of int
    and RelOp = LT
              | LE
              | GT
              | GE
              | EQ
              | NE
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
end; *)

structure AST = 
    struct 
        datatype AST = Program of string * ((AST list * AST) list * AST list)
                    | AssignBool of AST * AST
                    | AssignInt of AST * AST
                    | Read of AST
                    | Write of AST
                    | IfThenElse of AST * AST list * AST list
                    | WhileDo of AST * AST list
                    | Add of AST * AST
                    | Sub of AST * AST
                    | Multiply of AST * AST
                    | Divide of AST * AST
                    | Modulo of AST * AST
                    | Num of int
                    | Neg of AST
                    | IntExpInPar of AST
                    | Or of AST * AST
                    | And of AST * AST
                    | TT
                    | FF
                    | Comp of AST
                    | BoolExpInPar of AST
                    | Not of AST
                    | LessThan of AST * AST
                    | LessThanEq of AST * AST
                    | GreaterThan of AST * AST
                    | GreaterThanEq of AST * AST
                    | Equals of AST * AST
                    | NotEqual of AST * AST
                    | Variable of string
                    | Identifier of string
                    | PROGRAM 
                    | ASSIGNINT
                    | ASSIGNBOOL
                    | READ
                    | WRITE
                    | IFTHENELSE
                    | WHILEDO
                    | INT
                    | BOOL
                    | ADD
                    | SUB
                    | MULTIPLY
                    | DIVIDE
                    | MODULO
                    | OR
                    | AND
                    | NOT
                    | NEG
end;


(* structure AST =
    struct
        datatype AST =  AST of AST
                    | Program of string * ((AST list * string) list * AST list)
                    (* | Block of (AST list * string) list * AST list *)
                    (* | Declaration of AST list * string *)
                    | Type of string
                    | Assign of AST * AST
                    | Read of AST
                    | Write of AST
                    | IfThenElse of AST * AST list * AST list
                    | WhileDo of AST * AST list
                    | Add of AST * AST
                    | Sub of AST * AST
                    | Multiply of AST * AST
                    | Divide of AST * AST
                    | Modulo of AST * AST
                    | Neg of AST
                    | And of AST * AST
                    | Or of AST * AST
                    | Bool of bool
                    | Not of AST
                    | LessThan of AST * AST
                    | LessThanEq of AST * AST
                    | Equals of AST * AST
                    | GreaterThan of AST * AST
                    | GreaterThanEq of AST * AST
                    | NotEquals of AST * AST
                    | Numeral of int
                    | Id of string
end; *)
