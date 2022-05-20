functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\003\000\000\000\
\\001\000\002\000\005\000\000\000\
\\001\000\004\000\024\000\000\000\
\\001\000\005\000\125\000\015\000\135\000\019\000\135\000\022\000\125\000\
\\023\000\135\000\024\000\135\000\027\000\125\000\028\000\125\000\
\\029\000\125\000\030\000\125\000\031\000\125\000\032\000\125\000\
\\033\000\125\000\034\000\125\000\035\000\125\000\036\000\125\000\
\\037\000\125\000\000\000\
\\001\000\005\000\025\000\000\000\
\\001\000\005\000\076\000\000\000\
\\001\000\006\000\051\000\007\000\050\000\000\000\
\\001\000\009\000\012\000\000\000\
\\001\000\010\000\026\000\000\000\
\\001\000\011\000\027\000\000\000\
\\001\000\015\000\074\000\023\000\059\000\000\000\
\\001\000\016\000\095\000\000\000\
\\001\000\017\000\097\000\000\000\
\\001\000\019\000\060\000\023\000\059\000\000\000\
\\001\000\020\000\094\000\000\000\
\\001\000\021\000\042\000\025\000\041\000\026\000\040\000\033\000\039\000\
\\038\000\038\000\039\000\037\000\042\000\036\000\043\000\015\000\000\000\
\\001\000\021\000\046\000\033\000\039\000\038\000\038\000\042\000\036\000\
\\043\000\015\000\000\000\
\\001\000\022\000\091\000\023\000\059\000\000\000\
\\001\000\022\000\092\000\027\000\068\000\028\000\067\000\029\000\066\000\
\\030\000\065\000\031\000\064\000\032\000\063\000\033\000\062\000\
\\034\000\061\000\000\000\
\\001\000\022\000\092\000\033\000\062\000\034\000\061\000\000\000\
\\001\000\027\000\068\000\028\000\067\000\029\000\066\000\030\000\065\000\
\\031\000\064\000\032\000\063\000\033\000\062\000\034\000\061\000\000\000\
\\001\000\040\000\000\000\000\000\
\\001\000\043\000\004\000\000\000\
\\001\000\043\000\015\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\003\000\009\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\008\000\023\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\012\000\022\000\013\000\021\000\014\000\020\000\018\000\019\000\
\\043\000\015\000\000\000\
\\111\000\023\000\059\000\000\000\
\\112\000\027\000\068\000\028\000\067\000\029\000\066\000\030\000\065\000\
\\031\000\064\000\032\000\063\000\033\000\062\000\034\000\061\000\000\000\
\\113\000\000\000\
\\114\000\033\000\062\000\034\000\061\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\035\000\058\000\036\000\057\000\037\000\056\000\000\000\
\\118\000\035\000\058\000\036\000\057\000\037\000\056\000\000\000\
\\119\000\035\000\058\000\036\000\057\000\037\000\056\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\024\000\055\000\000\000\
\\130\000\024\000\055\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\033\000\062\000\034\000\061\000\000\000\
\\140\000\033\000\062\000\034\000\061\000\000\000\
\\141\000\033\000\062\000\034\000\061\000\000\000\
\\142\000\033\000\062\000\034\000\061\000\000\000\
\\143\000\033\000\062\000\034\000\061\000\000\000\
\\144\000\033\000\062\000\034\000\061\000\000\000\
\\145\000\000\000\
\"
val actionRowNumbers =
"\000\000\022\000\001\000\027\000\
\\027\000\007\000\024\000\023\000\
\\026\000\025\000\035\000\032\000\
\\002\000\069\000\004\000\008\000\
\\009\000\015\000\015\000\016\000\
\\023\000\023\000\006\000\035\000\
\\033\000\015\000\060\000\057\000\
\\055\000\048\000\044\000\013\000\
\\020\000\003\000\049\000\015\000\
\\016\000\016\000\059\000\058\000\
\\015\000\010\000\039\000\050\000\
\\016\000\038\000\031\000\005\000\
\\030\000\029\000\034\000\036\000\
\\037\000\015\000\016\000\016\000\
\\016\000\015\000\007\000\016\000\
\\016\000\016\000\016\000\016\000\
\\016\000\016\000\016\000\062\000\
\\052\000\053\000\017\000\018\000\
\\007\000\019\000\028\000\056\000\
\\047\000\046\000\045\000\054\000\
\\014\000\043\000\042\000\066\000\
\\065\000\068\000\067\000\064\000\
\\063\000\061\000\051\000\011\000\
\\041\000\007\000\012\000\040\000\
\\021\000"
val gotoT =
"\
\\001\000\096\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\005\000\004\000\000\000\
\\003\000\008\000\005\000\004\000\000\000\
\\004\000\009\000\000\000\
\\000\000\
\\006\000\012\000\008\000\011\000\000\000\
\\000\000\
\\000\000\
\\008\000\016\000\009\000\015\000\010\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\015\000\028\000\016\000\027\000\017\000\026\000\000\000\
\\008\000\033\000\011\000\032\000\012\000\041\000\013\000\030\000\
\\014\000\029\000\015\000\028\000\016\000\027\000\017\000\026\000\000\000\
\\008\000\043\000\011\000\042\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\045\000\000\000\
\\006\000\046\000\008\000\011\000\000\000\
\\007\000\047\000\000\000\
\\008\000\016\000\009\000\050\000\010\000\014\000\000\000\
\\000\000\
\\008\000\033\000\011\000\052\000\012\000\051\000\013\000\030\000\
\\014\000\029\000\015\000\028\000\016\000\027\000\017\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\033\000\011\000\032\000\013\000\030\000\014\000\029\000\
\\016\000\067\000\017\000\026\000\000\000\
\\008\000\043\000\014\000\068\000\000\000\
\\008\000\043\000\014\000\069\000\000\000\
\\000\000\
\\000\000\
\\008\000\033\000\011\000\071\000\012\000\070\000\013\000\030\000\
\\014\000\029\000\015\000\028\000\016\000\027\000\017\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\043\000\011\000\073\000\013\000\030\000\014\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\033\000\011\000\032\000\013\000\030\000\014\000\029\000\
\\016\000\075\000\017\000\026\000\000\000\
\\008\000\043\000\014\000\076\000\000\000\
\\008\000\043\000\014\000\077\000\000\000\
\\008\000\043\000\014\000\078\000\000\000\
\\008\000\033\000\011\000\032\000\013\000\030\000\014\000\029\000\
\\015\000\079\000\016\000\027\000\017\000\026\000\000\000\
\\004\000\080\000\000\000\
\\008\000\043\000\013\000\081\000\014\000\029\000\000\000\
\\008\000\043\000\013\000\082\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\083\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\084\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\085\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\086\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\087\000\013\000\030\000\014\000\029\000\000\000\
\\008\000\043\000\011\000\088\000\013\000\030\000\014\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 97
val numrules = 47
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDENTIFIER of unit ->  (string) | NUMERAL of unit ->  (int)
 | Comparison of unit ->  (AST.AST) | BoolFactor of unit ->  (AST.AST)
 | BoolTerm of unit ->  (AST.AST) | IntFactor of unit ->  (AST.AST)
 | IntTerm of unit ->  (AST.AST)
 | BoolExpression of unit ->  (AST.AST)
 | IntExpression of unit ->  (AST.AST) | Command of unit ->  (AST.AST)
 | Commands of unit ->  (AST.AST list)
 | Variable of unit ->  (AST.AST) | Type of unit ->  (AST.AST)
 | VariableList of unit ->  (AST.AST list)
 | Declaration of unit ->  (AST.AST list*AST.AST)
 | CommandSeq of unit ->  (AST.AST list)
 | DeclarationSeq of unit ->  ( ( AST.AST list * AST.AST )  list)
 | Block of unit ->  ( ( AST.AST list * AST.AST )  list*AST.AST list)
 | Program of unit ->  (AST.AST)
end
type svalue = MlyValue.svalue
type result = AST.AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 2) => true | (T 5) => true | (T 6) => true | (T 
11) => true | (T 12) => true | (T 13) => true | (T 14) => true | (T 15
) => true | (T 16) => true | (T 17) => true | (T 18) => true | (T 19)
 => true | (T 24) => true | (T 25) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 39) => true | _ => false
val showTerminal =
fn (T 0) => "PROGRAM"
  | (T 1) => "DCOLON"
  | (T 2) => "VAR"
  | (T 3) => "COLON"
  | (T 4) => "SEMICOLON"
  | (T 5) => "INT"
  | (T 6) => "BOOL"
  | (T 7) => "COMMA"
  | (T 8) => "LCURLY"
  | (T 9) => "RCURLY"
  | (T 10) => "ASSIGN"
  | (T 11) => "READ"
  | (T 12) => "WRITE"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | (T 16) => "ENDIF"
  | (T 17) => "WHILE"
  | (T 18) => "DO"
  | (T 19) => "ENDWH"
  | (T 20) => "LPAREN"
  | (T 21) => "RPAREN"
  | (T 22) => "OR"
  | (T 23) => "AND"
  | (T 24) => "TT"
  | (T 25) => "FF"
  | (T 26) => "LT"
  | (T 27) => "LE"
  | (T 28) => "GT"
  | (T 29) => "GE"
  | (T 30) => "EQ"
  | (T 31) => "NE"
  | (T 32) => "PLUS"
  | (T 33) => "MINUS"
  | (T 34) => "MUL"
  | (T 35) => "DIV"
  | (T 36) => "MOD"
  | (T 37) => "NEG"
  | (T 38) => "NOT"
  | (T 39) => "EOF"
  | (T 40) => "BADCH"
  | (T 41) => "NUMERAL"
  | (T 42) => "IDENTIFIER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROGRAM1left,
 _)) :: rest671)) => let val  result = MlyValue.Program (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (Block as Block1) = Block1 ()
 in ((AST.Program(IDENTIFIER, Block)))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in ((DeclarationSeq, CommandSeq))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in ((Declaration::DeclarationSeq))
end)
 in ( LrTable.NT 2, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.DeclarationSeq (fn _
 => (([])))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Type Type1
, _, _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) ::
 ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Declaration (fn _ => let val  (VariableList as VariableList1)
 = VariableList1 ()
 val  (Type as Type1) = Type1 ()
 in ((VariableList, Type))
end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => ((AST.INT)))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => ((AST.BOOL)))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in ((Variable::VariableList))
end)
 in ( LrTable.NT 5, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (([Variable]))
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 9, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.Commands 
Commands1, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let
 val  result = MlyValue.CommandSeq (fn _ => let val  (Commands as 
Commands1) = Commands1 ()
 in ((Commands))
end)
 in ( LrTable.NT 3, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Commands Commands1, _, Commands1right)) ::
 _ :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: rest671))
 => let val  result = MlyValue.Commands (fn _ => let val  (Command as 
Command1) = Command1 ()
 val  (Commands as Commands1) = Commands1 ()
 in ((Command::Commands))
end)
 in ( LrTable.NT 8, ( result, Command1left, Commands1right), rest671)

end
|  ( 11, ( rest671)) => let val  result = MlyValue.Commands (fn _ => (
([])))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.BoolExpression BoolExpression1, _, 
BoolExpression1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 val  (BoolExpression as BoolExpression1) = BoolExpression1 ()
 in ((AST.AssignBool(Variable, BoolExpression)))
end)
 in ( LrTable.NT 9, ( result, Variable1left, BoolExpression1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 val  (IntExpression as IntExpression1) = IntExpression1 ()
 in ((AST.AssignInt(Variable, IntExpression)))
end)
 in ( LrTable.NT 9, ( result, Variable1left, IntExpression1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in ((AST.Read(Variable)))
end)
 in ( LrTable.NT 9, ( result, READ1left, Variable1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 in ((AST.Write(IntExpression)))
end)
 in ( LrTable.NT 9, ( result, WRITE1left, IntExpression1right), 
rest671)
end
|  ( 16, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, _, _)) ::
 ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (BoolExpression as BoolExpression1)
 = BoolExpression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in ((AST.IfThenElse(BoolExpression, CommandSeq, CommandSeq)))
end)
 in ( LrTable.NT 9, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 17, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.BoolExpression 
BoolExpression1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.Command (fn _ => let val  (BoolExpression
 as BoolExpression1) = BoolExpression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in ((AST.WhileDo(BoolExpression, CommandSeq)))
end)
 in ( LrTable.NT 9, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: _
 :: ( _, ( MlyValue.IntExpression IntExpression1, IntExpression1left,
 _)) :: rest671)) => let val  result = MlyValue.IntExpression (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in ((AST.Add(IntExpression, IntTerm)))
end)
 in ( LrTable.NT 10, ( result, IntExpression1left, IntTerm1right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: _
 :: ( _, ( MlyValue.IntExpression IntExpression1, IntExpression1left,
 _)) :: rest671)) => let val  result = MlyValue.IntExpression (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in ((AST.Sub(IntExpression, IntTerm)))
end)
 in ( LrTable.NT 10, ( result, IntExpression1left, IntTerm1right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, 
IntTerm1right)) :: rest671)) => let val  result = 
MlyValue.IntExpression (fn _ => let val  (IntTerm as IntTerm1) = 
IntTerm1 ()
 in ((IntTerm))
end)
 in ( LrTable.NT 10, ( result, IntTerm1left, IntTerm1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in ((AST.Multiply(IntTerm, IntFactor)))
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 22, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in ((AST.Divide(IntTerm, IntFactor)))
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 23, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in ((AST.Modulo(IntTerm, IntFactor)))
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.IntFactor IntFactor1, IntFactor1left, 
IntFactor1right)) :: rest671)) => let val  result = MlyValue.IntTerm
 (fn _ => let val  (IntFactor as IntFactor1) = IntFactor1 ()
 in ((IntFactor))
end)
 in ( LrTable.NT 12, ( result, IntFactor1left, IntFactor1right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.NUMERAL NUMERAL1, NUMERAL1left, 
NUMERAL1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in ((AST.Num(NUMERAL)))
end)
 in ( LrTable.NT 13, ( result, NUMERAL1left, NUMERAL1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in ((Variable))
end)
 in ( LrTable.NT 13, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( 
MlyValue.IntExpression IntExpression1, _, _)) :: ( _, ( _, LPAREN1left
, _)) :: rest671)) => let val  result = MlyValue.IntFactor (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 in ((AST.IntExpInPar(IntExpression)))
end)
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.IntFactor (fn _ => let val  (IntFactor as IntFactor1) = 
IntFactor1 ()
 in ((AST.Neg(IntFactor)))
end)
 in ( LrTable.NT 13, ( result, NEG1left, IntFactor1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( _, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.IntFactor (fn _ => let val  (IntFactor as IntFactor1) = 
IntFactor1 ()
 in ((IntFactor))
end)
 in ( LrTable.NT 13, ( result, PLUS1left, IntFactor1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.BoolTerm BoolTerm1, _, BoolTerm1right)) ::
 _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, 
BoolExpression1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 val  (BoolTerm as BoolTerm1) = BoolTerm1 ()
 in ((AST.Or(BoolExpression, BoolTerm)))
end)
 in ( LrTable.NT 11, ( result, BoolExpression1left, BoolTerm1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.BoolTerm BoolTerm1, BoolTerm1left, 
BoolTerm1right)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolTerm as BoolTerm1) = 
BoolTerm1 ()
 in ((BoolTerm))
end)
 in ( LrTable.NT 11, ( result, BoolTerm1left, BoolTerm1right), rest671
)
end
|  ( 32, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: _ :: ( _, ( MlyValue.BoolTerm BoolTerm1, BoolTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.BoolTerm (fn _ => let val  (
BoolTerm as BoolTerm1) = BoolTerm1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in ((AST.And(BoolTerm, BoolFactor)))
end)
 in ( LrTable.NT 14, ( result, BoolTerm1left, BoolFactor1right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.BoolFactor BoolFactor1, BoolFactor1left, 
BoolFactor1right)) :: rest671)) => let val  result = MlyValue.BoolTerm
 (fn _ => let val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in ((BoolFactor))
end)
 in ( LrTable.NT 14, ( result, BoolFactor1left, BoolFactor1right), 
rest671)
end
|  ( 34, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => ((AST.TT)))
 in ( LrTable.NT 15, ( result, TT1left, TT1right), rest671)
end
|  ( 35, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => ((AST.FF)))
 in ( LrTable.NT 15, ( result, FF1left, FF1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.BoolFactor
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in ((Variable))
end)
 in ( LrTable.NT 15, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 37, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (Comparison as Comparison1) = 
Comparison1 ()
 in ((Comparison))
end)
 in ( LrTable.NT 15, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 38, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( 
MlyValue.BoolExpression BoolExpression1, _, _)) :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.BoolFactor
 (fn _ => let val  (BoolExpression as BoolExpression1) = 
BoolExpression1 ()
 in ((AST.BoolExpInPar(BoolExpression)))
end)
 in ( LrTable.NT 15, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 39, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (BoolFactor as BoolFactor1) = 
BoolFactor1 ()
 in ((AST.Not(BoolFactor)))
end)
 in ( LrTable.NT 15, ( result, NOT1left, BoolFactor1right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.LessThan(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 41, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.LessThanEq(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 42, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.Equals(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 43, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.NotEqual(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 44, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.GreaterThan(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 45, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in ((AST.GreaterThanEq(IntExpression1, IntExpression2)))
end)
 in ( LrTable.NT 16, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 46, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in ((AST.Variable(IDENTIFIER)))
end)
 in ( LrTable.NT 7, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BADCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.NUMERAL (fn () => i),p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
end
end
