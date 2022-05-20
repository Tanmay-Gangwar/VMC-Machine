structure T = Tokens                (* Abbreviation saves typing *)

(* Type of the line pointer *)
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

(* Initialize line pointer *)
val lin = ref 1;

(* Initialize character position of most recent newline *)
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn 
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["^Int.toString line
                 ^"."^Int.toString col^"] Invalid character \""
                 ^bad^"\"\n");
(* End of file management *)
val eof = fn fileName => T.EOF (!lin,!col);

(* Keyword management
structure KeyWord : 
sig
    val find : string -> (int * int -> (svalue,int) token) option
end 
=
struct
 val TableSize =  422 (* 211 *)
 val HashFactor = 5

 val hash = fn 
     s => List.foldr (fn (c,v)=>(v*HashFactor+(ord c)) mod TableSize) 0 
                     (explode s)

val HashTable = Array.array(TableSize,nil) :
       (string * (int * int -> (svalue,int) token)) list Array.array

 val add = fn 
     (s,v) => let val i = hash s
              in Array.update(HashTable,i,(s,v) 
                 :: (Array.sub(HashTable, i)))
              end

 val find = fn 
     s => let val i = hash s
              fun f ((key,v)::r) = if s=key then SOME v else f r
                | f nil = NONE
          in  f (Array.sub(HashTable, i))
          end

 val _ = (List.app add [
      ("program", T.PROGRAM),
      ("var", T.VAR),
      ("int", T.INT),
      ("bool", T.BOOL),
      ("read", T.READ),
      ("write", T.WRITE),
      ("if", T.IF),
      ("then", T.THEN),
      ("else", T.ELSE),
      ("endif", T.ENDIF),
      ("while", T.WHILE),
      ("do", T.DO),
      ("endwh", T.ENDWH),
      ("tt", T.TT),
      ("ff", T.FF)
     ])
    end

open KeyWord *)

%%
%full
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName:string);
%s WHILE COMMENT;
alpha         = [A-Za-z];
digit         = [0-9];
alhpaNumeric  = [A-Za-z0-9];
whitespace            = [\ \t];
eol           = ("\013\010"|"\010"|"\013");

%%
<INITIAL>{whitespace}* => (lin:=1; eolpos:=0; YYBEGIN WHILE; continue ());

<WHILE>{whitespace}*      => (continue ());
<WHILE>{eol}      => (lin:=(!lin)+1; eolpos:=yypos+String.size yytext;               continue ());
<WHILE>"::"                                    => (col:=yypos-(!eolpos); T.DCOLON(!lin, !col));
<WHILE>":"                                     => (col:=yypos-(!eolpos); T.COLON(!lin, !col));
<WHILE>";"                                     => (col:=yypos-(!eolpos); T.SEMICOLON(!lin, !col));
<WHILE>","                                     => (col:=yypos-(!eolpos); T.COMMA(!lin, !col));
<WHILE>"{"                                     => (col:=yypos-(!eolpos); T.LCURLY(!lin, !col));
<WHILE>"}"                                     => (col:=yypos-(!eolpos); T.RCURLY(!lin, !col));
<WHILE>":="                                    => (col:=yypos-(!eolpos); T.ASSIGN(!lin, !col));
<WHILE>"("                                     => (col:=yypos-(!eolpos); T.LPAREN(!lin,!col));
<WHILE>")"                                     => (col:=yypos-(!eolpos); T.RPAREN(!lin,!col));
<WHILE>"||"                                    => (col:=yypos-(!eolpos); T.OR(!lin,!col));
<WHILE>"&&"                                    => (col:=yypos-(!eolpos); T.AND(!lin,!col));
<WHILE>"!"                                     => (col:=yypos-(!eolpos); T.NOT(!lin,!col));
<WHILE>"*"                                     => (col:=yypos-(!eolpos); T.MUL(!lin, !col));
<WHILE>"/"                                     => (col:=yypos-(!eolpos); T.DIV(!lin, !col));
<WHILE>"%"                                     => (col:=yypos-(!eolpos); T.MOD(!lin, !col));
<WHILE>"+"                                     => (col:=yypos-(!eolpos); T.PLUS(!lin, !col));
<WHILE>"-"                                     => (col:=yypos-(!eolpos); T.MINUS(!lin, !col));
<WHILE>"<"                                     => (col:=yypos-(!eolpos); T.LT(!lin, !col));
<WHILE>">"                                     => (col:=yypos-(!eolpos); T.GT(!lin, !col));
<WHILE>"<="                                    => (col:=yypos-(!eolpos); T.LE(!lin, !col));
<WHILE>">="                                    => (col:=yypos-(!eolpos); T.GE(!lin, !col));
<WHILE>"="                                     => (col:=yypos-(!eolpos); T.EQ(!lin, !col));
<WHILE>"<>"                                    => (col:=yypos-(!eolpos); T.NE(!lin, !col));
<WHILE>"~"                                     => (col:=yypos-(!eolpos); T.NEG(!lin, !col));
<WHILE>"program"                               => (col:=yypos-(!eolpos); T.PROGRAM(!lin, !col));
<WHILE>"var"                                   => (col:=yypos-(!eolpos); T.VAR(!lin, !col));
<WHILE>"int"                                   => (col:=yypos-(!eolpos); T.INT(!lin, !col));
<WHILE>"bool"                                  => (col:=yypos-(!eolpos); T.BOOL(!lin, !col));
<WHILE>"read"                                  => (col:=yypos-(!eolpos); T.READ(!lin, !col));
<WHILE>"write"                                 => (col:=yypos-(!eolpos); T.WRITE(!lin, !col));
<WHILE>"if"                                    => (col:=yypos-(!eolpos); T.IF(!lin, !col));
<WHILE>"then"                                  => (col:=yypos-(!eolpos); T.THEN(!lin, !col));
<WHILE>"else"                                  => (col:=yypos-(!eolpos); T.ELSE(!lin, !col));
<WHILE>"endif"                                 => (col:=yypos-(!eolpos); T.ENDIF(!lin, !col));
<WHILE>"while"                                 => (col:=yypos-(!eolpos); T.WHILE(!lin, !col));
<WHILE>"do"                                    => (col:=yypos-(!eolpos); T.DO(!lin, !col));
<WHILE>"endwh"                                 => (col:=yypos-(!eolpos); T.ENDWH(!lin, !col));
<WHILE>"TT"                                    => (col:=yypos-(!eolpos); T.TT(!lin, !col));
<WHILE>"FF"                                    => (col:=yypos-(!eolpos); T.FF(!lin, !col));
<WHILE>{alpha}{alhpaNumeric}*                  => (col:=yypos-(!eolpos); T.IDENTIFIER(yytext, !lin, !col));
<WHILE>{digit}+                                => (col:=yypos-(!eolpos); T.NUMERAL(valOf(Int.fromString(yytext)), !lin, !col));
<WHILE>.                                       => (col := !col + 1; print("Bad Character: " ^ yytext ^ "\n"); T.BADCH(!lin, !col));