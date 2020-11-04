
#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2);;
  
module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(* start of my code *)

open PC ;;

(* number parses *)
let p_digit = range '0' '9' ;;
let p_natural = plus p_digit ;; 

(* char parsers *)
let p_dollar = char '$' ;;
let p_exclamation = char '!' ;;
let p_caret = char '^' ;;
let p_star = char '*' ;;
let p_minus = char '-' ;;
let p_underline = char '_' ;;
let p_equal = char '=' ;;
let p_plus = char '+' ;;
let p_bigger = char '>' ;;
let p_smaller = char '<' ;;
let p_question = char '?' ;;
let p_colon = char '=' ;;
let p_dot = char '.' ;; 
let p_f = char_ci 'f' ;;
let p_t = char_ci 't' ;;
let p_slash = char '\\'
let p_newline = word "newline" ;;
let p_nul = word "nul" ;;
let p_page = word "page" ;;
let p_return = word "return" ;;
let p_space = word "space" ;;
let p_tab = word "tab" ;;
let p_Lparentheses1 = char '(' ;;
let p_Rparentheses1 = char ')' ;;
let p_Lparentheses2 = char '{' ;;
let p_Rparentheses2 = char '}' ;;
let p_booleanF = caten p_hashtag p_f ;;
let p_booleanT = caten p_hashtag p_t ;;
let p_charPref = caten p_hashtag p_slash ;;
let p_quote = char '\'' ;;
let p_Qquote = char '`' ;;
let p_Uquote = char ',' ;;
let p_UASquote = word_ci ",@" ;;

(* compositions *)
let p_lower = range 'a' 'z' ;;
let p_upper = range 'A' 'Z' ;;
let p_NumSign = disj p_plus p_minus
let p_integer = caten ( maybe p_NumSign ) ;;
(* end of my code *)

let read_sexprs string = raise X_not_yet_implemented;;


end;; (* struct Reader *)
