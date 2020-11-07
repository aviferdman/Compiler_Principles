
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
let p_space = char ' ';;
let p_dollar = char '$' ;;
let p_exclamation = char '!' ;;
let p_caret = char '^' ;;
let p_star = char '*' ;;
let p_hashtag = char '#';;
let p_minus = char '-' ;;
let p_underline = char '_' ;;
let p_equal = char '=' ;;
let p_plus = char '+' ;;
let p_bigger = char '>' ;;
let p_smaller = char '<' ;;
let p_question = char '?' ;;
let p_colon = char ':' ;;
let p_dot = char '.' ;;
let p_semicolon = char ';' ;; 
let p_f = char_ci 'f' ;;
let p_t = char_ci 't' ;;
let p_backslash = word "\\";;
let p_slash = char '/';;
let p_backSlash_n  = word "\\n";;
let p_backSlash_t  = word "\\t";;
let p_backSlash_f  = word "\\f";;
let p_backSlash_r  = word "\\r";;
let p_doubleQuote  = word "\"";;
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
let p_charPref = caten p_hashtag p_backslash ;;
let p_quote = word "\'" ;;
let p_Qquote = char '`' ;;
let p_Uquote = char ',' ;;
let p_UASquote = word_ci ",@" ;;

(* compositions *)
(*boolean parsing*)
let p_booleanF = caten p_hashtag p_f ;;
let p_booleanT = caten p_hashtag p_t ;;
let p_boolean = disj p_booleanF p_booleanT;;

(*string parsing*)
let p_lower = range 'a' 'z' ;;
let p_upper = range 'A' 'Z' ;;
let p_letter = disj p_lower p_upper ;;
let p_punctuation = disj_list [p_exclamation; p_dollar; p_caret; p_star; p_minus; p_underline; p_equal; p_plus; p_bigger; p_smaller; p_slash; p_question] ;;
let p_string_meta_char = disj_list [p_backslash; p_backSlash_n; p_backSlash_t; p_backSlash_f; p_backSlash_r; p_doubleQuote];; (*look here*)
let p_named_char = disj_list [p_newline; p_nul; p_page; p_return; p_space; p_tab];;
let p_symbol_char_no_dot = disj_list [p_digit; p_lower; p_upper; p_dollar; p_exclamation; p_caret; p_star; 
p_minus; p_underline; p_equal; p_plus; p_bigger; 
p_smaller; p_question; p_colon];; (*need to add p_backslash to the list - throws exception*) (*maybe the problem was that we tried: '\' instead of: '/'*)
let p_symbol_char = disj p_symbol_char_no_dot p_dot;;
(*let p_symbol = disj p_symbol_char_no_dot (caten p_symbol_char (plus p_symbol_char));; - the last act of disjoing doesnt work well type matter*)
let p_visible_simple_char = range '!' '~' ;;
let p_string_literal_char = p_letter ;;



(*parsing numbers*)
let p_numSign = disj p_plus p_minus ;;
let p_integer = caten (maybe p_numSign) p_natural ;; (*corrected - wasn't concatnated with p_natural*)
(*let p_NotNormalizedfraction = caten_list [p_integer; p_backslash; p_natural];; (*not normalized by GCD fraction*)
let p_float = caten_list [p_integer; p_dot; p_natural];;
let p_number = disj_list [p_integer; p_NotNormalizedfraction; p_float];;
let p_e = char 'e' ;;
let p_E = char 'E' ;;
let p_exp = disj p_e p_E ;;
let p_scientific_number = caten_list [p_number; p_exp; p_number] ;;
*)

(* spcaes and comments*)
(* for the skipping we can use the nth_whitespaces from pc, and the guard / pred func from pc *)
let p_comment = caten p_hashtag p_semicolon ;;


(* useful actions *)
let make_paired nt_left nt_right nt = 
  let nt = caten nt_left nt in
  let nt = pack nt(function(_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt(function(e, _) -> e) in
    nt;;

(*start tests*)

let get_left = fun (x,y) -> x ;;
let get_right = fun (x,y) -> y ;;



print_char (get_left (p_digit (string_to_list "1")));;
print_string "\n";;
print_string (list_to_string (get_left (p_natural (string_to_list "123"))));;
print_string "\n";;
print_string (list_to_string (get_right(get_left (p_integer (string_to_list "-321")))));;
print_string "\n";;
let tmp = get_left(get_left (p_integer (string_to_list "-321")));;
match tmp with
| Some c -> print_char c
| None -> print_string "wrong";;
print_string "\n";;
print_string (list_to_string (get_left (p_backSlash_r (string_to_list "\\r"))));;
print_string "\n";;

(*end tests*)

(* end of my code *)

let read_sexprs string = raise X_not_yet_implemented;;


end;; (* struct Reader *)
