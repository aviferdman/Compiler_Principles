
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
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

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

(* *)

(* start of my code *)

open PC ;;

(* number parses *)
let p_digit = range '0' '9' ;;

(* char parsers *)
let p_white_space = char ' ';;
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
let p_f = char_ci 'T';;
let p_t = char_ci 'F' ;;
let p_backslash = char '\\';;
let p_slash = char '/';;
let p_backSlash_n  = char '\n';;
let p_backSlash_t  = char '\t';;
let p_backSlash_f  = char '\012';;
let p_backSlash_r  = char '\r';;
let p_doubleQuote  = char '\"';;
let p_newline = word_ci "newline" ;;
let p_my_newline = pack p_newline (fun (_) -> (char_of_int 10));;
let p_nul = word_ci "nul" ;;
let p_my_nul = pack p_nul (fun (_) -> (char_of_int 0));;
let p_page = word_ci "page" ;;
let p_my_page = pack p_page (fun (_) -> (char_of_int 12));;
let p_return = word_ci "return" ;;
let p_my_return = pack p_return (fun (_) -> (char_of_int 13));;
let p_space = word_ci "space" ;;
let p_my_space = pack p_space (fun (_) -> (char_of_int 32));;
let p_tab = word_ci "tab" ;;
let p_my_tab = pack p_tab (fun (_) -> (char_of_int 9));;
let p_Lparentheses1 = char '(' ;;
let p_Rparentheses1 = char ')' ;;
let p_Lparentheses2 = char '{' ;;
let p_Rparentheses2 = char '}' ;;
let p_char_pref = caten p_hashtag p_backslash ;;
let p_quote = char '\'' ;;
let p_Qquote = char '`' ;;
let p_Uquote = char ',' ;;
let p_UASquote = word_ci ",@" ;;


(* compositions *)
(*boolean parsing*)
let p_booleanF = caten p_hashtag p_f;;
let p_booleanT = caten p_hashtag p_t;;
let p_boolean = disj p_booleanF p_booleanT;; 
let tok_boolean = pack p_boolean (fun (bs) -> match bs with
| ('#' ,'f') -> Bool(false)
| ('#' ,'F') -> Bool(false)
| ('#' ,'t') -> Bool(true)
| ('#' ,'T') -> Bool(true)
| _ -> raise X_no_match);;

(*string parsing*)
let p_lower = range 'a' 'z' ;;
let p_upper = range 'A' 'Z' ;;
let p_letter = disj p_lower p_upper ;;
let p_punctuation = disj_list [p_exclamation; p_dollar; p_caret; p_star; p_minus; p_underline; p_equal; p_plus; p_bigger; p_smaller; p_slash; p_question] ;;
let p_string_meta_char = disj_list [p_backslash; p_backSlash_n; p_backSlash_t; p_backSlash_f; p_backSlash_r; p_doubleQuote];; 
let p_named_char = disj_list [p_my_newline; p_my_nul; p_my_page; p_my_return; p_my_space; p_my_tab];;
let p_string_literal_char = disj_list[ (range (char_of_int 0 ) (char_of_int 33) );
                                 (range (char_of_int 35) (char_of_int 91) );
                                 (range (char_of_int 93) (char_of_int 127)) ];;
let p_string_char = disj p_string_literal_char p_string_meta_char ;;
let p_string =  caten (char (char_of_int 34)) (caten (star p_string_char) (char (char_of_int 34)));;

(*Parsing Strings*)
let bs = caten (char '\\') (char '\\');;
let dq = caten (char '\\') (char '\"');; 
let bt = caten (char '\\') (char '\t');;
let bn = caten (char '\\') (char '\n');;
let bf = caten (char '\\') (char '\012');;
let br = caten (char '\\') (char '\r');;
let disj_characters = disj_list[bs; dq ;bt; bn; bf; br];;
let smc = pack disj_characters  (fun (a,b) -> b);;
let bs_or_dq = disj p_doubleQuote p_backslash;;
let slc = diff nt_any bs_or_dq;;
let sc = disj smc slc;;
let parse_string = caten p_doubleQuote  (caten (star sc) p_doubleQuote)   ;;
let tok_string = pack parse_string (fun (a,(b,c)) ->  String (list_to_string b));;

let p_symbol_char_no_dot = disj_list [p_digit; p_lower; pack p_upper (fun (ch) -> lowercase_ascii ch); p_dollar; p_exclamation; p_caret; p_star; 
p_minus; p_underline; p_equal; p_plus; p_bigger; 
p_smaller; p_question; p_colon; p_slash];;  
let p_symbol_char = disj p_symbol_char_no_dot p_dot;;
let tok_symbol_char_no_dot = pack p_symbol_char_no_dot (fun (sym_char) -> Symbol((list_to_string[sym_char]))) ;;
let p_symbol_chars = caten p_symbol_char (plus p_symbol_char) ;;
let tok_symbol_char = pack p_symbol_chars (fun (ch, chars) -> Symbol((list_to_string (ch::chars)))) ;;
let tok_symbol = disj tok_symbol_char tok_symbol_char_no_dot ;;
let p_visible_simple_char = range '!' '~' ;;

let p_pref_named_char = caten p_char_pref p_named_char;;
let p_pref_visible_simple_char = caten p_char_pref p_visible_simple_char;;
let tok_pref_named_char = pack p_pref_named_char (fun ((hash, back), ch) -> Char(ch));;
let tok_pref_visible_simple_char = pack p_pref_visible_simple_char (fun ((hash, back), ch) -> Char(ch));;
let tok_char = disj tok_pref_named_char tok_pref_visible_simple_char;;

let p_all_chars = (range (char_of_int 0) (char_of_int 127));;

(*parsing numbers*)
(*parsing numbers*)
let legal_after_number =  disj_list[ (disj (char 'e') (char 'E')); p_dot; p_slash; nt_whitespace;
                          char ')';char ']';char '}'; ];;
let p_bad_after_number = diff p_all_chars legal_after_number;;
(*helpful functions*)
let listToInt = (function (c, tail) ->
  match c with
  |Some '-' -> (int_of_string(list_to_string (tail)))*(-1)
  | _ -> (int_of_string(list_to_string (tail))*(1)));;

let listToFloat = (function (c, tail) ->
match c with
|Some '-' -> (float_of_string(list_to_string (tail)))*.(-1.0)
| _ -> (float_of_string(list_to_string (tail))*.(1.0)));;

(*parsing basic numbers*)
let p_natural = plus p_digit ;; 
let p_numSign = disj p_plus p_minus ;;

(*integer*)
let p_integer = caten (maybe p_numSign) p_natural ;;
let tok_integer= pack p_integer (fun (parsed) ->  Number (Fraction ((listToInt parsed), 1)));; 

let p_single_natural = not_followed_by p_natural p_bad_after_number ;;

let p_single_integer = not_followed_by p_integer p_bad_after_number ;;
let tok_single_integer = pack p_single_integer (fun (parsed) ->  Number (Fraction ((listToInt parsed), 1)));;


(*fraction*)
let p_integer_frac = pack p_integer (fun (parsed) ->   Fraction ((listToInt parsed), 1));; 
let p_natural_frac = pack p_single_natural (fun (parsed) -> Fraction ((int_of_string(list_to_string(parsed))), 1));; 

let p_frac  = 
  let nt1 = p_integer_frac in
  let nt2 = caten nt1 p_slash in
  caten nt2 p_natural_frac;;

let rec gcd x y = if y = 0 then x else gcd y (x mod y) ;;

let tok_num_frac = pack p_frac (fun (((a,b),c)) -> 
match a with
|Fraction(e, f) -> (match c with
  | Fraction(h, i) -> let the_gcd = (gcd e h) in
      Number (Fraction(e / the_gcd, h / the_gcd))
  |Float w -> raise X_no_match)
|Float z -> raise X_no_match);;


(*float*)
let p_integer_float = pack p_integer (fun (parsed) -> Float (listToFloat parsed));; 
let p_natural_float = pack p_single_natural (fun (parsed) -> Float (float_of_string(list_to_string('0'::'.'::parsed))));;

let p_float  = 
  let nt1 = p_integer_float in
  let nt2 = caten nt1 p_dot in
  caten nt2 p_natural_float;;

let tok_num_float = pack p_float (fun (((a,b),c)) -> 
match a with
|Float d -> (match c with
  |Float e -> if d >= 0.0 then Number (Float (d +. e)) else Number (Float (d -. e))
  | Fraction(f, g) -> raise X_no_match) 
|Fraction (f,g) -> raise X_no_match);;  

let tok_scn_float = pack p_float (fun (((a,b),c)) -> 
match a with
|Float d -> (match c with
  |Float e -> if d >= 0.0 then  Float (d +. e) else  Float (d -. e)
  | Fraction(f, g) -> raise X_no_match) 
|Fraction (f,g) -> raise X_no_match);; 
 

(*scientific*)


let p_e = disj (char 'e') (char 'E');;

let p_scientific = 
  let nt1 = (disj tok_scn_float p_integer_float) in
  let nt2 = caten nt1 p_e in
  caten nt2 p_integer_frac;;

let tok_num_scientific = pack p_scientific (fun (((a,b),c)) -> 
match a with
|Float d -> (match c with
  |Fraction (e,_) -> Number (Float ( (10.0 ** float e) *. d))
  | Float(f) -> raise X_no_match) 
|Fraction (f,g) -> raise X_no_match);; 

let tok_number = disj_list [tok_num_scientific ; tok_num_float; tok_num_frac; tok_single_integer ] ;;

let p_whitespaces = pack nt_whitespace (fun (spaces) -> ());;
let p_all_chars_under_nl = const (fun ch -> ch < '\n');;
let p_all_chars_above_nl = const (fun ch -> ch > '\n');;
let p_all_chars_except_nl = disj p_all_chars_under_nl p_all_chars_above_nl ;;
let p_all_chars_except_nl_ignore = pack (star p_all_chars_except_nl) (fun (_)->()) ;; 
let p_semicolon_ignore = pack p_semicolon (fun (_)->());;
let p_backSlash_n_ignore = pack p_backSlash_n (fun (_)->());;
let p_comment_semicolon = caten_list[p_semicolon_ignore; p_all_chars_except_nl_ignore; p_backSlash_n_ignore];;
let tok_comment_semicolon = pack p_comment_semicolon (fun (_) -> ());;
let p_hash_semicolon = caten p_hashtag p_semicolon;;
let p_end_of_line_do_nothing = pack p_backSlash_n (fun (_)->[char_of_int 10]);;

let p_lineComment = 
  let nt_endComment = disj p_end_of_line_do_nothing nt_end_of_input in
  let nt_char = diff nt_any nt_endComment in
  let nt_char_star = star nt_char in 
  let nt_comment_start = caten p_semicolon nt_char_star in 
  let nt_comment = caten nt_comment_start nt_endComment in
  let nt = pack nt_comment (function input-> ()) in 
  nt ;;


(* parse Nil *)
let p_nil = caten p_Lparentheses1 p_Rparentheses1;;
let tok_nil = pack p_nil (fun (_) -> Nil);;
let tok_pL1_to_nil = pack p_Lparentheses1 (fun (_) -> Nil);;
let tok_pR1_to_nil = pack p_Rparentheses1 (fun (_) -> Nil);;
let tok_dot_to_nil = pack p_dot (fun (_) -> Nil);;

let tok_quote = pack p_quote (fun (_)-> Symbol("'"));;

let get_left = fun (x,y) -> x ;;
let get_right = fun (x,y) -> y ;;

(* useful actions *)
let make_paired nt_left nt_right nt = 
  let nt = caten nt_left nt in
  let nt = pack nt(function(_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt(function(e, _) -> e) in
    nt;;

(*start tests*)

(*Print functions*)
let  unread_number n =
  match n with
    | Fraction(nom, denom) -> Printf.sprintf "%d/%d" nom denom
    | Float(f) -> Printf.sprintf "%f" f 

let unread_char c =
    let scm_char_name = 
        match c with
        | '\n' -> "newline"
        | '\r' -> "return"
        | '\x00' -> "nul"
        | '\x0c' -> "page"
        | ' ' -> "space"
        | '\t' -> "tab"
        | _ -> String.make 1 c in
    Printf.sprintf "#\\%s" scm_char_name

let rec unread s = 
  match s with
    | Bool(true) -> "#t"
    | Bool(false) -> "#f"
    | Nil -> "()"
    | Number(n) -> unread_number n
    | Char(c) -> unread_char c
    | String(s) -> Printf.sprintf "\"%s\"" s
    | Symbol(s) -> s
    | Pair(car, cdr) -> Printf.sprintf "(%s . %s)" (unread car) (unread cdr)

let print_sexpr s = 
    let s = unread s in
    Printf.printf "%s\n" s;;

let tok_sexp_atomic  =  disj_list[ tok_boolean; tok_number; tok_string; tok_char; tok_symbol] ;;
                        
let do_nothing_whitespace = pack nt_whitespace (fun (x)->());;

let rec tok_sexp char_list =
  let skip = (star p_skip_comment) in
  let empty_list = caten p_Lparentheses1 (caten (star p_skip_comment) p_Rparentheses1) in
  let tok_empty_list = pack empty_list (fun (a,(b,c)) -> Nil) in
  let all = (disj_list [tok_sexp_atomic; tok_qoute; tok_Qquote; tok_USquote; tok_Uqoute; tok_empty_list; tok_not_dot_list; tok_dot_list]) in
  let total = pack (caten skip (caten all skip)) (fun (a,(b,c))-> b) in
  total char_list 

  and tok_not_dot_list char_list =  
    let packed = caten p_Lparentheses1 (caten (star tok_sexp) p_Rparentheses1) in
    let paired a b = Pair(a,b) in
    pack packed (fun (a,(b,c)) -> List.fold_right paired b Nil) 
    char_list

  and tok_dot_list char_list =  
    let packed =  caten p_Lparentheses1 (caten (caten (plus tok_sexp) (caten p_dot tok_sexp)) p_Rparentheses1) in
    let paired a b = Pair(a,b) in
    pack packed (fun (a,((sexps,(dot,sexp)),r)) -> List.fold_right paired sexps sexp) 
    char_list
    
  and tok_qoute char_list=
    pack (caten p_quote tok_sexp)
    (function (q,sexpr) -> 
        Pair( Symbol( "quote") , Pair( sexpr , Nil ) ) 
    ) char_list
  
  and tok_Qquote char_list=
    pack (caten p_Qquote tok_sexp)
      (function (q,sexpr) -> 
          Pair( Symbol( "quasiquote") , Pair( sexpr , Nil ) ) 
      ) char_list

  and tok_USquote char_list=
    pack (caten p_UASquote tok_sexp)
      (function (q,sexpr) -> 
          Pair( Symbol( "unquote-splicing") , Pair( sexpr , Nil ) ) 
      ) char_list

  and tok_Uqoute char_list=
    pack (caten p_Uquote tok_sexp)
      (function (q,sexpr) -> 
          Pair( Symbol( "unquote") , Pair( sexpr , Nil ) ) 
      ) char_list   
    
  and p_sexp_comment_do_nothing char_list = 
    let p_hash_and_semicolon = caten p_hashtag p_semicolon in
    let p_sexp_comment = caten p_hash_and_semicolon tok_sexp in 
    pack p_sexp_comment (function ((hash,semicolon), singleSexp)->()) char_list 
  
  and p_skip_comment char_list = 
    let p_single_comment = disj_list [p_whitespaces; p_sexp_comment_do_nothing; p_lineComment] in
    p_single_comment char_list;;


let rec printElements lst = 
  match lst with 
  |[] -> print_string ""
  | head::[]-> Printf.printf "'%c" head
  |head :: tail-> 
  begin
    Printf.printf "'%c'," head;
    printElements tail
  end;;

  let printList lst = 
    begin
    print_string "[";
    printElements lst;
    print_string "]"
    end;;

   let printTouple tpl = match tpl with
   |(first,second) ->
   print_string "parsed: \n";
   printList first;
   print_string "\nrest:\n";
   printList second;;

(* *)


let read_sexprs string = let (h,t) = (star tok_sexp) (string_to_list string) in
  match (h,t) with
  | (h,[]) -> h
  | (h,t) -> let (a,b) = p_skip_comment t in
      if b = [] then h else raise X_no_match ;;
      
let read_and_print s = 
  Printf.printf "======\nRead input:{%s}\n" s;
  read_sexprs s;;

end;; (* struct Reader *)