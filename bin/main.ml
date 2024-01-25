open Dlexer.Lexing

type token_word = Keyword | Num

let rules: token_word rule list =
[
  (words "Fck", Token Keyword);
  (words "Shitty", Token Num)
]

and code = "Shitty Fck ss"
let main () = 
  let _ = next code rules in
  let t = next code rules in 
  let w = match t with
  | EOF -> "EOF"
  | Token Keyword -> "keyword"
  | Token Num -> "Number" in 
  print_endline w;;

main ()