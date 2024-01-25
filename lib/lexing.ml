type value = Words of string | Integer of int | Float of float

type condition_list = (char -> bool) list

type 't token = Token of 't | EOF

type 't rule = condition_list * 't token

let explode str = 
  let stack = Stack.create () in 
  let rec aux p = 
    if p = -1 then stack else 
      let () = Stack.push (String.get str p) stack in aux (p - 1)
  in aux (String.length str - 1)

let charater c = (=) c

let alphabet c = match c with 'a' .. 'b' | 'A' .. 'B' -> true | _ -> false

let number c = match c with '0' .. '9' -> true | _ -> false

let (<|>) f1 f2 = fun c -> f1 c || f2 c

let (<&>) f1 f2 = fun c -> f1 c && f2 c

let at = ref 0

let words str : condition_list = 
  let aux p = (=) (String.get str p) in 
  List.init (String.length str) aux

let clip str i = String.sub str i (String.length str - i)

let next str (rules: 'a rule list) = 
  let clipped = String.trim (clip str !at) in
      let rec aux p last_list = 
        if p == String.length str then EOF else

          let filter_fun = fun r -> match List.nth_opt (fst r) p with
          | None -> false 
          | Some cond -> cond (String.get clipped p) in

          let filtered = last_list |> List.filter filter_fun in 
          if List.length filtered = 0 then 
            (at := !at + p - 1; snd (List.nth last_list 0)) else 
            aux (p + 1) filtered in 

      aux 0 rules
