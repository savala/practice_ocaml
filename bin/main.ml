let () = print_endline "Hello, Sai!"

(*let () = Printf.printf "%s\n" Practice_ocaml.En.e*)

let last_elem list =
  match List.rev list with
  | [] -> print_endline "Empty list"
  | x :: _ -> print_endline (string_of_int x)

let rec sum_list list = 
  match list with
  | [] -> 0
  | x :: rest -> x + sum_list rest

let my_list = [1; 2; 3; 4]
let () = last_elem my_list
let () = let result = sum_list my_list in 
  print_endline (string_of_int result)