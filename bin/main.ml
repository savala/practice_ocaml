(*let () = Printf.printf "%s\n" Practice_ocaml.En.e*)

let last_elem list =
  match List.rev list with
  | [] -> print_endline "Empty list"
  | x :: _ -> print_endline (string_of_int x)

let rec sum_list list = 
  match list with
  | [] -> 0
  | x :: rest -> x + sum_list rest

let rec nth_elem n list = 
  match n, list with
  | _, [] -> failwith "err no more elements"
  | 0, x :: _ -> x
  | n, _ :: rest -> nth_elem (n-1) rest

let my_list = [1; 2; 3; 4]

let main () = 
  print_endline "Hello, Sai!";
  last_elem my_list;
  let result = sum_list my_list in
    print_endline (string_of_int result);
  let result = nth_elem 2 my_list in
    print_endline (string_of_int result)

let () = main ()