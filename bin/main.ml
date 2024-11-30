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

let rec list_length list curr = 
  match list with
  | [] -> curr
  | _ :: rest -> list_length rest (curr+1)

let palindrome list =
  List.rev list = list

let rev list =
  let rec aux acc = function
    | [] -> acc
    | x :: rest -> aux (x :: acc) rest
  in aux [] list

let rec remove_at n = function
  | [] -> []
  | item :: rest -> if n = 0 then rest else item :: remove_at (n-1) rest

let rec insert_at item pos items = 
  match items, pos with
  | [], _ -> [item]
  | h :: rest, 0 -> h :: item :: rest
  | h :: rest, _ -> h :: insert_at item (pos - 1) rest

let rec print_list_str = function
  | [] -> ()
  | h :: rest ->
    print_endline h;
    print_list_str rest

let numbers = [1; 2; 3; 4];;
let letters = ["a"; "b"; "a"];;

let main () = 
  print_endline "Hello, Sai!";
  last_elem numbers;
  let result = sum_list numbers in
    print_endline (string_of_int result);
  let result = nth_elem 2 numbers in
    print_endline (string_of_int result);
  let result = list_length numbers 0 in
    print_endline (string_of_int result);
  let result = palindrome letters in
    print_endline (string_of_bool result);
 let result = remove_at 2 numbers in 
    let rec b = function
    | [] -> ()
    | h :: rest ->
      print_endline (string_of_int h);
      b rest;
    in b result;
  let result = rev letters in
    print_list_str result;
  print_endline "";
  let result = insert_at "ah" 1 letters in
    print_list_str result

    

let () = main ()