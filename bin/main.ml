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

let rev list =
  let rec aux acc = function
    | [] -> acc
    | x :: rest -> aux (x :: acc) rest
  in aux [] list

let palindrome list = 
  let reversed_list = rev list in 
  reversed_list = list

let rec remove_at n = function
  | [] -> []
  | item :: rest -> if n = 0 then rest else item :: remove_at (n-1) rest

let rec insert_at item pos = function
  | [] -> [item]
  | h :: rest as l -> if pos = 0 then item :: l else h :: insert_at item (pos-1) rest

let range a b = 
  let rec aux a b accum =
    if a > b then aux (a-1) b (a :: accum)
    else if a < b then aux (a+1) b (a :: accum)
    else a :: accum
  in aux a b []

let rand_select list num =
  Random.init 0;
  let random_item arr = 
    let n = Random.int (Array.length arr) in
      Array.get arr n 
    in
  let rec aux acc list num count = 
    if count < num then aux ((random_item list) :: acc) list num (count+1)
    else acc
  in aux [] (Array.of_list list) num 0

let swap arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let permutation list = 
  let arr = Array.of_list list in
  let n = Array.length arr in
  let rec aux start n acc = 
    if start < n then
      let idx = Random.int (n) in
        swap acc start idx;
        aux (start + 1) n acc
    else Array.to_list acc
  in aux 0 n arr

let is_prime num = 
  let num = abs num in
  let rec is_not_divisor idx = 
    idx*idx > num || (num mod idx <> 0 && is_not_divisor (idx+1))
  in num > 1 && is_not_divisor 2

let rec gcd a b =
  if a = 0 then b else gcd (b mod a) a

let coprime a b = gcd a b = 1

let phi m =
  let rec count_coprime m idx count =
    if idx < m then
      count_coprime m (idx+1) (if coprime idx m then (count+1) else count)
    else
      count
  in count_coprime m 1 0

let factors n = 
  let rec aux n i = 
    if n = 1 then []
    else
      if n mod i = 0 then i :: aux (n/i) i else aux n (i+1)
  in aux n 2

let rec print_list_str = function
  | [] -> print_endline ""
  | h :: rest ->
    Printf.printf "%s " h;
    print_list_str rest

let rec print_list_int = function
  | [] -> ()
  | h :: rest -> 
    print_endline (string_of_int h);
    print_list_int rest

let encode list = 
  let rec aux count encoding = function
    | [] -> []
    | [head] -> (count+1, head) :: encoding
    | a :: (b :: _ as t) ->
      if a = b then aux (count+1) encoding t
      else aux 0 ((count+1, a) :: encoding) t
  in aux 0 [] list

let pack list = 
  let rec aux sublist acc = function
    | [] -> []
    | [head] -> (head :: sublist) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: sublist) acc t
      else aux [] ((a :: sublist) :: acc) t
    in aux [] [] list

let rec max_elem = function
  | [] -> min_int
  | [x] -> x
  | x :: rest ->  
    let item = max_elem rest in
    if x > item then x else item

let rec min_elem = function
  | [] -> max_int
  | [x] -> x
  | x :: rest ->
    let item = min_elem rest in
    if x < item then x else item

let calc_avg list = 
  let rec aux list curr_sum count = 
    match list with
    | [] -> 0
    | [x] -> (curr_sum + x) / (count + 1)
    | x :: xs -> aux xs (curr_sum + x) (count + 1)
  in aux list 0 0 

let rec duplicate result = function
  | [] -> rev result
  | x :: rest -> duplicate (x :: x :: result) rest

let replicate list n =
  let rec prepend n acc a = 
    if n = 0 then acc else prepend (n-1) (a :: acc) a
  in
  let rec aux list n acc =
    match list with 
    | [] -> acc
    | x :: rest -> aux rest n (prepend n acc x)
  in rev (aux list n [])

let numbers = [1; 2; 3; 4; 7; 1];;
let letters = ["c"; "b"; "a"; "d"; "e"; "e"; "e"];;

let pal = ["c"; "a"; "c"];;

let main () = 
  print_endline "Hello, Sai!";
  last_elem numbers;
  let result = sum_list numbers in
    print_endline (string_of_int result);
  let result = nth_elem 2 numbers in
    print_endline (string_of_int result);
  let result = list_length numbers 0 in
    print_endline (string_of_int result);
  print_endline "palindrome";
  let result = palindrome pal in
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
    print_list_str result;
  let result = range 2 7 in
    print_list_int result;
  print_endline "";
  let result = range 7 2 in
    print_list_int result;
  let result = rand_select letters 5 in
    print_list_str result;
  print_endline "";
  let result = permutation letters in
    print_list_str result;
  let result = is_prime 7 in print_endline (string_of_bool result);
  print_endline "";
  let result = gcd 20536 7826 in print_endline(string_of_int result);
  let result = coprime 13 27 in print_endline (string_of_bool result);
  let result = coprime 20536 7826 in print_endline (string_of_bool result);
  print_endline "";
  let result = phi 10 in print_endline (string_of_int result);
  print_endline "";
  let result = factors 315 in print_list_int result;
  print_endline "";
  let result = encode letters in
    let rec print_encoding = function
     | [] -> ()
     | [x] -> Printf.printf "%d %s\n" (fst x) (snd x)
     | x :: rest -> 
      Printf.printf "%d %s\n" (fst x) (snd x);
      print_encoding rest
     in print_encoding result;
  print_endline "";
  let result = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in 
    let rec print_sublist_str = function
    | [] -> print_endline ""
    | [x] -> 
      print_list_str x;
      print_endline ""
    | x :: rest -> 
      print_list_str x;
      print_endline "";
      print_sublist_str rest;
  in print_sublist_str result;
  print_endline "";
  let result = rev ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
    print_list_str result;
  let result = max_elem numbers in print_endline (string_of_int result);
  print_endline "min";
  let result = min_elem numbers in print_endline (string_of_int result);
  let result = duplicate [] ["a"; "b"; "c"; "c"; "d"] in
    print_list_str result;
  let result = replicate ["a"; "b"; "c"] 3 in 
    print_list_str result;
  let result = calc_avg numbers in print_endline (string_of_int result)

let () = main ()