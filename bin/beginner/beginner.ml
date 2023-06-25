(* In case we wanna print the result:
        let () =  match (get_tail [1;2;3;4;5]) with 
                | None -> print_endline "None"
                | Some x -> Printf.printf "%d\n" x 
   
   This code can be modified to print the result of each exercise *)


(* 1 - Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec get_tail list = 
        match list with 
        | [] -> None
        | [h] -> Some h
        | _ :: r -> get_tail r


(* 1 - minified version using fold *)
let get_tail_fold list = List.fold_left (fun _ v -> Some v) None list


(* 2 - Find the last but one (last and penultimate) elements of a list. *)
let rec get_last_two list = 
        match list with 
        | [] | [_] -> None
        |h1 :: h2 :: [] -> Some (h1, h2)
        | _ :: rest -> get_last_two rest


(* 3 - Find the N'th element of a list. *)
let rec nth_el list n = 
        match list with
        | [] -> None
        | h :: rest -> if n = 0 then Some h else nth_el rest (n-1)

(* 3 - Find the N'th element of a list using when *)
let rec nth_el_when list n = 
        match list with
        | [] -> None
        | h :: _ when n = 0 -> Some h 
        | _ :: rest -> nth_el_when rest(n-1)



(* 4 - Find the number of elements of a list. *)
let rec length = function 
        | [] -> 0
        | _ :: rest -> 1 + (length rest)



(* 4 - Find the number of elements of a list with tail recursion (Recommended way) *)
let length_opt list = List.fold_left (fun acc _ -> acc + 1) 0 list


(* 4 - Find the number of elements of a list with tail recursion without fold *)
let rec lenght_tail_call list accum = 
        match list with 
        | [] -> accum
        | _ :: rest -> lenght_tail_call rest (accum+1)

let () =  Printf.printf "%d\n" (lenght_tail_call [1;2;3;4;5;6;7] 0)

(* 5 - Reverse a list *)
let rev_list list = List.fold_left (fun accum v -> v :: accum) [] list


(* 5 - Reverse a list using append operator (probably not a good way) *)
let rec rev_list_pattern = function 
| [] as l -> l
| h :: rest -> rev_list_pattern rest @ [h]


(* 6 - Find out whether a list is a palindrome. *)
let is_palindrome list = List.equal (fun a b -> a = b) (List.rev list) list

(* 6 - Find out whether a list is a palindrome with implemented reverse list *)
let is_palindrome_fold_rev list = 
        let rev l = List.fold_left (fun accum v -> v :: accum) [] l 
        in 
        List.equal (fun a b -> a = b) (rev list) list;;

(* 6 - Find out whether a list is a palindrome with implemented reverse and equal list (without map2) ... it's a bad idea*)
let is_palindrome_hard_way list = 
        let rev l = List.fold_left (fun accum v -> v :: accum) [] l 
        in
        let rec equal_list = function
                | ([], []) -> true
                | (h1 :: r1, h2 :: r2) when h1 = h2 -> equal_list (r1, r2)
                | (_::_, []) | ([], _::_) | (_::_, _::_) -> false

        in
        equal_list ((rev list), list);;

