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


