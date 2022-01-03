type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid ; available : available list}

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

(* Analiza vseh moznosti za vsako polje *)
let get_elements_of_element element = (* Pobere vse vrednosti iz vrstice/stolpca/boxa *)
  let list_of_element = Array.to_list element in
  let rec filter acc l =
    match l with
    | [] -> acc
    | x :: xs -> if Option.is_some x then filter ((Option.get x) :: acc) xs else filter acc xs
  in filter [] list_of_element

let all_elements_for_element list = (* Pobere vse vrednosti za vsako vrstico/stolpec/box *)
  let rec aux acc l =
    match l with
    | [] -> acc 
    | x :: xs -> aux (Array.append acc [|get_elements_of_element x|]) xs
  in aux [||] list

let all_possibilities possibilities_row possibilities_column possibilities_box = (* Poisce vse stevke, ki jih se ni v dani vrstici, stolpcu in boxu *)
  let list = List.concat [possibilities_row; possibilities_column; possibilities_box] in
  let digits = List.init 9 (fun x -> x) in
  let rec filter acc l =
    match l with
    | [] -> acc
    | x :: xs -> if List.exists ((=) x) list then filter acc xs else filter (x :: acc) xs
  in filter [] digits

let all_available grid = (* Naredi seznam vseh moznosti po poljih *)
  let available_rows = all_elements_for_element (Model.rows grid) 
  and available_columns = all_elements_for_element (Model.columns grid)
  and available_boxes = all_elements_for_element (Model.boxes grid) in
  List.flatten (List.init 9 (fun i -> List.init 9 (fun j -> 
    if Option.is_some grid.(i).(j) then None else 
      let possible = all_possibilities available_rows.(i) available_columns.(j) available_boxes.(3 * (i / 3) + (j / 3)) in
      Some {loc=(i, j); possible=possible}
    )))
    
let initialize_state (problem : Model.problem) : state =
  let available = List.filter_map (fun x-> x) (all_available problem.initial_grid) in
  { current_grid = Model.copy_grid problem.initial_grid; problem ; available = available}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let insert_field (i, j) element grid = failwith "TODO"

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)  
  let compare_options option1 option2 = List.compare_lengths option1.possible option2.possible in
  let sorted = List.sort compare_options state.available in
  match sorted with
    | [] -> None
    | option :: rest -> 
      match option.possible with
      | [] -> None
      | opt1 :: other ->
        let (i, j) = option.loc in
        let other_options = {loc=(i, j); possible=other} :: rest in
        Some (
          {state with current_grid=Model.copy_grid (insert_field (i, j) opt1 state.current_grid); available=rest}, (* Je nujno kopirati? *)
          {state with current_grid=Model.copy_grid state.current_grid; available=other_options}
        )


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
