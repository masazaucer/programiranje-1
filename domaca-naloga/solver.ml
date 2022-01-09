type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; 
              current_grid : int option Model.grid; 
              available : available list; 
              just_added : (int * int) option;
              thermometers : (int * int) list list;
              arrows : ((int * int) * (int * int) list) list;
              cages : (int * (int * int) list) list}


type response = Solved of Model.solution | Unsolved of state | Fail of state

(*Funkcije za prikaz vmesnih stanj*)
let string_of_option (element : available) =
  let (x, y) = element.loc in
  let possible = element.possible in
  Model.tuple_to_string string_of_int (x, y) ^ ":" ^ (Model.string_of_list string_of_int "," possible)

let string_of_available available_list =
  Model.string_of_list string_of_option "\n" available_list

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid;
  Printf.printf "%s" (string_of_available state.available)

(* INITIAL OPTIONS *)

(* Pobere vse vrednosti iz vrstice/stolpca/boxa *)
let get_digits_of_row_column_box element = 
  let list_of_element = Array.to_list element in
  let rec filter acc l =
    match l with
    | [] -> acc
    | x :: xs -> if Option.is_some x then filter ((Option.get x) :: acc) xs else filter acc xs
  in filter [] list_of_element

(* Pobere vse vrednosti za vsako vrstico/stolpec/box *)
let all_digits_for_rows_columns_boxes list = 
  let rec aux acc l =
    match l with
    | [] -> acc 
    | x :: xs -> aux (Array.append acc [|get_digits_of_row_column_box x|]) xs
  in aux [||] list

(* Poisce vse stevke, ki jih se ni v dani vrstici, stolpcu in boxu *)
let all_possibilities possibilities_row possibilities_column possibilities_box = 
  let list = List.concat [possibilities_row; possibilities_column; possibilities_box] in
  let digits = List.init 9 (fun x -> x + 1) in
  let rec filter acc l =
    match l with
    | [] -> acc
    | x :: xs -> if List.exists ((=) x) list then filter acc xs else filter (x :: acc) xs
  in filter [] digits

(* Naredi seznam vseh moznosti po poljih *)
let all_available grid = 
  let available_rows = all_digits_for_rows_columns_boxes (Model.rows grid) 
  and available_columns = all_digits_for_rows_columns_boxes (Model.columns grid)
  and available_boxes = all_digits_for_rows_columns_boxes (Model.boxes grid) in
  List.flatten (List.init 9 (fun i -> List.init 9 (fun j -> 
    if Option.is_some grid.(i).(j) then None else 
      let possible = all_possibilities available_rows.(i) available_columns.(j) available_boxes.(3 * (i / 3) + (j / 3)) in
      Some {loc=(i, j); possible=possible}
    )))
    

(*CHECK THERMOMETERS*)

(*vrne vse moznosti za mesto (i, j)*)
let get_options (i, j) available = 
  let rec find = function
    | [] -> 
      (*Printf.printf "%s" (Model.tuple_to_string string_of_int (i, j));*)
      failwith "Ni moznosti"
    | option :: rest -> 
      if option.loc = (i, j) then option.possible else find rest
  in find available

(*vrne minimum int lista*)
let rec min current_min = function 
  | [] -> current_min
  | x :: xs -> 
    if x < current_min then min x xs else min current_min xs

(*vrne maximum int lista*)
let rec max current_max = function 
  | [] -> current_max
  | x :: xs -> 
    if x > current_max then max x xs else max current_max xs

(*direction = true -> minimum | false -> maximum*)
let rec check_thermometer acc m direction available grid thermometer = 
  match thermometer with
  | [] ->  acc
  | (i, j) :: rest -> 
    if Option.is_some grid.(i).(j) then
      check_thermometer acc (Option.get grid.(i).(j)) direction available grid rest
    else
      let options = get_options (i, j) available in
      let new_options = List.filter (if direction then ((<) m) else ((>)m)) options in
      check_thermometer ({loc = (i, j); possible = new_options} :: acc) (if direction then min 9 new_options else max 1 new_options) direction available grid rest


(*zamenja moznosti za element (i, j) z novimi*)
let rec switch_el acc available option =
  match available with
  | [] -> failwith "Ne obstaja!"
  | a :: rest ->
    if a.loc = option.loc then 
      (option :: rest) @ acc
    else 
      switch_el (a :: acc) rest option


(*vse moznosti iz seznama new_available vsavi namesto starih*)
let rec merge available new_available = 
  let rec switch available new_available =
    match new_available with 
    | [] -> available
    | x :: xs -> switch (switch_el [] available x) xs
  in 
  switch available new_available

(*presek dveh int listov*)
  let cross_section (list1 : int list) (list2 : int list) : int list =
  let rec aux acc =function
  | [] -> acc
  | x :: xs -> if List.exists (fun y -> x = y) list2 then aux (x :: acc) xs else aux acc xs
  in aux [] list1

(*zdruzi dva lista moznosti: pri tistih ki so v obeh vzame presek poznosti za tisto polje*)
let combine (available' : available list) (available'' : available list) : available list =
  let rec aux acc = function 
    | [] -> acc
    | {loc = (i, j) ; possible} :: rest -> 
      if List.exists (fun x -> x.loc = (i, j)) available'' then
        let options = get_options (i, j) available'' in 
        let new_possible = cross_section possible options in
        aux ({loc = (i, j) ; possible = new_possible} :: acc) rest
      else
        aux ({loc = (i, j) ; possible} :: acc) rest
  in
  let rec get_rest acc = function
    | [] -> acc
    | option :: rest -> 
      if List.exists (fun x -> x.loc = option.loc) acc then
        get_rest acc rest
      else 
        get_rest (option :: acc) rest
  in
  get_rest (aux [] available') available''

(*izrecuna novo stanje kjer uposteva vse termometre*)
let rec check_thermometers (state : state) : state = 
  let available = state.available in
  let thermometers = state.thermometers in
  let available' = List.fold_left combine [] (List.map (check_thermometer [] 0 true available state.current_grid) thermometers) in
  let available'' = List.fold_left combine [] (List.map (check_thermometer [] 10 false available state.current_grid) (List.map List.rev thermometers)) in
  let new_available = combine available' available'' in
  let available = merge available new_available in
  {state with available = available}
  

(*CHECK ARROWS*)

(*izracuna najmanjso mozno vsoto polj v repu*)
let rec min_max_of_tail acc1 acc2 state tail = 
  match tail with
  | [] -> (acc1, acc2)
  | (i, j) :: rest -> 
    match state.current_grid.(i).(j) with
    | Some x -> min_max_of_tail (acc1 + x) (acc2 + x) state rest
    | None ->
      let options = get_options (i, j) state.available in 
      min_max_of_tail (acc1 + min 9 options) (acc2 + max 1 options) state rest


(*poisce najvecjo mozno vrednost v glavi puscice*)
let min_max_of_head state (x, y) = 
  match state.current_grid.(x).(y) with
  | Some x -> (x, x)
  | None ->
    let options = get_options (x, y) state.available in 
    (min 9 options, max 1 options)


(*pobrise vse moznosti pri glavi, ki so manjse od najmanjse mozne vsote repa*)
let check_arrow_head ((x, y), tail) (state : state) : available list = 
  match state.current_grid.(x).(y) with
  | Some _ -> []
  | None -> 
    let (min, max) = min_max_of_tail 0 0 state tail in
    let options = get_options (x, y) state.available in 
    let new_options = List.filter ((>)(max + 1)) (List.filter ((<)(min - 1)) options) in
    [{loc = (x, y); possible = new_options}]
 

(*pobrise moznosti za vsako polje repa, ki so prevelike*)
let check_arrow_tail ((x, y), tail) state =
  let (min_t, max_t) = min_max_of_tail 0 0 state tail in 
  let (min_h, max_h) = min_max_of_head state (x, y) in 
  let max_v = max_h - min_t in 
  let min_v = min_h - max_t in
  let rec get_new_available acc = function
    | [] -> acc
    | (i, j) :: rest -> 
      match state.current_grid.(i).(j) with
      | Some _ -> get_new_available acc rest
      | None -> 
        let options = get_options (i, j) state.available in
        let min = min 9 options in 
        let max = max 1 options in
        let new_options = List.filter ((<)(min_v + max - 1)) (List.filter ((>)(max_v + min + 1)) options) in 
        get_new_available ({loc = (i, j) ; possible = new_options} :: acc) rest
  in 
  get_new_available [] tail


(*nove moznosti za vsa polja puscice zdruzi s stanjem*)
let check_arrow (state : state) arrow : state=
  let new_available = (check_arrow_head arrow state) @ (check_arrow_tail arrow state) in 
  (*Printf.printf "%s" ("new available\n" ^ string_of_available new_available);*)
  let available' = merge state.available new_available in
  (*Printf.printf "%s" ("merged available\n" ^ string_of_available available');*)
  {state with available = available'}

(*izracuna novo stanje upostevajoc vse puscice*)
let narrow_arrows (state : state) : state =
  let current_state = ref state in 
  let rec check = function
  | [] -> !current_state
  | arrow :: rest -> 
    current_state := check_arrow !current_state arrow;
    check rest
  in check state.arrows

(*CHECK CAGES*)

(*vrne najmanjso in najvecjo mozno vrednost polja (i, j)*)
let min_max (x, y) state =
  match state.current_grid.(x).(y) with
  | Some x -> (x, x)
  | None -> 
    let options = get_options (x, y) state.available in 
    (min 9 options, max 1 options)

(*vrne stanje s spremenjenim seznamom available*)
let check_cage state (sum, cages) =
  let extrem = List.map (fun (x, y) -> ((x, y), min_max (x, y) state)) cages in 
  let min_sum = List.fold_left (+) 0 (List.map (fun (_, (y, _)) -> y) extrem) in 
  let max_sum = List.fold_left (+) 0 (List.map (fun (_, (_, z)) -> z) extrem) in
  let rec get_new_available acc = function
    | [] -> acc
    | ((i, j), (min_v, max_v)) :: rest -> 
      match state.current_grid.(i).(j) with
      | Some _ -> get_new_available acc rest
      | None -> 
        let options = get_options (i, j) state.available in
        let new_options = List.filter ((>)(sum - min_sum + min_v + 1)) (List.filter ((<)(sum - max_sum + max_v - 1)) options) in 
        get_new_available ({loc = (i, j) ; possible = new_options} :: acc) rest
  in 
  let new_available = get_new_available [] extrem in 
  let available = merge state.available new_available in 
  {state with available = available}
  

(*zoza moznosti upostevajoc vse kletke*)
let narrow_cages (state : state) : state =
  let current_state = ref state in
  let rec check = function
  | [] -> !current_state
  | cage :: rest -> 
    current_state := check_cage !current_state cage;
    check rest
  in check state.cages
  

(*SOLVE PROBLEM*)
let initialize_state (problem : Model.problem) : state =
  let available = List.filter_map (fun x -> x) (all_available problem.initial_grid) in
  let state = { current_grid = Model.copy_grid problem.initial_grid; 
  problem ; 
  available = available; 
  just_added = None; 
  thermometers = problem.thermometers;
  arrows = problem.arrows;
  cages = problem.cages}
  |> check_thermometers |> narrow_arrows |> narrow_cages in 
  print_state state;
  state

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

let insert_field (i, j) element (grid : 'a Model.grid) : 'a Model.grid =
  grid.(i).(j) <- Some element;
  (*Printf.printf "%s" (Model.tuple_to_string string_of_int (i, j));*)
  grid

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
          {state with current_grid=insert_field (i, j) opt1 state.current_grid; available=rest; just_added = Some (i, j)},
          {state with current_grid=Model.copy_grid state.current_grid; available=other_options; just_added = None}
        )

(* Izbrise stevke, ki niso vec na razpolago na dolocenem polju *)
let narrow_options (state : state) : state = 
  match state.just_added with
  | None -> 
    state |> check_thermometers |> narrow_arrows
  | Some (i, j) -> 
    let el = Option.get state.current_grid.(i).(j) in
    let rec correct_related acc l =
      match l with
      | [] -> acc
      | option :: rest -> 
        let (x, y) = option.loc in
        if x = i || y = j || (((x / 3) = (i / 3)) && ((y / 3) = (j / 3))) then 
          let possible  = List.filter ((<>)el) option.possible in
          correct_related ({loc=(x, y); possible=possible} :: acc) rest
        else
          correct_related (option :: acc) rest
    in
    let available' = correct_related [] state.available in
    let state = {state with available = available'; just_added = None} in 
    let thermo = List.length state.thermometers
    and arrow = List.length state.arrows 
    and cages = List.length state.cages in 
    match (thermo, arrow, cages) with
    |(0, 0, 0) -> state
    |(_, 0, 0) -> state |> check_thermometers
    |(0, _, 0) -> state |> narrow_arrows
    |(0, 0, _) -> state |> narrow_cages
    | _ -> failwith "Prevec pogojev"


    
(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  
  let new_state = narrow_options state in
  (*print_state state;
  Printf.printf "%s" "\n Novo stanje :\n";
  print_state new_state;*)
  match validate_state new_state with
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
