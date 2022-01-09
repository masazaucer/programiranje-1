(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun col_ind -> grid.(row_ind).(col_ind))

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  let vrstica = box_ind / 3 
  and stolpec = box_ind mod 3 in
  let position x = (vrstica * 3 + x / 3, stolpec * 3 + x mod 3) in
  let element ind = 
    let (x, y) = position ind in 
    grid.(x).(y)
  in
  Array.init 9 element

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  let map_row ind = Array.init 9 (fun x -> f(grid.(ind).(x)))
  in Array.init 9 map_row


let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let string_to_tuple str =
  let lst = String.split_on_char ',' (String.sub str 1 3) in
  match lst with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith "Neveljaven termometer"

let tuple_to_string string_of_element (x, y) = "(" ^ string_of_element x ^ "," ^ string_of_element y ^ ")"

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.filter (fun x -> String.length x > 0 && x.[0] <> 'T' && x.[0] <> '#')
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true) (*ne rabimo vec?*)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  let termometri =
    str
    |> String.split_on_char '\n'
    |> List.filter (fun x -> String.length x > 0 && x.[0] = 'T')
    |> List.map (fun x -> String.sub x 3 (String.length x - 3))
    |> List.map ((String.split_on_char ';'))
    |> List.map (List.map (fun x -> string_to_tuple x))
  in
  (grid, termometri)

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid ; thermometers : (int * int) list list}

let print_problem problem : unit = 
  let string_of_cell = function
    | None -> "?"
    | Some i -> string_of_int i
  in print_grid string_of_cell problem.initial_grid;
  Printf.printf "%s" (string_of_nested_list (tuple_to_string string_of_int) ";" "\n" problem.thermometers)
    

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  let (grid, thermometers) = grid_of_string cell_of_char str in
  { initial_grid = grid; thermometers = thermometers }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution (solution : solution) = print_grid string_of_int solution

let rec is_valid_thermometer last_value grid remaining_thermometer =
  match remaining_thermometer with
  | [] -> true
  | (i, j) :: xs -> 
    let element = grid.(i).(j) in
    if element > last_value then is_valid_thermometer element grid xs else false


let rec check_thermometers thermometers solution = 
  List.fold_left (&&) true (List.map (is_valid_thermometer 0 solution) thermometers)

let check_part part = 
  let checked_cells = Array.init 9 (fun x -> Array.exists ((=) (x + 1)) part) in
  Array.for_all ((=) true) checked_cells

let check_all element =
  let rec check acc element = 
    match element with
    | [] -> acc
    | x :: xs -> check ((check_part x) :: acc) xs
  in List.for_all ((=) true) (check [] element)

let is_valid_solution problem solution = 
  let rows = rows solution 
  and columns = columns solution
  and boxes = boxes solution in

  check_all rows && check_all columns && check_all boxes && check_thermometers problem.thermometers solution

