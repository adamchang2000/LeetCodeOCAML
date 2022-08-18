let n_queens n =
    let update_valid current_valid (queen_placement : int * int) =
        let new_valid = Array.copy current_valid in 
        let (column : (int * int) list) = List.init n (fun row ->
            row, (snd queen_placement)
        ) in 
        let diag_right = List.init n (fun row ->
            (row, (snd queen_placement + (row - fst queen_placement)))
        ) in 
        let diag_left = List.init n (fun row ->
            (row, (snd queen_placement - (row - fst queen_placement)))
        ) in 
        let set_false (row, col) =
            if row >= 0 && row < n && col >= 0 && col < n then Array.set new_valid (row * n + col) false;
        in
        List.iter set_false column;
        List.iter set_false diag_right;
        List.iter set_false diag_left;
        new_valid
    in
    let rec dfs current_row current_valid current_config : int list list = 
        match current_row = n with 
        | true -> [current_config]
        | false -> (
            let row = List.init n Fun.id in 
            List.fold_left (
                fun valid_configs col ->
                    let valid_configs_to_add = 
                        match Array.get current_valid (current_row * n + col) with 
                        | false -> []
                        | true -> (
                            let new_valid = update_valid current_valid (current_row, col) in 
                            dfs (current_row + 1) new_valid ((current_row * n + col) :: current_config)
                        )
                    in
                    List.append valid_configs valid_configs_to_add
            ) [] row
        )
    in
    let board = Array.make (n * n) true in 
    dfs 0 board []
;;

let answer_1 = n_queens 1 in
print_int (List.length answer_1);
print_newline ();

let answer_6 = n_queens 6 in 
print_int (List.length answer_6);
print_newline ();

let answer_7 = n_queens 7 in 
print_int (List.length answer_7);
print_newline ();