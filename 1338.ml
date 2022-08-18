module IntMap = Map.Make(Int);;

let reduce_array_size_to_half nums = 
    let freqs = List.fold_left (fun freqs num ->
        IntMap.update num (fun freq ->
            match freq with
            | None -> Some 1
            | Some freq -> Some (freq + 1) 
        ) freqs
    ) IntMap.empty nums
    in
    let freqs = IntMap.bindings freqs in 
    let freqs = List.map snd freqs in 
    let freqs_sorted = List.rev (List.sort Int.compare freqs) in 
    snd (List.fold_left (fun (removed_total, removed_count) freq ->
        match removed_total < (List.length nums) / 2 with
        | false -> removed_total, removed_count
        | true -> removed_total + freq, removed_count + 1
    ) (0, 0) freqs_sorted)
;;

let arr1 = [3;3;3;3;5;5;5;2;2;7] in 
print_int (reduce_array_size_to_half arr1);
print_newline();

let arr2 = [7;7;7;7;7;7] in 
print_int (reduce_array_size_to_half arr2);
print_newline();

