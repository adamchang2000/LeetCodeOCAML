let rec first_unique_char_in_string s =
    match String.length s with 
    | 0 -> -1
    | _ -> (
        match String.contains_from s 1 (String.get s 0) with
        | false -> 0
        | true ->
            (match first_unique_char_in_string (String.sub s 1 ((String.length s) - 1)) with
            | -1 -> -1
            | x -> x + 1
            )
    )
in
print_int(first_unique_char_in_string("leetcode"));
print_newline ();
print_int(first_unique_char_in_string("loveleetcode"));
print_newline ();
print_int(first_unique_char_in_string("aabb"));
print_newline ();