module StringSet = Set.Make(String);;

let unique_morse_representations words =
    let char_list_of_string s = List.init (String.length s) (String.get s) in

    let ascii_list_of_string s = 
        let char_list = char_list_of_string s in
        List.fold_right (
            fun (char : Char.t) ascii_list ->
                Char.code char :: ascii_list
        ) char_list []

    in
    let morse_transformations = [".-";"-...";"-.-.";"-..";".";"..-.";"--.";"....";"..";".---";"-.-";".-..";"--";"-.";"---";".--.";"--.-";".-.";"...";"-";"..-";"...-";".--";"-..-";"-.--";"--.."] in
    let words_transformed = List.map (
        fun word -> 
            let morse_transformations_idxs = 
                ascii_list_of_string word
                |> List.map (Int.add (-97))
            in
            List.fold_left (
                fun str morse_idx ->
                    str ^ (List.nth morse_transformations morse_idx)
            )  "" morse_transformations_idxs
    ) words 
    in 
    StringSet.of_list words_transformed |> StringSet.cardinal
in
print_int (unique_morse_representations ["a"]);
print_newline ();
print_int (unique_morse_representations ["gin";"zen";"gig";"msg"]);
print_newline ();