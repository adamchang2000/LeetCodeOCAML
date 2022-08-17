open Utils;;

let valid_parentheses paren_str = 
    let paren_char_list = char_list_of_string paren_str in 
    List.fold_left (
        fun paren_stack paren ->
            match paren with 
            | '(' | '[' | '{' -> paren :: paren_stack
            | ')' -> if List.length paren_stack = 0 || List.hd paren_stack <> '(' then 'x' :: paren_stack else List.tl paren_stack
            | ']' -> if List.length paren_stack = 0 || List.hd paren_stack <> '[' then 'x' :: paren_stack else List.tl paren_stack
            | '}' -> if List.length paren_stack = 0 || List.hd paren_stack <> '{' then 'x' :: paren_stack else List.tl paren_stack
            | _ -> 'x' :: paren_stack
    ) [] paren_char_list
    |> List.length
    |> Int.equal 0
in
print_int(Bool.to_int (valid_parentheses "()"));
print_newline ();
print_int(Bool.to_int (valid_parentheses "()[]{}"));
print_newline ();
print_int(Bool.to_int (valid_parentheses "(]"));
print_newline ();
print_int(Bool.to_int (valid_parentheses "((((((()"));
print_newline ();

