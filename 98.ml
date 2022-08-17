open Utils;;

module BinaryNode = struct
    include Utils.BinaryNode
end

let validate_binary_search_tree (root : BinaryNode.t) : bool =
    let rec validate_binary_search_tree_range root lower upper =
        match root with 
        | None -> true
        | Some (root : BinaryNode.t) -> (
            let root_in_range = 
                root.value > (Option.value lower ~default:(root.value - 1)) && root.value < (Option.value upper ~default:(root.value + 1))
            in
            let left_in_range = validate_binary_search_tree_range root.left lower (Some root.value) in 
            let right_in_range = validate_binary_search_tree_range root.right (Some root.value) upper in 

            root_in_range && left_in_range && right_in_range
        )
    in
    validate_binary_search_tree_range (Option.some root) Option.none Option.none

let () =
    let tree1 = {BinaryNode.value=2; left=Option.some ({BinaryNode.value = 1; left=Option.none; right=Option.none}); right=Option.some ({BinaryNode.value = 3; left=Option.none; right=Option.none})} in
    print_int (Bool.to_int (validate_binary_search_tree tree1));
    print_newline ();

    let tree2 = {BinaryNode.value=5; left=Option.some ({BinaryNode.value = 1; left=Option.none; right=Option.none}); right=Option.some ({BinaryNode.value = 4; left=Option.some ({BinaryNode.value = 3; left=Option.none; right=Option.none}); right=Option.some ({BinaryNode.value = 6; left=Option.none; right=Option.none})})} in 
    print_int(Bool.to_int (validate_binary_search_tree tree2));
    print_newline ();


    