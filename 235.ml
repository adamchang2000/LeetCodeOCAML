open Utils;;

module BinaryNode = struct
    include Utils.BinaryNode
end

let rec lowest_common_ancestor_bst (root : BinaryNode.t) value1 value2 = 
    let min_value = if value1 < value2 then value1 else value2 in 
    let max_value = if value1 > value2 then value1 else value2 in
    match root.value = min_value || root.value = max_value || (root.value > min_value && root.value < max_value) with 
    | true -> root
    | false -> (
        match root.value > min_value with
        | true -> lowest_common_ancestor_bst (Option.get root.left) value1 value2
        | false -> lowest_common_ancestor_bst (Option.get root.right) value1 value2
    )
;;

let () =
    let tree1 : BinaryNode.t = 
        let left_tree = {BinaryNode.value=2; left=Option.some {BinaryNode.value=0; left=Option.none; right=Option.none}; right=Option.some {BinaryNode.value=4; left=Option.some {BinaryNode.value=3; left=Option.none; right=Option.none}; right=Option.some {BinaryNode.value=5; left=Option.none; right=Option.none}}} in
        let right_tree = {BinaryNode.value=8; left=Option.some {BinaryNode.value=7; left=Option.none; right=Option.none}; right=Option.some {BinaryNode.value=9; left=Option.none; right=Option.none}} in
        {BinaryNode.value = 6; left=Option.some left_tree; right=Option.some right_tree}
    in
    let lca1 = lowest_common_ancestor_bst tree1 2 8 in 
    print_int(lca1.value);
    print_newline();

    let lca2 = lowest_common_ancestor_bst tree1 2 4 in 
    print_int(lca2.value);
    print_newline();