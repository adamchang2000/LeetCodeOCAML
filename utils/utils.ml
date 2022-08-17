let char_list_of_string s = List.init (String.length s) (String.get s);;

module BinaryNode = struct
    type t = 
        {value: int; 
        left: t option;
        right: t option;
        }
end