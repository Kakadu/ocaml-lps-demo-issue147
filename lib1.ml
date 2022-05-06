let _ =
  List.concat
    [ [ 1; 2; 4 ]
    ; [ 5; 6; 7 ]
    ; List.map (( + ) 1) [ 0; 2 ]
    ]
;;

type goal = unit

let delay _ = ()
let ( ?& ) _ = ()
let ( === ) _ _ = ()

module Fresh = struct
  let one _ = ()
end

let rev xs =
  let rec helper acc = function
    | [] -> acc
    | h :: tl -> helper (h :: acc) tl
  in
  helper [] xs
;;

(* * *)
let foo r = fresh a (r === a) (fresh (d e) (d === e))
let boo r = fresh a (r === a)
