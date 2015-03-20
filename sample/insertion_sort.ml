let rec get_min ary i n =
  let rec gmsub ary i n m = if i >= n then m
    else gmsub ary i n (if ary.(m) > ary.(i) then i else m)
  in gmsub ary i n 0
in
let rec insertion_sort ary n = 
  let rec is_sub a i n = 
    if i >= n then ()
    else
      let j = get_min a i n in
      let t = a.(j) in
      a.(j) <- a.(i);
      a.(i) <- t;
      is_sub ary (i + 1) n
  in is_sub ary 0 n
in
let ary = Array.create 10 0 in
insertion_sort ary 10
