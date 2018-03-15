let coins = [200;100;50;20;10;5;2;1]

let rec count coins sum =
    match coins with
    | [] -> if sum = 0 then 1 else 0
    | head::tail -> (count tail sum) + if head > sum then 0 else (count coins (sum - head))

count coins 200