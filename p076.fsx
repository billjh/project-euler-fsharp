let countSummations total =
    let ints = [total - 1 .. -1 .. 1]
    let rec count ints sum =
        match ints with
        | [] -> if sum = 0 then 1 else 0
        | head::tail -> (count tail sum) + if head > sum then 0 else (count ints (sum - head))
    count ints total

countSummations 100