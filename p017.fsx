let smallNums = [|
    "zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine";"ten";"eleven";
    "twelve";"thirteen";"fourteen";"fifteen";"sixteen";"seventeen";"eighteen";"nineteen"
|]

let tens = [|
    "IGNORE";"IGNORE";"twenty";"thirty";"forty";"fifty";"sixty";"seventy";"eighty";"ninety"
|] 

let underHundred n = 
    match n with
    | _ when n < 20 -> smallNums.[n]
    | _ -> if n % 10 = 0 then tens.[n / 10] else tens.[n / 10] + "-" + smallNums.[n % 10]

// only handle number from -1000 to 1000
let rec numberToWords n =
    match n with
    | _ when n < 0 -> "negative " + numberToWords -n
    | _ when n < 100 -> underHundred n
    | _ when n < 1000 -> smallNums.[n / 100] + " hundred" + if n % 100 = 0 then "" else (" and " + underHundred (n % 100))
    | 1000 -> "one thousand"
    | _ -> "NOT SUPPORTED"

{1 .. 1000} |> Seq.sumBy (numberToWords >> (Seq.filter System.Char.IsLetter) >> Seq.length)
