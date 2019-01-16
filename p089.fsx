// reference: https://fsharpforfunandprofit.com/posts/roman-numerals/

type RomanDigit = 
    | I | II | III | IIII
    | IV | V 
    | IX | X | XX | XXX | XXXX
    | XL | L 
    | XC | C | CC | CCC | CCCC
    | CD | D 
    | CM | M | MM | MMM | MMMM

type RomanNumeral = RomanDigit list

let romanDigitToString = function
    | I -> "I" | II -> "II" | III -> "III" | IIII -> "IIII"
    | IV -> "IV" | V -> "V" 
    | IX -> "IX" | X -> "X" | XX -> "XX" | XXX -> "XXX" | XXXX -> "XXXX"
    | XL -> "XL" | L -> "L" 
    | XC -> "XC" | C -> "C" | CC -> "CC" | CCC -> "CCC" | CCCC -> "CCCC"
    | CD -> "CD" | D -> "D" 
    | CM -> "CM" | M -> "M" | MM -> "MM" | MMM -> "MMM" | MMMM -> "MMMM"

let romanNumeralToString = List.map romanDigitToString >> String.concat ""

let romanDigitToInt = function
    | I -> 1 | II -> 2 | III -> 3 | IIII -> 4
    | IV -> 4 | V -> 5
    | IX -> 9 | X -> 10 | XX -> 20 | XXX -> 30 | XXXX -> 40
    | XL -> 40 | L -> 50
    | XC -> 90 | C -> 100 | CC -> 200 | CCC -> 300 | CCCC -> 400
    | CD -> 400 | D -> 500
    | CM -> 900 | M -> 1000 | MM -> 2000 | MMM -> 3000 | MMMM -> 4000

let rec romanNumeralToInt (num:RomanNumeral) =
    match num with
    | [] -> 0
    | digit::ns -> romanDigitToInt digit + romanNumeralToInt ns

let intToRomanNumeral x =
    List.fold (fun (result, num) digit -> 
        match num >= romanDigitToInt digit with
        | true ->  (result @ [digit], num - romanDigitToInt digit)
        | false -> (result,           num)
    ) ([], x) [MMMM;MMM;MM;M;CM;D;CD;CCC;CC;C;XC;L;XL;XXX;XX;X;IX;V;IV;III;II;I]
    |> fst

let rec parseRomanNumeral = function
    // match 4 digits
    | 'I'::'I'::'I'::'I'::ns -> IIII :: parseRomanNumeral ns
    | 'X'::'X'::'X'::'X'::ns -> XXXX :: parseRomanNumeral ns
    | 'C'::'C'::'C'::'C'::ns -> CCCC :: parseRomanNumeral ns
    | 'M'::'M'::'M'::'M'::ns -> MMMM :: parseRomanNumeral ns
    // match 3 digits
    | 'I'::'I'::'I'::ns      -> III  :: parseRomanNumeral ns
    | 'X'::'X'::'X'::ns      -> XXX  :: parseRomanNumeral ns
    | 'C'::'C'::'C'::ns      -> CCC  :: parseRomanNumeral ns
    | 'M'::'M'::'M'::ns      -> MMM  :: parseRomanNumeral ns
    // match 2 digits
    | 'I'::'I'::ns           -> II   :: parseRomanNumeral ns
    | 'X'::'X'::ns           -> XX   :: parseRomanNumeral ns
    | 'C'::'C'::ns           -> CC   :: parseRomanNumeral ns
    | 'M'::'M'::ns           -> MM   :: parseRomanNumeral ns
    // match subtractive combination
    | 'I'::'V'::ns           -> IV   :: parseRomanNumeral ns
    | 'I'::'X'::ns           -> IX   :: parseRomanNumeral ns
    | 'X'::'L'::ns           -> XL   :: parseRomanNumeral ns
    | 'X'::'C'::ns           -> XL   :: parseRomanNumeral ns
    | 'C'::'D'::ns           -> CD   :: parseRomanNumeral ns
    | 'C'::'M'::ns           -> CM   :: parseRomanNumeral ns
    // match 1 digit
    | 'I'::ns                -> I    :: parseRomanNumeral ns
    | 'V'::ns                -> V    :: parseRomanNumeral ns
    | 'X'::ns                -> X    :: parseRomanNumeral ns
    | 'L'::ns                -> L    :: parseRomanNumeral ns
    | 'C'::ns                -> C    :: parseRomanNumeral ns
    | 'D'::ns                -> D    :: parseRomanNumeral ns
    | 'M'::ns                -> M    :: parseRomanNumeral ns
    // ignore other character
    | _::ns -> parseRomanNumeral ns
    // emtpy
    | [] -> []

System.IO.File.ReadAllLines "./p089_roman.txt"
    |> Array.map (fun s -> s, s.ToCharArray() |> List.ofArray |> parseRomanNumeral |> romanNumeralToInt |> intToRomanNumeral |> romanNumeralToString)
    |> Array.sumBy (fun (original, minimal) -> String.length original - String.length minimal)