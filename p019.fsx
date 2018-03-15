{1901 .. 2000} 
    |> Seq.collect (fun year -> {1 .. 12} |> Seq.map (fun month -> System.DateTime(year, month, 1)))
    |> Seq.filter (fun date -> date.DayOfWeek = System.DayOfWeek.Sunday) 
    |> Seq.length