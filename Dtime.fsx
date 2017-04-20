open System

let date y m d = new DateTime (y, m, d)

let datet y m d = new DateTime(y, m, d)

/// <summary>
///  Used to create sequence of dates that happens in same day of month
/// </summary>
///
let dateFixDay y d =
    fun m -> date y m d 

let toTuple (d: DateTime) =
    (d.Year, d.Month, d.Day)

let day (d: DateTime) =
    d.Day

let month (d: DateTime) =
    d.Month 

let year (d: DateTime) =
    d.Year

let dayOfWeek (d: DateTime) =
    d.DayOfWeek

let addDays (d: DateTime) ndays =
    d.AddDays(ndays)


let diff (d1: DateTime) (d2: DateTime) =
    (d1 - d2).Days

let curYear () =
    DateTime.Today.Year

let curMonth () =
    DateTime.Today.Month 

let curDay () =
    DateTime.Today.Day 

let today () = DateTime.Today

let nextDay () = DateTime.Today.AddDays(1.0)

let prevDay () = DateTime.Today.AddDays(-1.0)

let nextDate d =
    addDays d 1.0 

let prevDate d =
    addDays d (-1.0)    

let isWeekend (d: DateTime) =
    match d.DayOfWeek with
    | DayOfWeek.Saturday -> true 
    | DayOfWeek.Sunday   -> true
    | _                  -> false 

let isWeekDay (d: DateTime) =
    match d.DayOfWeek with
    | DayOfWeek.Saturday -> false
    | DayOfWeek.Sunday   -> false
    | _                  -> true 

let isMonday (d: T) =
    d.DayOfWeek = DayOfWeek.Monday

let isTuesday (d: T) =
    d.DayOfWeek = DayOfWeek.Tuesday

let isWednesday (d: T) =
    d.DayOfWeek = DayOfWeek.Wednesday

let isThursday (d: T) =
    d.DayOfWeek = DayOfWeek.Thursday

let isFriday (d: T) =
    d.DayOfWeek = DayOfWeek.Friday

let isSaturday (d: T) =
    d.DayOfWeek = DayOfWeek.Saturday

let isSunday (d: T) =
    d.DayOfWeek = DayOfWeek.Sunday

let toIso8601Date (d: DateTime) =
     d.ToString("yyyy-MM-dd")


let rec dateFind (date: DateTime) pred iterator =
    match pred date with
    | true  ->  date
    | false ->  dateFind (iterator date) pred iterator 




// let d = date (2012, 1, 20)

// d.AddDays(100.0)
