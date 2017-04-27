namespace FsxTool.Dtime

open System

module TimeZone =

    type T = TimeZone

    /// Get current timezone
    let getCurrentTimezone() =
        TimeZone.CurrentTimeZone

    /// Timezone abbreviations
    let private tzAbrev tzname =
        match tzname with
        | "utc" | "UTC" -> "UTC"
        | "gmt" | "GMT" -> "GMT"
        | "nyc"         -> "America/New_York"
        | "chi"         -> "America/Chicago"
        | "sp"          -> "America/Sao_Paulo"
        | "lnd"         -> "Europe/London"
        | "hkg"         -> "Asia/Hong_Kong"
        | "tky"         -> "Asia/Tokyo"
        | _             -> tzname

    /// Get a timezone by name. Throws exception if the name is invalid.
    let getTimeZone (tzname: string) =
        TimeZoneInfo.FindSystemTimeZoneById(tzAbrev tzname)

    /// Find a timezone by name, returning None if it is not found.
    let findTimeZone (tzname: string) =
        try Some <| TimeZoneInfo.FindSystemTimeZoneById(tzAbrev tzname)
        with
            :? System.TimeZoneNotFoundException
               -> None

    let getUtcOffset (tz: T) =
        tz.GetUtcOffset(DateTime.Now)

    /// Get all system timezones
    let getTimeZones () =
        TimeZoneInfo.GetSystemTimeZones()

    let getTimeZonesInfo () =
        TimeZoneInfo.GetSystemTimeZones()
        |> Seq.map (fun tz -> tz.Id, tz.StandardName, tz.BaseUtcOffset.Hours)


module Date =

    type T = DateTime

    /// Create DateTime object 
    let date y m d = new DateTime (y, m, d)

    /// Create DateTime object from date Year, Month, Day tuple
    let datet (y, m, d) = new DateTime(y, m, d)

    /// <summary>
    ///  Used to create sequence of dates that happens in same day of month
    /// </summary>
    ///
    let dateFixDay y d =
        fun m -> date y m d 

    let lengthOfMonth y m =
        DateTime.DaysInMonth(y, m)


    let toTuple (d: DateTime) =
        (d.Year, d.Month, d.Day)

    let day (d: DateTime) =
        d.Day

    let month (d: DateTime) =
        d.Month 

    let year (d: DateTime) =
        d.Year

    /// Parse date in no safe format, throws
    /// exception if date is not valid.
    let parse2 format str =
        DateTime.ParseExact(str,
                            format,
                            null
                            )

    let parse format str =
        try
            Some <| DateTime.ParseExact(str,
                                        format,
                                        null)
        with
            :? System.FormatException -> None

    /// Read date in yyyy-mm-dd format
    let read str = parse "yyyy-mm-dd" str

    /// Read date in yyyy-mm-dd format, not safe.
    let read2 str =
        Option.get <| parse "yyyy-mm-dd" str

    /// Format date to ISO 8601 format (yyyy-mm-dd).
    let toString (d: T) =
        d.ToString("yyyy-MM-dd")

    let dayOfWeek (d: DateTime) =
        d.DayOfWeek

    let addDays ndays (d: DateTime) =
        d.AddDays(ndays)


    let diff (d1: DateTime) (d2: DateTime) =
        (d1 - d2).Days

    /// Get current year 
    let curYear () =
        DateTime.Today.Year

    /// Get current month 
    let curMonth () =
        DateTime.Today.Month 

    /// Get current day 
    let curDay () =
        DateTime.Today.Day 

    let today () = DateTime.Today

    let nextDay (dt: T) = dt.AddDays(1.0)

    let prevDay (dt: T) = dt.AddDays(-1.0)

    let nextDate d =
        addDays 1.0 d

    let prevDate d =
        addDays (-1.0) d

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

    let unixZeroDate =
        DateTime(1970, 1, 1).ToUniversalTime()


    let toUnixTimestamp (dt: T) =
        let x = dt.ToUniversalTime() - unixZeroDate
        x.TotalSeconds

    let fromUnixTimestamp tstamp =
        unixZeroDate.AddSeconds(tstamp)

module DateYMD =
    open System

    type Date = {
                 Y: int
               ; M: int
               ; D: int
                 }

    /// Create a date
    let date y m d =
        {Y = y; M = m ; D = d }

    /// Convert DateTime to Date
    let dtimeToDate (dt: DateTime) =
        {Y = dt.Year; M = dt.Month; D = dt.Day }

    /// Convert Date to DateTime
    let dateToDtime (date: Date) =
        new DateTime(date.Y, date.M, date.D)

    /// Get day of a Date
    let day (date: Date) = date.D

    /// Get month of Date
    let month (date: Date) = date.M

    /// Get year of Date
    let year (date: Date) = date.Y

    /// Get day of week
    let dayOfWeek (date: Date) =
        let d = dateToDtime date
        d.DayOfWeek

    let addDays ndays (date: Date) =
        let d = dateToDtime date
        dtimeToDate <| d.AddDays ndays

    /// Subtracts two dates d1 and d2 (d1 - d2)
    let diff (d1: Date) (d2: Date) =
        let dt = dateToDtime d1 - dateToDtime d2
        dt.Days

    let today () =
        dtimeToDate DateTime.Today

    let nextDay (date: Date) =
        addDays 1.0 date

    let prevDay (date: Date) =
        addDays (-1.0) date

    let toString (date: Date) =
        let d = dateToDtime date
        d.ToString("yyyy-mm-dd")

    let isWeekend (date: Date) =
        let d = dateToDtime date
        match d.DayOfWeek with
        | DayOfWeek.Saturday -> true
        | DayOfWeek.Sunday   -> true
        | _                  -> false

    let isWeekDay (date: Date) =
        let d = dateToDtime date
        match d.DayOfWeek with
        | DayOfWeek.Saturday -> false
        | DayOfWeek.Sunday   -> false
        | _                  -> true


    module Instant =
        let now () = DateTime.Now

    


// let d = date (2012, 1, 20)

// d.AddDays(100.0)
