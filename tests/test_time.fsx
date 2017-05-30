#r "bin/FsxTool.dll"

module Dt = FsxTool.Dtime.Dtime ;;   
module Tz = FsxTool.Dtime.TimeZone ;;

 
let displayTime (tzones: System.TimeZoneInfo list) (time: System.DateTime) =
    tzones |> List.iter (fun tz ->
                         let t = Dt.utcToTz tz time 
                         printfn "%s\t\t%A " tz.Id t 
                         
                         )

let now = Dt.nowUTC()

let tzList = List.map Tz.getTimeZone ["America/Recife"; "lnd" ; "nyc"; "tky"; "utc"] 

displayTime (Tz.getLocal()::tzList) now     
