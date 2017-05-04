/// Demonstration of FsxTool.PPTable

#r "../bin/FsxTool.dll" ;;

open System

type PPTable = FsxTool.PPTable.PPTable

module Dt = FsxTool.Dtime.Dtime 
module Tz = FsxTool.Dtime.TimeZone 

let () =
    let countries = [| "Netherlands"; "Belgium"; "France"; "Uk" |]
    let capitals = [| "Amsterdam"; "Brussels"; "Paris"; "London"|]
    let code = [| "1034"; "423434"; "83434"; "98723" |]
    let headers = [| "Country"; "Capital"; "Code" |]
    PPTable.Print([| countries ; capitals; code |], headers)

Console.WriteLine ("\n\n")
 
let () = 
    let syms    = [| "EUR" ; "USD"; "BRL"; "CAD" |]
    let rates   = [| 1.2  ; 2.123; 3.25 ; 4.6   |]
    let amounts = [| 3.0  ; 5.0 ; 2.449489743 ; 10.0 |]
    let prices  = [| 10.23; 20.30; 3.002; 4.56 |]
    PPTable.Print([| rates; amounts; prices|]
                  ,header = [| "rate"; "amount"; "price" |]
                  ,space  = 6
                  ,offset = 5
                  )

Console.WriteLine ("\n\n")

let () = 
    let now = Dt.nowUTC()
    let tzList = Array.map Tz.getTimeZone [| "America/Recife"; "lnd" ; "nyc"; "tky"; "utc" |]
    let tzNames = Array.map Tz.getName tzList
    let times = tzList
                |> Seq.map (fun tz -> Dt.utcToTz tz  now )
                |> Seq.map (fun s -> s.ToString())
                |> Array.ofSeq
    PPTable.Print([| tzNames ; times |]
                  ,header = [| "Time zone" ; "Time" |]
                  )
