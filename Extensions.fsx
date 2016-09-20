/// Modules Extensions

open System


module String  =

    let splitByWords (words: string []) (str: string) =
        str.Split(words, StringSplitOptions.RemoveEmptyEntries)
    
    let splitByChars (chrs: char []) (str: string) =
        str.Split(chrs,  StringSplitOptions.RemoveEmptyEntries)

    let splitByChar (ch: char) (str: string) =
        splitByChars [|ch|] str

    let splitLines (str: string) =
        splitByChars [| '\n'; '\r' |] str


    let splitSpace (str: string) =
        splitByChars [| ' '; '\t' |] str
        
    // let split ch (str: string) =   
    

let lineToPort (row: string []) =
    (row.[0], int << Array.head <| String.splitByChar '/' row.[1])


let portList = System.IO.File.ReadLines "tcp-ports.txt"    
               |> Seq.map (String.splitSpace >> lineToPort)
               |> Seq.toList
               
            
            
