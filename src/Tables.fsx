// Table manipulation utilities
// Objective: Display tables and matrices in a easy way.
//

// Column of Strings
type StrCol = string list

let countries = [| "Netherlands"; "Belgium"; "France"; "Uk" |]
let capitals = [| "Amsterdam"; "Brussels"; "Paris"; "London"|]
let code = [| "1034"; "423434"; "83434"; "98723" |]

let headers = [| "Country"; "Capital"; "Code" |]

/// Get column width 
let getWidth col =
    let colLen = col |> Seq.ofArray
                     |> Seq.map (fun (s: string) -> s.Length)
                     |> Seq.max
    colLen 
module TableFormat = 

    /// Table Format options 
    type TableFormat = {
        TableSpaces:   int   /// Space offset between each cell 
      ; TableOffset:   int   /// Table offset from left screen 
      ; TableLine:     bool  /// If true prints a header line
      ; TableLineChar: string 
        }


open TableFormat

/// Standard table format             
let tableStdFmt = { TableSpaces = 3
                  ; TableOffset = 0
                  ; TableLine = false
                  ; TableLineChar = "-"
                  }

let setSpaces n (fmt: TableFormat) =
    { fmt with TableSpaces = n}

let setOffset n (fmt: TableFormat) =
    { fmt with TableOffset = n}

let setLineHeader flag (fmt: TableFormat) =
    { fmt with TableLine = flag }


/// Print table with format 
let printTableFmt (fmt: TableFormat) (headers: string []) (columns: string [] [])  =
    let nspaces = fmt.TableSpaces

    let headersWidth = Array.map String.length headers
    let colsWidth =  Array.map getWidth columns
    
    let widths = if Array.isEmpty headers
                 then colsWidth
                 else Array.map2 max colsWidth headersWidth

    let ncols = Array.length columns
    let nrows = Array.length (Array.item 0 columns)
    let offset = String.replicate fmt.TableOffset " "
    
    // Print table header
    if not <| Array.isEmpty headers
    then (
        System.Console.Write(offset)
        
        for c = 0 to ncols - 1 do
           let cell = headers.[c]
           let n = nspaces + widths.[c] - cell.Length
           let spaces = String.replicate n " " 
           System.Console.Write(cell + spaces)
        
        System.Console.WriteLine()
        )
    

    if fmt.TableLine
    then (
        System.Console.Write(offset)
        
        for c = 0 to ncols - 1 do            
            let line = String.replicate widths.[c] fmt.TableLineChar            
            let sep  = String.replicate nspaces " "
            System.Console.Write(line + sep)
        
        System.Console.WriteLine()
        )                                    
        
    // Print table rows 
    for r = 0 to nrows - 1 do
        System.Console.Write(offset)
        for c = 0 to ncols - 1 do
            let cell = columns.[c].[r]
            let spaces = String.replicate (nspaces + widths.[c] - cell.Length) " "
            System.Console.Write(cell + spaces)
        System.Console.WriteLine()
            
/// Print table (Array of columns) without headers. 
let printTable columns =
    printTableFmt tableStdFmt [||] columns 

/// Print table (Array of columns) with column headers.
let printTableHeader headers columns  =
    printTableFmt { tableStdFmt with TableLine = true }  headers columns



    

    
printTableHeader headers [| countries ; capitals; code |] 

System.Console.WriteLine ()

printTable [| countries ; capitals; code |]  
   



