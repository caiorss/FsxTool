namespace FsxTool.PPtable 

// Table manipulation utilities
// Objective: Display tables and matrices in a easy way.
//


module TableFormat = 

    /// Table Format options 
    type TableFormat = {
        TableSpace:   int   /// Space offset between each cell
      ; TableOffset:   int   /// Table offset from left screen 
      ; TableLine:     bool  /// If true prints a header line
      ; TableLineChar: string 
        }


open TableFormat

module PPTable =

    /// Standard table format             
    let tableStdFmt = { TableSpace = 3
                      ; TableOffset = 0
                      ; TableLine = false
                      ; TableLineChar = "-"
                      }

    let setSpaces n (fmt: TableFormat) =
        { fmt with TableSpace = n}

    let setOffset n (fmt: TableFormat) =
        { fmt with TableOffset = n}

    let setLineHeader flag (fmt: TableFormat) =
        { fmt with TableLine = flag }


    /// Get column width
    let getWidth format col =
        let colLen = col |> Seq.ofArray
                         |> Seq.map (format >> String.length)
                         |> Seq.max
        colLen


    let arrayToStr (xs: 'a []) =
        Array.map (fun s -> s.ToString()) xs


    /// Print table with format 
    let printTableFmt (fmt: TableFormat) (format: 'a -> string) (headers: string []) (columns: 'a [] [])  =
        let nspaces = fmt.TableSpace

        let headersWidth = Array.map String.length headers
        let colsWidth =  Array.map (getWidth format) columns

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
                let cell = columns.[c].[r].ToString()
                let spaces = String.replicate (nspaces + widths.[c] - cell.Length) " "
                System.Console.Write(cell + spaces)
            System.Console.WriteLine()


type TableDisp =
    
    static member Print(columns, ?header, ?space, ?offset, ?lineChar) =
        let header = defaultArg header [||]
        let offset = defaultArg offset 0
        let space  = defaultArg space  3
        let lineChar = defaultArg lineChar "-"
        
        let fmt = { TableSpace = space
                  ; TableOffset = offset
                  ; TableLine   = if Array.isEmpty header then false else true
                  ; TableLineChar = lineChar
                  }
        PPTable.printTableFmt fmt id header columns

    static member Print(columns: float [] [], ?header, ?space, ?offset, ?lineChar) =
        let header = defaultArg header [||]
        let offset = defaultArg offset 0
        let space  = defaultArg space  3
        let lineChar = defaultArg lineChar "-"        
        let fmt = { TableSpace = space
                  ; TableOffset = offset
                  ; TableLine   = if Array.isEmpty header then false else true
                  ; TableLineChar = lineChar
                  }
        PPTable.printTableFmt fmt (fun s -> s.ToString()) header columns
       
    // static member Print(columns: float [] []) =        

    static member Print(columns, headers, format) =     
        PPTable.printTableFmt PPTable.tableStdFmt format headers columns
                
