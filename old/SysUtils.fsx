module Tools



module Sys =

    let listDirectoryA path =
        let dir = new System.IO.DirectoryInfo(path)
        in Array.map (fun (x: System.IO.FileInfo) -> x.FullName)  (dir.GetFiles ())

    let listDirectory path =
        let dir = new System.IO.DirectoryInfo(path)
        in Array.map (fun (x: System.IO.FileInfo) -> x.Name)  (dir.GetFiles ())

    let listDirectoryExt path ext =
        System.IO.Directory.GetFiles(path, ext)
        

    let changeExtension ext filename =
         System.IO.Path.ChangeExtension (filename, ext)

    let exec (command: string) (args: string option) =
        let p = match args with
                | None   ->   System.Diagnostics.Process.Start (command)
                | Some x ->   System.Diagnostics.Process.Start (command, x)
            
        p.StartInfo.UseShellExecute        <- false ;
        p.StartInfo.RedirectStandardOutput <- true  ;
        p.WaitForExit () ;
        p.ExitCode 
;;        

let quoteFname fname = "'" + fname + "'" ;;        
        
let runPandoc fromExt toExt filename =
    let outname = Sys.changeExtension toExt filename
    Sys.exec "pandoc" (Some ("-s " + (quoteFname filename) + " -f " + fromExt + " -t " + toExt + " -o " + (quoteFname outname)))    
;;    

let makeInfo filename =
    Sys.exec "makeinfo" (Some filename)
    ;;

// let iterProcessArr fn xs =
//     let x = ref 0


let runPandocBatch fromExt toExt path =
    Sys.listDirectoryExt path ("*." + fromExt)
    |> Array.iter (fun filen -> let _ = runPandoc fromExt toExt filen in ()) ;;


// > Sys.listDirectoryExt "." "*.texinfo" |> Array.iter (fun x -> ignore ( makeInfo-  x)) ;;   
