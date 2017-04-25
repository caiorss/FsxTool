module FsxTool.Sys 


module Path =

    let join pathList =
        System.IO.Path.Combine(Array.ofList pathList)

    let combine path1 path2 =
        System.IO.Path.Combine(path1, path2)

    let getDirectory path =
        System.IO.Path.GetDirectoryName path

    let getFileName path =
        System.IO.Path.GetFileName path

    let changeExt extension path =
        System.IO.Path.ChangeExtension(path, extension)
        

module File =

    /// Delete file. 
    let delete file =
        System.IO.File.Delete file

    /// Check if file exists.    
    let exists file = 
        System.IO.File.Exists file

    let move srcFile destFile =
        System.IO.File.Move(srcFile, destFile)

    let copy srcFile destFile =
        System.IO.File.Copy(srcFile, destFile)        

    /// Move file to directory     
    let moveTo directory file =
        let destFile = System.IO.Path.Combine(directory, file)
        System.IO.File.Move(file, destFile)

    /// Copy file to directory 
    let copyTo directory file =
        let destFile = System.IO.Path.Combine(directory, file)
        System.IO.File.Copy(file, destFile)

    let readFile file =
        System.IO.File.ReadAllText(file)

    let readAllLines file =
        System.IO.File.ReadAllLines(file)

    let writeFile file contents =
        System.IO.File.WriteAllText(file, contents)

    let readAllBytes file =
        System.IO.File.ReadAllBytes file 

        

module Directory =

    /// Get all directory files with absolute path 
    let listDirectoryA path =
        let dir = new System.IO.DirectoryInfo(path)
        in Array.map (fun (x: System.IO.FileInfo) -> x.FullName)  (dir.GetFiles ())

    /// Get all directory files without absolute path     
    let listDirectory path =
        let dir = new System.IO.DirectoryInfo(path)
        in Array.map (fun (x: System.IO.FileInfo) -> x.Name)  (dir.GetFiles ())

    /// Get all directory files with given extension 
    let listDirectoryExt path ext =
        System.IO.Directory.GetFiles(path, ext)
        |> Seq.ofArray
        
    /// Change file name extension        
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


