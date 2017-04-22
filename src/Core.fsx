namespace FsxTool.Core
/// Useful extension methods

/// Option extension module
///
module Option =

    module Op =
        let (>>=) ma fn = Option.bind fn ma
        let (>=+) ma fn = Option.map fn ma 

    let map2 fn ma mb =
        match ma, mb with
        | Some a, Some b -> Some (fn a b)
        | _              -> None


    let value defaultVal t =
       match t with
       | Some x -> x
       | None   -> defaultVal

    let both o1 o2  =
       match o1, o2 with
       | Some a1, Some a2  -> Some (a1, a2)
       | _                 -> None

    let tryWith f =
       try Some (f ())
       with _ -> None 

      
    let forp consumer handler ma : unit =
        match ma with
        | Some a -> consumer a
        | None   -> handler ()
        
    let filter f = function
        | Some v as o when f v -> o
        | _                    -> None


module Result = 

    type Result<'ok,'err> =
        | Ok of 'ok
        | Error of 'err


    let map (f: 'a -> 'b) (res: Result<'a,'err>): Result<'b,'err> =
        match res with
        | Ok x          -> Ok (f x)
        | Error err     -> Error err



    let bind (res: Result<'a, 'err>) (f: 'a -> Result<'b, 'err>): Result<'b, 'err> =
        match res with
        | Ok x      -> f x
        | Error err -> Error err


    let iter (f: 'a -> unit) (res: Result<'a, 'err>) =
        match res with
        | Ok x    -> f x
        | Error _ -> ()
 

    let validate pred error input =
        if pred input
        then Ok input
        else Error error

    let validateTry parser error input =
        try   Ok (parser input)            
        with  _   -> Error error 

    let ofOk (res: Result<'a, 'err>) =
        match res with
        | Ok x      -> x
        | Error err -> failwith "Error: Not Ok"

    let ofError (res: Result<'a, 'err>) =
        match res with
        | Ok x      -> failwith "Not Error type constructor"
        | Error err -> err
 

    let tryWith f  =
        try Ok (f ())
        with exn -> Error exn

    let tryWithMsg f =
        try Ok (f ())
        with exn -> Error exn.Message 


    let call f x =
        match f with
        | Ok g         -> g x
        | Error _      -> ()


module String  =
    open System
    open System.Text.RegularExpressions

    let join prefix (strlist: string seq) =
        String.Join(prefix, strlist)

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

    /// Add suffix to string 
    let addPrefix (prefix: string) (str: string) =
        prefix + str

    /// Add suffix to string     
    let addSuffix (suffix: string) (str: string) =
        str + suffix

    /// Get regex matches 
    let reMatches expr (text: string) =
        Regex.Matches(text, expr)

    /// Replace regex pattern in string
    ///
    /// - expr - Regex expression
    /// - rep  - Replacement
    /// - text - Input text
    ///
    let reReplace (expr: string) (rep: string) (text: string) =
        let rgx = new Regex(expr)
        rgx.Replace(text, rep)

    
        
    // let split ch (str: string) =   
    
