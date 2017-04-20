/// Unit Testing inspired by Haskell HUnit
namespace FsxTool.HUnit

module TestTypes =
    type Assertion = unit -> unit

    type TestCounts = {
          Cases:    int
        ; Tried:    int
        ; Errors:   int
        ; Failures: int  
        }
    
    type Test =
        | TestCase  of Assertion
        | TestList  of Test list
        | TestLabel of string * Test

    let testLabel msg test = TestLabel(msg, TestCase test)

module Test =
    open TestTypes
        
    
    let assertFailure msg =
        failwith <| "Hunit: " + msg

    let assertBool msg cond =
        if not cond then (assertFailure msg ())

    let assertEq msg expected value =
        let text = sprintf "\nExpected %A, but got %A" expected value
        assertBool (msg + text) (expected = value)

    let assertAEq msg tol expected value =
        let res = abs <| (expected - value) / expected
        assertBool msg (res < tol)

    let assertFn msg fn expected input =
        let value = fn input
        assertEq msg expected value

    /// Assertion operators or syntax sugars.
    module Op =
        let (?=) expected value =
            assertEq "" expected value 
        

    let runSingleTest fn =
        try fn () ; 0
        with _ as exn -> printfn "%s" exn.Message ; 1

    let sumCounts (countList: TestCounts list) =
        let step (acc: TestCounts) (x: TestCounts) =
            { Cases    = acc.Cases + x.Cases
            ; Tried    = acc.Tried + x.Tried
            ; Errors   = acc.Errors + x.Errors
            ; Failures = acc.Failures + x.Failures
            }
        let init = {Cases = 0; Tried = 0; Errors = 0; Failures = 0 }
        List.fold step init countList
        

    let rec runCounts (test: Test) =
        match test with
        | TestCase fn 
          -> { Cases  = 1
             ; Tried = 1
             ; Errors = 0 
             ; Failures = runSingleTest fn 
             }
          
        | TestLabel (msg, TestCase fn)
          ->
             { Cases  = 1
             ; Tried = 1
             ; Errors = 0 
             ; Failures = runSingleTest fn 
             }

        | TestLabel (msg, TestList tlist)
          -> sumCounts <| List.map runCounts tlist 
       
        | TestList tlist 
          -> sumCounts <| List.map runCounts tlist 
              
        | _ -> failwith "Error: Not implemented"

           
    let runTests (test: Test) =
        printfn "%A" <| runCounts test
        

    
