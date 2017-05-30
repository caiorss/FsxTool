#r "../bin/FsxTool.dll"

type Httpr = FsxTool.HttpReq.Httpr
module Httpu = FsxTool.HttpReq.HttpUtils

/// Post request with Form payload 
let postForm () = 
    Httpr.ReadResponse <| Httpr.PostForm(
        "http://www.httpbin.org/post"
        ,[ "name", "john"
          ;"surname"  , "something" 
          ;"language" , "en-UK"
          ;"currency" , "GBP"
          ;"value"    , "100.23" 
         ]
        ,basicAuth = Some ("user", "password")
        )

/// Post request with json payload 
let postJson () =
    Httpr.ReadResponse <| Httpr.PostJson(
        "http://www.httpbin.org/post"
       ,"""
{ "name":     "john"
 ,"surname":  "something"
 ,"language": "en-UK"
 ,"currency": "GBP"
 ,"value":    100.23
} """
        ,basicAuth = Some ("user", "password")
        )

/// Get request with basic authentication 
let basicAuth () = 
    Httpr.Request(
        "http://www.httpbin.org/basic-auth/login/password"
       ,basicAuth = Some ("login", "password")
        )        
    |> Httpr.ReadResponse


              


