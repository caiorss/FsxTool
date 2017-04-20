module HttpReq

open System 
open System.Net
open System.Web 
// open System.Collections.Specialized 

/// Http Requst user types
///
module HttpType =
    
    type HttpMethod =
        | GET 
        | POST
        | PUT
        | HEAD
        | DELETE
        | PATCH 

    type HttpProp =
        | Method            of HttpMethod 
        | ContentType       of string
        | UserAgent         of string
        | Headers           of (string * string) list 
        | PostParams        of (string * string) list
        | PostPayload       of string
        | PostPayloadBytes  of byte []
        | Timeout           of int
        | KeepAlive         of bool
        | Redirect          of bool 

/// Module to perform http requests 
///     
module Http =

    open HttpType 
    
    let encodePostParameters parameters =

        let data = List.fold (fun acc (key: string, value: string) ->
                              HttpUtility.UrlEncode(key)
                              + "="
                              + HttpUtility.UrlEncode(value) +
                              "&" + acc
                              )
                              ""
                              parameters

        System.Text.Encoding.UTF8.GetBytes(data)

    let setContentType contentType (req: HttpWebRequest) =
        req.ContentType <- contentType 
        req

    let setUserAgent userAgent (req: HttpWebRequest) =
        req.UserAgent <- userAgent
        req 

    let setHttpMethod hmethod (req: HttpWebRequest)  =
        match hmethod with
        | GET      -> req.Method <- "GET"
        | POST     -> req.Method <- "POST"
        | PUT      -> req.Method <- "PUT"
        | HEAD     -> req.Method <- "HEAD"
        | DELETE   -> req.Method <- "DELETE"
        | PATCH    -> req.Method <- "PATCH"
        req 

    let setPostParams postParams (req: HttpWebRequest) =
        let dataStream = req.GetRequestStream ()
        let data = encodePostParameters postParams 
        dataStream.Write(data, 0, data.Length)
        dataStream.Close ()    
        req 

    let setPostPayload (payload: string) (req: HttpWebRequest) =
        let dataStream = req.GetRequestStream ()
        let data = System.Text.Encoding.UTF8.GetBytes(payload) 
        dataStream.Write(data, 0, data.Length)
        dataStream.Close ()    
        req

    let setHeaders headers (req: HttpWebRequest) =
        List.iter (fun (k: string, v: string) -> req.Headers.[k] <- v)
                  headers
        req 

    let setPropSingle prop (req: HttpWebRequest) =
        match prop with
        | Method m      -> setHttpMethod m req
        | ContentType c -> setContentType c req
        | UserAgent a   -> setUserAgent a req 
        | PostParams p  -> setPostParams p req
        | PostPayload p -> setPostPayload p req
        | Headers h     -> setHeaders h req
        | Timeout t     -> req.Timeout <- t; req 
        | KeepAlive f   -> req.KeepAlive <- f; req
        | Redirect f    -> req.AllowAutoRedirect <- f; req
        | _             -> req 

    let setProp propList (req: HttpWebRequest) =
        List.iter (fun p -> ignore <| setPropSingle p req) propList
        req 


    let getResponse (req: HttpWebRequest) =
        let resp = req.GetResponse() :?> HttpWebResponse
        resp 

    let getResponseString (req: HttpWebRequest) =
        let resp = req.GetResponse() :?> HttpWebResponse
        let respStream = resp.GetResponseStream ()
        let reader = new System.IO.StreamReader (respStream)
        let output = reader.ReadToEnd()
        respStream.Close()
        reader.Close ()
        output 


    let request (url: string) propList =
        let req = WebRequest.Create(url) :?> HttpWebRequest
        setProp propList req 

    let requestString (url: string) propList =
        let req = request url propList
        getResponseString req 

/// Utilities for simple http request
///         
module HttpUtils = 

    let downloadString (url: string) =
        let client = new WebClient ()
        client.DownloadString(url)

    let downloadFile (url: string) filename =
        let client = new WebClient ()
        client.DownloadFile (url, filename)



/// Module that provides functions to test this library 
///         
module HttpTests =
    open Http
    open HttpType
    
    let httpPostForm () =

        requestString "http://www.httpbin.org/post"
                      [
                          Method POST;
                          ContentType "application/x-www-form-urlencoded";
                          UserAgent "Firefox" ;
                          PostParams [("key1", "value1");
                                      ("key2", "value2");
                                      ("key3", "value3")
                                      ]
                          ]

       
    let  httpPostJson () =
        requestString
        <| "http://www.httpbin.org/post"
        <| [
            Method POST;
            // Headers     [("Accept", "application/json")];
            ContentType "application/json";
            UserAgent   "Firefox Fake User Agent";
            PostPayload "{\"name\": \"John\", \"id\": 2010, \"lang\" : \"es\" }" ;
            ]

    
