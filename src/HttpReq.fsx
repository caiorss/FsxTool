namespace FsxTool.HttpReq

open System 
open System.Net
open System.Web 
// open System.Collections.Specialized 

/// Http Requst user types
///
module HttpTypes =
    
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

    open HttpTypes

    /// Add query parameters to URL
    let addParams (url: string) queryParams =
        let uribuilder = new UriBuilder(url)
        let query = HttpUtility.ParseQueryString(uribuilder.Query)
        queryParams |> List.iter (fun (k, v) -> query.Add(k, v))
        uribuilder.Query <- query.ToString()
        uribuilder.ToString()
    
    let private encodePostParameters parameters =

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

    let private setPropSingle prop (req: HttpWebRequest) =
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

    let request (url: string) queryParams propList =
        let req = addParams url queryParams |> WebRequest.Create
                                            :?> HttpWebRequest
        setProp propList req 

    let requestString (url: string) queryParams propList =
        let req = request url queryParams propList
        getResponseString req


    let requestSimple (url: string) =
        let client = new WebClient()
        client.DownloadString(url)

    let downloadFile (url: string) filename =
        let client = new WebClient ()
        client.DownloadFile (url, filename)


type HttpRq =

    static member private EncodePostParams(postparams) =
        let data = List.fold (fun acc (key: string, value: string) ->
                              HttpUtility.UrlEncode(key)
                              + "="
                              + HttpUtility.UrlEncode(value) +
                              "&" + acc
                              )
                              ""
                              postparams
        System.Text.Encoding.UTF8.GetBytes(data)


    static member Request(url: string,
                          ?httpMethod,
                          ?contentType,
                          ?headers,
                          ?userAgent,
                          ?accept,
                          ?postParams,
                          ?postPayload) =

        let headers     = defaultArg headers [||]
        let contentType = defaultArg contentType "text/html"
        let httpMethod  = defaultArg httpMethod  "GET"
        let userAgent   = defaultArg userAgent   "fsharp browser"
        let accept      = defaultArg accept     "*/*"
        let postParams  = defaultArg postParams []
        let postPayload = defaultArg postPayload None

        let req = System.Net.WebRequest.Create url :?> System.Net.HttpWebRequest
        req.Method      <- httpMethod
        req.UserAgent   <- userAgent
        req.ContentType <- contentType
        req.Accept      <- accept

        if not <| List.isEmpty postParams
        then ignore <| Http.setPostParams postParams req

        postPayload |> Option.iter (fun p -> Http.setPostPayload p  req
                                             |> ignore
                                    )

        req

    /// Read response content as string
    static member ReadResponse (req: System.Net.HttpWebRequest) =
        let resp = req.GetResponse() :?> System.Net.HttpWebResponse
        let respStream = resp.GetResponseStream ()
        let reader = new System.IO.StreamReader (respStream)
        let output = reader.ReadToEnd()
        respStream.Close()
        reader.Close ()
        output

    /// Simple http request returning string as response.
    ///
    static member RequestSimple(url: string) =
        let client = new System.Net.WebClient()
        client.DownloadString(url)

    static member Download (url: string) filename =
        let client = new System.Net.WebClient ()
        client.DownloadFile (url, filename)
