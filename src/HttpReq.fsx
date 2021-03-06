namespace FsxTool.HttpReq

open System 
open System.Net
open System.Web 
// open System.Collections.Specialized 


/// Module to perform http requests 
///     
module HttpUtils =

    /// Add query parameters to URL
    let addQueryParams (url: string) queryParams =
        let uribuilder = new UriBuilder(url)
        let query = HttpUtility.ParseQueryString(uribuilder.Query)
        queryParams |> List.iter (fun (k, v) -> query.Add(k, v))
        uribuilder.Query <- query.ToString()
        uribuilder.ToString()
    
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

    let addBasicAuthHeaders (login, password) (req: HttpWebRequest) =
        let isoEncode = System.Text.Encoding.GetEncoding("ISO-8859-1").GetBytes(login + ":" + password)
        let encoded = System.Convert.ToBase64String(isoEncode)
        req.Headers.Add("Authorization", "Basic " + encoded)
        req 

    let setHeaders headers (req: HttpWebRequest) =
        List.iter (fun (k: string, v: string) -> req.Headers.[k] <- v)
                  headers
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



type Httpr =

    static member Request(url: string
                         ,?query
                         ,?httpMethod   // Http Method - GET, POST, HEAD
                         ,?contentType
                         ,?headers      // Http Headers
                         ,?userAgent    // Browser User Agent
                         ,?accept
                         ,?postParams
                         ,?postPayload
                         ,?basicAuth     // Basic Authentication with login and password
                          ) =

        let query       = defaultArg query   []
        let headers     = defaultArg headers [||]
        let contentType = defaultArg contentType "text/html"
        let httpMethod  = defaultArg httpMethod  "GET"
        let userAgent   = defaultArg userAgent   "fsharp browser"
        let accept      = defaultArg accept     "*/*"
        let postParams  = defaultArg postParams []
        let postPayload = defaultArg postPayload None
        let basicAuth   = defaultArg basicAuth   None 

        let urlP = if not <| List.isEmpty query
                   then HttpUtils.addQueryParams url query
                   else url

        let req = System.Net.WebRequest.Create urlP
                  :?> System.Net.HttpWebRequest

        req.Method      <- httpMethod
        req.UserAgent   <- userAgent
        req.ContentType <- contentType
        req.Accept      <- accept

        if not <| List.isEmpty postParams
        then ignore <| HttpUtils.setPostParams postParams req


        postPayload |> Option.iter (fun p -> HttpUtils.setPostPayload p  req
                                             |> ignore
                                    )

        match basicAuth with
        | None               -> ()
        | Some (login, pass) -> ignore <| HttpUtils.addBasicAuthHeaders (login, pass) req 

        req


    /// Post request with Json payload.
    static member PostJson (url: string, json, ?basicAuth) =
        let basicAuth   = defaultArg basicAuth   None 
        Httpr.Request(url
                      ,httpMethod = "POST"
                      ,contentType = "application/json"
                      ,postPayload = Some json
                      ,basicAuth   = basicAuth
                      )

    /// Post request with a Form payload.
    static member PostForm (url: string, formParams, ?basicAuth) =
        let basicAuth   = defaultArg basicAuth   None         
        Httpr.Request(url
                      ,httpMethod = "POST"
                      ,contentType = "application/x-www-form-urlencoded"
                      ,postParams = formParams
                      ,basicAuth  = basicAuth
                      )


    static member GetResponse (req: System.Net.HttpWebRequest) =
        let resp = req.GetResponse() :?> HttpWebResponse
        resp

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
