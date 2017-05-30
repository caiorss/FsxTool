namespace FsxTool.HttpReq

open System 
open System.Net
open System.Web 
// open System.Collections.Specialized 


/// Module to perform http requests 
///     
module HttpUtils =

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
        then ignore <| HttpUtils.setPostParams postParams req

        postPayload |> Option.iter (fun p -> HttpUtils.setPostPayload p  req
                                             |> ignore
                                    )

        req


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
