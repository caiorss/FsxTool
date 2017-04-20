open System 
open System.Net
open System.Web 

let req = WebRequest.Create("http://www.httpbin.org/post") :?> HttpWebRequest



let data = encodePostParameters [("key1", "value1"); ("key2", "value2")]

req.Method <- "POST"

req.UserAgent <- "Firefox"

let dataStream = req.GetRequestStream ()
dataStream.Write(data, 0, data.Length)
dataStream.Close ()


let reply = req.GetResponse() :?> HttpWebResponse


reply.StatusCode

reply.StatusDescription

reply.Headers

reply.ContentLength

reply.Headers

reply.ContentType

reply.St

let respStream = reply.GetResponseStream ()
let reader = new System.IO.StreamReader (respStream)

let output = reader.ReadToEnd()
respStream.Close()
reader.Close ()



let req = WebRequest.Create("http://www.httpbin.org/post") :?> HttpWebRequest
req.ContentType <- "application/octet-stream"
req.Method <- "POST"
req.AllowAutoRedirect <- true
req.Timeout <- 2000
req.KeepAlive <- true 
let rs = req.GetRequestStream()

let header = ""
let header = System.Text.Encoding.UTF8.GetBytes(header)
rs.
