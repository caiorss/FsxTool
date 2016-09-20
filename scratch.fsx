open System
open System.Net

Dns.GetHostName()

let toString obj =
    obj.ToString ()
    

let withTimeout timeout action =
    async {
        let! child = Async.StartChild( action, timeout )
        return! child
  }

let runWithTimeout timeout action =
    async {
        try 
           let! result = withTimeout timeout action
           return (Some result)
        with
           :?  System.TimeoutException -> return None        
        }
    

let runWithTimeoutSync timeout action =
    try
            Some <| Async.RunSynchronously (withTimeout timeout action)
    with 
        :?  System.TimeoutException -> None
        

let sleep = async {
    do! Async.Sleep(1000)
    return 1
    }




let toAsync fn x =
    async {
        let y = fn x
        return y 
    }
  


let result = client.BeginConnect("192.168.1.27", 8080, null, null)
// let status = result.AsyncWaitHandle.WaitOne(500)
client.ConnectAsync("192.168.1.27", 22).Wait(500)



let getHostName () =
    Dns.GetHostName () 

let localAddress () =
    let host = Dns.GetHostName()
    (Dns.GetHostAddresses host).[0].ToString ()


let getHostAdresses hostname =
    try 
           Array.map (fun a -> a.ToString ()) (Dns.GetHostAddresses hostname)
    with
        :? System.Net.Sockets.SocketException -> [||]

let getHostAddress hostname =
    match getHostAdresses hostname with
    | [| |]     -> None
    | _    as z -> Some (Array.head z)





module NetTools = 

    let pingHost (address: string) =
        let p = new System.Net.NetworkInformation.Ping()
        let reply = p.Send(address)
        reply.Status = System.Net.NetworkInformation.IPStatus.Success

    let pingHostAsync address =
        async {
            let status  = pingHost address
            return (address, status)
        }


    let pingAllNetwork baseAddress  =
        List.init 255 (fun i -> baseAddress + (i + 1).ToString())
        |> Seq.map pingHostAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.filter (fun (host, status) -> status)
        |> Seq.map    (fun (host, status) -> host)


    let pingShowNetwork baseAddress  =
        pingAllNetwork baseAddress
        |> Array.iter (fun host -> printfn "Host %s up" host)

    /// Function to test if TCP connection is opened. 
    ///
    let checkTCPPortStatus (timeoutMs: int) (host: string) (port: int) =
        let client = new System.Net.Sockets.TcpClient ()

        try
            let status = client.ConnectAsync(host, port).Wait(timeoutMs)
            client.Close ();
            status 
        with
            :? System.AggregateException -> client.Close();
                                            false



let protStatus2 (hostname: string) port (timeout: int) =
    let client = new System.Net.Sockets.TcpClient()
    let conn = client.ConnectAsync(hostname, port)
    conn.Wait(timeout)


let portStatus (hostname: string) port (timeout: TimeSpan)= 
    let sock = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork,
                                             System.Net.Sockets.SocketType.Stream,
                                             System.Net.Sockets.ProtocolType.Tcp
                                             )

    try 
      let conn = sock.ConnectAsync(hostname, port)
      conn.
      let status = sock.Connected
      sock.Shutdown(System.Net.Sockets.SocketShutdown.Both)
      sock.Close()
      status
    with
       | :? System.Net.Sockets.SocketException -> false
       

let portStatusAsync hostname port  = async {
    let status = portStatus hostname port
    if status
    then printfn "Port %d opened" port 
    }



portStatus "localhost" 210

System.Net.NetworkInformation.NetworkInterface.GetIsNetworkAvailable()

let iplist = List.init 255 (fun i -> "192.168.0." + (i + 1).ToString())

iplist.[254]





            

[100..2000]
|> List.map (portStatusAsync "192.168.1.9")
|> Async.Parallel
|> Async.RunSynchronously
|> ignore



Timespan.

testHost "192.168.1.1"
