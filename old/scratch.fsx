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



module Async =
    
    let toAsync fn x =
        async {
            let y = fn x
            return y 
        }

    let iter f action =
        async {
            let! res = action
            f res
    }
    
    let map f action =
        async{
            let! res = action
            return (f res)
        }

    let bind action f  =
        async {
            let!    a  = action
            let!    b  = f a
            return  b 
        }

    let mapJoin (f: 'a -> Async<'b>) (xs: 'a seq) =
        xs
        |> Seq.map f
        |> Async.Parallel
        |> Async.RunSynchronously
        

let compAsync fasyn fn =
    fun  x ->
        async {
            let! res = fasyn (fn x)
            return res 
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
        |> Seq.iter (fun host -> printfn "Host %s up" host)

    /// Function to test if TCP connection is opened. 
    ///
    let tcpPortStatus (timeoutMs: int) (host: string) (port: int) =
        let client = new System.Net.Sockets.TcpClient ()

        try
            let status = client.ConnectAsync(host, port).Wait(timeoutMs)
            client.Close ();
            status 
        with
            :? System.AggregateException -> client.Close();
                                            false

    let tcpPortStatusAsync (timeout: int) (host: string) (port: int) =
        async {
            let    status = tcpPortStatus timeout host port
            return status 
        }




[1000..60000]
|> List.map (fun port -> async {
                                let status = NetTools.tcpPortStatus 500 "192.168.1.3" port
                                if status
                                then printfn "Port %d opened" port
                                })
|> Async.Parallel
|> Async.Ignore
|> Async.RunSynchronously

NetTools.pingShowNetwork "192.168.1." ;;
Host 192.168.1.1 up
Host 192.168.1.3 up
Host 192.168.1.5 up
Host 192.168.1.7 up
Host 192.168.1.27 up
val it : unit = ()
> 

let portList2 = portList |> List.takeWhile (fun (_, port) -> port < 30)

portList
|> List.map (fun (name, port) -> async {
                                        let status = NetTools.tcpPortStatus 1000 "192.168.1.27" port
                                        return (name, port, status)
                                })
|> Async.Parallel
|> Async.RunSynchronously
|> Array.iter (fun (name, port, status) ->
              if status
              then printfn "Port %s %d opened" name port
              )
                                      
                                 
                                 


                                 
let lines = System.IO.File.ReadLines "tcp-ports.txt"

for port in [1..1000] do
    if NetTools.tcpPortStatus 500 "192.168.1.3" port
    then printfn "TCP Port %d opened" port
    


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
