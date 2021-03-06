#r "bin/httpReq.dll";;

open System
open HttpReq.HttpType

let data = HttpReq.Http.requestString "http://www3.bcb.gov.br/selic/consulta/taxaSelic.do"
                           [
                            Method POST;
                            ContentType "application/x-www-form-urlencoded";
                            UserAgent   "Firefox";
                            PostParams [("dataInicial",       "22/10/2012");
                                        ("dataFinal",         "30/12/2012");
                                        ("method",            "listarTaxaDiaria");
                                        ("tipoApresentacao",  "arquivo");
                                        ("Submit",            "Consultar");
                                        ];
                            ]

                           

                           
- data.Trim ([| ' '; '\t'; '\n' |]) ;;
val it : string =
  "Lista de indices Diarios da TaxaSelic;20121022;20121231;
Data;Taxa (%a.a.);Fator di�rio;Base de c�lculo (R$);M�dia;Mediana;Moda;Desvio padr�o;�ndice de curtose;
22/10/2012;7,14;1,00027371;291.364.135.035,40;7,14;7,13;7,14;0,02;509,11;
23/10/2012;7,14;1,00027371;291.458.148.728,12;7,14;7,14;7,14;0,02;739,13;
24/10/2012;7,14;1,00027371;287.464.941.524,08;7,14;7,14;7,14;0,02;739,01;
25/10/2012;7,14;1,00027371;285.878.096.119,82;7,14;7,14;7,14;0,02;755,24;
26/10/2012;7,14;1,00027371;271.416.346.950,58;7,14;7,14;7,14;0,02;583,69;
29/10/2012;7,14;1,00027371;276.666.390.729,38;7,14;7,14;7,14;0,02;643,18;
30/10/2012;7,14;1,00027371;276.436.388.083,48;7,13;7,14;7,14;0,04;318,79;
31/10/2012;7,14;1,00027371;270.903.953.122,38;7,14;7,14;7,14;0,02;392,63;
01/11/2012;7,14;1,00027371;274.888.329.586,55;7,14;7,14;7,14;0,02;667,96;
05/11/2012;7,14;1,00027371;271.110.969.895,91;7,14;7,14;7,14;0,02;708,81;
06/11/2012;7,14;1,00027371;271.429.838.596,18;7,14;7,14;7,14;0,02;653,42;
  ...
"  

//------------------------------------- 
// Send Json data
//   
let data = HttpReq.Http.requestString
           <| "http://www.httpbin.org/post"
           <| [
               Method POST;
               ContentType "application/json";
               UserAgent   "Firefox Fake User Agent";
               PostPayload "{\"name\": \"John\", \"id\": 2010, \"lang\" : \"es\" }" ;
               ]
;;

> data ;;
val it : string =
  "{
  "args": {}, 
  "data": "{\"name\": \"John\", \"id\": 2010, \"lang\" : \"es\" }", 
  "files": {}, 
  "form": {}, 
  "headers": {
    "Content-Length": "44", 
    "Content-Type": "application/json", 
    "Host": "www.httpbin.org", 
    "User-Agent": "Firefox Fake User Agent"
  }, 
  "json": {
    "id": 2010, 
    "lang": "es", 
    "name": "John"
  }, 
  "origin": "179.180.152.240", 
  "url": "http://www.httpbin.org/post"
}
"
