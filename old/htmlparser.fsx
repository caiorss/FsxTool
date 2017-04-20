module HtmlParser

#if INTERACTIVE
#r "html/Net40/HtmlAgilityPack.dll"
#endif

// namespace HtmlParser

module HtmlXpath =

    let loadFile (file: string) =
        
         let htmldoc = new HtmlAgilityPack.HtmlDocument ()
         htmldoc.Load (file);
         htmldoc

    let loadUrl (url: string) =
        let htmldoc = new HtmlAgilityPack.HtmlWeb ()
        htmldoc.Load (url)
    
    let htmlNodeInnerText (doc: HtmlAgilityPack.HtmlNode) = doc.InnerText

    let htmlNodeAttribute (attr: string) (doc: HtmlAgilityPack.HtmlNode)  =
        doc.Attributes.[attr].Value

    let xpathNodes xpath (doc: HtmlAgilityPack.HtmlDocument) =
        Array.ofSeq <| doc.DocumentNode.SelectNodes(xpath)
   

    let xpathValues xpath  (doc: HtmlAgilityPack.HtmlDocument) =
        xpathNodes xpath doc
        |> Array.map htmlNodeInnerText

    let xpathAttr  xpath attr (doc: HtmlAgilityPack.HtmlDocument) =
        xpathNodes xpath doc
        |> Array.map (htmlNodeAttribute attr)
           

// let doc = HtmlXpath.loadFile "twitter.html" 

// let nodes = HtmlXpath.xpathNodes  doc "//div[@class='js-tweet-text-container']/p/text()"

// let news = HtmlXpath.xpathValues  doc "//div[@class='js-tweet-text-container']/p/text()"

module ScrapTwitter =

    type Tweet = {
        tweetId:           string;
        tweetUrl:          string;
        tweetUrlExpanded:  string;
        tweetPermanlink:   string;
        tweetText:         string;              
        tweetTimestamp:    string;
        tweetTimeago:      string;
        }
    
    let getNews doc = HtmlXpath.xpathValues  "//div[@class='js-tweet-text-container']/p[string-length(text()) > 0]" doc
    
    let getUrl doc = HtmlXpath.xpathAttr "//div[@class='js-tweet-text-container']/p/a[1]/@href" "href" doc

    let getUrlExpanded doc = HtmlXpath.xpathAttr "//div[@class='js-tweet-text-container']/p/a[1]/@data-expanded-url"  "data-expanded-url" doc

    let getTweetIds doc = HtmlXpath.xpathAttr "//div/@data-tweet-id" "data-tweet-id" doc

    let permanlinkPath doc = HtmlXpath.xpathAttr "//div/@data-permalink-path" "data-permalink-path" doc 

    let getTimestamp  doc = HtmlXpath.xpathAttr "//span/@data-time" "data-time" doc 

    let getTimestampMs doc = HtmlXpath.xpathAttr "//span/@data-time-ms" "data-time-ms" doc

    let getTime doc = HtmlXpath.xpathValues "//span/@data-time-ms" doc

    let getTweetsOfdoc doc =
        
        let news = getNews doc 
        let urls = getUrl doc
        let urlExpanded = getUrlExpanded doc 
        let time = getTime doc
        let timestamp = getTimestamp doc
        let permanlink = permanlinkPath doc
        let tweetsIDs  = getTweetIds doc
        
        let n = List.min [news.Length; urls.Length; urlExpanded.Length;
                          time.Length; timestamp.Length;
                          permanlink.Length; tweetsIDs.Length]

        printfn "n = %d" n
        printfn "changed" 


        [| for i in 0..(n-1) ->
         {            
            tweetId           = tweetsIDs.[i];
            tweetUrl          = urls.[i];
            tweetUrlExpanded  = urlExpanded.[i];
            tweetPermanlink   = permanlink.[i];
            tweetText         = news.[i];
            tweetTimestamp    = timestamp.[i];
            tweetTimeago     = time.[i]
         }|]
                                

    let getTweets userName =
        let url = sprintf "https://twitter.com/%s?lang=en" userName
        let doc = HtmlXpath.loadUrl url
        getTweetsOfdoc doc


    let printTweets userName =
        let tweets = getTweets userName

        Array.iter (fun tweet ->  printfn  "ID:           %s\nUrl:          %s\nUrl Expanded: %s\nText        : %s\nTime Ago      : %s\n\n"

                                            tweet.tweetId
                                            tweet.tweetUrl
                                            tweet.tweetUrlExpanded
                                            tweet.tweetText
                                            tweet.tweetTimeago
                    )
                    tweets
                              
                                     
        

    let showTweets url =
        let doc = HtmlXpath.loadUrl url
        let news = getNews doc 
        let urls = getUrl doc 
        let time = getTime doc
        let permanlink = permanlinkPath doc
        let tweetsIDs  = getTweetIds doc
        let n    = time.Length - 1 
        
        for i = 0 to n do
            printfn "News: %s\n\nUrl: %s\nTime: %s\nPermanlink: %s\nTweeter ID: %s\n------------\n\n"
                     news.[i] urls.[i] time.[i] permanlink.[i] tweetsIDs.[i];
            

    let testShowTweets () =
        showTweets "https://twitter.com/business?lang=en"

    // let test2 () =
    //      let doc = HtmlXpath.loadUrl "https://twitter.com/business?lang=en"
    //      getNews doc


// Bloomberg 
// ScrapTwitter.showTweets "https://twitter.com/business?lang=en"

// CNBC 
// ScrapTwitter.showTweets "https://twitter.com/CNBC?lang=en"


