rm(list=ls())
require(data.table)
require(rtweet)
source("nyuTweetFuncs.r")
source("makeNet2.r")
#search terms
#keywords <- c("#nyu","#tech","#summit","#nyutechsummit","#nyutechnologysummit")
#keywords <- c("@nyu", "#nyu","#nyutechsummit","#nyutechnologysummit")
keywords <- c("@nyu","#nyc","#nyu", "@nyc")
#keywords <- c("#nyu")
q <- paste(keywords, collapse = " OR ")
#[oldest] status id beyond which search results should resume returning
max_sid <- "1062563933221777409"

cachesize <- 60
batchsize <- 60
load(file="tweetCache.Rdata") # tweetcache
tweetcache <- tweetcache[1:cachesize,]

runs <- 1

for(i in seq(batchsize, runs*batchsize, batchsize)){
  tweets <- search_tweets(q, n = cachesize, since_id = max_sid,
    retryonratelimit = T, include_rts = FALSE, lang= "en")

    # ORDER, most recent first
    tweets <- tweets[order(tweets$created_at, decreasing=T),]
      
      
  if (length(tweets) > 0) {
	  
	  newtweets <- nrow(tweets)		  
  	  print(paste(newtweets, "tweets returned"))
  	  
  	  if (newtweets < cachesize) {
	  	oldnum <- cachesize - newtweets
	  	print(paste("retrieving",oldnum,"old tweets"))
	  	tweets <- rbind(tweets, tweetcache[1:oldnum,])
	  } else {
	  	tweets <- tweets[1:cachesize,]
	  }
	  ### update tweetcache
	  tweetcache <- tweets

	  dt <- data.table(tweets)
	  max_sid <- max(dt[,status_id])
	
		### save
	  print("exporting data...")
	  fwrite(x = dt,file = paste("rawData_",i,".csv",sep = ""))
	  fwrite(x = dt[,.(created_at,screen_name,reply_to_screen_name,
	                   quoted_screen_name,mentions_screen_name)],
	         file = paste("edgeData_",i,".csv",sep = ""))
	    ### viz
	  print("making network...")
	  edgelist <- makeTweetEdgeList(tweets[1:batchsize,])
	  # edgelist_limited <- edgelist[edgelist$type=="reply" |edgelist$type=="quote",]
	  # edgelist <- edgelist[edgelist$type=="mention",] 
	  if (nrow(edgelist) > 100) {edgelist <- edgelist[1:100,]}
	  makeTwitterNetworkAnimation(edgelist, agg = 1, intv=3)
	  makeNet2(edgelist)
	
  } else {
  	# print("zero tweets returned. Press 1 to keep going or 0 to abort")
  	# value <- readline()
  	# if (value == "0") break
  }
	
  if (runs > 1) {
  	print("going to sleep....")
  #sleep for 60 sec
  Sys.sleep(120)
  }
}
