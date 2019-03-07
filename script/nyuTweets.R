require(rtweet)
require(dplyr)
require(igraph)
require(intergraph)
require(network)
require(ndtv)
require(networkDynamic)

# #search for 5K tweets with #nyu
##  nyu <- search_tweets(q= '#nyu', n = 100, parse = TRUE, include_rts = TRUE)
# load("nyutweet.Rdata")

# # nyu_users <- users_data(nyu) %>% unique()

# ### does this work???
# # stream nyu tweets for a minute 
# stream_tweets(
  # "nyu",
  # timeout = 60,
  # file_name = "nyuTweets.json",
  # parse = FALSE
# )

# # read in the data as a tidy tbl data frame
# nyuTweets <- parse_stream("nyuTweets.json")

# ### build network ###

# # select variables
# columns <- c("text", "favorite_count", "reply_to_screen_name", "created_at", "user_id", "screen_name", "retweet_count", "is_retweet")
# tweets <- nyu[columns]


# These are the possible link types
# "screen_name"          
# "reply_to_screen_name" 
# "mentions_screen_name" (can be multiple)
# "quoted_screen_name"  
# "retweet_screen_name"  

# for each tweet
#		check for reply, mentions, quotes, retweets,
#		make edge for each one with creation time
# # # # edgelist <- data.frame(from=character(),
						# # to=character(),
						# # type=character(),
						# # time=as.POSIXct(character()))
						
makeNYUTweetEdgeList <- function(nyuTweets) {
	edgelist <- c()
	for (i in 1:nrow(nyuTweets)) {
		from = nyuTweets[[i, c("screen_name")]]
		ttime = nyuTweets[[i, c("created_at")]]
		if (!is.na(nyuTweets[i, "reply_to_screen_name"])) {
			replyto = nyuTweets[[i, "reply_to_screen_name"]]
			edgelist <- rbind(edgelist, c(from, replyto, "reply", ttime))
		}
		if (!is.na(nyuTweets[i, "quoted_screen_name"])) {
			quotee = nyuTweets[[i, "quoted_screen_name"]]
			edgelist <- rbind(edgelist, c(from, quotee, "quote", ttime))
		}
		if (!is.na(nyuTweets[i, "mentions_screen_name"])) {
			mentionss = nyuTweets[[i, "mentions_screen_name"]]
			for (j in 1:length(mentionss)) {
				edgelist <- rbind(edgelist, c(from, mentionss[j], "mention", ttime))
			}
		}
		##### UNCOMMENT TO INCLUDE RETWEETS
		# if (!is.na(nyuTweets[i, "retweet_screen_name"])) {
		# retweeted = nyuTweets[[i, "retweet_screen_name"]]
		# edgelist <- rbind(edgelist, c(from, retweeted, "retweet", ttime))
		# }
	}
	edgelist_df <- as.data.frame(edgelist)
	names(edgelist_df) <- c("from", "to", "type", "time")
	# sort in chronological order
	edgelist_df <- edgelist_df[order(edgelist_df$time),]
	return(edgelist_df)
}
						

# edgelist_df <- makeNYUTweetEdgeList(nyu)
# write.table(edgelist_df, "nyu_edgelist.txt", row.names=FALSE)
edgelist_df <- read.table("nyu_edgelist.txt", header=T)

### DYNAMIC NETWORK CANNOT HANDLE MULIGRAPH, SO PICK MENTION --- MOST COMMON TYPE ANYWAY
edgelist_df <- edgelist_df[edgelist_df$type=="mention",]

### FOR DEBUGGING: KEEP ONLY FIRST N EDGES
edgelist_df <- edgelist_df[1:90,]


### NEED UNIQUE TO AVOID MULTIGRAPH
# g <- graph_from_edgelist(as.matrix(edgelist_df[,1:2]), directed=TRUE)
g <- graph_from_edgelist(unique(as.matrix(edgelist_df[,1:2])), directed=TRUE)
nyunet <- intergraph::asNetwork(g)
# nyunet %e% "type" <- edgelist_df$type
# nyunet %e% "time" <- 1:nrow(edgelist) # in lieu of actual edgelist_df$time

require(networkDynamic)

## NOTE: need a smarter way to make onsets the same for tweets at the same time

vs <- data.frame(onset=0, terminus=network.edgecount(nyunet), vertex.id=1:network.size(nyunet))
es <- data.frame(onset=1:network.edgecount(nyunet), terminus=network.edgecount(nyunet), 
                 head=as.matrix(nyunet, matrix.type="edgelist")[,1],
                 tail=as.matrix(nyunet, matrix.type="edgelist")[,2])
#create dynamic network
nyunet.dyn <- networkDynamic(base.net=nyunet, edge.spells=es, vertex.spells=vs)

compute.animation(nyunet.dyn, 
				# animation.mode = "MDSJ",
                  slice.par=list(start=0, 
                  				end=(network.edgecount(nyunet)-1), interval=2, 
                                 aggregate.dur=2, rule='any'))

render.d3movie(nyunet.dyn, usearrows = T, displaylabels = T, label= nyunet %v% "vertex.names",
               bg="white", 
               #vertex.border="#ffffff", 
               # vertex.col =  net %v% "col",
               vertex.cex = function(slice){log(degree(slice)+1)},  
               ####  I can't get dynamic label sizing o work
               #label.cex = function(slice){(degree(slice))},  
               ####
               # edge.lwd = (net %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (nyunet %v% "vertex.names")),
               # edge.tooltip = paste("<b>Edge type:</b>", (net %e% 'type'), "<br>", 
                                    # "<b>Edge weight:</b>", (net %e% "weight" ) ),
               launchBrowser=T, filename="scratch_NetworkDynamic.html",
               render.par=list(tween.frames = 10, show.time = F), ## WHAT DOES THIS DO??
               d3.options = list(animationDuration=3000,
               					playControls=FALSE, 
               					animateOnLoad=TRUE, 
               					slider=FALSE, 
               					debugFrameInfo=FALSE, 
               					debugDurationControl=FALSE),
               script.type='remoteSrc')



# # #users who tweet
# tweeters <- unique(tweets$screen_name)

# #users who are tweeted at
# ind <- !is.na(tweets$reply_to_screen_name)
# responders <- unique(tweets$reply_to_screen_name[ind])

# #build the the adjacency matrix
# ind <- which(!is.na(tweets$reply_to_screen_name))
# tweets.matrix <- as.matrix(tweets[ind,c("screen_name", "reply_to_screen_name", "created_at")]) 

# #create graph
# library(igraph)
# g.w <- graph.edgelist(tweets.matrix[,1:2], directed = FALSE)

# #create network
# net<-intergraph::asNetwork(g.w)
