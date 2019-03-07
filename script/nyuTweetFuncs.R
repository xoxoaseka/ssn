# rm(list=ls())
require(rtweet)
require(dplyr)
require(igraph)
require(intergraph)
require(network)
require(ndtv)
require(networkDynamic)
						
makeTweetEdgeList <- function(nyuTweets) {
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
						
makeTwitterNetworkAnimation <- function(edgelist, agg=2, intv=2) {
	### NEED UNIQUE TO AVOID MULTIGRAPH
	g <- graph_from_edgelist(unique(as.matrix(edgelist[,1:2])), directed=TRUE)
	nyunet <- intergraph::asNetwork(g)
	# nyunet %e% "type" <- edgelist_df$type
	# nyunet %e% "time" <- 1:nrow(edgelist) # in lieu of actual edgelist_df$time
	
	
	## NOTE: need a smarter way to make onsets the same for tweets at the same time
	vs <- data.frame(onset=0, terminus=network.edgecount(nyunet), vertex.id=1:network.size(nyunet))
	es <- data.frame(onset=1:network.edgecount(nyunet), terminus=network.edgecount(nyunet), 
	                 head=as.matrix(nyunet, matrix.type="edgelist")[,2],
	                 tail=as.matrix(nyunet, matrix.type="edgelist")[,1])
	#create dynamic network
	nyunet.dyn <- networkDynamic(base.net=nyunet, edge.spells=es, vertex.spells=vs)
	
	compute.animation(nyunet.dyn, 
				# animation.mode = "MDSJ",
                  slice.par=list(start=0, 
                  				end=(network.edgecount(nyunet)-1), interval=intv, 
                                 aggregate.dur=agg, rule='any'))

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
               launchBrowser=T, filename=paste0("NetworkDynamic",Sys.time(),".html"),
               render.par=list(tween.frames = 10, show.time = F), ## WHAT DOES THIS DO??
               d3.options = list(animationDuration=3000,
               					playControls=FALSE, 
               					animateOnLoad=TRUE, 
               					slider=TRUE 
               					),
               script.type='remoteSrc')
}


