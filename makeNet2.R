makeNet2 <- function(edgelist_df) {
		
	require(igraph)
	require(intergraph)
	require(network)
	require(ndtv)
	require(networkDynamic)
	
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
	


}