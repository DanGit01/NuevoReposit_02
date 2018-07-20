fixChlorophyllData <- function(chla){
  
  
  actualYear <- chla$Year[1]
  
  chla$Date <- ymd("1900-01-01")
  
  for(i in 1:nrow(chla)){
    
    if(is.na(chla$Year[i])) {
      
      chla$Year[i] <- actualYear
      
    } else {
      
      actualYear <- chla$Year[i]
    }
    
    fechaTmp <- ymd( paste(chla$Year[i], chla$Month[i], 1 ) )
    
    if(is.na(fechaTmp)) {
      if(chla$Month[i]=="Mar"){
        fechaTmp <- ymd( paste(chla$Year[i], 3, 1 ) )
      }else if(chla$Month[i]=="Ago"){
        fechaTmp <- ymd( paste(chla$Year[i], 8, 1 ) )
      }else{
        fechaTmp <- dmy(chla$Month[i])
      }
    } 
    
    chla$Date[i] <- fechaTmp
    
  }
    
  
  chla$IntegE1 <- abs(chla$IntegE1)
  
  chla$IntegE2 <- abs(as.numeric(chla$IntegE2))
  
  
  return(chla)
  
}



readEcoNetwork <- function(nameData){
  web <- read.csv(nameData,  header = T,check.names = F)

  if(ncol(web)==2){
    
    # si es matriz de adyacencia
    
    web <- web[,c(2,1)]
    
    g <- graph_from_data_frame(web)
    
  } else {
    
    # si es matriz de adyacencia, debe ser cuadrada
    
    if( (ncol(web)-1) == nrow(web)  ) {
      g <- graph_from_adjacency_matrix(as.matrix(web[,2:ncol(web)]))
      
    } else {
      g <- NULL
      warning("Invalid file format: ",nameData)
    }   
  }

  return(g)

}



plotNetworkTrophLevel <- function(objGraph, vertexLabel=FALSE,vertexSizeFactor=5){
  deg <- degree(objGraph, mode="all") # calculate the degree: the number of edges 
  # or interactions
  
  V(objGraph)$size <- log10(deg)*vertexSizeFactor+vertexSizeFactor    # add node degrees to igraph object
  
  V(objGraph)$frame.color <- "white"    # Specify plot options directly on the object 
  
  V(objGraph)$color <- "orange"         #
  
  if(!vertexLabel)
    V(objGraph)$label <- NA
  
  ######## require(NetIndices)
  
  tl <- TrophInd(get.adjacency(objGraph,sparse=F))  # Calculate the trophic level
  
  lMat <-matrix(                      # Layout matrix to specify the position of each vertix
    nrow=vcount(objGraph),            # Rows equal to the number of vertices (species)
    ncol=2
  )
  
  lMat[,2]<-jitter(tl$TL,0.1)              # y-axis value based on trophic level
  lMat[,1]<-runif(vcount(objGraph))        # randomly assign along x-axis
  
  ######### require(RColorBrewer)                   # Use color brewer palettes
  
  colTL <-as.numeric(cut(tl$TL,11))       # Divide trophic levels in 11 segments
  colnet <- brewer.pal(11,"RdYlGn")       # Assign colors to trophic levels
  V(objGraph)$color <- colnet[12-colTL]   # Add colors to the igraph object

  plot(objGraph, edge.width=.3,edge.arrow.size=.4, vertex.label.color="white", edge.color="grey50", edge.curved=0.3, layout=lMat)
  
  
  #plot(objGraph, edge.width=.3,edge.arrow.size=.4,
    #vertex.label=NA,
  #  vertex.label.color="white",
  #  edge.color="grey50",
  #  edge.curved=0.3, layout=lMat)
  
  #plot(objGraph, edge.arrow.size=.4,
  #  vertex.label=NA,vertex.size=8,
  #  layout=layout_with_fr) # Smaller vertex size, specifying a layout 
}



# devolver dataFrame
#  data.frame(size <nombre columna = nombre variable >, conectance <nombre columna> = conn <nombre variable>)
#


#topologicalIndicesEcoNetwork <- function(objFile){
#  tl <- TrophInd(get.adjacency(objFile,sparse = F))
  
#  return(tl)
  
#}




topologicalIndicesEcoNetwork <- function(g){
  
    deg <- degree(simplify(g), mode="out") # calculate the out-degree: the number of predators  
    V(g)$outdegree <-  deg
    
    nTop <- length(V(g)[outdegree==0]) # Top predators do not have predators
    
    deg <- degree(g, mode="in") # calculate the in-degree: the number of preys
    
    V(g)$indegree <-  deg
    
    nBasal <- length(V(g)[indegree==0]) # Basal species do not have preys 
    
    vcount(g)-nTop-nBasal
    
    size <- vcount(g)
    
    links <- ecount(g)
    
    linkDen <- links/size          # Linkage density
    
    conn <- links/size^2           # Connectance
    
    pathLength <- average.path.length(g)   # Average path length 
    
    clusCoef <- transitivity(g, type = "global") 
    
    cannib <- sum(which_loop(g))
    
    data.frame(Size=size,Top=nTop,Basal=nBasal,Links=links, LD=linkDen,Connectance=conn,PathLength=pathLength,Clustering=clusCoef, Cannib=cannib)

}
  
