clustop <- function(data, 
                   dist=c("euc", "max", "abs", "bin"), 
                   method=c("ward", "single", "complete", "average"),
                   minclust=1, maxclust=15) {
   require(NbClust)
   #
   # Create lookup for distance method and distance title
   #
   distdf <- data.frame(inp=c("euc", "max", "abs", "bin"), 
                        outp=c("euclidean", "maximum", 
                               "manhattan", "binary"), 
                        dtitle=c("Euclidean", "Maximum", 
                                 "Absolute", "Binary"))
   outdist <- distdf[,2:3]
   rownames(outdist) <- distdf[,1]
   dtype <- outdist[dist,1]
   dtitle <- outdist[dist,2]
   #
   # Create lookup for linkage method and linkage title
   #
   linkdf <- data.frame(inp=c("ward", "single", "complete", "average"), 
                        outp=c("ward.D", "single", "complete", "average"), 
                        ltitle=c("Ward's D", "Single", "Complete", "Average"))
   outlink <- linkdf[,2:3]
   rownames(outlink) <- linkdf[,1]
   ltype <- outlink[method,1]
   ltitle <- outlink[method,2]

   duda <- NbClust(data, distance=dtype, min.nc=minclust, max.nc=maxclust, method=ltype, index="duda")$All.index
   pseudot2 <- NbClust(data, distance=dtype, min.nc=minclust, max.nc=maxclust, method=ltype, index="pseudot2")$All.index
   out <- data.frame("Num.Clusters"=minclust:maxclust,"Duda/Hart"=duda, "pseudo-t^2"=pseudot2)
   out
}