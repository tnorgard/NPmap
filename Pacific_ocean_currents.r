################################################################################
## Program name:  Revised_Pacific_Ocean_currents_figure.r
## Author:        M. Thiess
## Date:          June 2017
## Additional information:  This program recreates the plot of Pacific Ocean currents
##                          from Thomson 2003.
## Additional information:
################################################################################

##______________________________________________________________________________
##                            READ ME FILE
################################################################################
# The files that the program will use should be stored in the root.dir defined below
# By default, the program will save file in the root.dir defined below
#
#
#
################################################################################


##______________________________________________________________________________
##                            INITIAL SETUP
################################################################################

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
root.dir <- paste(getwd(),"/",sep="")
oldpar <- par(no.readonly=TRUE)

filename <- c("zone_polys.csv")    #### CHANGE DATA FILE NAME HERE ####
data.file <- paste(root.dir, filename, sep="");
alldata <- read.csv(file=data.file, header=TRUE, sep=",")

filename2 = c("zone_labels.csv")
data.file <- paste(root.dir, filename2, sep="");
zone.labs <- read.csv(file=data.file, header=TRUE, sep=",")

##______________________________________________________________________________
##                              LIBRAIRIES
################################################################################
require(PBSmapping)       #Package to draw maps
require(maps)
require(igraph)

map.dir <- c("c:/mary/maps/GSHHS/");

##______________________________________________________________________________
##                                DATA
################################################################################

zone.poly<-as.PolySet(alldata,projection="LL",zone=11)
zone.labs.poly = as.PolySet(zone.labs,projection="LL",zone=11)

##______________________________________________________________________________
##                              PARAMETERS
################################################################################



##______________________________________________________________________________
##                              PROCEDURES
################################################################################

importGSHHSborders <- function (gshhsDB, xlim, ylim, maxLevel = 4, n = 0)
{
          if (missing(gshhsDB))
                    gshhsDB <- paste(system.file(package = "PBSmapping"),"gshhs_f.b", sep = "/")
          else gshhsDB <- path.expand(gshhsDB)
          if (!file.exists(gshhsDB))
                    stop("unable to find gshhsDB \"", gshhsDB, "\"")
          limits <- c(xlim[1], xlim[2], ylim[1], ylim[2])
          .checkClipLimits(limits)
          x <- .Call("importGSHHS", as.character(gshhsDB), as.numeric(limits),
                     as.integer(maxLevel), as.integer(n), PACKAGE = "PBSmapping")
          if (is.null(x) || !length(x$PID))
                    return(NULL)
          PolyData <- as.data.frame(attr(x, "PolyData"))
          attr(x, "PolyData") <- NULL
          clipAsPolys <- attr(x, "clipAsPolys")
          attr(x, "clipAsPolys") <- NULL
          x <- as.PolySet(as.data.frame(x), projection = "LL")
          if (clipAsPolys)
                    x <- clipPolys(x, xlim = xlim, ylim = ylim)
          else x <- clipLines(x, xlim = xlim, ylim = ylim)
          x$oldPOS <- NULL
          x <- x[order(x$PID, x$SID), ]
          attr(x, "PolyData") <- PolyData
          return(x)
}

limits <- list(x = c(185, 260), y = c(20, 65))
polys <- importGSHHS (paste(map.dir,"gshhs_h.b",sep=""),xlim=limits$x,ylim=limits$y,maxLevel=1)

jpeg("Pacific_ocean_currents.jpg", width=1600, height=1200, quality=100, pointsize=24)
plotMap(polys,border="grey20",plt=c(.05,.99,.075,.99),projection="LL",colHoles=NA,tckLab=FALSE, col="grey20", bg="transparent") 
addPolys(zone.poly,border="grey20",col=c("dodgerblue","yellow","red"))
addPolys(polys,border="grey20",col="grey50") 
addLabels(zone.labs,placement="DATA")

#add arrows for currents

dev.off ()


#reset the graphics parameters
par(oldpar)

## -- END OF THE PROGRAM -- ##
#----------------------------------------------------------------------------


