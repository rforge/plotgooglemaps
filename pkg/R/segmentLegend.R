segmentLegend <-
function (attribute,
                        colPalette=rainbow(length(attribute)),
                        border=NA,
                        legendName="Legend",
                        bgc='white') {


png(filename=paste(legendName,'.png',sep=""), width=300,
       height=300,units = "px", bg="white")
       
 par(mar=c(2.1,5.1,2.1,5.1), bg=bgc)


	niv  <- attribute
  cols <-as.character(substr(colPalette,1,7))

                  x<-rep(1,length(niv))

            pie(  x,
                  labels=niv,
                  clockwise=FALSE,
                  radius=1,
                  col= cols,
                  bg=bgc,
                  init.angle=90)

                graph1 <- dev.cur()
                dev.off(graph1)



    }
