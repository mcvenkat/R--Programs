install.packages("qcc")
install.packages("ca")
install.packages("FactoMineR")
library(ca)
library(qcc)
library(FactoMineR)
data(smoke)

pareto.contrib <- function(data,type,x, which){
  if (type=="table"){
    numb.dim.cols<-ncol(data)-1
    numb.dim.rows<-nrow(data)-1
    dimensionality <- min(numb.dim.cols, numb.dim.rows)
    res.CA <- CA(data, ncp=dimensionality, graph=FALSE)  #requires FactoMineR
    if (which=="R") {
      cntr= res.CA$row$contrib[,x]
      names(cntr) <- rownames(data)
      title <- paste("Rows contribution to the Inertia of Dim.", x)
    } else {
      cntr= res.CA$col$contrib[,x]
      names(cntr) <- colnames(data)
      title <- paste("Columns contribution to the Inertia of Dim.", x)
    }
    pareto.chart(cntr, cumperc = seq(0, 100, by = 20), ylab="Percentage", main=title, cex.axis=0.8, cex.names=0.8) #requires qcc
  }else{
    if (type=="obj"){
      res.CA <- data
      if (which=="R") {
        cntr= res.CA$row$contrib[,x]
        names(cntr) <- rownames(res.CA$row$contrib)
        title <- paste("Rows contribution to the Inertia of Dim.", x)
      } else {
        cntr= res.CA$col$contrib[,x]
        names(cntr) <- rownames(res.CA$col$contrib)
        title <- paste("Columns contribution to the Inertia of Dim.", x)
      }
      pareto.chart(cntr, cumperc = seq(0, 100, by = 20), ylab="Percentage", main=title, cex.axis=0.8, cex.names=0.8) #requires qcc
    }
  }
}


pareto.contrib(smoke, type="table", 1, which="R")
