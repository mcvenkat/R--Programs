#
# Wallpaper
#
#xn+1 = yn - sign(xn) | b xn - c |1/2
#yn+1 = a - xn
#
# a=1
# b=4
# c=60

wallpaper<-function(n=4E4,x0=1,y0=1,a=1,b=4,c=60){
  x<-c(x0,rep(NA,n-1))
  y<-c(y0,rep(NA,n-1))
  cor<-rep(0,n)
  
  
  for (i in 2:n){
    
    x[i] = y[i-1] - sign(x[i-1])*sqrt(abs( b*x[i-1] - c) )
    y[i] = a - x[i-1]
    cor[i]<-round(sqrt((x[i]-x[i-1])^2+(y[i]-y[i-1])^2),0)
  }
  n.c<-length(unique(cor))
  cores<-heat.colors(n.c)
  
  plot(x,y,pch=".",col=cores[cor])
  
}

wallpaper()


