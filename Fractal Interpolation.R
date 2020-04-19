###########################
## FRACTAL INTERPOLATION ##
###########################

#Points, X- and Y-coordinates.
X=c(0,20,60,95,110)
Y=c(0,60,30,10,20)
# Coefficients for affine transformation.
A=c()
B=rep(0,(length(Y)-1))
C=c()
# D is for vertical scaling, free parameter, 0 < D < 1.
D=c(0.3,0.2,0.4,0.1) # Check the number of D's: n-1.
E=c()
F=c()
N=length(X)
for(n in 1:(length(X)-1))
{
  A[n]=(X[n+1]-X[n])/(X[N]-X[1])
  E[n]=((X[N]*X[n])-(X[1]*X[n+1]))/(X[N]-X[1])
  C[n]=(Y[n+1]-Y[n])/(X[N]-X[1])-(D[n]*((Y[N]-Y[1])/(X[N]-X[1])))
  F[n]=((X[N]*Y[n]-X[1]*Y[n+1])/(X[N]-X[1]))-(D[n]*((X[N]*Y[1]-X[1]*Y[N])/(X[N]-X[1])))
}

BMat=cbind(A,B,C,D,E,F) 

#-----------------------------------#

x=0;y=0;x1=c();y1=c()

# This loop is for finding the limits of the picture:
for(i in 1:3000)
{
  x2=x
  i=sample(1:length(BMat[,1]),1)
  x=BMat[i,1]*x+BMat[i,2]*y+BMat[i,5]
  y=BMat[i,3]*x+BMat[i,4]*y+BMat[i,6]
  x1=c(x1,x);y1=c(y1,y)
}

X11()

plot(0,0,xlim=c(min(x1),max(x1)),ylim=c(min(y1),max(y1)+10),col="white",xlab="x", ylab="f(x)")
points(X,Y,pch=20,type="b")

for(j in 1:40000)
{
  x3=x
  i=sample(1:length(BMat[,1]),1)
  x=BMat[i,1]*x+BMat[i,2]*y+BMat[i,5]
  y=BMat[i,3]*x3+BMat[i,4]*y+BMat[i,6]
  points(x,y,pch=".",cex=1)
}

title(main="Fractal Interpolation")