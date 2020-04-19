##########################################################################################
## MANDELBROT SET  MOUNTANIZE AND DIAMOND SQUARE ALGORITHM FOR GRAPHICAL PLAIN LANDSCAPE##
##########################################################################################

Limits=c(-2,0.8)
MaxIter=25
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.005)
S=floor(length(cl)/MaxIter)
Dist=0
PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
t=0


for(a in Step)
{
  for(b in Step+0.6)
  {
    x=0;y=0;n=0;Dist=0
    while(n<MaxIter & Dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      Dist=newx^2+newy^2
      x=newx;y=newy
    }
    if(Dist<4) colour=24 # black colour
    else colour=n*S
    t=t+1
    PointsMatrix[t,]=c(a,b,colour)
  }
}



mountainize <- function(a, roughness, sdev) {
  # Recursive function to create a one-dimensional "mountain range."
  # Arguments:
  #    a: numeric vector
  #    roughness: number between 0 and 1, controlling how 
  #    quickly the random perturbations tail off as the scale decreases.
  #    sdev: standard deviation of the random perturbations.
  # Value:
  #    A numeric vector the same length as a.
  size <- length(a)
  if (size < 2) {
    return(a)
  } else {
    middle <- (size + 1) / 2
    a[middle] <- mean(c(a[1], a[size])) + rnorm(1, 0, sdev)
    a[1:middle] <- mountainize(a[1:middle], 
                               roughness, sdev * roughness)
    a[middle:size] <- mountainize(a[middle:size], 
                                  roughness, sdev * roughness)
    return(a)
  }
}

iterations <- 8  # set the number of times to repeat the algorithm
mountains <- rep(0, 2 ^ iterations + 1)  # make an array of the correct size
set.seed(1)   # delete or change the seed to get other mountain ranges
mountains <- mountainize(mountains, 0.5, 1)
plot(mountains, ty='l')


# Sam Urmy 10/27/2010
#
# I adapted this implementation of the diamond/square algorithm
# for terrain generation from C++ code by Daniel Beard.
# 
# http://danielbeard.wordpress.com/2010/08/07/terrain-generation-and-smoothing/
# Install/load the rgl package to be able to render the
# terrain in 3-D.  It's worth it.
install.packages("rgl")
library(rgl)

mountains <- function(iter, roughness=0.5, m=0, sdev=1) {
  # Function to create mountainous terrain via the 
  # diamond-square algorithm.
  #
  # Arguments:
  # iter: integer. Number of iterations (determines
  #   size of finished terrain matrix).
  # roughness: float, between 0 and 1.  Factor by
  #   which the random deviations are reduced
  #   between iterations.
  # m: optional initial matrix, must be square
  #   with a dimension equal to a power of
  #   two (i.e., 2 ^ number of iterations) plus 1.
  # sdev: optional float. Inital standard deviation
  #   for the random deviations.
  # Value:
  # A square matrix of elevation values.
  
  size <- 2^iter + 1
  # If the user does not supply a matrix,
  # initalize one with zeros.
  if (! m) {
    m <- matrix(0, size, size)
  }
  # Loop through side lengths, starting with the size of the
  # entire matrix, and moving down by factors of 2.
  for (side.length in 2^(iter:1)) {
    half.side <- side.length / 2
    # Square step
    for (col in seq(1, size - 1, by=side.length)) {
      for (row in seq(1, size - 1, by=side.length)) {
        avg <- mean(c(
          m[row, col],        # upper left
          m[row + side.length, col],  # lower left
          m[row, col + side.length],  # upper right
          m[row + side.length, col + side.length] #lower right
        ))
        avg <- avg + rnorm(1, 0, sdev)
        m[row + half.side, col + half.side] <- avg
      }
    }
    
    # Diamond step
    for (row in seq(1, size, by=half.side)) {
      for (col in seq((col+half.side) %% side.length, size, side.length)) {
        # m[row, col] is the center of the diamond
        avg <- mean(c(
          m[(row - half.side + size) %% size, col],# above
          m[(row + half.side) %% size, col], # below
          m[row, (col + half.side) %% size], # right
          m[row, (col - half.side) %% size]  # left
        ))
        m[row, col] <- avg + rnorm(1, 0, sdev)
        # Handle the edges by wrapping around to the
        # other side of the array
        if (row == 0) { m[size - 1, col] = avg }
        if (col == 0) { m[row, size - 1] = avg }
      }
    }
    # Reduce the standard deviation of the random deviation
    # by the roughness factor.
    sdev <- sdev * roughness
  }
  return(m)
}

color.matrix <- function(m, ncolors, palette=terrain.colors) {
  # Takes a matrix, and maps it onto a matrix of the same size
  # in color space.
  # Useful for draping a color map on top of a persp3d map.
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  col.index <- m - min(m)
  col.index <- (ncolors - 1) * col.index / max(col.index) + 1
  return(matrix(palette(ncolors)[col.index], nrows, ncols))
}

iter <- 7  # set number of iterations
set.seed(1) # change or delete seed to get different terrains
m <- mountains(iter, .5)

filled.contour(m, col=terrain.colors(36))
# make a color overlay for the 3-D plot
colors <- color.matrix(m, 24)
persp3d(m, aspect=c(1,1,.5), col=colors)