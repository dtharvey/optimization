# set up grid of x,y values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# define function to calculate response for simplex verticies
model2 = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# create data frame to store saved simplex information
# VX is vertex on x-axis; VY is vertex on y-axis; 
# VZ is response for simplex; STEPS is simplex number
VX = rep(NA,100)
VY = rep(NA,100)
VZ = rep(NA,100)
SIMPLEX = rep(NA,100)
DF = data.frame(SIMPLEX,VX,VY,VZ)

# values to define first simplex
a = 0
b = 0
sa = 1
sb = 1

# create temporary data frame to evaluate current simplex order them
# so that worst response is in first row
vx = c(a, a+sa, a + 0.5*sa)
vy = c(b, b, b + 0.87*sb)
vz = model2(vx,vy)
simplex = rep(1,3)
df = data.frame(simplex,vx,vy,vz)
df = df[order(df$vz),]

# fill first three rows of DF with this initial simplex
DF$SIMPLEX[1:3] = df$simplex
DF$VX[1:3] = df$vx
DF$VY[1:3] = df$vy
DF$VZ[1:3] = df$vz

# begin plot
plot(x = c(DF$VX[1], DF$VX[2], DF$VX[3], DF$VX[1]), 
     y = c(DF$VY[1], DF$VY[2], DF$VY[3], DF$VY[1]),
     type = "b", pch = 19, col = "blue",
     xlim = c(0,10), ylim = c(0,10))

# loop to (a) populate temporary dataframe, (b) reorder dataframe,
# (c) calculate new vertex and its response, (d) add three new 
# rows to DF for the new simplex
# for (i in 4:100){
  
  i = 4
  
# populate temporary dataframe
  vx = DF$VX[(i-3:i-1)]
  vy = DF$VY[(i-3:i-1)]
  vz = DF$VZ[(i-3:i-1)]
  simplex = rep((i-2),3)
  df = data.frame(simplex, vx, vy, vz)
  
# reorder temporary data frame by response with worst at top
df = df[order(df$vz),]

# calculate the new vertex and its response
vxnew = df$vx[2] + df$vx[2] - df$vx[1]
vynew = df$vy[2] + df$vy[2] - df$vx[1]
vznew = model2(vxnew, vynew)

# move new vertex and response into DF
DF$SIMPLEX = df$simplex
DF$VX[(i-3):(i-1)] = c(vxnew)
DF$VY[i] = vynew
DF$VZ[i] = vznew


# add to plot
lines(x = c(DF$VX[1], DF$VX[2], DF$VX[3]), 
     y = c(DF$VY[1], DF$VY[2], DF$VY[3]),
     type = "b", pch = 19, col = "blue",
     xlim = c(0,10), ylim = c(0,10))

# }




