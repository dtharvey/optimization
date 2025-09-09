# set up grid of x,y values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# define function to calculate response for simplex vertices
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# values to define location and step-size for first simplex
a = 0
b = 0
sa = 1
sb = 1

# calculate responses for initial simplex
vx = c(a, a+sa, a + 0.5*sa)
vy = c(b, b, b + 0.87*sb)
vz = response(vx,vy)
df = data.frame(vx,vy,vz)
df = df[order(df$vz), ]

# create data frame to store saved simplex information
# vx is vertex on x-axis; vy is vertex on y-axis; 
# vz is response for simplex; steps is simplex number
VX = rep(NA,100)
VY = rep(NA,100)
VZ = rep(NA,100)
optdata = data.frame(VX,VY,VZ)
optdata$VX[1:3] = vx
optdata$VY[1:3] = vy
optdata$VZ[1:3] = vz

# plot initial simplex
plot(x = c(df$vx, df$vx[1]), y = c(df$vy, df$vy[1]),
     xlim = c(-1,10), ylim = c(-1,10),
     type = "b", pch = 19, col = "red")

# calculate new vertex to replace worst vertex
df$vx[1] = df$vx[2] + df$vx[3] - df$vx[1]
df$vy[1] = df$vy[2] + df$vy[3] - df$vy[1]
df$vz[1] = response(df$vx[1],df$vy[1])

# add simplex to plot
lines(x = c(df$vx, df$vx[1]), y = c(df$vy, df$vy[1]),
      type = "b", pch = 19, col = "blue")

optdata$VX[1:3] = df$vx
optdata$VY[1:3] = df$vy
optdata$VZ[1:3] = df$vz

# place following in loop

for (i in 1:100){
  
# order three vertices from worst-to-best

worst_response = df$vz[1]

# calculate new vertex to replace worst vertex
df$vx[1] = df$vx[2] + df$vx[3] - df$vx[1]
df$vy[1] = df$vy[2] + df$vy[3] - df$vy[1]
df$vz[1] = response(df$vx[1],df$vy[1])

# add simplex to plot
lines(x = c(df$vx, df$vx[1]), y = c(df$vy, df$vy[1]),
      type = "b", pch = 19, col = "blue")

# is new vertex worse than next best
if (df$vz[1] < worst_response){
  df$vz[2] = 0
}

df = df[order(df$vz), ]

optdata$VX[i:(i + 2)] = df$vx
optdata$VY[i:(i + 2)] = df$vy
optdata$VZ[i:(i + 2)] = df$vz

}
