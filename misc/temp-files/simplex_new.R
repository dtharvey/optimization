# set up grid of x,y values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# define function to calculate response for simplex vertices
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# create data frame to store saved simplex information
# vx is vertex on x-axis; vy is vertex on y-axis; 
# vz is response for simplex; steps is simplex number
vx = rep(NA,100)
vy = rep(NA,100)
vz = rep(NA,100)
optdata = data.frame(vx,vy,vz)

# values to define first simplex
a = 0
b = 0
sa = 1
sb = 1

vxtemp = c(a, a+sa, a + 0.5*sa)
vytemp = c(b, b, b + 0.87*sb)
vztemp = response(vxtemp,vytemp)
opt_temp = data.frame(vxtemp, vytemp, vztemp, row.names = NULL)

# add first simplex vertices to optdata
optdata$vx[1:3] = opt_temp$vxtemp[1:3]
optdata$vy[1:3] = opt_temp$vytemp[1:3]
optdata$vz[1:3] = opt_temp$vz[1:3]

# order the three vertices of simplex from W to N to B in rows
# optdata = optdata[order(optdata$vz), ]

# need to start for loop here to add rows beyond the initial three (1:3)

for (i in 1:5){
  
# read in the last three rows of optdata into opt_temp
  opt_temp = optdata[(i):(i+2),]

# order the three vertices of simplex from W to N to B in rows
  opt_temp = opt_temp[order(opt_temp$vz), ]

# calculate new vertex to replace worst vertex and store in next row
  optdata$vx[i+3] = opt_temp$vx[2] + opt_temp$vx[3] - opt_temp$vx[1]
  optdata$vy[i+3] = opt_temp$vy[2] + opt_temp$vy[3] - opt_temp$vy[1]
  optdata$vz[i+3] = response(optdata$vx[i+3],optdata$vy[i+3])

}





#  test to see if new vertex has a negative value for vxnew or vynew
# if (vxnew < 0 | vynew <0){
#   vxnew = optdata$vx[1] + optdata$vx[3] - optdata$vx[2]
#   vynew = optdata$vy[1] + optdata$vy[3] - optdata$vy[2]
#   vznew = response(vxnew,vynew)
# }
# 
#  test to see if response of new vertex is smaller than worst
# if (vznew < optdata$vz[1]){
#   vxnew = optdata$vx[1] + optdata$vx[3] - optdata$vx[2]
#   vynew = optdata$vy[1] + optdata$vy[3] - optdata$vy[2]
#   vznew = response(vxnew,vynew)
# }
# 
# store new



