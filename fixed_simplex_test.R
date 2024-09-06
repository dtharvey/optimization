# new attempt at fixed size simplex

# set color scheme
palette("Okabe-Ito")

# set up grid of (x,y) values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# function to calculate responses for (x,y)
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# define the location of first point and the step-sizes
a = 9
b = 0
sa = 1
sb = 1

# calculate responses for initial simplex
vx = c(a, a + sa, a + 0.5 * sa)
vy = c(b, b, b + 0.87 * sb)
vz = response(vx,vy)

# sort vertices from worst-to-best
vx = vx[order(vz)]
vy = vy[order(vz)]
vz = vz[order(vz)]

max_response = vz[3]

# plot initial simplex
plot(x = c(vx,vx[1]), y = c(vy,vy[1]),
     xlim = c(-1,11), ylim = c(-1,11), xlab = "x", ylab = "y",
     type = "b", pch = 19, cex = 1.2, col = 6, lwd = 2)
grid()

# loop for remaining simplexes
for (i in 1:100){
  
  # save current next best response for testing
  next_best = vz[2]
  
  # determine values for new vertex based on the worst vertex in [1]
  vx_temp = vx[2] + vx[3] - vx[1]
  vy_temp = vy[2] + vy[3] - vy[1]
  vz_temp = response(vx_temp, vy_temp)
  
  # test new vertex against worst and boundary conditions
  # if it fails go to next
  # else it passes and defines new set of vertices
  if(vx_temp < 0 | vy_temp < 0 | vx_temp > 10 | vy_temp >10 | vz_temp < next_best){
    vz[2] = -1000
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    # lines(x = c(vx,vx[1]), y = c(vy,vy[1]),
    #       type = "b", col = 6, lwd = 2)
    next
  } else {
    vx[1] = vx_temp
    vy[1] = vy_temp
    vz[1] = vz_temp
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    lines(x = c(vx,vx[1]), y = c(vy,vy[1]),
          type = "b", col = 6, lwd = 2)
    if(vz[3] > max_response){
      max_response = vz[3]
      max_x = vx[3]
      max_y = vy[3]}
  }
}

# mark end point 
points(x = max_x, y = max_y,
       pch = 15, col = 6, cex = 1.5, lwd = 2)

# add contour plot
z = outer(x,y,response)
contour2D(z,x,y, lwd = 2, colkey = FALSE, add = TRUE)
