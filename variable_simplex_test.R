# attempt at variable size simplex

# set color scheme
palette("Okabe-Ito")

# set up grid of (x,y) values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# function to calculate responses for (x,y)
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# define the location of first point and the step-sizes
a = 0
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
  
  # save current next best and best response for testing
  next_best = vz[2]
  best = vz[3]
  
  # initial attempt at new vertex: 
  # vx[2]/2 + vx[3]/2 = centroid between vx[2] and vx[3]
  centroid_x = vx[2]/2 + vx[3]/2
  centroid_y = vy[2]/2 + vy[3]/2
  vx_temp = centroid_x + (centroid_x - vx[1])
  vy_temp = centroid_y + (centroid_y - vy[1])
  vz_temp = response(vx_temp, vy_temp)
  
  # have we exceeded a boundary condition? if so, then set response
  # for next best so that it is worst, reorder the three points
  # and break out of for loop
  if(vx_temp < 0 | vy_temp < 0 | vx_temp > 10 | vy_temp > 10){
    vz[2] = -1000
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    next
  }
  
  # should we expand the simplex? test new response to best response;
  # test to see if the expanded response exceeds the best response
  # then keep the expansion; otherwise retain 
  if (vz_temp > vz[3]){
    vx_temp_expand = centroid_x + 2 * (centroid_x - vx[1])
    vy_temp_expand = centroid_y + 2 * (centroid_y - vy[1])
    vz_temp_expand = response(vx_temp, vy_temp)
    if(vz_temp_expand > vz_temp){
      vx[1] = vx_temp_expand
      vy[1] = vy_temp_expand
      vz[1] = vz_temp_expand
    } else {
      vx[1] = vx_temp
      vy[1] = vy_temp
      vz[1] = vz_temp
    }
    next
  }
  
  
  # should we hold tight?
  
  
  
  # should we contract toward reflected point?
  
  
  
  # should we contract toward worst?
  
  
  
  
  # test new vertex against worst
  # if it fails go to next
  # else it passes and define new set of vertices
  if(vx_temp < 0 | vy_temp < 0 | vx_temp > 10 | vy_temp >10 | vz_temp < next_best){
    vz[2] = -1000
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    # lines(x = c(vx,vx[1]), y = c(vy,vy[1]),
    #       type = "b", col = 6, lwd = 2)
    # next
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
