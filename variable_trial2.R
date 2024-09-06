# attempt at variable size simplex

# set color scheme
palette("Okabe-Ito")

# load packages
library(plot3D)

# set up grid of (x,y) values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# function to calculate responses for (x,y)
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# define the location of first point and the step-sizes
a = 1
b = 1
sa = 1
sb = 1

# calculate responses for initial simplex
vx = c(a, a + sa, a + 0.5 * sa)
vy = c(b, b, b + 0.87 * sb)
vz = response(vx,vy)

# sort vertices from worst-to-best;
# worst is in position 1, next is in position 2, best is in position 3
vx = vx[order(vz)]
vy = vy[order(vz)]
vz = vz[order(vz)]

# plot initial simplex
plot(x = c(vx,vx[1]), y = c(vy,vy[1]),
     xlim = c(-1,11), ylim = c(-1,11), xlab = "x", ylab = "y",
     type = "b", pch = 19, cex = 1.2, col = 6, lwd = 2)
grid()

# loop for remaining simplexes
for (i in 1:3){
  # add new simplex to plot
  lines(x = c(vx,vx[1]), y = c(vy,vy[1]),type = "b", col = 6, lwd = 2)
  
  # save current worst, next, and best responses for testing
  Worst = vz[1]
  Next = vz[2]
  Best = vz[3]
  
  # calculate centroid of vx[2] & vx[3]
  centroid_x = vx[2]/2 + vx[3]/2
  centroid_y = vy[2]/2 + vy[3]/2
  
  # calculate values for R
  vx_r = centroid_x + (centroid_x - vx[1])
  vy_r = centroid_y + (centroid_y - vy[1])
  vz_r = response(vx_r, vy_r)
  
  # calculate values for E
  vx_e = centroid_x + 2 * (centroid_x - vx[1])
  vy_e = centroid_y + 2 * (centroid_y - vy[1])
  vz_e = response(vx_e, vy_e)
  
  # calculate values for CR
  vx_cr = centroid_x + 0.5 * (centroid_x - vx[1])
  vy_cr = centroid_y + 0.5 * (centroid_y - vy[1])
  vz_cr = response(vx_cr, vy_cr)
  
  # calculate values for CW
  vx_cw = centroid_x - 0.5 * (centroid_x - vx[1])
  vy_cw = centroid_y - 0.5 * (centroid_y - vy[1])
  vz_cw = response(vx_cw, vy_cw)
  
  # keep E if it is larger than R
  if (vz_e > vz[3]){
    vx[1] = vx_e
    vy[1] = vy_e
    vz[1] = vz_e
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    next
  } 
  
  if (vz_e < vz[3] & vz_e > vz[2]) {
    vx[1] = vx_r
    vy[1] = vy_r
    vz[1] = vz_r
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    next
  } 
  
  if (vz_r < vz[3] & vz_r > vz[2]){
    vx[1] = vx_r
    vy[1] = vy_r
    vz[1] = vz_r
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    next
  } 
  
  if (vz_r < vz[2] & vz_r > vz[1]){
    vx[1] = vx_cr
    vy[1] = vy_cr
    vz[1] = vz_cr
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    next
  } else {
    vx[1] = vx_cw
    vy[1] = vy_cw
    vz[1] = vz_cw
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
  }
}

z = outer(x,y,response)
contour2D(z,x,y, lwd = 2, colkey = FALSE, add = TRUE)
