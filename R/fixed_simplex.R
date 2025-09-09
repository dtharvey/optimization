# set up grid of x,y values for use in plotting
x = seq(0,10,1)
y = seq(0,10,1)

# function to calculate response for simplex vertices
response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}

# values to define location and step-size for first simplex
a = 1
b = 1
sa = 1
sb = 1

# create vectors for the simplex points and responses
VX = rep(NA,100)
VY = rep(NA,100)
VZ = rep(NA,100)
INDEX = rep(NA,100)

# calculate responses for initial simplex
vx = c(a, a+sa, a + 0.5*sa)
vy = c(b, b, b + 0.87*sb)
vz = response(vx,vy)

# sort worst-to-best with worst in first row, best in third row
vx = vx[order(vz)]
vy = vy[order(vz)]
vz = vz[order(vz)]

# store values
VX[1:3] = vx
VY[1:3] = vy
VZ[1:3] = vz

vx_temp = rep(NA,3)
vy_temp = rep(NA,3)
vz_temp = rep(NA,3)

# plot initial simplex
plot(x = c(vx, vx[1]), y = c(vy, vy[1]),
     xlim = c(-1,10), ylim = c(-1,10),
     type = "b", pch = 19, cex = 1.5, col = 6, lwd = 2)

# loop here

for (i in 1:20){

# save response for worst
worst = vz[1]

# reject worst and calulate new vertex for next simplex
vx[1] = vx[2] + vx[3] - vx[1]
vy[1] = vy[2] + vy[3] - vy[1]
vz[1] = response(vx[1], vy[1])

# plot new simplex
# lines(x = c(vx, vx[1]), y = c(vy, vy[1]),
#       type = "b", pch = 19, col = "blue")

# if then to determine if simplex must change direction
# first, check to see new vertex is smaller than next-best or
# have a negative vertex
if (vx[1] < 0 | vy[1] < 0 | vz[1] < worst | vx[1] > 10 | vy[1] > 10){
  vz[2] = -1000
  vx = vx[order(vz)]
  vy = vy[order(vz)]
  vz = vz[order(vz)]
  VX[i:(i+2)] = vx
  VY[i:(i+2)] = vy
  VZ[i:(i+2)] = vz
  lines(x = c(vx, vx[1]), y = c(vy, vy[1]),
        type = "b", col = 6)
  # next

# } else if (vz[1] < worst){
#   vz[2] = -1000
#    vx = vx[order(vz)]
#    vy = vy[order(vz)]
#    vz = vz[order(vz)]
#    VX[i:(i+2)] = vx
#    VY[i:(i+2)] = vy
#    VZ[i:(i+2)] = vz
#    lines(x = c(vx, vx[1]), y = c(vy, vy[1]),
#          type = "b", col = 6, lwd = 2)
#    next
} else {
  vz[2] = 0
  vx = vx[order(vz)]
  vy = vy[order(vz)]
  vz = vz[order(vz)]
  VX[i:(i+2)] = vx
  VY[i:(i+2)] = vy
  VZ[i:(i+2)] = vz
  lines(x = c(vx, vx[1]), y = c(vy, vy[1]),
        type = "b", col = 6, lwd = 2)
  # next
 }
}
max_response = which.max(VZ)
points(x = VX[max_response], y = VY[max_response],
       pch = 15, col = 6, cex = 1.5, lwd = 2)

response = function(a,b){5.5 + 1.5*a + 0.6*b - 0.15*a^2 - 0.0254*b^2 - 0.0857*a*b}
z = outer(x,y,response)
contour2D(z,x,y, lwd = 2, colkey = FALSE, add = TRUE)
