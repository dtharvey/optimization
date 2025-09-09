# set up grid of x,y values
x = seq(0,10,1)
y = seq(0,10,1)

# create function for response surface's model
model2 = function(a,b){4.0 - 0.4*a + 0.08*a*b}

# calculate values for response at all grid points
z = outer(x,y,model2)

# create vectors to store points
X = rep(NA,100)
Y = rep(NA,100)
Z = rep(NA,100)
steps = rep(NA,100)

# initial set-up
steps[1] = 1
X[1] = 5
Y[1] = 5
Z[1] = z[X[1]+1,Y[1]+1]


# increment (step size)
increment = 1

optimize = "factorA"

# loop to evaluate movement
for (i in 2:100){
  
  # x coordinates and y coordinates for four possible movements
  if (optimize == "factorA"){
    ex = c(X[i-1]+1, X[i-1], X[i-1]-1)
    why = c(Y[i-1], Y[i-1], Y[i-1])
  } else {
    ex = c(X[i-1], X[i-1], X[i-1])
    why = c(Y[i-1]+1, Y[i-1], Y[i-1]-1)
  }
  
  # ex = c(X[i-1]+1, X[i-1]-1, X[i-1], X[i-1], 
  #        X[i-1]+1, X[i-1]+1, X[i-1]-1, X[i-1]-1)
  # why = c(Y[i-1], Y[i-1], Y[i-1]+1, Y[i-1]-1, 
  #         Y[i-1]+1, Y[i-1]-1, Y[i-1]-1, Y[i-1]+1)
  
  # which of the possible new coordinates are in bounds
  possible.id = which(ex >= 0 & ex <= 10 & why >= 0 & why <=10)
  
  # responses, shifting coordinates to account for 0,0
  responses = rep(NA,3)
  for (j in 1:length(possible.id)){
    responses[possible.id[j]] = z[ex[possible.id[j]]+1,why[possible.id[j]]+1]
  }

  # what is maximum response
  new.id = which.max(responses)

  # store values
  steps[i] = steps[i-1] + 1
  X[i] = ex[new.id]
  Y[i] = why[new.id]
  Z[i] = responses[new.id]
  
  if (responses[new.id] == max(z)){
    break
  }
  
  if (Z[i-1] >= responses[new.id]){
    if (optimize == "factorA"){
      optimize = "factorB"
    } else {optimize = "factorA"
      }
  }
  
}

# paths:3D
scatter3D(x = X[1], y = Y[1], z = Z[1],
          xlim = c(0,10), xlab = "X",
          ylim = c(0,10), ylab = "Y",
          zlim = c(0,max(z)), zlab = "R",
          pch = 19, col = 1, cex = 2,
          ticktype = "detailed", type = "h")
lines3D(x = X[1: sum(!is.na(steps))], 
        y = Y[1: sum(!is.na(steps))], 
        z = Z[1: sum(!is.na(steps))],
        lwd = 3, type = "b", col = 1, 
        add = TRUE)
points3D(x = X[1: sum(!is.na(steps))], 
         y = Y[1: sum(!is.na(steps))], 
         z = Z[1: sum(!is.na(steps))],
         col = 1, lty = 2, type = "h",
         add = TRUE)

# paths:2D
scatter2D(x = X[1], y = Y[1], 
          xlim = c(0,10), xlab = "factor A", 
          ylim = c(0,10), ylab = "factor B",
          pch = 19, cex = 3, col = 1)
grid()
lines2D(x = X[1: sum(!is.na(steps))],
        y = Y[1: sum(!is.na(steps))],
        type = "b",
        lwd = 3, col = 1, add = TRUE)

# surface:2D
contour2D(x = x, y = y, z = z,
          xlab = "factor A", ylab = "factor B",
          xlim = c(0,10), ylim = c(0,10), 
          lwd = 2, labcex = 1.5)
scatter2D(x = X[1], y = Y[1], 
          add = TRUE, pch = 19, cex = 3, col = 1)
lines2D(x = X[1: sum(!is.na(steps))],
        y = Y[1: sum(!is.na(steps))], 
        lwd = 3, col = 1, 
        type = "b", add = TRUE)
grid()

# surface:3D
nrz = nrow(z)
ncz = ncol(z)
nbcol = 100
col.pal<-colorRampPalette(c(5,3))
color<-col.pal(nbcol)
zfacet = (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4
facetcol = cut(zfacet,nbcol)
z_range = range(z, na.rm = TRUE)
if ((z_range[2] - z_range[1]) != 0) {
  p = persp(x,y,z,scale = TRUE, shade = NA, col = color[facetcol],
            theta = 45, phi = 35, ticktype = "detailed",
            xlab = "factor A", ylab = "factor B", 
            zlab = "response", xlim = c(0,10), 
            ylim = c(0,10), zlim = c(0,max(z)))
  points(trans3d(x = X[1], y = Y[1], z = Z[1], pmat = p), 
         col = 1, pch = 19, cex = 3, type = "p")
  lines(trans3d(x = X[1: sum(!is.na(steps))], 
                y = Y[1: sum(!is.na(steps))], 
                z = Z[1: sum(!is.na(steps))], pmat = p), 
        col = 1, lwd = 3, type = "b")
}

