# model three: ridge
x = seq(1,10,1)
y = seq(1,10,1)
model3 = function(a,b){3.264 + 1.537*a + 0.5664*b - 0.1505*a^2 - 0.02734*b^2 - 0.05785*a*b}
z = outer(x,y,model3)

# create vectors for peroxide, sulfuric acid, absorbance values, and steps
X = rep(NA,100)
Y = rep(NA,100)
Z = rep(NA,100)
steps = rep(NA,100)

# initial setup
steps[1] = 1
X[1] = 1
Y[1] = 9
Z[1] = z[Y[1],X[1]]

for (i in 2:100){
  if (X[i-1] < 10 & is.na(X[i-1]) == FALSE){
    if(z[Y[i-1],X[i-1]+1] > Z[i-1]){
      Z[i] = z[Y[i-1],X[i-1]+1]
      X[i] = X[i-1]+1
      Y[i] = Y[i-1]
      steps[i] = steps[i-1]+1
      next
    }}
  if (X[i-1] > 1 & is.na(X[i-1]) == FALSE) {
    if (z[Y[i-1],X[i-1]-1] > Z[i-1]){
      Z[i] = z[Y[i-1],X[i-1]-1]
      X[i] = X[i-1]-1
      Y[i] = Y[i-1]
      steps[i] = steps[i-1]+1
      next
    }}
  if (Y[i-1] < 10 & is.na(Y[i-1]) == FALSE){
    if (z[Y[i-1]+1,X[i-1]] > Z[i-1]){
      Z[i] = z[Y[i-1]+1,X[i-1]]
      X[i] = X[i-1]
      Y[i] = Y[i-1]+1
      steps[i] = steps[i-1]+1
      next
    }}
  if (Y[i-1] > 1 & is.na(Y[i-1]) == FALSE) {
    if (z[Y[i-1]-1,X[i-1]] > Z[i-1]){
      Z[i] = z[Y[i-1]-1,X[i-1]]
      X[i] = X[i-1]
      Y[i] = Y[i-1]-1
      steps[i] = steps[i-1]+1
      next
    }} else {break} 
}

df = data.frame(steps,X,Y,Z)  

scatter3D(x = X[1: sum(!is.na(steps))], 
          y = Y[1: sum(!is.na(steps))], 
          z = Z[1: sum(!is.na(steps))], 
          pch = 19, type = "h", col = gg.col(30),
          xlim = c(1,10), ylim = c(1,10), zlim = c(0,10),
          theta = 35, phi = 35, ticktype = "detailed")

contour2D(x = x, y = y, z = z,
          xlab = "values of x", ylab = "values of y",
          xlim = c(0,10), ylim = c(0,10))
scatter2D(x = Y[1],
          y = X[1], 
          add = TRUE, pch = 19)
scatter2D(x = X[sum(!is.na(steps))],
          y = Y[sum(!is.na(steps))], 
          pch = 18, add = TRUE)
lines2D(x = Y[1: sum(!is.na(steps))],
        y = X[1: sum(!is.na(steps))], 
        lwd = 3, add = TRUE)
persp3D(x = x,y = y, z = z)
scatter3D(y = X[1: sum(!is.na(steps))], 
          x = Y[1: sum(!is.na(steps))], 
          z = Z[1: sum(!is.na(steps))],
          lwd = 3, type = "l", add = TRUE)
