# create grid of response surface values
# x-axis is drops of peroxide and y-axis is drops of sulfuric acid
# z-axis is absorbance
peroxide = c(20,20,10,10,15,22,8,15,15,15,15,15)
sulfuric = c(20,10,20,10,15,15,15,22,8,15,15,15)
absorbance = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000

lm.r = lm(absorbance ~ peroxide * sulfuric + I(peroxide^2) + I(sulfuric^2))

per_scale = seq(1,30,1)
sulf_scale = seq(1,30,1)

model = function(a,b){
  lm.r$coefficients[1] + lm.r$coefficients[2]*a + lm.r$coefficients[3]*b + lm.r$coefficients[4]*a^2 + lm.r$coefficients[5]*b^2 + lm.r$coefficients[6]*a*b
}
abs_values = outer(per_scale,sulf_scale,model)

palette("Okabe-Ito")

# create vectors for peroxide, sulfuric acid, absorbance values, and steps
per = rep(NA,100)
sulf = rep(NA,100)
abs = rep(NA,100)
steps = rep(NA,100)

# initial setup
steps[1] = 1
per[1] = 25
sulf[1] = 15
abs[1] = abs_values[sulf[1],per[1]]

for (i in 2:100){
  if (per[i-1] < 30 & is.na(per[i-1]) == FALSE){
    if(abs_values[sulf[i-1],per[i-1]+1] > abs[i-1]){
      abs[i] = abs_values[sulf[i-1],per[i-1]+1]
      per[i] = per[i-1]+1
      sulf[i] = sulf[i-1]
      steps[i] = steps[i-1]+1
      next
    }}
  if (per[i-1] > 1 & is.na(per[i-1]) == FALSE) {
    if (abs_values[sulf[i-1],per[i-1]-1] > abs[i-1]){
      abs[i] = abs_values[sulf[i-1],per[i-1]-1]
      per[i] = per[i-1]-1
      sulf[i] = sulf[i-1]
      steps[i] = steps[i-1]+1
      next
    }}
  if (sulf[i-1] < 30 & is.na(sulf[i-1]) == FALSE){
    if (abs_values[sulf[i-1]+1,per[i-1]] > abs[i-1]){
      abs[i] = abs_values[sulf[i-1]+1,per[i-1]]
      per[i] = per[i-1]
      sulf[i] = sulf[i-1]+1
      steps[i] = steps[i-1]+1
      next
    }}
  if (sulf[i-1] > 1 & is.na(sulf[i-1]) == FALSE) {
    if (abs_values[sulf[i-1]-1,per[i-1]] > abs[i-1]){
      abs[i] = abs_values[sulf[i-1]-1,per[i-1]]
      per[i] = per[i-1]
      sulf[i] = sulf[i-1]-1
      steps[i] = steps[i-1]+1
      next
    }} else {break} 
  }

df = data.frame(steps,per,sulf,abs)  

scatter3D(x = per[1: sum(!is.na(steps))], 
          y = sulf[1: sum(!is.na(steps))], 
          z = abs[1: sum(!is.na(steps))], 
          pch = 19, type = "h", col = gg.col(30),
          xlim = c(1,30), ylim = c(1,30), zlim = c(0,1),
          theta = 35, phi = 35, ticktype = "detailed")

contour2D(x = per_scale,y = sulf_scale, z = abs_values)
lines2D(x = per[1: sum(!is.na(steps))], 
        y = sulf[1: sum(!is.na(steps))], lwd = 3, add = TRUE)
persp3D(x = per_scale,y = sulf_scale, z = abs_values, zlim = c(0,1))

        
