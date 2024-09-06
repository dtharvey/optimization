# read in packages
library(fields)

# set color scheme
palette("Okabe-Ito")

# central composite design data
peroxide = c(20,20,10,10,15,22,8,15,15,15,15,15)
sulfuric = c(20,10,20,10,15,15,15,22,8,15,15,15)
absorbance = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000

# build linear model
lm.r = lm(absorbance ~ peroxide * sulfuric + I(peroxide^2) + I(sulfuric^2))

# use model to build response surface
xmod = seq(0,30,1)
ymod = seq(0,30,1)
ccmod = function(a,b){
  lm.r$coefficients[1] + lm.r$coefficients[2]*a + lm.r$coefficients[3]*b + lm.r$coefficients[4]*a^2 + lm.r$coefficients[5]*b^2 + lm.r$coefficients[6]*a*b
}
zmod = outer(xmod,ymod,ccmod)
nrz = nrow(zmod)
ncz = ncol(zmod)
nbcol = 100
col.pal<-colorRampPalette(c(5,3))
color<-col.pal(nbcol)
zfacet = (zmod[-1, -1] + zmod[-1, -ncz] + zmod[-nrz, -1] + zmod[-nrz, -ncz])/4
facetcol = cut(zfacet,nbcol)

# create perspective plot
p = persp(xmod, ymod, zmod, scale = TRUE, shade = NA, 
          col = color[facetcol],
          theta = 45, phi = 35, 
          ticktype = "detailed",
          xlab = "drops of peroxide (x)", 
          ylab = "drops of sulfuric acid (y)", 
          border = NULL,
          zlab = "response (z)", zlim = c(0,1))

# add points to perspective plot
points(trans3d(x = peroxide, y = sulfuric, absorbance, pmat = p), 
       col = 1, pch = 19, type = "p")

# add color scale
imagePlot(legend.only=T, zlim=range(zfacet), col = color)

# create contour plot and add points
contour(x = xmod, y = ymod, z = zmod,
        levels = seq(0,1,0.05), asp = 1,
        xlim = c(0,30), xlab = "drops of peroxide",
        ylim = c(0,30), ylab = "drops of sulfuric acid",
        lwd = 4, col = seq(1,9,1), labcex = 1.25)
points(x = sulfuric, y = peroxide, pch = 19, col = "black", cex = 2)
grid()

b0 = lm.r$coefficients[1] 
b1 = lm.r$coefficients[2] 
b2 = lm.r$coefficients[3] 
b11 = lm.r$coefficients[4]
b22 = lm.r$coefficients[5] 
b12 = lm.r$coefficients[6]

peroxide_initial = 15
sulfuric_initial = 15
absorbance_initial = b0 + b1*peroxide_initial + b2*sulfuric_initial + b11*peroxide_initial + b22*sulfuric_initial + b12*peroxide_initial*sulfuric_initial

peroxide_new = peroxide_initial + 1
sulfuric_new = sulfuric_initial + 0
absorbance_new = b0 + b1*peroxide_new + b2*sulfuric_new + b11*peroxide_new + b22*sulfuric_new + b12*peroxide_new*sulfuric_new


step = 1
per = 15
sul = 15
id = which.max(zmod[sul+1,])
abs = zmod[sul+1,id]
