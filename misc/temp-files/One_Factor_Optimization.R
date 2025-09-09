# scripts to create figures SM14.1,2,3,4

# load plot3D
library(plot3D)

# define quartz window
quartz(width = 6, height = 6)

# define the response surface for problem 1a
a = seq(0,10,0.25)
b = seq(0,10,0.25)
model = function(a,b){
	1.68+0.24*a+0.56*b-0.04*a^2-0.04*b^2
}
r = outer(a,b,model)

# define the response surface for problem 1b
a = seq(0,10,0.25)
b = seq(0,10,0.25)
model = function(a,b){
	4.0-0.4*a+0.08*a*b
}
r = outer(a,b,model)

# define the response surface for problem 1c
a = seq(0,10,0.25)
b = seq(0,10,0.25)
model = function(a,b){
	3.264+1.537*a+0.5664*b-0.1505*a^2-0.02734*b^2-0.05785*a*b
}
r = outer(a,b,model)

# enter individual values for probem 1a
A=c(0,1,2,3,4,3,3,3,3,3,3,3,3,3,3,4,3,2,3)
B=c(0,0,0,0,0,0,1,2,3,4,5,6,7,8,7,7,7,7,7)
R=c(1.68,1.88,2.00,2.04,2.00,2.04,2.56,3.00,3.36,3.64,3.84,3.96,4.00,3.96,4.00,3.96,4.00,3.96,4.00)

# enter individual values for probem 1b
A=c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,2,3,4,5,6,7,8,9,10,10,10,10,10)
B=c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,6,6,6,6,6,7,8,9,10)
R=c(4.00,3.60,4.00,4.00,3.68,4.00,4.00,3.76,4.00,4.00,3.84,4.00,4.00,3.92,4.00,4.00,4.00,4.00,4.00,4.08,4.16,4.24,4.32,4.40,4.48,4.56,4.64,4.72,4.80,5.60,6.40,7.20,8.00)

# enter individual values for probem 1c
A=c(0,1,2,3,4,5,6,5,5,5,5,5,5,5,5,6,5,4,3,4,4,4,4,3)
B=c(0,0,0,0,0,0,0,0,1,2,3,4,5,6,5,5,5,5,5,5,6,7,6,6)
R=c(3.267,4.651,5.736,6.521,7.004,7.187,7.068,7.187,7.436,7.631,7.772,7.858,7.889,7.865,7.889,7.481,7.889,7.996,7.801,7.996,8.030,8.009,8.030,7.893)

#enter individual values for problem 2
A=c(0,1,0.5,1.5,2,2.5,3,3.5,4,4.5,4,5,4.5,5.5,5,4,4.5,3.5,4,5,4.5,3.5,3,3.5,4.5)
B=c(0,0,0.87,0.87,0,0.87,0,0.87,0,0.87,1.74,1.74,2.61,2.61,3.48,3.48,4.35,4.35,5.22,5.22,6.09,6.09,5.22,4.35,4.35)
R=c(3.264,4.651,4.442,5.627,5.736,6.512,6.521,7.096,7.004,7.378,7.504,7.586,7.745,7.626,7.820,7.839,7.947,7.866,8.008,7.888,7.983,8.002,7.826,7.866,7.947)

#plot 3D Surface
persp3D(a, b, r, xlab = "values of a", ylab = "values of b", zlab = "response", border = "black", lwd = 0.25, colkey=F)

#plot 3D Contour + 3d Scatterplot
contour3D(x=a,y=b,z=0,colvar=r,zlim=c(0,1.1*max(r)),xlab = "values of a", ylab = "values of b", zlab = "response")
scatter3D(x=A,y=B,z=R,type="b",add=T,pch=19,col="black")
scatter3D(x=A,y=B,z=R,type="h",add=T,col="black")

#plot 2D Contour + 2d Scatterplot
contour2D(r,x=a,y=b,xlab="values of a", ylab="values of b",colkey=F)
lines2D(x=A,y=B,col="black",lwd=1.5,add=T)

#simplex for problem 2
#index is vector that gives all vertices to plot
#df is data from with the unique vertices
#for loop creates new ABR with all vertices to plot
df=data.frame(A,B,R)
index=c(1,2,3,1,2,3,4,3,2,4,5,2,4,5,6,4,5,6,7,5,6,7,8,6,7,8,9,7,8,9,10,9,8,10,11,8,10,11,12,10,11,12,13,11,12,13,14,12,13,14,15,14,13,15,16,13,15,16,17,15,16,17,18,16,17,18,19,18,17,19,20,17,19,20,21,20,19,21,22,21,19,22,23,22,19,23,24,23,19,24,25)
for (i in 1:length(index)) A[i]=df$A[index[i]]
for (i in 1:length(index)) B[i]=df$B[index[i]]
for (i in 1:length(index)) R[i]=df$R[index[i]]

