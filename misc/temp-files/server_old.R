# original server (this works)

# load necessary packages
library(shiny)
library(shinythemes)
library(plot3D)

# set color scheme
palette("Okabe-Ito")

shinyServer(function(input, output, session){

  output$act1_plot = renderPlot({
    x = seq(1,10,1)
    y = seq(1,10,1)
    if (input$eqn == "example 1"){
      model1 = function(a,b){1.68 + 0.24*a + 0.56*b - 0.04*a^2 - 0.04*b^2}
      z = outer(x,y,model1)
    } else if (input$eqn == "example 2"){
      model2 = function(a,b){4.0 - 0.4*a + 0.08*a*b}
      z = outer(x,y,model2)
    } else if (input$eqn == "example 3"){
      model3 = function(a,b){3.264 + 1.537*a + 0.5664*b - 0.1505*a^2 - 0.02734*b^2 - 0.05785*a*b}
      z = outer(x,y,model3)
    } else {
      model4 = function(a,b){4 + 0.8*a - 0.8*b - 0.08*a^2 + 0.08*b^2}
      z = outer(x,y,model4)
    }
    X = rep(NA,100)
    Y = rep(NA,100)
    Z = rep(NA,100)
    steps = rep(NA,100)
    steps[1] = 1
    X[1] = input$b_initial
    Y[1] = input$a_initial
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
    
    if (input$display == "paths:2D"){
      scatter2D(x = Y[1], y = X[1], 
                xlim = c(1,10), xlab = "factor A", 
                ylim = c(1,10), ylab = "factor B",
                pch = 19, cex = 3, col = 1)
      grid()
      lines2D(x = Y[1: sum(!is.na(steps))],
              y = X[1: sum(!is.na(steps))],
              type = "b",
              lwd = 3, col = 1, add = TRUE)
    }
    if (input$display == "paths:3D"){
      scatter3D(x = Y[1], y = X[1], z = Z[1], 
                pch = 19, col = 1, cex = 3,
                xlim = c(0,10), xlab = "factor A",
                ylim = c(0,10), ylab = "factor B",
                zlim = c(0,max(z)), zlab = "response",
                ticktype = "detailed")
      lines3D(x = Y[1: sum(!is.na(steps))], 
                y = X[1: sum(!is.na(steps))], 
                z = Z[1: sum(!is.na(steps))],
                lwd = 3, type = "b", col = 1, 
                add = TRUE)
      points3D(x = Y[1: sum(!is.na(steps))], 
               y = X[1: sum(!is.na(steps))], 
               z = Z[1: sum(!is.na(steps))],
               col = 1, lty = 2, type = "h",
               add = TRUE)
    }
    
    if (input$display == "surface:2D"){
      contour2D(x = x, y = y, z = z,
                xlab = "factor A", ylab = "factor B",
                xlim = c(0,10), ylim = c(0,10), 
                lwd = 2, labcex = 1.5)
      scatter2D(x = Y[1], y = X[1], 
                add = TRUE, pch = 19, cex = 3, col = 1)
      lines2D(x = Y[1: sum(!is.na(steps))],
              y = X[1: sum(!is.na(steps))], 
              lwd = 3, col = 1, 
              type = "b", add = TRUE)
      grid()
    }
    
    if (input$display == "surface:3D"){
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
              zlab = "response")
      points(trans3d(x = Y[1], y = X[1], z = Z[1], pmat = p), 
             col = 1, pch = 19, cex = 3, type = "p")
      lines(trans3d(x = Y[1: sum(!is.na(steps))], 
                    y = X[1: sum(!is.na(steps))], 
                    z = Z[1: sum(!is.na(steps))], pmat = p), 
             col = 1, lwd = 3, type = "b")
      }
    }
  })
  
  
}) 
