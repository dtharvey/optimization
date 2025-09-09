# server for response surface optimization application

# load necessary packages
library(shiny)
library(shinythemes)
library(plot3D)

# set color scheme
palette("Okabe-Ito")

# functions to create response surface values
model1 = function(a,b){1.68 + 0.24*a + 0.56*b - 0.04*a^2 - 0.04*b^2}
model2 = function(a,b){4.0 - 0.4*a + 0.08*a*b}
model3 = function(a,b){3.264 + 1.537*a + 0.5664*b - 0.1505*a^2 - 0.02734*b^2 - 0.05785*a*b}
model4 = function(a,b){4 + 0.8*a - 0.8*b - 0.08*a^2 + 0.08*b^2}

shinyServer(function(input, output, session){
 
# code to plot sequential optimization plots 
  output$act1_plot = renderPlot({
    # set up grid of x,y values
    x = seq(0,10,1)
    y = seq(0,10,1)
    
    # create model and calculate values on grid
    if (input$eqn1 == "example 1"){
      z = outer(x,y,model1)
    } else if (input$eqn1 == "example 2"){
      z = outer(x,y,model2)
    } else if (input$eqn1 == "example 3"){
      z = outer(x,y,model3)
    } else {
      z = outer(x,y,model4)
    }
    
    # create vectors to store points
    X = rep(NA,100)
    Y = rep(NA,100)
    Z = rep(NA,100)
    steps = rep(NA,100)
    
    # initial set-up
    steps[1] = 1
    X[1] = input$a_initial1
    Y[1] = input$b_initial1
    Z[1] = z[X[1]+1,Y[1]+1]
    
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
      
      # which of the possible new coordinates are in bounds
      possible.id = which(ex >= 0 & ex <= 10 & why >= 0 & why <=10)
      
      # calculate responses, shifting coordinates to account for 0,0
      responses = rep(NA,3)
      # responses = rep(NA,8)
      for (j in 1:length(possible.id)){
        responses[possible.id[j]] = z[ex[possible.id[j]]+1,
                                      why[possible.id[j]]+1]
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
    
    if (input$display1 == "path only" & input$dim1 == "2D"){
      par(mar = c(5,4,1,1))
      scatter2D(x = X[1], y = Y[1], 
                xlim = c(0,10), xlab = "factor A", 
                ylim = c(0,10), ylab = "factor B",
                pch = 19, cex = 3, col = 1)
      grid()
      lines2D(x = X[1: sum(!is.na(steps))],
              y = Y[1: sum(!is.na(steps))],
              type = "b",
              lwd = 3, col = 1, add = TRUE)
    }
    if (input$display1 == "path only" & input$dim1 == "3D"){
      par(mar = c(5,4,1,1))
      scatter3D(x = X[1], y = Y[1], z = Z[1],
                xlim = c(0,10), xlab = "factor A",
                ylim = c(0,10), ylab = "factor B",
                zlim = c(0,max(z)), zlab = "response",
                pch = 19, col = 1, cex = 2,
                ticktype = "detailed", type = "h",
                theta = input$act1_turn, phi = input$act1_tilt)
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
    }
    
    if (input$display1 == "path & surface" & input$dim1 == "2D"){
      par(mar = c(5,4,1,1))
      contour2D(x = x, y = y, z = z,
                xlab = "factor A", ylab = "factor B",
                xlim = c(0,10), ylim = c(0,10), 
                lwd = 2, labcex = 1.5, colkey = FALSE, col = 1:8)
      scatter2D(x = X[1], y = Y[1], 
                add = TRUE, pch = 19, cex = 3, col = 1)
      lines2D(x = X[1: sum(!is.na(steps))],
              y = Y[1: sum(!is.na(steps))], 
              lwd = 3, col = 1, 
              type = "b", add = TRUE)
      grid()
    }
    
    if (input$display1 == "path & surface" & input$dim1 == "3D"){
      par(mar = c(5,4,1,1))
      nrz = nrow(z)
      ncz = ncol(z)
      nbcol = 100
      col.pal<-colorRampPalette(c(5,3))
      color<-col.pal(nbcol)
      zfacet = (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4
      facetcol = cut(zfacet,nbcol)
      z_range = range(z, na.rm = TRUE)
      if ((z_range[2] - z_range[1]) != 0) {
        p = persp(x,y,z,scale = TRUE, shade = NA, 
                  col = color[facetcol],
                  ticktype = "detailed",
                  xlab = "factor A", ylab = "factor B", 
                  zlab = "response", xlim = c(0,10), 
                  ylim = c(0,10), zlim = c(0,max(z)),
                  theta = input$act1_turn, phi = input$act1_tilt)
        points(trans3d(x = X[1], y = Y[1], z = Z[1], pmat = p), 
               col = 1, pch = 19, cex = 3, type = "p")
        lines(trans3d(x = X[1: sum(!is.na(steps))], 
                      y = Y[1: sum(!is.na(steps))], 
                      z = Z[1: sum(!is.na(steps))], pmat = p), 
              col = 1, lwd = 3, type = "b")
      }
    }
  })
  
# code for displaying steepest  
  output$act2_plot = renderPlot({
    # set up grid of x,y values
    x = seq(0,10,1)
    y = seq(0,10,1)
    
    # create model and calculate values on grid
    if (input$eqn2 == "example 1"){
      z = outer(x,y,model1)
    } else if (input$eqn2 == "example 2"){
      z = outer(x,y,model2)
    } else if (input$eqn2 == "example 3"){
      z = outer(x,y,model3)
    } else {
      z = outer(x,y,model4)
    }
    
    # create vectors to store points
    X = rep(NA,100)
    Y = rep(NA,100)
    Z = rep(NA,100)
    steps = rep(NA,100)
    
    # initial set-up
    steps[1] = 1
    X[1] = input$a_initial2
    Y[1] = input$b_initial2
    Z[1] = z[X[1]+1,Y[1]+1]

    # loop to evaluate movement
    for (i in 2:100){
      
      # x coordinates and y coordinates for four possible movements
      # ex = c(X[i-1]+1, X[i-1]-1, X[i-1], X[i-1])
      # why = c(Y[i-1], Y[i-1], Y[i-1]+1, Y[i-1]-1)
      
      ex = c(X[i-1]+1, X[i-1]-1, X[i-1], X[i-1],
             X[i-1]+1, X[i-1]+1, X[i-1]-1, X[i-1]-1)
      why = c(Y[i-1], Y[i-1], Y[i-1]+1, Y[i-1]-1,
              Y[i-1]+1, Y[i-1]-1, Y[i-1]-1, Y[i-1]+1)
      
      # which of the possible new coordinates are in bounds
      possible.id = which(ex >= 0 & ex <= 10 & why >= 0 & why <=10)
      
      # calculate responses, shifting coordinates to account for 0,0
      # responses = rep(NA,4)
      responses = rep(NA,8)
      for (j in 1:length(possible.id)){
        responses[possible.id[j]] = z[ex[possible.id[j]]+1,
                                      why[possible.id[j]]+1]
      }
      
      # what is maximum response
      new.id = which.max(responses)
      
      # break out of loop if response decreases
      if (Z[i-1] > responses[new.id]){
        break
      }
      
      # store values
      steps[i] = steps[i-1] + 1
      X[i] = ex[new.id]
      Y[i] = why[new.id]
      Z[i] = responses[new.id]
      
    }
    
    if (input$display2 == "path only" & input$dim2 == "2D"){
      par(mar = c(5,4,1,1))
      scatter2D(x = X[1], y = Y[1], 
                xlim = c(0,10), xlab = "factor A", 
                ylim = c(0,10), ylab = "factor B",
                pch = 19, cex = 3, col = 1)
      grid()
      lines2D(x = X[1: sum(!is.na(steps))],
              y = Y[1: sum(!is.na(steps))],
              type = "b",
              lwd = 3, col = 1, add = TRUE)
    }
    if (input$display2 == "path only" & input$dim2 == "3D"){
      par(mar = c(5,4,1,1))
      scatter3D(x = X[1], y = Y[1], z = Z[1],
                xlim = c(0,10), xlab = "factor A",
                ylim = c(0,10), ylab = "factor B",
                zlim = c(0,max(z)), zlab = "response",
                pch = 19, col = 1, cex = 2,
                ticktype = "detailed", type = "h",
                theta = input$act2_turn, phi = input$act2_tilt)
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
    }
    
    if (input$display2 == "path & surface" & input$dim2 == "2D"){
      par(mar = c(5,4,1,1))
      contour2D(x = x, y = y, z = z,
                xlab = "factor A", ylab = "factor B",
                xlim = c(0,10), ylim = c(0,10), 
                lwd = 2, labcex = 1.5, colkey = FALSE, col = 1:8)
      scatter2D(x = X[1], y = Y[1], 
                add = TRUE, pch = 19, cex = 3, col = 1)
      lines2D(x = X[1: sum(!is.na(steps))],
              y = Y[1: sum(!is.na(steps))], 
              lwd = 3, col = 1, 
              type = "b", add = TRUE)
      grid()
    }
    
    if (input$display2 == "path & surface" & input$dim2 == "3D"){
      par(mar = c(5,4,1,1))
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
                  ticktype = "detailed",
                  xlab = "factor A", ylab = "factor B", 
                  zlab = "response", xlim = c(0,10), 
                  ylim = c(0,10), zlim = c(0,max(z)),
                  theta = input$act2_turn, phi = input$act2_tilt)
        points(trans3d(x = X[1], y = Y[1], z = Z[1], pmat = p), 
               col = 1, pch = 19, cex = 3, type = "p")
        lines(trans3d(x = X[1: sum(!is.na(steps))], 
                      y = Y[1: sum(!is.na(steps))], 
                      z = Z[1: sum(!is.na(steps))], pmat = p), 
              col = 1, lwd = 3, type = "b")
      }
    }
  })
  
  
}) 
