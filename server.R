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
model4 = function(a,b){4 + 0.7*a - 0.9*b - 0.08*a^2 + 0.08*b^2}

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
      ex = c(X[i-1]+1, X[i-1]-1, X[i-1], X[i-1],
             X[i-1]+1, X[i-1]+1, X[i-1]-1, X[i-1]-1)
      why = c(Y[i-1], Y[i-1], Y[i-1]+1, Y[i-1]-1,
              Y[i-1]+1, Y[i-1]-1, Y[i-1]-1, Y[i-1]+1)
      
      # which of the possible new coordinates are in bounds
      possible.id = which(ex >= 0 & ex <= 10 & why >= 0 & why <=10)
      
      # calculate responses, shifting coordinates to account for 0,0
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
    
    if (input$display2 == "contours (2D)"){
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
    
    if (input$display2 == "surface (3D)"){
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
  }) # close plot for steepest ascent
  
  # code for fixed size simplex optimization
  output$act3_plot = renderPlot({
    
    # adjust margins for plot
    par(mar = c(5,4,1,1))
    
    # set up grid of x,y values
    x = seq(0,10,1)
    y = seq(0,10,1)
    
    # create model and calculate values on grid
    if (input$eqn3 == "example 1"){
      z = outer(x,y,model1)
    } else if (input$eqn3 == "example 2"){
      z = outer(x,y,model2)
    } else if (input$eqn3 == "example 3"){
      z = outer(x,y,model3)
    } else {
      z = outer(x,y,model4)
    }
    
    # define the location of first point and the step-sizes
    if (input$sim_orient == 1){
      vx = c(input$va, input$va - 0.5*input$stepsize, input$va + 0.5*input$stepsize)
      vy = c(input$vb, input$vb + 0.866*input$stepsize, input$vb + 0.886*input$stepsize)
    } else if (input$sim_orient == 2){
      vx = c(input$va, input$va + 0.5*input$stepsize, input$va + 1*input$stepsize)
      vy = c(input$vb, input$vb + 0.866*input$stepsize, input$vb)
    } else if (input$sim_orient == 3){
      vx = c(input$va, input$va + 1*input$stepsize, input$va + 0.5*input$stepsize)
      vy = c(input$vb, input$vb, input$vb - 0.866*input$stepsize)
    } else if (input$sim_orient == 4){
      vx = c(input$va, input$va + 0.5*input$stepsize, input$va - 0.5*input$stepsize)
      vy = c(input$vb, input$vb - 0.866*input$stepsize, input$vb - 0.866*input$stepsize)
    } else if (input$sim_orient == 5){
      vx = c(input$va, input$va - 0.5*input$stepsize, input$va - 1*input$stepsize)
      vy = c(input$vb, input$vb - 0.866*input$stepsize, input$vb)
    } else {
      vx = c(input$va, input$va - 1*input$stepsize, input$va - 0.5*input$stepsize)
      vy = c(input$vb, input$vb, input$vb + 0.866*input$stepsize)
    }
    
    # calculate responses for initial simplex
    if (input$eqn3 == "example 1"){
    vz = model1(vx,vy)
  } else if(input$eqn3 == "example 2"){
    vz = model2(vx,vy)
  } else if (input$eqn3 == "example 3"){
    vz = model3(vx,vy)
  } else {
    vz = model4(vx,vy)
  }
    
    # sort vertices from worst-to-best
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    
    max_response = vz[3]
    max_x = vx[3]
    max_y = vy[3]
    
    # plot initial simplex
    plot(x = c(vx,vx[1]), y = c(vy,vy[1]),
         xlim = c(0,10), ylim = c(0,10), 
         xlab = "factor A", ylab = "factor B",
         type = "b", pch = 19, cex = 2, col = 1, lwd = 3)
    grid()
    
    # loop for remaining simplexes
    for (i in 1:30){
      
      lines(x = c(vx,vx[1]), y = c(vy,vy[1]),type = "b", 
            col = 1, lwd = 2)
      
      # save current next best response for testing
      next_best = vz[2]
      
      # determine values for new vertex based on the worst vertex in [1]
      vx_temp = vx[2] + vx[3] - vx[1]
      vy_temp = vy[2] + vy[3] - vy[1]
      if (input$eqn3 == "example 1"){
        vz_temp = model1(vx_temp,vy_temp)
      } else if(input$eqn3 == "example 2"){
        vz_temp = model2(vx_temp,vy_temp)
      } else if (input$eqn3 == "example 3"){
        vz_temp = model3(vx_temp,vy_temp)
      } else {
        vz_temp = model4(vx_temp,vy_temp)
      }
      
      # vz_temp = response(vx_temp, vy_temp)
      
      # test new vertex against worst and boundary conditions
      # if it fails go to next
      # else it passes and defines new set of vertices
      if(vx_temp < 0 | vy_temp < 0 | vx_temp > 10 | vy_temp >10 | vz_temp < next_best){
        vz[2] = -1000
        vx = vx[order(vz)]
        vy = vy[order(vz)]
        vz = vz[order(vz)]
        next
      } else {
        vx[1] = vx_temp
        vy[1] = vy_temp
        vz[1] = vz_temp
        vx = vx[order(vz)]
        vy = vy[order(vz)]
        vz = vz[order(vz)]
        # lines(x = c(vx,vx[1]), y = c(vy,vy[1]),
        #       type = "b", col = 6, lwd = 2)
        if(vz[3] > max_response){
          max_response = vz[3]
          max_x = vx[3]
          max_y = vy[3]}
      }
    }
    
    # mark end point 
    # points(x = max_x, y = max_y,
    #        pch = 15, col = 6, cex = 1.5, lwd = 2)
    
    # add contour plot
    # z = outer(x,y,response)
    
    contour2D(z,x,y, lwd = 2, colkey = FALSE, add = TRUE,
              labcex = 1.5, col = 1:8)
    
  })
  
  # code to plot variable sized simplex
  output$act4_plot = renderPlot({
    # adjust margins for plot
    par(mar = c(5,4,1,1))
    
    # set up grid of x,y values
    x = seq(0,10,1)
    y = seq(0,10,1)
    
    # create model and calculate values on grid
    if (input$eqn4 == "example 1"){
      z = outer(x,y,model1)
    } else if (input$eqn4 == "example 2"){
      z = outer(x,y,model2)
    } else if (input$eqn4 == "example 3"){
      z = outer(x,y,model3)
    } else {
      z = outer(x,y,model4)
    }
    
    # define the location of first point and the step-sizes
    if (input$sim_orient4 == 1){
      vx = c(input$va4, input$va4 - 0.5, input$va4 + 0.5)
      vy = c(input$vb4, input$vb4 + 0.866, input$vb4 + 0.886)
    } else if (input$sim_orient4 == 2){
      vx = c(input$va4, input$va4 + 0.5, input$va4 + 1)
      vy = c(input$vb4, input$vb4 + 0.866, input$vb4)
    } else if (input$sim_orient4 == 3){
      vx = c(input$va4, input$va4 + 1, input$va4 + 0.5)
      vy = c(input$vb4, input$vb4, input$vb4 - 0.866)
    } else if (input$sim_orient4 == 4){
      vx = c(input$va4, input$va4 + 0.5, input$va4 - 0.5)
      vy = c(input$vb4, input$vb4 - 0.866, input$vb4 - 0.866)
    } else if (input$sim_orient4 == 5){
      vx = c(input$va4, input$va4 - 0.5, input$va4 - 1)
      vy = c(input$vb4, input$vb4 - 0.866, input$vb4)
    } else {
      vx = c(input$va4, input$va4 - 1, input$va4 - 0.5)
      vy = c(input$vb4, input$vb4, input$vb4 + 0.866)
    }
    
    # calculate responses for initial simplex
    if (input$eqn4 == "example 1"){
      vz = model1(vx,vy)
    } else if(input$eqn4 == "example 2"){
      vz = model2(vx,vy)
    } else if (input$eqn4 == "example 3"){
      vz = model3(vx,vy)
    } else {
      vz = model4(vx,vy)
    }
    
    # sort vertices from worst-to-best;
    # worst is in position 1, next is in position 2, best is in position 3
    vx = vx[order(vz)]
    vy = vy[order(vz)]
    vz = vz[order(vz)]
    
    # plot initial simplex
    plot(x = c(vx,vx[1]), y = c(vy,vy[1]),
         xlim = c(0,10), ylim = c(0,10), xlab = "Factor A", ylab = "Factor B",
         type = "b", pch = 19, cex = 2, col = 1, lwd = 3)
    grid()
    
    # loop for remaining simplexes
    for (i in 1:input$iterations4){
      # add new simplex to plot
      lines(x = c(vx,vx[1]), y = c(vy,vy[1]),type = "b", col = 1, lwd = 3)
      
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
      if (input$eqn4 == "example 1"){
        vz_r = model1(vx_r,vy_r)
      } else if(input$eqn4 == "example 2"){
        vz_r = model2(vx_r,vy_r)
      } else if (input$eqn4 == "example 3"){
        vz_r = model3(vx_r,vy_r)
      } else {
        vz_r = model4(vx_r,vy_r)
      }
      
      # calculate values for E
      vx_e = centroid_x + 2 * (centroid_x - vx[1])
      vy_e = centroid_y + 2 * (centroid_y - vy[1])
      if (input$eqn4 == "example 1"){
        vz_e = model1(vx_e,vy_e)
      } else if(input$eqn4 == "example 2"){
        vz_e = model2(vx_e,vy_e)
      } else if (input$eqn4 == "example 3"){
        vz_e = model3(vx_e,vy_e)
      } else {
        vz_e = model4(vx_e,vy_e)
      }
      
      # calculate values for CR
      vx_cr = centroid_x + 0.5 * (centroid_x - vx[1])
      vy_cr = centroid_y + 0.5 * (centroid_y - vy[1])
      if (input$eqn4 == "example 1"){
        vz_cr = model1(vx_cr,vy_cr)
      } else if(input$eqn4 == "example 2"){
        vz_cr = model2(vx_cr,vy_cr)
      } else if (input$eqn4 == "example 3"){
        vz_cr = model3(vx_cr,vy_cr)
      } else {
        vz_cr = model4(vx_cr,vy_cr)
      }
      
      # calculate values for CW
      vx_cw = centroid_x - 0.5 * (centroid_x - vx[1])
      vy_cw = centroid_y - 0.5 * (centroid_y - vy[1])
      if (input$eqn4 == "example 1"){
        vz_cw = model1(vx_cw,vy_cw)
      } else if(input$eqn4 == "example 2"){
        vz_cw = model2(vx_cw,vy_cw)
      } else if (input$eqn4 == "example 3"){
        vz_cw = model3(vx_cw,vy_cw)
      } else {
        vz_cw = model4(vx_cw,vy_cw)
      }
      
      # keep E if it is larger than R
      if (vz_e > vz[3] & vx_e > 0 & vy_e > 0 & vx_e < 10 & vy_e < 10){
        vx[1] = vx_e
        vy[1] = vy_e
        vz[1] = vz_e
        vx = vx[order(vz)]
        vy = vy[order(vz)]
        vz = vz[order(vz)]
        next
      } 
      
      if (vz_e < vz[3] & vz_e > vz[2] & vx_e > 0 & vy_e > 0 & vx_e < 10 & vy_e < 10) {
        vx[1] = vx_r
        vy[1] = vy_r
        vz[1] = vz_r
        vx = vx[order(vz)]
        vy = vy[order(vz)]
        vz = vz[order(vz)]
        next
      } 
      
      if (vz_r < vz[3] & vz_r > vz[2] & vx_e > 0 & vy_e > 0 & vx_e < 10 & vy_e < 10){
        vx[1] = vx_r
        vy[1] = vy_r
        vz[1] = vz_r
        vx = vx[order(vz)]
        vy = vy[order(vz)]
        vz = vz[order(vz)]
        next
      } 
      
      if (vz_r < vz[2] & vz_r > vz[1] & vx_e > 0 & vy_e > 0 & vx_e < 10 & vy_e < 10){
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
    
    contour2D(z,x,y, lwd = 2, colkey = FALSE, add = TRUE, 
              labcex = 1.5, col = 1:8)
    
  })
  
  output$wrapup_plot = renderPlot({
    x = seq(0,10,1)
    y = seq(0,10,1)
    z1 = outer(x,y,model1)
    z2 = outer(x,y,model2)
    z3 = outer(x,y,model3)
    z4 = outer(x,y,model4)
    old.par = par(mfrow = c(2,2))
      persp3D(x = x, y = y, z = z1, ticktype = "detailed",
              xlab = "factor A", ylab = "factor B",
              col = palette("Okabe-Ito"), border = 1)
      persp3D(x = x, y = y, z = z2, ticktype = "detailed",
              xlab = "factor A", ylab = "factor B",
              col = palette("Okabe-Ito"), border = 1)
      persp3D(x = x, y = y, z = z3, ticktype = "detailed",
              xlab = "factor A", ylab = "factor B",
              col = palette("Okabe-Ito"), border = 1)
      persp3D(x = x, y = y, z = z4, ticktype = "detailed",
              xlab = "factor A", ylab = "factor B",
              col = palette("Okabe-Ito"), border = 1)
    par(old.par)
  })
  
}) 
