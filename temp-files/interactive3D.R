library("shiny")
library("shinyjs")
library("colourpicker")


ui <- shinyUI(pageWithSidebar(
  titlePanel("", "3D Perspective Plot for Interaction Effects"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Data", 
               sliderInput("b0", "Intercept:", min = -10, max = 10, value = 0, step = 0.5),
               sliderInput("b1", "Beta 1 (x1):", min = -10, max = 10, value = 0, step = 0.5),
               sliderInput("b2", "Beta 2 (x2):", min = -10, max = 10, value = 0, step = 0.5),
               sliderInput("b3", "Beta 3 (x1*x2):", min = -10, max = 10, value = 0, step = 0.5),
               sliderInput("nobs", "Number of Observations:", min = 10, max = 1000, value = 100, step = 10)
      ),
      tabPanel("Model", 
               helpText("Select a model from those listed below"),
               selectInput("modelformula", "Model Formula", 
                           c("y ~ x1", "y ~ x2", "y ~ x1 * x2", "y ~ x1 + x1:x2", "y ~ x2 + x1:x2", "y ~ x1:x2"), 
                           selected = "y ~ x1 * x2", multiple = FALSE, selectize = TRUE)
      ),
      tabPanel("View", 
               sliderInput("theta", "View azimuthal direction (theta):",
                           min = 0, max = 360, value = 45, step = 15),
               sliderInput("phi", "View colatitude (phi):",
                           min = 0, max = 360, value = 15, step = 15),
               sliderInput("distance", "View distance (d):",
                           min = 0, max = 1, value = 1, step = 0.05)
      ),
      tabPanel("Surface", 
               sliderInput("density", "Grid Density:",
                           min = 2, max = 50, value = 10, step = 1),
               colourInput("color", "Surface color:", value = "gray", allowTransparent = FALSE),
               colourInput("border", "Border color:", value = "black", allowTransparent = FALSE),
               sliderInput("shade", "Shade:", min = 0, max = 1, value = 0.75, step = 0.05),
               sliderInput("ltheta", "Illumination azimuthal direction l(theta):",
                           min = 0, max = 360, value = 20, step = 10),
               sliderInput("lphi", "Illumination colatitude (lphi):",
                           min = 0, max = 360, value = 20, step = 10)
      ),
      tabPanel("Axes", 
               checkboxInput("box", "Bounding box?", value = TRUE),
               checkboxInput("axes", "Axis Ticks?", value = TRUE),
               selectInput("ticktype", "Tick type", c("simple", "detailed"), 
                           selected = "simple", multiple = FALSE, selectize = TRUE),
               sliderInput("nticks", "Number of ticks:", min = 2, max = 50, value = 6, step = 2),
               textInput("xlab", "x1 axis label:", value = "x1"),
               textInput("ylab", "x2 axis label:", value = "x2"),
               textInput("zlab", "Vertical axis label:", value = "y")
      ),
      tabPanel("Marginal Effects", 
               selectInput("me_x2", "Marginal Effect of X1 at X2 = ", c(NA, as.character(seq(0, 1, by = 0.1))), 
                           multiple = FALSE, selectize = TRUE),
               colourInput("me_x2_color", "Line Color:", value = "red", allowTransparent = FALSE),
               selectInput("me_x1", "Marginal Effect of X2 at X1 = ", c(NA, as.character(seq(0, 1, by = 0.1))), 
                           multiple = FALSE, selectize = TRUE),
               colourInput("me_x1_color", "Line Color:", value = "red", allowTransparent = FALSE)
      )
    )#,
    #downloadButton("downloadImage", label = "Download Plot (.png)")
  ),
  mainPanel(
    headerPanel("Plot of Interaction Effects"),
    tabsetPanel(
      tabPanel("Perspective Plot", plotOutput("plot1"))
    )
  )
))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    set.seed(1)
    x1 <- runif(input$nobs, 0, 1)
    x2 <- runif(input$nobs, 0, 1)
    y <- input$b0 + 
      (input$b1 * x1) + 
      (input$b2 * x2) + 
      (input$b3 * x1 * x2) + rnorm(input$nobs)
    m <- lm(as.formula(input$modelformula))
    
    nx <- seq(0, 1, length.out = input$density)
    z <- outer(nx, nx, FUN = function(a, b) predict(m, data.frame(x1 = a, x2 = b)))
    par(mar = rep(1, 4))
    p <- persp(nx, nx, z, 
               xlab = input$xlab, ylab = input$ylab, zlab = input$zlab,
               r = input$distance,
               theta = input$theta, phi = input$phi, shade = input$shade, 
               col = input$color, border = input$border,
               ltheta = input$ltheta, lphi = input$lphi,
               box = input$box, axes = input$axes, 
               ticktype = input$ticktype, nticks = input$nticks)
    if (!is.na(input$me_x1)) {
      lines(trans3d(x = rep(as.numeric(input$me_x1), 2), y = c(0,1), 
                    z = predict(m, data.frame(x1 = rep(as.numeric(input$me_x1), 2), x2 = c(0,1))), 
                    pmat = p), 
            col = input$me_x1_color, lwd = 2)
    }
    if (!is.na(input$me_x2)) {
      lines(trans3d(x = c(0,1), y = rep(as.numeric(input$me_x2), 2),
                    z = predict(m, data.frame(x1 = c(0,1), x2 = rep(as.numeric(input$me_x2), 2))), 
                    pmat = p), 
            col = input$me_x2_color, lwd = 2)
    }
  })
  
  output$downloadImage <- downloadHandler(
    filename = "perspective.png",
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)
