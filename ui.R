# user interface for response surface optimization application

ui = navbarPage("AC 3.0: Optimization Strategies",
     theme = shinytheme("journal"),
     header = tags$head(
       tags$link(rel = "stylesheet",
                 type = "text/css",
                 href = "style.css") 
     ),
     
  # introduction
     tabPanel("Introduction",
      fluidRow(
        withMathJax(),
          column(width = 6,
            wellPanel(
              class = "scrollable-well",
              div(
                class = "html-fragment",
                includeHTML("text/introduction.html")
              ))),
          column(width = 6, 
            align = "center",
            br(), br(),
          img(src = "badlands.png", width = "100%")
      ))),
     
  #first activity   
  tabPanel("Sequential",
    fluidRow(
      column(width = 6,
        wellPanel(
          class = "scrollable-well",
          div(
            class = "html-fragment",
            includeHTML("text/activity1.html")
          ))),
        column(width = 6,
          align = "center",
            splitLayout(
              selectInput("eqn1","response surface",
              choices = c("example 1","example 2",
                          "example 3","example 4"), 
              selected = "example 1",
              selectize = FALSE, width = "200px"),
            sliderInput("a_initial1",
                        "factor A: initial value", 
                        min = 0, max = 10,
                        step = 1, value = 0, round = TRUE, 
                        width = "200px", ticks = FALSE),
              sliderInput("b_initial1", 
                          "Factor B: initial value",
                           min = 0, max = 10, step = 1, 
                           value = 0, round = TRUE, 
                           width = "200px", ticks = FALSE)
                    ),
              splitLayout(
                radioButtons("display1", "display...", 
                             choices = c("path only", "path & surface"),
                              selected = "path only", 
                              inline = FALSE,
                              width = "100px"),
                radioButtons("dim1","... in",
                            choices = c("2D", "3D"),
                            selected = "2D", inline = FALSE, 
                            width = "100px"),
                sliderInput("act1_turn", "rotate the xy plane",
                            min = -180, max = 180, step = 1, 
                            value = 45, width = "150px", 
                            ticks = FALSE),
                sliderInput("act1_tilt", "tilt z axis",
                            min = -180, max = 180, step = 1, 
                            value = 35,
                                width = "150px", ticks = FALSE),
                       ),
                       plotOutput("act1_plot", height = "450px")
                ))),
    
     
  # second activity
     tabPanel("Steepest Ascent",
      fluidRow(
        column(width = 6,
          wellPanel(
            class = "scrollable-well",
            div(
              class = "html-fragment",
              includeHTML("text/activity2.html")
            ))),
        column(width = 6,
          align = "center",
          splitLayout(
            selectInput("eqn2","response surface",
                        choices = c("example 1","example 2","example 3",
                                    "example 4"), selected = "example 1",
                        selectize = FALSE, width = "200px"),
            sliderInput("a_initial2","factor A: initial value", 
                        min = 0, max = 10,
                       step = 1, value = 0, 
                       round = TRUE, width = "200px", ticks = FALSE),
            sliderInput("b_initial2", "Factor B: initial value",
                        min = 0, max = 10, step = 1, value = 0,
                        round = TRUE, width = "200px", ticks = FALSE)
          ),
          splitLayout(
          radioButtons("display2", "response surface", 
                       choices = c("contours (2D)","surface (3D)"),
                       selected = "contours (2D)", inline = FALSE,
                       width = "100px"),
          sliderInput("act2_turn", "rotate the xy plane",
                      min = -180, max = 180, step = 1, value = 45,
                      width = "150px", ticks = FALSE),
          sliderInput("act2_tilt", "tilt z axis",
                      min = -180, max = 180, step = 1, value = 35,
                      width = "150px", ticks = FALSE),
          ),
          plotOutput("act2_plot", height = "450px")
          ))),
     
  # third activity
     tabPanel("Fixed-Size Simplex",
      fluidRow(
        column(width = 6,
          wellPanel(
            class = "scrollable-well",
            div(
              class = "html-fragment",
              includeHTML("text/activity3.html")
            ))),
          column(width = 6,
                  align = "center",
            splitLayout(
              img(src = 'simplex-orientation.png',
                  width = "85px"),
               sliderInput("sim_orient",
                            "iniital orientation",
                            min = 1, max = 6, value = 4,
                            ticks = FALSE, width = "150px"),
                sliderInput("va","initial factor A",
                            min = 2, max = 8,
                            step = 0.5, value = 2,
                            ticks = FALSE, width = "150px"),
                sliderInput("vb", "initial factor B",
                            min = 2, max = 8, 
                            step = 0.5, value = 2,
                            ticks = FALSE, width = "150")
                       ),
                splitLayout(
                  selectInput("eqn3","response surface",
                              choices = c("example 1","example 2",
                                          "example 3","example 4"),
                              selected = "example 1",
                              selectize = FALSE,
                              width = "200px"),
                        img(src = 'fixedsimplexMovement.png',
                            width = "200px"),
                        
                      sliderInput("stepsize", "size of simplex",
                                   min = 1, max = 2, step = 0.1,
                                   value = 1, width = "200px", 
                                  ticks = FALSE)
                       ),
                       plotOutput("act3_plot", height = "450px")
                ))),
     
  # fourth activity
     tabPanel("Variable-Sized Simplex",
      fluidRow(
        column(width = 6,
          wellPanel(
            class = "scrollable-well",
            div(
              class = "html-fragment",
              includeHTML("text/activity4.html")
            ))),
         column(width = 6,
            align = "center",
              splitLayout(img(src = 'simplex-orientation.png',
                              width = "80px"),
                  sliderInput("sim_orient4",
                              "iniital orientation",
                               min = 1, max = 6, value = 4,
                              ticks = FALSE, width = "150px"),
                                sliderInput("va4","initial factor A",
                                      min = 1, max = 9,
                                      step = 0.5, value = 1,
                                      ticks = FALSE, width = "150px"),
                                  sliderInput("vb4", "initial factor B",
                                      min = 1, max = 9, 
                                      step = 0.5, value = 1,
                                      ticks = FALSE, width = "150")
                       ),
                  splitLayout(
                      selectInput("eqn4","response surface",
                                  choices = c("example 1","example 2",
                                              "example 3","example 4"),
                                  selected = "example 1",
                                  selectize = FALSE,
                                  width = "200px"),
                        img(src = 'simplexMovement.png',
                             width = "100%"),
                         sliderInput("iterations4", "number of iterations",
                                     min = 1, max = 30, step = 1,
                                     value = 1,width = "200px", 
                                     ticks = FALSE)
                       ),
                       plotOutput("act4_plot", height = "450px")
                ) )),
     
  # wrapping up
     tabPanel("Wrapping Up",
      fluidRow(
        column(width = 6,
          wellPanel(
            class = "scrollable-well",
            div(
              class = "html-fragment",
              includeHTML("text/wrapup.html")
            ))),
          column(width = 6,
                align = "center",
                plotOutput("wrapup_plot", height = "650px")
                )
                
              )) 

     ) 
