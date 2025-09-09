# server.R file for optimization learning module

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
