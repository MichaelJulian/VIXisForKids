
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Silly Trader, VIX is For Kids!"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3('VIX:'), textOutput('VIX'),
      h4('fuck with it'),
      numericInput('strike','Strike',900,step=5),
      numericInput('inc','Increment Midpoint', 0, step=.025),
      actionButton("goButton", "Go!")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Expiry 1', plotOutput('contributionPlot1'),
                 dataTableOutput('chain1')),
        tabPanel('Expiry 2', plotOutput('contributionPlot2'),
                 dataTableOutput('chain2'))
        #tabPanel('Expiry 2', plotOutput(output$contributionPlot1), 
         #        tableOutput('chain1')),
    )
  )
)))
