library(git2rdata)
library(shiny)
library(tidyverse)
library(INBOtheme)

soort_a <- read_vc("soort_a")
soort_b <- read_vc("soort_b")
soort_c <- read_vc("soort_c")
soort_d <- read_vc("soort_d")
soort_e <- read_vc("soort_e")
deze_ref <- read_vc("deze_ref")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 1,
      radioButtons(
        "voorkeur", "voorkeur", choices = c("", "A", "B"), selected = ""
      )
    ),
    mainPanel = mainPanel(
      width = 11, plotOutput("graph_a"), plotOutput("graph_b")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$graph_a <- renderPlot({
    ggplot(data = soort_c, aes(x = naar, y = schatting)) +
      geom_line() +
      ggtitle("A") + theme(plot.title = element_text(hjust = 0))
  })
  output$graph_b <- renderPlot({
    ggplot(data = soort_c, aes(x = naar, y = schatting)) +
      geom_point() +
      ggtitle("B") + theme(plot.title = element_text(hjust = 0))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
