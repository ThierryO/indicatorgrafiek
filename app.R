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

base_plot <- list(
  a = ggplot(soort_a), b = ggplot(soort_b), c = ggplot(soort_c),
  d = ggplot(soort_d), e = ggplot(soort_e)
)

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
  data <- reactiveValues(
    base = sample(base_plot, 1)[[1]]
  )
  output$graph_a <- renderPlot({
    data$base +
      geom_line(aes(x = naar, y = schatting)) +
      ggtitle("A") + theme(plot.title = element_text(hjust = 0))
  })
  output$graph_b <- renderPlot({
    data$base +
      geom_point(aes(x = naar, y = schatting)) +
      ggtitle("B") + theme(plot.title = element_text(hjust = 0))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
