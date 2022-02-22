library(shiny)

source("helper.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 1,
      textInput("path", "output path", value = normalizePath(".")),
      actionButton("kies_a", label = "kies A"),
      actionButton("kies_b", label = "kies B")
    ),
    mainPanel = mainPanel(
      width = 8, plotOutput("graph_a"), plotOutput("graph_b")
    )
  )
)

server <- function(input, output) {

  data <- reactiveValues(
    base = sample(names(base_plot), 1),
    user_input = define_user_input()
  )

  observeEvent(input$path, {
    data$user_input <- define_user_input(input$path)
  })

  combination <- reactive({
    sc <- sample_combination(data$user_input)
    change_element <- paste0(as.character(sc$element), "_b")
    sc %>%
      select(-.data$a, -.data$b) %>%
      rename(
        href_a = .data$href, pref_a = .data$pref, trend_a = .data$trend,
        uncertainty_a = .data$uncertainty, y_scale_a = .data$y_scale
      ) %>%
      mutate(
        href_b = .data$href_a, pref_b = .data$pref_a, trend_b = .data$trend_a,
        uncertainty_b = .data$uncertainty_a, y_scale_b = .data$y_scale_a,
        !!change_element := .data$value
      ) %>%
      select(-.data$element, -.data$value) %>%
      pivot_longer(c(-"id", -"session"), values_ptypes = character()) %>%
      extract(.data$name, c("element", "level"), "(.*)_(.*)") %>%
      pivot_wider(names_from = .data$level, values_from = .data$value)
  })

  output$graph_a <- renderPlot({
    if (is.null(data$df)) {
      data$df <- combination()
      return(NULL)
    }
    p <- base_plot[[data$base]] +
      ggtitle("A") +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[data$df$a[data$df$element == "y_scale"]]]
    h <- href[[data$df$a[data$df$element == "href"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- pref[[data$df$a[data$df$element == "pref"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[data$df$a[data$df$element == "uncertainty"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[data$df$a[data$df$element == "trend"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    p
  })

  output$graph_b <- renderPlot({
    if (is.null(data$df)) {
      return(NULL)
    }
    titel <- paste(
      "B",
      c(
        href = "referentielijn", pref = "referentiepunt", trend = "schatting",
        uncertainty = "onzekerheid", y_scale = "schaal y as"
      )[data$df$element[data$df$a != data$df$b]]
    )
    p <- base_plot[[data$base]] +
      ggtitle(titel) +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[data$df$b[data$df$element == "y_scale"]]]
    h <- href[[data$df$b[data$df$element == "href"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- pref[[data$df$b[data$df$element == "pref"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[data$df$b[data$df$element == "uncertainty"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[data$df$b[data$df$element == "trend"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    p
  })

  observeEvent(input$kies_a, {
    if (is.null(input$path)) {
      showNotification("Please set data directory first", type = "error")
      return(NULL)
    }
    data$user_input[data$user_input$id == data$df$id[1], "a"] <-
      data$user_input[data$user_input$id == data$df$id[1], "a"] + 1
    data$user_input %>%
      filter(.data$a > 0 | .data$b > 0) %>%
      write_vc(file = "user_input", sorting = "id", root = input$path)
    data$base <- sample(names(base_plot), 1)
    data$df <- combination()
  })

  observeEvent(input$kies_b, {
    if (is.null(input$path)) {
      showNotification("Please set data directory first", type = "error")
      return(NULL)
    }
    data$user_input[data$user_input$id == data$df$id[1], "b"] <-
      data$user_input[data$user_input$id == data$df$id[1], "b"] + 1
    data$user_input %>%
      filter(.data$a > 0 | .data$b > 0) %>%
      write_vc(file = "user_input", sorting = "id", root = input$path)
    data$base <- sample(names(base_plot), 1)
    data$df <- combination()
  })
}

shinyApp(ui = ui, server = server)
