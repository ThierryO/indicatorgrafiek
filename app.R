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
c(rev(traffic_palette(7)), "grey65", "grey35", "grey50") %>%
  setNames(
    c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
  ) -> kleurgradient
kleurgradient[4] <- inbo_steun_blauw
ribbon_opacity <- 0.6
drempel <- 0.75
index_breaks <- function(x) {
  z <- 1 - c(
    9 / 10, 4 / 5, 3 / 4, 2 / 3, 1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 10, 1 / 20,
    1 / 50, 1 / 100, 1 / 200, 0
  )
  z <- log(sort(z))
  z <- z[which(z >= min(-abs(x)))[1] + -1:2]
  c(z, 0, -z)
}
rev_labels <- function(x) {
  sprintf("%+.0f%%", 100 * (exp(x) - 1))
}
index_labels <- function(x) {
  sprintf("%.0f", 100 * exp(x))
}
base_plot <- list(
  a = ggplot(soort_a), b = ggplot(soort_b), c = ggplot(soort_c),
  d = ggplot(soort_d), e = ggplot(soort_e)
)
y_scale <- list(
  index = scale_y_continuous(
    "index (2007 = 100)", breaks = index_breaks, labels = index_labels
  ),
  relatief = scale_y_continuous(
    "procentuele wijziging t.o.v. 2007", breaks = index_breaks,
    labels = rev_labels
  )
)
href <- list(
  geen = NULL,
  ref = geom_hline(yintercept = 0, linetype = 1),
  grenzen = geom_hline(
    yintercept = c(0, 1, -1) * log(drempel), linetype = c(1, 2, 2)
  ),
  tekst = list(
    geom_hline(yintercept = c(0, 1, -1) * log(drempel), linetype = c(1, 2, 2)),
    annotate(
      "text", x = Inf, y = 0, vjust = -0.3, hjust = 1, label = "referentie",
      colour = inbo_steun_donkerroos
    ),
    annotate(
      "text", x = Inf, y = log(drempel), vjust = -0.3, hjust = 1,
      label = "ondergrens", colour = inbo_steun_donkerroos
    ),
    annotate(
      "text", x = Inf, y = -log(drempel), vjust = -0.3, hjust = 1,
      label = "bovengrens", colour = inbo_steun_donkerroos
    )
  ),
  band = list(
    geom_hline(yintercept = 0, linetype = 1),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = log(drempel),
      ymax = -log(drempel), fill = "black", alpha = 0.1
    )
  )
)
pref <- list(
  geen = NULL,
  ref = list(
    geom_point(data = deze_ref, aes(x = naar, y = schatting), size = 6),
    geom_text(
      data = deze_ref, aes(x = naar, y = schatting), size = 4, label = "R",
      colour = "white"
    )
  )
)
trend <- list(
  lijn = geom_line(aes(x = naar, y = schatting), size = 2),
  punt = list(
    geom_point(aes(x = naar, y = schatting), size = 6),
    geom_text(
      aes(x = naar, y = schatting, label = klasse), size = 4, colour = "white"
    )
  ),
  kleur = list(
    geom_point(aes(x = naar, y = schatting, colour = klasse), size = 6),
    geom_text(
      aes(x = naar, y = schatting, label = klasse), size = 4, colour = "white"
    ),
    scale_colour_manual(values = kleurgradient)
  )
)
uncertainty <- list(
  ribbon = geom_ribbon(
    aes(x = naar, ymin = lcl, ymax = ucl),
    alpha = ribbon_opacity
  ),
  rect = geom_rect(
    aes(xmin = naar - 0.5, xmax = naar + 0.5, ymin = lcl, ymax = ucl),
    alpha = ribbon_opacity
  ),
  rect_kleur = list(
    geom_rect(
      aes(
        xmin = naar - 0.5, xmax = naar + 0.5, ymin = lcl, ymax = ucl,
        fill = klasse
      ),
      alpha = ribbon_opacity
    ),
    scale_fill_manual(values = kleurgradient)
  )
)

define_user_input <- function() {
  expand_grid(
    href_a = factor(names(href)), href_b = factor(names(href)),
    pref_a = factor(names(pref)), pref_b = factor(names(pref)),
    trend_a = factor(names(trend)), trend_b = factor(names(trend)),
    uncertainty_a = factor(names(uncertainty)),
    uncertainty_b = factor(names(uncertainty)),
    y_scale_a = factor(names(y_scale)), y_scale_b = factor(names(y_scale))
  ) %>%
    filter(
      (.data$href_a != .data$href_b) | (.data$pref_a != .data$pref_b) |
        (.data$trend_a != .data$trend_b) | (.data$y_scale_a != .data$y_scale_b) |
        (.data$uncertainty_a != .data$uncertainty_b)
    ) %>%
    mutate(id = row_number()) -> full_grid
  full_grid %>%
    mutate(a = 0L, b = 0L)
}

get_information <- function(x, variable) {
  x %>%
    select(
      var_a = .data[[paste0(variable, "_a")]], .data$a,
      var_b = .data[[paste0(variable, "_b")]], .data$b
    ) %>%
    filter(.data$var_a != .data$var_b) %>%
    pivot_longer(
      c(-"a", -"b"), names_to = "test", values_ptypes = character()
    ) %>%
    mutate(
      win = ifelse(str_detect(.data$test, "_a"), .data$a, .data$b),
    ) %>%
    group_by(.data$value) %>%
    summarise(
      wins = sum(.data$win), tests = sum(.data$a + .data$b), .groups = "drop"
    ) %>%
    transmute(
      element = variable, .data$value,
      prop = (.data$wins + 1) / sum(.data$wins + 1),
      shannon = sum(-.data$prop * log(.data$prop)),
      prop = .data$prop / (.data$tests + 1)
    )
}

sample_combination <- function(x) {
  to_do <- c("href", "pref", "trend", "uncertainty", "y_scale")
  while (length(to_do)) {
    proportions <- map_dfr(to_do, get_information, x = x)
    proportions %>%
      distinct(.data$element, .data$shannon) %>%
      slice_max(.data$shannon, n = 1) %>%
      semi_join(x = proportions, by = "element") %>%
      slice_sample(n = 1, weight_by = .data$prop) -> selected
    x %>%
      filter(
        (.data[[paste0(selected$element, "_a")]] == selected$value) |
        (.data[[paste0(selected$element, "_b")]] == selected$value)
      ) -> x
    to_do <- to_do[to_do != selected$element]
  }
  x %>%
    slice_sample(n = 1)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 1,
      actionButton("kies_a", label = "kies A"),
      actionButton("kies_b", label = "kies B")
    ),
    mainPanel = mainPanel(
      width = 11, plotOutput("graph_a"), plotOutput("graph_b")
    )
  )
)

server <- function(input, output) {
  data <- reactiveValues(
    base = sample(names(base_plot), 1),
    user_input = define_user_input()
  )

  combination <- reactive({
    if (nrow(data$user_input) == 0) {
      return(NULL)
    }
    sample_combination(data$user_input) %>%
      select(-.data$a, -.data$b) -> tmp
    tmp %>%
      pivot_longer(-.data$id, values_ptypes = character()) %>%
      extract(.data$name, c("element", "level"), "(.*)_(.*)") %>%
      pivot_wider(names_from = .data$level, values_from = .data$value)
  })

  output$graph_a <- renderPlot({
    df <- combination()
    if (is.null(df)) {
      return(NULL)
    }
    p <- base_plot[[data$base]] +
      ggtitle("A") +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[df$a[df$element == "y_scale"]]]
    h <- href[[df$a[df$element == "href"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- href[[df$a[df$element == "pref"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[df$a[df$element == "uncertainty"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[df$a[df$element == "trend"]]]
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
    df <- combination()
    if (is.null(df)) {
      return(NULL)
    }
    p <- base_plot[[data$base]] +
      ggtitle("B") +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[df$b[df$element == "y_scale"]]]
    h <- href[[df$b[df$element == "href"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- href[[df$b[df$element == "pref"]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[df$b[df$element == "uncertainty"]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[df$b[df$element == "trend"]]]
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
    data$user_input[data$user_input$id == combination()$id, "a"] <-
      data$user_input[data$user_input$id == combination()$id, "a"] + 1
  })

  observeEvent(input$kies_b, {
    data$user_input[data$user_input$id == combination()$id, "b"] <-
      data$user_input[data$user_input$id == combination()$id, "b"] + 1
  })
}

shinyApp(ui = ui, server = server)
