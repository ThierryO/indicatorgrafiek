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
      "text", x = Inf, y = 0, vjust = -0.1, hjust = 1, label = "referentie",
      colour = inbo_steun_donkerroos
    ),
    annotate(
      "text", x = Inf, y = log(drempel), vjust = -0.1, hjust = 1,
      label = "ondergrens", colour = inbo_steun_donkerroos
    ),
    annotate(
      "text", x = Inf, y = -log(drempel), vjust = -0.1, hjust = 1,
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
    base = sample(names(base_plot), 1),
    href = sample(names(href), 2, replace = FALSE),
    pref = sample(names(pref), 2, replace = TRUE),
    trend = sample(names(trend), 2, replace = TRUE),
    uncertainty = sample(names(uncertainty), 2, replace = TRUE),
    y_scale = sample(names(y_scale), 2, replace = TRUE)
  )
  output$graph_a <- renderPlot({
    p <- base_plot[[data$base]] +
      ggtitle("A") +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[data$y_scale[1]]]
    h <- href[[data$href[1]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- href[[data$pref[1]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[data$uncertainty[1]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[data$trend[1]]]
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
    p <- base_plot[[data$base]] +
      ggtitle("B") +
      theme(
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0)
      ) +
      y_scale[[data$y_scale[2]]]
    h <- href[[data$href[2]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- href[[data$pref[2]]]
    if (!is.null(h)) {
      if (inherits(h, "list")) {
        for (z in h) {
          p <- p + z
        }
      } else {
        p <- p + h
      }
    }
    h <- uncertainty[[data$uncertainty[2]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    h <- trend[[data$trend[2]]]
    if (inherits(h, "list")) {
      for (z in h) {
        p <- p + z
      }
    } else {
      p <- p + h
    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
