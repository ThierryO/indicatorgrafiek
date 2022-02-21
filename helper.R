library(git2rdata)
library(tidyverse)
library(effectclass)
library(digest)
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
  d = ggplot(soort_d), e = ggplot(soort_e),
  am = soort_a %>%
    mutate(
      schatting = -.data$schatting, old = -.data$ucl, ucl = -.data$lcl,
      lcl = .data$old,
      klasse = classification(.data$lcl, .data$ucl, log(drempel))
    ) %>%
    ggplot(),
  bm = soort_b %>%
    mutate(
      schatting = -.data$schatting, old = -.data$ucl, ucl = -.data$lcl,
      lcl = .data$old,
      klasse = classification(.data$lcl, .data$ucl, log(drempel))
    ) %>%
    ggplot(),
  cm = soort_c %>%
    mutate(
      schatting = -.data$schatting, old = -.data$ucl, ucl = -.data$lcl,
      lcl = .data$old,
      klasse = classification(.data$lcl, .data$ucl, log(drempel))
    ) %>%
    ggplot(),
  em = soort_e %>%
    mutate(
      schatting = -.data$schatting, old = -.data$ucl, ucl = -.data$lcl,
      lcl = .data$old,
      klasse = classification(.data$lcl, .data$ucl, log(drempel))
    ) %>%
    ggplot()
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
  ribbon_fan = stat_fan(aes(x = naar, y = schatting, link_sd = sd)),
  rect = geom_rect(
    aes(xmin = naar - 0.5, xmax = naar + 0.5, ymin = lcl, ymax = ucl),
    alpha = ribbon_opacity
  ),
  rect_fan = stat_fan(
    aes(xmin = naar - 0.5, xmax = naar + 0.5, y = schatting, link_sd = sd),
    geom = "rect"
  ),
  rect_fan_kleur = list(
    stat_fan(
      aes(
        xmin = naar - 0.5, xmax = naar + 0.5, y = schatting, link_sd = sd,
        fill = klasse
      ),
      geom = "rect"
    ),
    scale_fill_manual(values = kleurgradient)
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
  alternative <- sprintf(
    "%s:%s",
     c(
       rep("href", length(href)), rep("pref", length(pref)),
       rep("trend", length(trend)), rep("uncertainty", length(uncertainty)),
       rep("y_scale", length(y_scale))
     ),
     c(
       names(href), names(pref), names(trend), names(uncertainty),
       names(y_scale)
     )
  )
  expand_grid(
    href = factor(names(href)), pref = factor(names(pref)),
    trend = factor(names(trend)), uncertainty = factor(names(uncertainty)),
    y_scale = factor(names(y_scale)), alternative = alternative
  ) %>%
    extract(.data$alternative, c("element", "value"), "(.*):(.*)") %>%
    filter(
      !(.data$element == "href" & .data$href == .data$value),
      !(.data$element == "pref" & .data$pref == .data$value),
      !(.data$element == "trend" & .data$trend == .data$value),
      !(.data$element == "uncertainty" & .data$uncertainty == .data$value),
      !(.data$element == "y_scale" & .data$y_scale == .data$value)
    ) %>%
    mutate(
      a = 0L, b = 0L, element = factor(.data$element),
      value = factor(.data$value)
    ) -> full_grid
  if (is_git2rdata("user_input")) {
    old <- read_vc("user_input")
    full_grid %>%
      mutate(
        session = sha1(as.list(Sys.getenv())),
        id = map_chr(
          paste(
            .data$href, .data$pref, .data$trend, .data$uncertainty,
            .data$y_scale, .data$element, .data$value, .data$session
          ),
          sha1
        )
      ) %>%
      anti_join(old, by = "id") %>%
      bind_rows(old)
  } else {
    full_grid %>%
      mutate(
        session = sha1(as.list(Sys.getenv())),
        id = map_chr(
          paste(
            .data$href, .data$pref, .data$trend, .data$uncertainty,
            .data$y_scale, .data$element, .data$value, .data$session
          ),
          sha1
        )
      )
  }
}

get_information <- function(x, variable, correction = 1) {
  x %>%
    filter(
      .data$element == variable, as.character(.data$value) != .data[[variable]]
    ) %>%
    select(
      var_a = .data[[variable]], .data$a,
      var_b = .data$value, .data$b
    ) %>%
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
      prop = pmax(.data$wins, correction) / sum(pmax(.data$wins, correction)),
      shannon = sum(-.data$prop * log(.data$prop))
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
        (.data[[selected$element]] == selected$value) |
          (.data$element == selected$element & .data$value == selected$value)
      ) -> x
    to_do <- to_do[to_do != selected$element]
  }
  x %>%
    slice_sample(n = 1)
}

