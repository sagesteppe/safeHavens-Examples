library(tidyverse)
library(plotly)
library(htmlwidgets)
library(scales)

metrics_dat <- read_csv(
  file.path('..', "raw_data", "ManuscriptSummaries", "evaluation_metrics.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

# Pre-compute viridis colors from balanced accuracy so each per-species
# trace keeps the continuous color encoding
ba        <- metrics_dat$balanced_accuracy
color_fn  <- col_numeric("viridis", domain = range(ba))
metrics_dat <- metrics_dat |> mutate(pt_color = color_fn(ba))

species_list <- sort(unique(metrics_dat$species))

# anti-diagonal reference lines for constant balanced accuracy
iso <- function(b, col = "rgba(128,128,128,0.4)") {
  s    <- 2 * b
  ends <- if (s <= 1) list(0, s, s, 0) else list(s - 1, 1, 1, s - 1)
  list(type = "line", x0 = ends[[1]], y0 = ends[[2]],
       x1 = ends[[3]], y1 = ends[[4]],
       line = list(color = col, dash = "dot", width = 1), layer = "below")
}

fig <- plot_ly()

for (sp in species_list) {
  row <- metrics_dat |> filter(species == sp)

  fig <- fig |>
    add_trace(
      x = row$sensitivity, y = row$specificity,
      type = 'scatter', mode = 'markers',
      marker = list(
        color    = row$pt_color,
        size     = row$f1 * 20,
        sizemode = 'diameter',
        line     = list(width = 0.5, color = 'rgba(0,0,0,0.3)')
      ),
      name       = sp,
      showlegend = TRUE,
      text = paste0(row$species,
                    "<br>Sens: ",    round(row$sensitivity,       3),
                    "<br>Spec: ",    round(row$specificity,       3),
                    "<br>Bal Acc: ", round(row$balanced_accuracy, 3),
                    "<br>F1: ",      round(row$f1,                3)),
      hoverinfo = 'text'
    )
}

fig <- fig |>
  layout(
    title = 'Model Evaluation; Balanced Accuracy, and F1 Scores',
    xaxis = list(title = "Sensitivity", range = c(0.70, 1)),
    yaxis = list(title = "Specificity", range = c(0.70, 1),
                 scaleanchor = "x", scaleratio = 1),
    shapes = lapply(c(0.8, 0.85, 0.9, 0.95), iso),
    updatemenus = list(
      list(
        type      = "buttons",
        direction = "right",
        x = 0.8, xanchor = "left",
        y = 0.0, yanchor = "top",
        buttons = list(
          list(
            method = "restyle", 
            args = list("visible", TRUE),         
            label = "Show all"),
          list(
            method = "restyle", 
            args = list("visible", "legendonly"), 
            label = "Hide all")
        )
      )
    )
  )

p <- fig
p

saveWidget(p,
  file.path('..', 'accuracy', 'accuracy.html'),
  selfcontained = TRUE)

saveWidget(p,
  file.path('..', 'docs', 'accuracy', 'accuracy.html'),
  selfcontained = TRUE)
