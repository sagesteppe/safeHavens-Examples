library(tidyverse)
library(plotly)
library(htmlwidgets)

loo_dat <- read_csv(
  file.path('..', "raw_data", "ManuscriptSummaries", "loo_roc.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

fig <- plot_ly(
  loo_dat,
  x = ~fpr,
  y = ~tpr,
  split = ~species,
  type = 'scatter',
  mode = 'lines',
  text = ~species,
  hoverinfo = 'text',
  line = list(color = 'steelblue')
) |>
  layout(
    title = 'Leave-one-out ROC',
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    xaxis = list(
      title = "False Positive Rate",
      range = c(0, 1)#,
   #   gridcolor = "rgba(128,128,128,0.3)", 
   #   zerolinecolor = "rgba(128,128,128,0.5)"
    ),
    yaxis = list(
      title = "True Positive Rate", 
      range = c(0, 1)#,
    #  gridcolor = "rgba(128,128,128,0.3)", 
    #  zerolinecolor = "rgba(128,128,128,0.5)"
    ),
    font = list(color = "#ddd"),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.6, xanchor = "left",
        y = 0.15, yanchor = "top",
        buttons = list(
          list(method = "restyle", args = list("visible", TRUE),         label = "Show all"),
          list(method = "restyle", args = list("visible", "legendonly"), label = "Hide all")
        )
      )
    )
  )
fig

saveWidget(fig, 
  file.path('..', 'loo-roc', 'loo_curve.html'),
  selfcontained = TRUE)
