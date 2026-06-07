library(tidyverse)
library(plotly)
library(htmlwidgets)

ppc_dat <- read_csv(
  file.path('..', "raw_data", "ManuscriptSummaries", "ppc_density.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

species_list <- sort(unique(ppc_dat$species))

col_pred  <- 'rgba(70,130,180,0.35)'
col_obs   <- 'rgba(210,90,70,0.35)'
line_pred <- 'steelblue'
line_obs  <- '#d25a46'

fig <- plot_ly()

for (sp in species_list) {

  sp_dat    <- ppc_dat |> filter(species == sp)
  pred_vals <- sp_dat  |> filter(!is_y) |> pull(value)
  obs_vals  <- sp_dat  |> filter(is_y)  |> pull(value)

  d_pred <- density(pred_vals)
  d_obs  <- density(obs_vals)

  m_pred <- mean(pred_vals)
  m_obs  <- mean(obs_vals)
  max_y  <- max(c(d_pred$y, d_obs$y))

  fig <- fig |>
    add_trace(
      x = d_pred$x, y = d_pred$y,
      type = 'scatter', mode = 'lines',
      fill = 'tozeroy', fillcolor = col_pred,
      line = list(color = line_pred, width = 1.5),
      name = sp, legendgroup = sp, showlegend = TRUE,
      hovertemplate = paste0('<b>', sp, '</b><br>Predicted<br>Value: %{x:.3f}<extra></extra>')
    ) |>
    add_trace(
      x = d_obs$x, y = d_obs$y,
      type = 'scatter', mode = 'lines',
      fill = 'tozeroy', fillcolor = col_obs,
      line = list(color = line_obs, width = 1.5),
      name = sp, legendgroup = sp, showlegend = FALSE,
      hovertemplate = paste0('<b>', sp, '</b><br>Observed<br>Value: %{x:.3f}<extra></extra>')
    ) |>
    add_segments(
      x = m_pred, xend = m_pred, y = 0, yend = max_y,
      line = list(color = line_pred, dash = 'dash', width = 1.5),
      legendgroup = sp, showlegend = FALSE, inherit = FALSE,
      hovertemplate = paste0('<b>', sp, '</b><br>Predicted mean: ', round(m_pred, 3), '<extra></extra>')
    ) |>
    add_segments(
      x = m_obs, xend = m_obs, y = 0, yend = max_y,
      line = list(color = line_obs, dash = 'dash', width = 1.5),
      legendgroup = sp, showlegend = FALSE, inherit = FALSE,
      hovertemplate = paste0('<b>', sp, '</b><br>Observed mean: ', round(m_obs, 3), '<extra></extra>')
    )
}

fig <- fig |>
  layout(
    title         = 'Predicted vs Observed Distributions',
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    xaxis  = list(title = "Value"),
    yaxis  = list(title = "Density"),
    font   = list(color = "#ddd"),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.8, xanchor = "left",
        y = 0.0, yanchor = "top",
        buttons = list(
          list(method = "restyle", args = list("visible", TRUE),         label = "Show all"),
          list(method = "restyle", args = list("visible", "legendonly"), label = "Hide all")
        )
      )
    )
  )

fig

saveWidget(fig,
  file.path('..', 'ppc-mean', 'ppc_mean.html'),
  selfcontained = TRUE)

saveWidget(fig,
  file.path('..', 'docs', 'ppc-mean', 'ppc_mean.html'),
  selfcontained = TRUE)
