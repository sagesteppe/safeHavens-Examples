library(tidyverse)
library(jsonlite)

bio_groups <- tibble::tribble(
  ~variable,  ~group,           ~label,
  "bio_01",   "Temperature",    "Mean Annual Temp",
  "bio_02",   "Variability",    "Mean Diurnal Range",
  "bio_03",   "Variability",    "Isothermality",
  "bio_04",   "Variability",    "Temp Seasonality",
  "bio_05",   "Temperature",    "Max Temp Warmest Mo.",
  "bio_06",   "Temperature",    "Min Temp Coldest Mo.",
  "bio_07",   "Variability",    "Temp Annual Range",
  "bio_08",   "Temperature",    "Mean Temp Wettest Qtr",
  "bio_10",   "Temperature",    "Mean Temp Warmest Qtr",
  "bio_11",   "Temperature",    "Mean Temp Coldest Qtr",
  "bio_12",   "Precipitation",  "Annual Precip",
  "bio_15",   "Precipitation",  "Precip Seasonality",
  "bio_16",   "Precipitation",  "Precip Wettest Qtr",
  "bio_17",   "Precipitation",  "Precip Driest Qtr",
  "bio_18",   "Precipitation",  "Precip Warmest Qtr"
)

bio_groups = mutate(
  bio_groups, label = factor(label, levels = label))

me = read.csv(
  file.path(
    '..',
    'raw_data',
    'ManuscriptSummaries', 
    'marginal_effects.csv'
  )
) |>
  left_join(bio_groups, join_by(predictor == variable)) |>
  select(-predictor, -group)

asex = me |>
  filter(species == 'Asclepias exaltata')

asex[sample(1:nrow(asex), 5),]

ggplot(data = asex, aes(x = x_value, y = estimate)) +
  facet_wrap(~label, scales = 'free_x', ncol = 3) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    fill = "#689689") +
  geom_line(color = '#7261A3', lwd = 1.5) +
  theme_minimal()+
  ylab('Estimate') + xlab('Value')

# ── Export for web page ───────────────────────────────────────────────────────
me_web <- read.csv(
    file.path('..', 'raw_data', 'ManuscriptSummaries', 'marginal_effects.csv')
  ) |>
  left_join(bio_groups |> mutate(label = as.character(label)),
            join_by(predictor == variable)) |>
  select(-group) |>
  arrange(species, predictor, x_value)

me_web <- me_web |>
  group_by(species, predictor) |>
  slice(seq(1, n(), by = 2)) |>
  ungroup()

dir.create('docs/marginal_effects', showWarnings = FALSE, recursive = TRUE)
write_json(me_web, '../docs/marginal_effects/data.json', digits = 8, na = 'null')
