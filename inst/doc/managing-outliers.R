## -----------------------------------------------------------------------------
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#| eval: !expr 'rlang::is_installed("magick")'
#| include: false
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)


## -----------------------------------------------------------------------------
#| label: setup
#| message: false
library(tidynorm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdensity)
library(khroma)
library(tinytable)


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: Theme setup

theme_set(
  theme_minimal(
    base_size = 16
  ) +
    theme(
      palette.colour.continuous = as.character(color(palette = "lipari")(50)),
      palette.color.discrete = color(palette = "bright")
    )
)


## -----------------------------------------------------------------------------
speaker_data |>
  identify_outliers(
    # Use F1 through F3 to identify outliers
    F1:F3,

    # Identify outliers within speaker and
    # within vowel class
    .by = c(speaker, plt_vclass),

    # The probability level beyond which
    # rows are marked as outliers
    .level = 0.95
  ) |>
  norm_nearey(
    F1:F3,
    .by = speaker,

    # exclude rows marked as outliers
    .exclude = .outlier,
  ) ->
normed


## -----------------------------------------------------------------------------
nrow(speaker_data)
nrow(normed)


## -----------------------------------------------------------------------------
speaker_data |>
  identify_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass)
  ) |>
  filter(
    plt_vclass == "ae"
  ) ->
ae_identified


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: Plotting Code
ae_identified |>
  ggplot(
    aes(F2, F1)
  ) +
  geom_point(
    aes(fill = sqrt(.mahal)),
    shape = 21
  ) +
  scale_y_continuous(
    trans = c("log10", "reverse")
  ) +
  scale_x_continuous(
    trans = c("log10", "reverse")
  ) +
  expand_limits(
    fill = 0
  ) +
  facet_wrap(~speaker)


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: Plotting Code
ae_identified |>
  ggplot(
    aes(F2, F1)
  ) +
  geom_point(
    aes(fill = .prob),
    shape = 21
  ) +
  scale_y_continuous(
    trans = c("log10", "reverse")
  ) +
  scale_x_continuous(
    trans = c("log10", "reverse")
  ) +
  scale_fill_continuous(
    limits = c(0, 1)
  ) +
  facet_wrap(~speaker)


## -----------------------------------------------------------------------------
speaker_tracks |>
  identify_track_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass),
    .token_id_col = id,
    .time_col = t,
    # DCT order
    .order = 5,
    .pre_trans = log,
    .return_dct = TRUE
  ) |>
  filter(plt_vclass == "ey") ->
ey_identified


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: Plotting Code
ey_identified |>
  reframe_with_idct(
    F1:F2,
    .by = c(speaker, id),
    .param_col = .param
  ) |>
  ggplot(
    aes(F2, F1)
  ) +
  geom_path(
    aes(
      color = .prob,
      group = id
    ),
    arrow = arrow(
      type = "closed",
      length = unit(0.1, "cm")
    )
  ) +
  scale_color_continuous(
    limits = c(0, 1)
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  facet_wrap(~speaker)


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: Plotting Code
#| fig-asp: 0.5
ey_identified |>
  mutate(
    .inv_prob = 1 - .prob
  ) |>
  summarise(
    .by = c(speaker, .param),
    across(
      F1:F2,
      list(
        weighted = \(x) weighted.mean(x, .inv_prob),
        unweighted = mean
      )
    )
  ) |>
  pivot_longer(
    starts_with("F")
  ) |>
  separate(
    name,
    into = c("formant", "weight")
  ) |>
  pivot_wider(
    names_from = formant,
    values_from = value
  ) |>
  reframe_with_idct(
    F1:F2,
    .by = c(speaker, weight),
    .param_col = .param
  ) |>
  ggplot(
    aes(F2, F1, color = weight)
  ) +
  geom_path(
    arrow = arrow(
      type = "closed",
      length = unit(0.1, "cm")
    )
  ) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~speaker)


## -----------------------------------------------------------------------------
speaker_data |>
  identify_outliers(
    F1:F3,
    .by = c(speaker, plt_vclass)
  ) |>
  mutate(
    .inv_prob = 1 - .prob
  ) |>
  # a weighted log-mean normalization
  norm_generic(
    F1:F3,
    .by = speaker,
    .pre_trans = log,
    .by_formant = FALSE,
    .exclude = is.na(.inv_prob),
    .L = weighted.mean(.formant, .inv_prob, na.rm = T),
    .names = "{.formant}_lmw"
  ) |>
  # A weighted Lobanov normalization
  norm_generic(
    F1:F3,
    .by = speaker,
    .by_formant = TRUE,
    .exclude = is.na(.inv_prob),
    .L = weighted.mean(.formant, .inv_prob, na.rm = T),
    .S = sqrt(Hmisc::wtd.var(.formant, .inv_prob)),
    .names = "{.formant}_zw"
  ) ->
weighted_norm

