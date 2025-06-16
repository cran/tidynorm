## -----------------------------------------------------------------------------
#| eval: !expr 'rlang::is_installed("magick")'
#| echo: false
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)


## -----------------------------------------------------------------------------
#| label: setup
library(tidynorm)


## -----------------------------------------------------------------------------
#| eval: !expr 'rlang::is_installed(c("ggplot2"))'
library(ggplot2)


## -----------------------------------------------------------------------------
speaker_tracks


## -----------------------------------------------------------------------------
normed_tracks <- speaker_tracks |>
  norm_track_lobanov(
    # identify the formant columns
    F1:F3,

    # provide speaker grouping
    .by = speaker,

    # provide token id
    .token_id_col = id,

    # provide an optional time column
    .time_col = t
  )


## -----------------------------------------------------------------------------
#| fig-align: center
#| out-width: 80%
#| eval: !expr 'rlang::is_installed(c("ggplot2"))'
normed_tracks |>
  ggplot(
    aes(F2_z, F1_z)
  ) +
  geom_path(
    aes(
      group = interaction(speaker, id)
    ),
    alpha = 0.3
  ) +
  scale_y_reverse() +
  scale_x_reverse() +
  facet_wrap(~speaker) +
  coord_cartesian(
    xlim = c(3.5, -3.5),
    ylim = c(3.5, -3.5)
  ) +
  theme(
    aspect.ratio = 1
  )

