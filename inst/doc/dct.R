## -----------------------------------------------------------------------------
#| eval: !expr 'rlang::is_installed("magick")'
#| echo: false
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)


## -----------------------------------------------------------------------------
#| label: setup
#| message: false
library(tidynorm)
library(dplyr)
library(tibble)
library(ggplot2)


## -----------------------------------------------------------------------------
#| label: plotting defaults
#| code-fold: true
#| code-summary: plotting defaults

options(
  ggplot2.discrete.colour = c(
    lapply(
      1:6,
      \(x) c(
        "#4477AA", "#EE6677", "#228833",
        "#CCBB44", "#66CCEE", "#AA3377"
      )[1:x]
    )
  ),
  ggplot2.discrete.fill = c(
    lapply(
      1:6,
      \(x) c(
        "#4477AA", "#EE6677", "#228833",
        "#CCBB44", "#66CCEE", "#AA3377"
      )[1:x]
    )
  )
)

theme_set(
  theme_minimal(
    base_size = 16
  )
)


## -----------------------------------------------------------------------------
one_track <- speaker_tracks |>
  filter(
    speaker == "s01",
    id == 9
  )


## -----------------------------------------------------------------------------
#| label: fig-f1
#| fig-cap: "F1 Formant Track"
one_track |>
  ggplot(aes(t, F1)) +
  geom_point() +
  geom_line()


## -----------------------------------------------------------------------------
dct(one_track$F1)


## -----------------------------------------------------------------------------
#| label: fig-f1-idct
#| fig-cap: "idct(dct(F1))"
one_track |>
  mutate(
    F1_dct = dct(F1),
    F1_idct = idct(F1_dct)
  ) |>
  ggplot(
    aes(t, F1_idct)
  ) +
  geom_point() +
  geom_line()


## -----------------------------------------------------------------------------
#| label: fig-f1-dctsmooth
#| fig-cap: DCT smoothed F1
one_track |>
  mutate(
    F1_dct = dct(F1),
    F1_idct = idct(F1_dct[1:5], n = n())
  ) |>
  ggplot(
    aes(t, F1_idct)
  ) +
  geom_point() +
  geom_line()


## -----------------------------------------------------------------------------
#| warning: false
# focusing on one speaker
one_speaker <- speaker_tracks |>
  filter(speaker == "s01")

dct_smooths <- one_speaker |>
  # step 1, reframing as dct coefficients
  reframe_with_dct(
    F1:F3,
    .token_id_col = id,
    .time_col = t
  ) |>
  # step 2, averaging over parameter number and vowel
  summarise(
    across(F1:F3, mean),
    .by = c(.param, plt_vclass)
  ) |>
  # step 3, reframing with inverse DCT
  reframe_with_idct(
    F1:F3,
    # this time, the id column is the vowel class
    .token_id_col = plt_vclass,
    .param_col = .param
  )


## -----------------------------------------------------------------------------
#| label: fig-dct-smooths
#| fig-cap: DCT smoothed formant trajectories.
dct_smooths |>
  filter(
    plt_vclass %in% c("iy", "ey", "ay", "ay0", "oy")
  ) |>
  ggplot(
    aes(F2, F1)
  ) +
  geom_path(
    aes(
      group = plt_vclass,
      color = plt_vclass
    ),
    arrow = arrow()
  ) +
  scale_y_reverse() +
  scale_x_reverse()


## -----------------------------------------------------------------------------
basis <- dct_basis(100, 5)
matplot(basis, type = "l", lty = 1, lwd = 2)


## -----------------------------------------------------------------------------
dct(one_track$F1)[1:5]


## -----------------------------------------------------------------------------
lm(
  one_track$F1 ~ dct_basis(20, 5) - 1
) |>
  coef()

