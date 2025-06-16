## -----------------------------------------------------------------------------
#| eval: !expr 'rlang::is_installed("magick")'
#| echo: false
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)


## -----------------------------------------------------------------------------
#| label: setup
library(tidynorm)
library(dplyr)


## -----------------------------------------------------------------------------
point_norm <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker
  )


## -----------------------------------------------------------------------------
track_norm <- speaker_tracks |>
  norm_track_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )


## -----------------------------------------------------------------------------
dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )


## -----------------------------------------------------------------------------
point_norm <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker
  )


## -----------------------------------------------------------------------------
track_norm <- speaker_tracks |>
  norm_track_nearey(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )


## -----------------------------------------------------------------------------
dct_norm <- speaker_tracks |>
  mutate(across(F1:F3, log)) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_nearey(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )


## -----------------------------------------------------------------------------
point_norm <- speaker_data |>
  norm_deltaF(
    F1:F3,
    .by = speaker
  )


## -----------------------------------------------------------------------------
track_norm <- speaker_tracks |>
  norm_track_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )


## -----------------------------------------------------------------------------
dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )


## -----------------------------------------------------------------------------
point_norm <- speaker_data |>
  norm_wattfab(
    F1:F3,
    .by = speaker
  )


## -----------------------------------------------------------------------------
track_norm <- speaker_tracks |>
  norm_track_wattfab(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )


## -----------------------------------------------------------------------------
dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_wattfab(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )


## -----------------------------------------------------------------------------
point_norm <- speaker_data |>
  norm_barkz(
    F1:F3,
    .by = speaker
  )


## -----------------------------------------------------------------------------
track_norm <- speaker_tracks |>
  norm_track_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )


## -----------------------------------------------------------------------------
dct_norm <- speaker_tracks |>
  mutate(
    across(F1:F3, hz_to_bark)
  ) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )

