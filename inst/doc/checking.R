## -----------------------------------------------------------------------------
#| label: setup
library(tidynorm)


## -----------------------------------------------------------------------------
norm_data <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_norm1",
    .silent = T
  ) |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_norm2",
    .silent = T
  )


## -----------------------------------------------------------------------------
check_norm(norm_data)

