norm_pivot <- function(.data, target_pos, .pre_trans){
  tidyr::pivot_longer(
    .data,
    any_of(names(target_pos)),
    names_to = ".formant_name",
    values_to = ".formant_orig"
  ) |>
    dplyr::mutate(
      .formant_num = name_to_formant_num(!!sym(".formant_name")),
      .formant = .pre_trans(!!sym(".formant_orig"))
    ) |>
    arrange(
      !!sym(".formant_num")
    )
}
