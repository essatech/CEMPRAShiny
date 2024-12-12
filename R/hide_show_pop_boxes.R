# hide_show_pop_boxes.R
hide_show_pop_boxes <- function(n_stage) {
  
  # Define helper functions to add or remove classes based on conditions
  update_class <- function(id_prefix, stage, add = TRUE) {
    suffixes <- c("surv", "year", "cr", "mat", "smig", "u", "eps")
    for (suffix in suffixes) {
      full_id_label <- paste0(id_prefix, suffix, "_", stage, "-label")
      full_id <- paste0(id_prefix, suffix, "_", stage)
      if (add) {
        addClass(id = full_id_label, class = "hide-this", asis = TRUE)
        addClass(id = full_id, class = "hide-this", asis = TRUE)
      } else {
        removeClass(id = full_id_label, class = "hide-this", asis = TRUE)
        removeClass(id = full_id, class = "hide-this", asis = TRUE)
      }
    }
  }
  
  
  # Hide or show for stages based on `n_stage`
  if (n_stage < 10) {
    update_class("matrix_model-mm_inputs-", 10, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 10, add = FALSE)
  }
  
  if (n_stage < 9) {
    update_class("matrix_model-mm_inputs-", 9, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 9, add = FALSE)
  }
  
  if (n_stage < 8) {
    update_class("matrix_model-mm_inputs-", 8, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 8, add = FALSE)
  }
  
  if (n_stage < 7) {
    update_class("matrix_model-mm_inputs-", 7, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 7, add = FALSE)
  }
  
  if (n_stage < 6) {
    update_class("matrix_model-mm_inputs-", 6, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 6, add = FALSE)
  }
  
  if (n_stage < 5) {
    update_class("matrix_model-mm_inputs-", 5, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 5, add = FALSE)
  }
  
  if (n_stage < 4) {
    update_class("matrix_model-mm_inputs-", 4, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 4, add = FALSE)
  }
  
  if (n_stage < 3) {
    update_class("matrix_model-mm_inputs-", 3, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 3, add = FALSE)
  }
  
  if (n_stage < 2) {
    update_class("matrix_model-mm_inputs-", 2, add = TRUE)
  } else {
    update_class("matrix_model-mm_inputs-", 2, add = FALSE)
  }
  
  
  
  # if (n_stage < 10) {
  #   addClass(id = "matrix_model-mm_inputs-surv_10-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_10",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_10-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_10",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_10-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_10",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_10-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_10",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_10-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_10",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_10-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_10",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_10-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_10",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_10-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_10",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 9) {
  #   addClass(id = "matrix_model-mm_inputs-surv_9-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_9",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_9-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_9",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_9-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_9",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_9-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_9",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_9-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_9",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_9-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_9",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_9-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_9",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_9-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_9",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 8) {
  #   addClass(id = "matrix_model-mm_inputs-surv_8-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_8",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_8-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_8",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_8-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_8",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_8-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_8",
  #            class = "hide-this",
  #            asis = TRUE)
  #   
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_8-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_8",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_8-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_8",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_8-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_8",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_8-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_8",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 7) {
  #   addClass(id = "matrix_model-mm_inputs-surv_7-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_7",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_7-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_7",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_7-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_7",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_7-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_7",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_7-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_7",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_7-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_7",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_7-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_7",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_7-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_7",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 6) {
  #   addClass(id = "matrix_model-mm_inputs-surv_6-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_6",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_6-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_6",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_6-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_6",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_6-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_6",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_6-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_6",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_6-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_6",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_6-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_6",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_6-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_6",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 5) {
  #   addClass(id = "matrix_model-mm_inputs-surv_5-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_5",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_5-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_5",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_5-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_5",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_5-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_5",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_5-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_5",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_5-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_5",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_5-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_5",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_5-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_5",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 4) {
  #   addClass(id = "matrix_model-mm_inputs-surv_4-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_4",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_4-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_4",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_4-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_4",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_4-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_4",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_4-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_4",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_4-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_4",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_4-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_4",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_4-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_4",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 3) {
  #   addClass(id = "matrix_model-mm_inputs-surv_3-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_3",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_3-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_3",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_3-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_3",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_3-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_3",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_3-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_3",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_3-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_3",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_3-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_3",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_3-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_3",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  # if (n_stage < 2) {
  #   addClass(id = "matrix_model-mm_inputs-surv_2-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-surv_2",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_2-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-year_2",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_2-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-cr_2",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_2-label",
  #            class = "hide-this",
  #            asis = TRUE)
  #   addClass(id = "matrix_model-mm_inputs-mat_2",
  #            class = "hide-this",
  #            asis = TRUE)
  # } else {
  #   removeClass(id = "matrix_model-mm_inputs-surv_2-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-surv_2",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_2-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-year_2",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_2-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-cr_2",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_2-label",
  #               class = "hide-this",
  #               asis = TRUE)
  #   removeClass(id = "matrix_model-mm_inputs-mat_2",
  #               class = "hide-this",
  #               asis = TRUE)
  # }
  
  
  
  
} # end of hide_show_pop_boxes function