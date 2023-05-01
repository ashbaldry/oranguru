#' Checks whether an ailment can be applied
#'
#' @noRd
check_ailment <- function(ailments, new_ailment) {
  if (new_ailment %in% ailments) {
    FALSE
  } else if (new_ailment %in% AILMENT_NON_VOLITILE && isTRUE(any(ailments %in% AILMENT_NON_VOLITILE))) {
    FALSE
  } else {
    TRUE
  }
}

AILMENT_STATUS <- c(
  "PRZ" = 1L,
  "SLP" = 2L,
  "FRZ" = 3L,
  "BRN" = 4L,
  "PSN" = 5L
)

AILMENT_NON_VOLITILE <- c(
  "paralysis" = 1L,
  "sleep" = 2L,
  "freeze" = 3L,
  "burn" = 4L,
  "poison" = 5L
)

AILMENT_VOLITILE <- c(
  "confusion" = 6L,
  "trap" = 8L,
  "disable" = 13L,
  "leech-seed" = 18L
)

AILMENT_CHANGES <- c(
  "paralysed",
  "put to sleep",
  "frozen",
  "burnt",
  "poisoned",
  "confused",
  "infatuated",
  "trapped",
  "put in a nightmare",
  "tormented",
  "disabled",
  "yawned",
  "heal blocked",
  "have their immunity removed",
  "ingrained",
  "embargoed",
  "have perish song applied",
  "leech seeded",
  "silenced",
  "tar shotted"
)
