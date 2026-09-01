perils <- c("TC", "SU", "PF", "IF", "PF_SU_TC")
comps <- c("TC", "SU", "PF", "IF")

cols <- c(
  TC = "#4AA57F",
  SU = "#E07E3C",
  PF = "#F0B323",
  IF = "#C6AA76",
  PF_SU_TC = "#D1495B"
)

load_aal <- function(p) {
  d <- read_csv(p, show_col_types = FALSE)

  need <- c("State", "Peril", "LossPersp", "Commercial", "Personal", "Total")
  missing <- setdiff(need, names(d))

  if (length(missing)) {
    stop("AAL CSV missing: ", paste(missing, collapse = ", "))
  }

  d %>%
    mutate(
      State = toupper(State),
      Peril = toupper(Peril),
      LossPersp = toupper(LossPersp),
      across(c(Commercial, Personal, Total), as.numeric)
    )
}

load_ep <- function(p) {
  d <- read_csv(p, show_col_types = FALSE)

  need <- c("State", "LossPerspective", "EPType", "Peril", "LOB", "RP", "Loss")
  missing <- setdiff(need, names(d))

  if (length(missing)) {
    stop("EP CSV missing: ", paste(missing, collapse = ", "))
  }

  d %>%
    mutate(
      State = toupper(State),
      LossPerspective = toupper(LossPerspective),
      EPType = toupper(EPType),
      Peril = toupper(Peril),
      LOB = as.character(LOB),
      RP = as.numeric(RP),
      Loss = as.numeric(Loss)
    )
}

fmt <- function(x) {
  ifelse(
    is.na(x),
    "N/A",
    scales::label_dollar(
      scale_cut = scales::cut_short_scale(),
      accuracy = 0.1
    )(x)
  )
}


aal_long <- function(d, state, loss, lob) {
  d %>%
    filter(State == state, LossPersp == loss) %>%
    select(State, Peril, all_of(lob)) %>%
    rename(Value = all_of(lob))
}

vals <- function(x) {
  setNames(
    sapply(comps, function(p) {
      z <- x$Value[x$Peril == p]
      if (length(z)) sum(z, na.rm = TRUE) else NA_real_
    }),
    comps
  )
}

shares_aal <- function(d, loss, lob, selected_perils, inc = TRUE) {
  x <- d %>%
    filter(
      LossPersp == loss,
      Peril %in% selected_perils
    ) %>%
    select(
      State,
      Peril,
      all_of(lob)
    ) %>%
    rename(Value = all_of(lob)) %>%
    tidyr::complete(
      State,
      Peril = selected_perils,
      fill = list(Value = 0)
    ) %>%
    group_by(State) %>%
    mutate(
      Sum = sum(Value, na.rm = TRUE),
      Share = ifelse(Sum > 0, Value / Sum, 0)
    ) %>%
    ungroup()

  if (!inc) {
    x <- x %>%
      filter(State != "US")
  }

  x
}

ep_slice <- function(d, state, loss, lob, type, rp) {
  d %>%
    filter(
      State == state,
      LossPerspective == loss,
      LOB == lob,
      EPType == type,
      RP == rp
    )
}

ep_shares <- function(d, loss, lob, type, rp, selected_perils) {
  d %>%
    filter(
      LossPerspective == loss,
      LOB == lob,
      EPType == type,
      RP == rp,
      Peril %in% selected_perils
    ) %>%
    tidyr::complete(
      State,
      Peril = selected_perils,
      fill = list(Loss = 0)
    ) %>%
    group_by(State) %>%
    mutate(
      Sum = sum(Loss, na.rm = TRUE),
      Share = ifelse(Sum > 0, Loss / Sum, 0)
    ) %>%
    ungroup()
}
