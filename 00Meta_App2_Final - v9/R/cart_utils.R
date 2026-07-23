## =============================================================================
## CART UTILITIES MODULE  (module/cart_utils.R)
## =============================================================================
## Handles persistence of each user's cart to cart/<username>_cart.rds and
## generates unique ids for cart items.
##
## Used by: App.R, vulnerability_server.R, secmod_server.R, cart_server.R
##
## Cart item structure (each element of the saved list):
##   list(
##     id         = "<unique id>",
##     module     = "Vulnerability" | "Secondary Modifier",
##     plot       = <ggplot object>,      # optional but usually present
##     commentary = "<character string>", # optional
##     timestamp  = <POSIXct>
##   )
## =============================================================================

CART_DIR <- "cart"

## -----------------------------------------------------------------------
## Build (and ensure) the path to a user's cart .rds file
## -----------------------------------------------------------------------
get_cart_path <- function(username) {

  if (!dir.exists(CART_DIR)) {
    dir.create(CART_DIR, recursive = TRUE)
  }

  safe_username <- gsub("[^A-Za-z0-9_-]", "_", as.character(username))

  file.path(CART_DIR, paste0(safe_username, "_cart.rds"))
}

## -----------------------------------------------------------------------
## Load a user's cart. Returns an empty list if none exists yet.
## -----------------------------------------------------------------------
load_cart <- function(username) {

  path <- get_cart_path(username)

  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) list()
    )
  } else {
    list()
  }
}

## -----------------------------------------------------------------------
## Save a user's cart back to disk
## -----------------------------------------------------------------------
save_cart <- function(username, cart_items) {

  path <- get_cart_path(username)

  saveRDS(cart_items, path)

  invisible(path)
}

## -----------------------------------------------------------------------
## Generate a unique id for a new cart item (timestamp + random suffix)
## -----------------------------------------------------------------------
generate_item_id <- function() {

  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    sample(1000:9999, 1)
  )
}
