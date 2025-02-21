#' Gravel-Bed River Bank Erosion Model
#'
#' Runs an event hydrograph through a channel and applies an erosion engine
#' to determine lateral erosion.
#'
#' @param hydrograph Event hydrograph carried by the stream, with time units
#' in hours. See `hydrograph()`.
#' @param sx Channel cross sections containing features needed by the
#' erosion engine, such as created by the `sx()` functions such as
#'`sx_ferguson()` or `sx_manning()`.
#' If running the engine `"manning-volume"`, then `sx` needs to have
#' a column named `xs2d` containing the two-dimensional cross section
#' objects extended sufficiently far beyond the banks to accommodate
#' erosion.
#' @param niter Number of time chunks to discretize the hydrograph into.
#' @param engine Paradigm for modelling channel resistance; one of
#' `"ferguson"` for using Ferguson's equation,
#' `"manning-original"` for Manning's equation (original algorithm on
#' simplified channels), or
#' `"manning-volume"` for Manning's equation on 2D cross sections.
#' @param side Which side of the channel to erode? One of
#' `"both"` (erode equally on both sides), `"left"`, or `"right"`
#' (erode entirely on one side of the channel).
#' @details This algorithm implements one of two paradigms for modelling
#' channel resistance.
#'
#' 1. **Manning's Equation**: FILL_THIS_IN
#' 2. **Ferguson's Equation**: FILL_THIS_IN
#'
#' An error is thrown if the input `sx` channel does not have sufficiently
#' described properties.
#'
#' @returns A list of the following components:
#'
#' - `sx`: The original cross section.
#' - `dw_pred`: predicted widening.
#' - `dw_const`: change in width constrained by transport capacity, the most
#    important thing here.
#' - `v_b`: transport capacity * time. Volume of transport that can be moved
#'   by the river. Matrix, with rows representing cross sections corresponding
#'   to the rows of `sx`; columns are time steps.
#' - `resistance`: the resistance paradigm used.
#' @examples
#' library(sf)
#' library(sxchan)
#'
#' ## Set up the channel.
#' cross_sections <- xt_generate_sxc(demo_bankline, n = 20)
#' n <- length(cross_sections)
#' channel <- sx_manning(
#'   cross_sections, grad = 0.02, d50 = 45, d84 = 90,
#'   roughness = append(rep(0.01, n / 2), rep(0.05, n / 2))
#' )
#'
#' ## Create an event hydrograph.
#' hg <- hyd_snow(200, baseflow = 20)
#'
#' ## Run the hydrograph through the channel, using Manning's method.
#' demo_gbem <- gbem(hg, channel, niter = 100, engine = "manning-original")
#'
#' ## Erode the channel
#' (new_channel <- erode(demo_gbem)) # Erosion occurs
#'
#' plot(demo_bankline)
#' plot(st_geometry(new_channel), add = TRUE, col = "blue")
#'
#' ## Run a smaller event through that does no erosion.
#' q <- min(eroding_flow(channel, engine = "manning-original"))
#' hg2 <- hyd_rain(q / 2, baseflow = q / 10)
#' demo_gbem2 <- gbem(hg2, channel, niter = 100, engine = "manning-original")
#' new_channel2 <- erode(demo_gbem2)
#'
#' ## No erosion:
#' identical(st_geometry(channel), st_geometry(new_channel2))
#' @export
gbem <- function(hydrograph, sx, niter = 1000,
                 engine = c("manning-original", "manning-volume", "ferguson"),
                 side = c("both", "left", "right")) {
  engine <- rlang::arg_match(engine)
  side <- rlang::arg_match(side)
  nsx <- nrow(sx)
  event <- discretize_hydrograph(hydrograph, niter)
  peak <- max(event$flow)
  dt <- diff(event$time[1:2])
  # Matrix ROWS are cross sections (i), COLUMNS are time (t).
  erosion <- matrix(nrow = nsx, ncol = niter)
  v_b <- matrix(nrow = nsx, ncol = niter)
  w <- sxchan::xt_width(sf::st_geometry(sx))
  if (engine == "ferguson") {
    stop("Ferguson not available yet.")
  }
  if (engine == "manning-original") {
    if (side != "both") {
      stop(
        "Sorry, the original Manning's method can only accept erosion ",
        "occuring on both sides of the channel."
      )
    }
    grad <- sx[["grad"]]
    d84 <- sx[["d84"]]
    d50 <- sx[["d50"]]
    roughness <- sx[["roughness"]]
    rootdepth <- sx[["rootdepth"]]
    dw_pred <- numeric()
    for (i in seq_len(nsx)) {
      dw_pred[i] <- gbem0_manning(
        peak, dt, width = w[i], grad = grad[i], d50 = d50[i],
        d84 = d84[i], roughness = roughness[i], rootdepth = rootdepth[i]
      )$dw_pred
      for (t in seq_len(niter)) {
        current_flow <- event$flow[t]
        gbem_ <- gbem0_manning(
          current_flow, dt, width = w[i], grad = grad[i], d50 = d50[i],
          d84 = d84[i], roughness = roughness[i], rootdepth = rootdepth[i]
        )
        erosion[i, t] <- gbem_$dw_const
        v_b[i, t] <- gbem_$v_b
        w[i] <- w[i] + gbem_$dw_const
      }
    }
    dw_const <- apply(erosion, 1, sum)
    v_b_total <- apply(v_b, 1, sum)
  }
  if (engine == "manning-volume") {
    grad <- sx[["grad"]]
    d84 <- sx[["d84"]]
    d50 <- sx[["d50"]]
    roughness <- sx[["roughness"]]
    rootdepth <- sx[["rootdepth"]]
    xs2d <- sx[["xs2d"]]
    dw_pred <- numeric()
    for (i in seq_len(nsx)) {
      dw_pred[i] <- gbem0_manning_with_volume(
        peak, dt, xs2d = xs2d[[i]], grad = grad[i], d50 = d50[i],
        d84 = d84[i], roughness = roughness[i], rootdepth = rootdepth[i],
        side = side
      )$dw_pred
      current_xs2d <- xs2d[[i]]
      for (t in seq_len(niter)) {
        current_flow <- event$flow[t]
        gbem_ <- gbem0_manning_with_volume(
          current_flow, dt, xs2d = current_xs2d, grad = grad[i], d50 = d50[i],
          d84 = d84[i], roughness = roughness[i], rootdepth = rootdepth[i],
          side = side
        )
        erosion[i, t] <- gbem_$dw_const
        v_b[i, t] <- gbem_$v_b
        current_xs2d <- gbem_$xs2d_const
      }
    }
    dw_const <- apply(erosion, 1, sum)
    v_b_total <- apply(v_b, 1, sum)
  }
  l <- list(
    sx = sx,
    dw_pred = dw_pred,
    dw_const = dw_const,
    v_b = v_b,
    engine = engine
  )
  new_gbem(l)
}
