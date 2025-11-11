#' Convert tabular data into a musical signal
#'
#' @param data Matrix or data frame with at least two numeric columns (time and value).
#' @param startx Numeric start of the interpolation domain.
#' @param endx Numeric end of the interpolation domain.
#' @param sequence Number of interpolated points (and resulting notes).
#' @param startseq Starting note index within the scale (1-7).
#' @param key Major key of the sequence (character string).
#' @param span Tonal span of the sequence (typically 14 for two octaves).
#' @param fs Sampling rate in Hz for the generated signal.
#' @param duration Duration of each tone in seconds.
#' @param octave Octave multiplier applied to the generated frequencies.
#' @param outfile Base name for the generated ABC file.
#' @param abc Integer flag passed to [sonification()].
#'
#' @return Numeric vector containing the normalised audio signal.
data2music <- function(data,
                       startx,
                       endx,
                       sequence,
                       startseq,
                       key,
                       span,
                       fs,
                       duration,
                       octave,
                       outfile,
                       abc) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("`data` must be a matrix or data frame with two numeric columns.")
  }
  data <- as.matrix(data)
  if (ncol(data) < 2) {
    stop("`data` must have at least two columns.")
  }

  time <- seq(startx, endx, length.out = sequence)
  interpolated <- stats::approx(data[, 1], data[, 2], xout = time)$y

  sonif <- sonification(interpolated, startseq, key, span, outfile, abc)
  signal <- make_sound_scale(sonif$new_scale, sonif$scale_indices, fs, duration, octave)
  max_val <- max(abs(signal))
  if (max_val > 0) {
    signal <- signal / max_val
  }

  if (interactive()) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(2, 1))
    graphics::plot(data[, 1], data[, 2], type = "l", xlab = "x", ylab = "y")
    graphics::grid()
    graphics::plot(sonif$scale_indices, type = "o", ylab = "Scale index", xlab = "Step")
    graphics::grid()
  }

  attr(signal, "scale_indices") <- sonif$scale_indices
  attr(signal, "new_scale") <- sonif$new_scale
  signal
}
