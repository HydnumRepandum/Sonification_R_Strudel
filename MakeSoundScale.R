#' Convert scale indices into an audio signal
#'
#' @param new_scale Numeric vector with frequencies of the selected scale.
#' @param scale_indices Integer vector mapping each data point to a note in
#'   `new_scale`.
#' @param fs Sampling rate in Hz.
#' @param duration Duration in seconds of each note.
#' @param octave Multiplier to shift the notes by octaves (default 1).
#'
#' @return Numeric vector representing the concatenated waveform.
make_sound_scale <- function(new_scale,
                             scale_indices,
                             fs,
                             duration,
                             octave = 1) {
  if (!is.numeric(new_scale) || length(new_scale) == 0) {
    stop("`new_scale` must be a non-empty numeric vector.")
  }
  if (!is.numeric(scale_indices) || length(scale_indices) == 0) {
    stop("`scale_indices` must be a non-empty numeric vector.")
  }
  if (!is.numeric(fs) || fs <= 0) {
    stop("`fs` must be a positive numeric sampling rate.")
  }
  if (!is.numeric(duration) || duration <= 0) {
    stop("`duration` must be a positive numeric value.")
  }
  if (!is.numeric(octave) || octave <= 0) {
    stop("`octave` must be a positive numeric multiplier.")
  }

  step <- 1 / fs
  t <- seq(0, duration - step, by = step)
  fade_duration <- 0.01
  fade_samples <- min(length(t), max(1L, round(fade_duration * fs)))
  fade_in <- seq(0, 1, length.out = fade_samples)
  fade_out <- seq(1, 0, length.out = fade_samples)

  signal <- numeric(0)
  for (idx in scale_indices) {
    freq <- new_scale[idx] * octave
    tone <- sin(2 * pi * freq * t)
    tone[seq_len(fade_samples)] <- tone[seq_len(fade_samples)] * fade_in
    tone[(length(tone) - fade_samples + 1):length(tone)] <-
      tone[(length(tone) - fade_samples + 1):length(tone)] * fade_out
    signal <- c(signal, tone)
  }

  signal
}
