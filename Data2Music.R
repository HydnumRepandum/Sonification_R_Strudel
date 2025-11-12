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

source("MakeSoundScale.R")

source("Sonification.R")

library(tuneR)
library(dplyr)
library(magrittr)

data <- read.csv("~/Desktop/strudel_composition/dataset/Deliberative Democracy Index.csv")
#data <- read.csv("~/Desktop/strudel_composition/dataset/Freedom of expression index.csv")
#data <- read.csv("~/Desktop/strudel_composition/dataset/Liberal Democracy Index.csv")


data_xy <- subset(data, select = c("Year", "X.World"))

# Example data loading

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

fs <- 44100
KEYS <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")


safe_key <- function(k) {
  k %>%
    gsub("#", "s", ., fixed = TRUE) %>%  # F# -> Fs
    identity()                           # "b" doesn't need changing
}
# ---- your playback helper (unchanged)
play_signal <- function(x, fs, repeats = 1, pause = 0) {
  for (i in seq_len(repeats)) {
    audio::play(x, rate = fs)
    Sys.sleep(length(x) / fs + pause)
  }
}

# ---- main: render, play, and save for each key
render_all_keys <- function(data_xy,
                            fs,
                            keys         = KEYS,
                            sequence     = 12,
                            startseq     = 2,
                            span         = 14,
                            duration     = 0.25,
                            octave       = 1,
                            outfile_base = "ddi_demo",
                            pause_between = 0.5,
                            write_concat  = TRUE,
                            concat_outfile = "ddi_demo_allkeys.wav",
                            concat_pause_sec = 0.35) {
  
  # precompute start/end on x-axis
  startx <- min(data_xy[,1], na.rm = TRUE)
  endx   <- max(data_xy[,1], na.rm = TRUE)
  
  all_segments <- list()
  
  for (k in keys) {
    message(sprintf("Key: %s", k))
    
    # 1) build signal for this key
    sig <- data2music(
      data      = data_xy,
      startx    = startx,
      endx      = endx,
      sequence  = sequence,
      startseq  = startseq,
      key       = k,          # <- key looped here
      span      = span,
      fs        = fs,
      duration  = duration,
      octave    = octave,
      outfile   = paste0(outfile_base, "_", safe_key(k)),
      abc       = 0           # set to 1 only if your implementation writes ABC
    )
    
    # 2) normalize defensively to avoid clipping in extreme keys
    sig <- as.numeric(sig)
    peak <- max(1e-9, max(abs(sig), na.rm = TRUE))
    sig  <- sig / peak
    
    # 3) play
    play_signal(sig, fs, repeats = 1, pause = pause_between)
    
    # 4) save per-key WAV
    #w <- Wave(left = as.integer(sig * 32767), samp.rate = fs, bit = 16)
    #wav_path <- paste0(outfile_base, "_", safe_key(k), ".wav")
    #writeWave(w, wav_path)
    #message(sprintf("  -> wrote %s", wav_path))
    
    # 5) store for optional concatenated export
    if (write_concat) {
      silence <- rep(0, max(1, round(concat_pause_sec * fs)))
      all_segments[[length(all_segments) + 1]] <- sig
      all_segments[[length(all_segments) + 1]] <- silence
    }
  }
  
  # 6) optional: one long file cycling all keys with short silences
  if (write_concat && length(all_segments) > 0) {
    full <- do.call(c, all_segments)
    # normalize final export (just in case)
    full <- full / max(1e-9, max(abs(full), na.rm = TRUE))
    w_all <- Wave(left = as.integer(full * 32767), samp.rate = fs, bit = 16)
    writeWave(w_all, concat_outfile)
    message(sprintf("  -> wrote concatenated: %s", concat_outfile))
  }
  
  invisible(TRUE)
}

# ---- run it
render_all_keys(
  data_xy      = data_xy,
  fs           = fs,
  keys         = KEYS,           # 12 keys; switch to a subset if you like
  sequence     = 8,
  startseq     = 2,
  span         = 14,
  duration     = 0.25,
  octave       = 1,
  outfile_base = "ddi_demo",
  pause_between = 0.5,
  write_concat  = TRUE,
  concat_outfile = "ddi_demo_allkeys.wav",
  concat_pause_sec = 0.35
)

##################################### If you want to hear just one key (e.g., C) ################################

sig <- data2music(
  data      = data_xy,
  startx    = min(data_xy[,1], na.rm=TRUE),
  endx      = max(data_xy[,1], na.rm=TRUE),
  sequence  = 12,          # number of notes
  startseq  = 2,           # starting scale degree
  key       = "F",         # musical key
  span      = 14,          # ~2 octaves
  fs        = fs,          
  duration  = 0.25,        # seconds per note
  octave    = 1,           # transposition
  outfile   = "ddi_chosen",  # base name used by sonification() if it writes ABC
  abc       = 0            # depends on your sonification() implementation
)

# 4) Listen/save
# Convert numeric [-1,1] to 16-bit Wave and play/save
w <- Wave(left = as.integer(sig * 32767), samp.rate = fs, bit = 16)
# listen (on some systems)

play_signal <- function(x, fs, repeats = 1, pause = 0) {
  # x is a numeric vector in [-1, 1]
  for (i in seq_len(repeats)) {
    audio::play(x, rate = fs)         # streams directly, no temp file
    Sys.sleep(length(x) / fs + pause) # wait until playback ends
  }
}
play_signal(sig, fs, repeats = 1, pause = 0.5)

writeWave(w, "ddi_chosen.wav")



