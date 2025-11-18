#!/usr/bin/env Rscript
#' Generate a MIDI CC automation curve from time/value data
#'
#' This script mirrors the data-to-note workflow from Data2Music.R but instead of
#' sonifying the values as notes (see data2music()), it maps a numeric curve to a
#' MIDI Control Change lane that can be imported into a DAW such as Ableton Live.

# =======================================
# Load CSV data (Year + china) if present
# =======================================
# Put your CSV path here. The file should have at least two columns named
# "Year" and "china". Year will be used as the timeline; china will be mapped
# to CC values after normalization.
curve_csv <- "china_curve.csv"

load_curve_from_csv <- function(path,
                                time_col = "Year",
                                value_col = "china",
                                seconds_per_unit = 1,
                                normalize = TRUE) {
  if (!file.exists(path)) {
    stop(sprintf("File '%s' was not found.", path))
  }

  df_raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c(time_col, value_col) %in% names(df_raw))) {
    stop(sprintf("CSV must contain '%s' and '%s' columns.", time_col, value_col))
  }

  time_vals <- as.numeric(df_raw[[time_col]])
  if (anyNA(time_vals)) {
    stop(sprintf("Column '%s' must be numeric (Year values).", time_col))
  }

  # Convert the Year axis to seconds by subtracting the minimum and scaling by
  # `seconds_per_unit` (so 1 year difference becomes `seconds_per_unit` seconds).
  time_sec <- (time_vals - min(time_vals)) * seconds_per_unit

  value <- as.numeric(df_raw[[value_col]])
  if (anyNA(value)) {
    stop(sprintf("Column '%s' must be numeric (automation values).", value_col))
  }

  if (normalize) {
    rng <- range(value)
    if (diff(rng) == 0) {
      warning("All values are identical; using zeros for the automation curve.")
      value_norm <- rep(0, length(value))
    } else {
      value_norm <- (value - rng[1]) / diff(rng)
    }
  } else {
    value_norm <- value
  }

  data.frame(time_sec = time_sec, value_norm = value_norm)
}

# =============================================
# Example data fallback (if CSV not found) -----
# =============================================
if (file.exists(curve_csv)) {
  message(sprintf("Loading curve data from '%s'...", curve_csv))
  # seconds_per_unit controls how quickly the Year axis unfolds in seconds.
  df <- load_curve_from_csv(curve_csv, seconds_per_unit = 1)
} else {
  message("CSV not found; generating demo curve instead.")
  set.seed(42)
  raw_curve <- sin(seq(0, 4 * pi, length.out = 60)) + runif(60, min = -0.1, max = 0.1)
  raw_curve <- raw_curve - min(raw_curve)
  raw_curve <- raw_curve / max(raw_curve)
  df <- data.frame(
    time_sec   = seq(0, 12, length.out = 60),          # seconds from 0 onward
    value_norm = raw_curve
  )
}

# Make sure the curve stays in [0, 1]
df$value_norm <- pmin(pmax(df$value_norm, 0), 1)

# ==================================
# Helper functions for MIDI writing
# ==================================
# Convert an integer to a vector of big-endian bytes
i2bytes <- function(value, size) {
  stopifnot(size >= 1)
  ints <- vapply((size - 1):0, function(shift) {
    bitwAnd(bitwShiftR(as.integer(value), 8 * shift), 0xFFL)
  }, integer(1))
  as.integer(ints)
}

# Convert ticks to MIDI variable-length quantity encoding
vlq <- function(value) {
  stopifnot(value >= 0)
  bytes <- value %% 128
  value <- value %/% 128
  while (value > 0) {
    bytes <- c(value %% 128, bytes)
    value <- value %/% 128
  }
  if (length(bytes) > 1) {
    bytes[seq_len(length(bytes) - 1)] <- bytes[seq_len(length(bytes) - 1)] + 128
  }
  bytes
}

# Write a complete Standard MIDI file containing a single track
write_cc_midi <- function(events, bpm, ppq, channel, cc_number, outfile) {
  microsec_per_quarter <- round(60000000 / bpm)
  tempo_event <- c(0x00, 0xFF, 0x51, 0x03, i2bytes(microsec_per_quarter, 3))

  status_byte <- 0xB0 + (channel - 1)
  cc_events <- unlist(lapply(seq_len(nrow(events)), function(i) {
    delta <- vlq(events$delta_ticks[i])
    c(delta, status_byte, cc_number, events$value[i])
  }))

  end_event <- c(0x00, 0xFF, 0x2F, 0x00)
  track_data <- c(tempo_event, cc_events, end_event)

  track_chunk <- c(as.integer(charToRaw("MTrk")), i2bytes(length(track_data), 4), track_data)

  header_chunk <- c(
    as.integer(charToRaw("MThd")),
    i2bytes(6, 4),            # header length
    i2bytes(0, 2),            # format 0 (single track)
    i2bytes(1, 2),            # number of tracks
    i2bytes(ppq, 2)           # division / PPQ
  )

  midi_bytes <- as.raw(c(header_chunk, track_chunk))
  writeBin(midi_bytes, outfile)
  invisible(outfile)
}

# =========================================================
# Main conversion routine (wrap everything in a neat helper)
# =========================================================
curve_to_midi <- function(df,
                          bpm = 120,
                          ppq = 480,
                          cc_number = 1,
                          channel = 1,
                          outfile = "automation_curve.mid") {
  if (!all(c("time_sec", "value_norm") %in% names(df))) {
    stop("df must contain `time_sec` and `value_norm` columns.")
  }
  if (channel < 1 || channel > 16) {
    stop("`channel` must be between 1 and 16 (inclusive).")
  }
  if (cc_number < 0 || cc_number > 127) {
    stop("`cc_number` must be between 0 and 127.")
  }

  df <- df[order(df$time_sec), ]

  # Map time (sec) -> beats -> ticks
  # beats = time_sec * bpm / 60 converts absolute seconds into quarter-note units
  beats <- df$time_sec * bpm / 60
  # ticks = round(beats * ppq) resolves beats into MIDI ticks using the PPQ setting
  ticks <- round(beats * ppq)

  # Map normalized value [0,1] -> MIDI CC [0,127]
  # MIDI CC values must be integers, so we round after scaling to 0-127
  midi_values <- round(df$value_norm * 127)
  midi_values <- pmin(pmax(midi_values, 0), 127)

  # Build event data frame with delta times
  events <- data.frame(
    ticks = ticks,
    value = midi_values
  )
  events$delta_ticks <- c(events$ticks[1], diff(events$ticks))

  write_cc_midi(events, bpm, ppq, channel, cc_number, outfile)

  message(sprintf("Wrote %s with %d CC events (CC%d on channel %d)",
                  outfile, nrow(events), cc_number, channel))
  print(utils::head(data.frame(time_ticks = events$ticks,
                               delta_ticks = events$delta_ticks,
                               cc_value = events$value)))
}

# ========================
# Run the demonstration ---
# ========================
# Tweak bpm, ppq, cc_number, channel, or outfile here to suit your session
curve_to_midi(df,
              bpm = 120,
              ppq = 480,
              cc_number = 1,
              channel = 1,
              outfile = "automation_curve.mid")
