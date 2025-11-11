#' Sonification of numeric data into musical notation
#'
#' @param y_data Numeric vector representing the data series.
#' @param starting_seq Integer indicating the starting note (1-7) within the key.
#' @param key Character string naming the major key (e.g. "C", "Db", "F").
#' @param seq_length Integer specifying the tonal span of the sequence.
#' @param file_name Character string without extension used for the output file.
#' @param flag Integer toggle: 0 writes only the ABC file; 1 also calls
#'   `abcm2ps` and `ps2pdf` to produce a PDF (requires the external tools).
#'
#' @return A list with `scale_indices`, `new_scale`, and `note_labels`.
sonification <- function(y_data,
                         starting_seq,
                         key,
                         seq_length,
                         file_name,
                         flag = 0) {
  if (!is.numeric(y_data) || length(y_data) == 0) {
    stop("`y_data` must be a non-empty numeric vector.")
  }
  if (!is.numeric(starting_seq) || length(starting_seq) != 1) {
    stop("`starting_seq` must be a single numeric value.")
  }
  if (!is.character(key) || length(key) != 1) {
    stop("`key` must be a single character string.")
  }
  if (!is.numeric(seq_length) || length(seq_length) != 1) {
    stop("`seq_length` must be a single numeric value.")
  }
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("`file_name` must be a single character string.")
  }
  if (!is.numeric(flag) || length(flag) != 1) {
    stop("`flag` must be a single numeric value (0 or 1).")
  }

  sequence <- length(y_data)
  if (sequence %% 16 != 0) {
    warning("Length of the data series is not a multiple of 16; ABC output " ,
            "may be truncated.")
  }
  lines <- ceiling(sequence / 16)

  scales <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")
  scale_start <- c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7)
  scale_choice <- match(key, scales)
  if (is.na(scale_choice)) {
    stop("Key '", key, "' is not supported. Choose from: ",
         paste(scales, collapse = ", "))
  }

  scale_freq <- matrix(
    c(
      65.41, 73.42, 82.41, 87.31, 98.00, 110.00, 123.47,
      130.81, 146.83, 164.81, 174.61, 196.00, 220.00, 246.94,
      261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88,
      523.25, 587.33, 659.25, 698.46, 783.99, 880.00, 987.77,

      69.30, 77.78, 87.31, 92.50, 103.83, 116.54, 130.81,
      138.59, 155.56, 174.61, 185.00, 207.65, 233.08, 261.63,
      277.18, 311.13, 349.23, 369.99, 415.30, 466.16, 523.25,
      554.37, 622.25, 698.46, 739.99, 830.61, 932.33, 1046.50,

      73.42, 82.41, 92.50, 98.00, 110.00, 123.47, 138.59,
      146.83, 164.81, 185.00, 196.00, 220.00, 246.94, 277.18,
      293.66, 329.63, 369.99, 392.00, 440.00, 493.88, 554.37,
      587.33, 659.25, 739.99, 783.99, 880.00, 987.77, 1108.73,

      77.78, 87.31, 98.00, 103.83, 116.54, 130.81, 146.83,
      155.56, 174.61, 196.00, 207.65, 233.08, 261.63, 293.66,
      311.13, 349.23, 392.00, 415.30, 466.16, 523.25, 587.33,
      622.25, 698.46, 783.99, 830.61, 932.33, 1046.50, 1174.66,

      82.41, 92.50, 103.83, 110.00, 123.47, 138.59, 155.56,
      164.81, 185.00, 207.65, 220.00, 246.94, 277.18, 311.13,
      329.63, 369.99, 415.30, 440.00, 493.88, 554.37, 622.25,
      659.25, 739.99, 830.61, 880.00, 987.77, 1108.73, 1244.51,

      87.31, 98.00, 110.00, 116.54, 130.81, 146.83, 164.81,
      174.61, 196.00, 220.00, 233.08, 261.63, 293.66, 329.63,
      349.23, 392.00, 440.00, 466.16, 523.25, 587.33, 659.25,
      698.46, 783.99, 880.00, 932.33, 1046.50, 1174.66, 1318.51,

      92.50, 103.83, 116.54, 130.81, 138.59, 155.56, 174.61,
      185.00, 207.65, 233.08, 261.63, 277.18, 311.13, 349.23,
      369.99, 415.30, 466.16, 554.37, 622.25, 698.46, 783.99,
      830.61, 932.33, 1046.50, 1108.73, 1244.51, 1396.91, 1567.98,

      98.00, 110.00, 123.47, 130.81, 146.83, 164.81, 185.00,
      196.00, 220.00, 246.94, 261.63, 293.66, 329.63, 369.99,
      392.00, 440.00, 493.88, 523.25, 587.33, 659.25, 739.99,
      783.99, 880.00, 987.77, 1046.50, 1174.66, 1318.51, 1479.98,

      103.83, 116.54, 130.81, 138.59, 155.56, 174.61, 196.00,
      207.65, 233.08, 261.63, 277.18, 311.13, 349.23, 392.00,
      415.30, 466.16, 523.25, 554.37, 622.25, 698.46, 783.99,
      830.61, 932.33, 1046.50, 1108.73, 1244.51, 1396.91, 1567.98,

      110.00, 123.47, 138.59, 146.83, 164.81, 185.00, 207.65,
      220.00, 246.94, 277.18, 293.66, 329.63, 369.99, 415.30,
      440.00, 493.88, 554.37, 587.33, 659.25, 739.99, 830.61,
      880.00, 987.77, 1108.73, 1174.66, 1318.51, 1479.98, 1661.22,

      116.54, 130.81, 146.83, 155.56, 174.61, 196.00, 220.00,
      233.08, 261.63, 293.66, 311.13, 349.23, 392.00, 440.00,
      466.16, 523.25, 587.33, 622.25, 698.46, 783.99, 880.00,
      932.33, 1046.50, 1174.66, 1244.51, 1396.91, 1567.98, 1760.00,

      123.47, 138.59, 155.56, 164.81, 185.00, 207.65, 233.08,
      246.94, 277.18, 311.13, 329.63, 369.99, 415.30, 466.16,
      493.88, 554.37, 622.25, 659.25, 739.99, 830.61, 932.33,
      987.77, 1108.73, 1244.51, 1318.51, 1479.98, 1661.22, 1864.66
    ),
    nrow = length(scales),
    byrow = TRUE
  )

  note_labels <- c(
    "C,", "D,", "E,", "F,", "G,", "A,", "B,",
    "C", "D", "E", "F", "G", "A", "B",
    "c", "d", "e", "f", "g", "a", "b",
    "c'", "d'", "e'", "f'", "g'", "a'", "b'"
  )

  starting_seq <- as.integer(starting_seq)
  if (scale_freq[scale_choice, starting_seq] < 98) {
    starting_seq <- starting_seq + 7
  }
  if (scale_freq[scale_choice, starting_seq] > 196) {
    starting_seq <- starting_seq - 7
  }

  x0 <- scale_start[scale_choice] + starting_seq - 1
  x1 <- x0 + seq_length
  if (x1 > ncol(scale_freq)) {
    stop("Requested sequence length exceeds available scale range.")
  }

  new_scale <- scale_freq[scale_choice, x0:x1]
  new_array <- note_labels[x0:x1]

  y_min <- min(y_data)
  y_max <- max(y_data)
  if (isTRUE(all.equal(y_max, y_min))) {
    scale_indices <- rep(1L, length(y_data))
  } else {
    scale_indices <- round(1 + (y_data - y_min) / (y_max - y_min) *
                             (length(new_scale) - 1))
    scale_indices <- pmax(1L, pmin(length(new_scale), scale_indices))
  }

  command <- sprintf("%s.abc", file_name)
  con <- file(command, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c("X: 1",
               "T: Sonification",
               "M: 4/4",
               "L: 1/4",
               sprintf("K: %s", key)),
             con)

  cnt <- 1L
  notes <- new_array[scale_indices]
  for (i in seq_len(lines)) {
    line_content <- character()
    for (j in 1:4) {
      measure_notes <- character()
      for (k in 1:4) {
        if (cnt <= length(notes)) {
          measure_notes <- c(measure_notes, notes[cnt])
          cnt <- cnt + 1L
        }
      }
      line_content <- c(line_content, paste0(measure_notes, collapse = ""), "|")
    }
    writeLines(paste0(line_content, collapse = ""), con)
  }

  if (flag == 1) {
    abc_file <- sprintf("%s.abc", file_name)
    ps_file <- sprintf("%s.ps", file_name)
    abcm2ps_exec <- Sys.which("abcm2ps")
    ps2pdf_exec <- Sys.which("ps2pdf")
    
    if (!nzchar(abcm2ps_exec)) {
      warning("'abcm2ps' executable not found; skipping PostScript/PDF generation.")
    } else {
      status <- tryCatch(
        system2(abcm2ps_exec, c("-O", ps_file, abc_file)),
        warning = function(w) w,
        error = function(e) e
      )
      if (inherits(status, "condition") || !identical(status, 0L)) {
        warning("'abcm2ps' failed; skipping PostScript/PDF generation.")
      } else if (!file.exists(ps_file)) {
        warning("Expected PostScript output '", ps_file, "' was not created.")
      } else if (!nzchar(ps2pdf_exec)) {
        warning("'ps2pdf' executable not found; skipping PDF generation.")
      } else {
        status_pdf <- tryCatch(
          system2(ps2pdf_exec, c(ps_file, sprintf("%s.pdf", file_name))),
          warning = function(w) w,
          error = function(e) e
        )
        if (inherits(status_pdf, "condition") || !identical(status_pdf, 0L)) {
          warning("'ps2pdf' failed to convert '", ps_file, "' to PDF.")
        }
      }
    }
  }

  list(
    scale_indices = scale_indices,
    new_scale = new_scale,
    note_labels = new_array
  )
}

