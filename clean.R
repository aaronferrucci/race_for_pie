timestr <- function(ms) {
  # convert to s
  seconds <- ms / 1000.0
  hours <- as.integer(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60, digits=2)

  minute_prefix <- ifelse(minutes < 10, "0", "")
  minutes <- paste0(minute_prefix, minutes)
  second_prefix <- ifelse(seconds < 10, "0", "")
  seconds <- paste0(second_prefix, seconds)

  time <- paste(hours, minutes, seconds, sep=":")
  return(time)
}
split_to_ms <- function(split) {
  val <- as.numeric(split)
  seconds <- 3600 * val[1] + 60 * val[2] + val[3]
  ms <- seconds * 1000
  return(ms)
}

hms_to_ms <- function(hms) {
  splits <- strsplit(hms, ':')
  ms <- lapply(splits, FUN=split_to_ms)
  return(unlist(ms))
}

clean <- function(f) {
  raw <- read.csv(f, sep='\n')
  names(raw) <- c("data")
  rec_indices <- grep("^\\d+\\.$", raw$data, perl=T)
  all(diff(rec_indices) >= 8)

  # Extract each runner's data into a data frame.
  data <- data.frame(t(sapply(rec_indices, FUN=function(i) raw[i:(i+7),])))
  names(data) <-
    c("rank", "bib", "name", "sex", "age.group", "elapsed", "gap", "pace")
  data$rank <- sub("\\.", "", data$rank)
  data$rank <- as.integer(data$rank)
  data$bib <- as.integer(data$bib)
  data$name <- as.character(data$name)
  data$elapsed <- as.character(data$elapsed)
  data$elapsed <- hms_to_ms(data$elapsed)
  data$elapsed_str <- timestr(data$elapsed)
  data$gap <- as.character(data$gap)
  data$gap <- sub("--", "0:0", data$gap)
  data$gap <- paste0("0:", data$gap)
  data$gap <- hms_to_ms(data$gap)
  data$gap_str <- timestr(data$gap)
  data$pace <- as.character(data$pace)
  data$pace <- paste0("0:", data$pace)
  data$pace <- hms_to_ms(data$pace)
  data$pace_str <- timestr(data$pace)

  # Translate age group to approximate age
  data$age <- data$age.group
  data$age <- sub("U(\\d)", "\\1", data$age, perl=T)
  data$age <- sub("M(\\d)", "\\1", data$age, perl=T)
  data$age <- sub("W(\\d)", "\\1", data$age, perl=T)

  # Experimentally, prepending a space to single-digit age gives 
  # desirable sorting in the plot axis.
  data$age <- sub("^(.)$", " \\1", data$age)

  one_digit <- length(data$age) == 1
  paste0(" ", data$age[one_digit])
  data$sex <- sub("m", "M", data$sex)
  data$sex <- sub("w", "F", data$sex)

  return(data)
}


