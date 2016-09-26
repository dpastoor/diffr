PRE_START <- "<pre id=\"result\">" 
PRE_END <- "</pre>"
DASH_COMMENTARY <- "{data-commentary-width=600}"
create_raw_diff <- function(step, next_step) {
  ct <- V8::v8()
  ct$source("./bundle.js")
  result <- ct$call("jsdiff.diffWords", 
          paste0(step, collapse = "\n"), 
          paste0(next_step, collapse = "\n"))
  ## if no lines were added/removed that column will not exist
  POSSIBLE_COLS <- c("added", "removed")
  missing_cols_index <- which(!POSSIBLE_COLS %in% names(result))
  ## add column of NA's 
  if(!(length(missing_cols_index) == length(POSSIBLE_COLS))) {
    for (i in missing_cols_index) {
      result[POSSIBLE_COLS[i]] <- NA
    }
  }
  return(result)
}

str_detect_indices <- function(string, pattern) {
  mstring <- stringr::str_detect(string, pattern)
  if (any(mstring)) {
    indices <- seq_along(mstring)
    return(indices[mstring])
  }
  return(NULL)
}

add_diff_html <- function(x) {
  if(isTRUE(x$removed)) {
    return(paste0("<span style=\"background:rgba(255, 0, 0, 0.5);\"><del>", x$value, "</del></span>"))
  } else if (isTRUE(x$added)) {
    return(paste0("<span style=\"background:rgba(127, 255, 0, 0.5);\"><ins>", x$value, "</ins></span>"))
  } else {
    return(x$value)
  }
}

full_diff_html <- function(result, interactive = FALSE) {
  body <- purrr::by_row(result, add_diff_html, .collate = "row", .to=".html")
  body_html <- paste0(body$.html, collapse="")
  result <- paste0(PRE_START, body_html,  PRE_END)
  if(interactive) {
    cat(result)
  }
  return(result)
}

trim_inward <- function(.x, .num) {
  return(c(.x[1] + .num, .x[2]-.num))
}
## testing 
library(dplyr)
flines <- readr::read_lines("__parsing_dash01.Rmd")

starts <- str_detect_indices(flines, "@start")
ends <- str_detect_indices(flines, "@end")

steps <- data.frame(starts, ends)

extract_trimmed_lines <- function(flines, start, end, trim = 2) {
  c("Plot code:", "", "```r", flines[(start+trim):(end - trim)], "```")
}
setup_diffs <- function(flines, steps) {
  result_list <- list()
  for (si in 1:(nrow(steps)-1)) {
    s1 <- steps %>% slice(si) %>% unlist %>% trim_inward(2)
    s2 <- steps %>% slice(si+1) %>% unlist %>% trim_inward(2)
    raw_diff <- create_raw_diff(flines[s1[1]:s1[2]], flines[s2[1]:s2[2]])
    out <- full_diff_html(raw_diff)
    result_list[[si]] <- out
  }
  return(result_list)
}
diffs <- setup_diffs(flines, steps)
inject_diffs <- function(flines, steps, diffs) {
  ## before any diffs
  output_lines <- flines[1:steps$ends[2]]
  ## each diff
  for (i in seq_along(diffs)) {
    ## add flex dashboard boilerplate
    ## add diff
    output_lines <- c(output_lines, "", "***", "", "Changes:", "", 
                      stringr::str_split(diffs[[i]], "\\n")[[1]],
                      extract_trimmed_lines(flines, steps$starts[i+1], steps$ends[i+1], 2))
    ## don't append more code chunk lines if last one, as none exist
    if (i != length(diffs)) {
      output_lines <- c(output_lines, flines[steps$ends[i+1]:steps$ends[i+2]])
    }
  }
  ## end
  output_lines <- c(output_lines, flines[steps$ends[length(steps$ends)]:length(flines)])
  ## trim all @ based lines
  
  starts <- str_detect_indices(output_lines, "@start")
  ends <- str_detect_indices(output_lines, "@end")
  rm_indices <- c(starts, ends)
  rm_indices <- rm_indices[order(rm_indices)]
  output_lines <- output_lines[-rm_indices]
  result <- add_flex_commentary(output_lines, 
                                str_detect_indices(output_lines, "###"), 
                                DASH_COMMENTARY)
  return(result)
}

str_detect_indices(flines, "###")
add_flex_commentary <- function(lines, indices, injection_text = DASH_COMMENTARY) {
  for (i in indices) {
    lines[i] <- paste(lines[i], injection_text)
  }
  return(lines)
}
add_flex_commentary(flines, str_detect_indices(flines, "###"))
writeLines(inject_diffs(flines, steps, diffs), "parsing_dash01.Rmd")
