PRE_START <- "<pre id=\"result\">" 
PRE_END <- "</pre>"

create_raw_diff <- function(step, next_step) {
  ct <- V8::v8()
  ct$source("./bundle.js")
  ct$call("jsdiff.diffWords", 
          paste0(step, collapse = "\n"), 
          paste0(next_step, collapse = "\n"))
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
flines <- readr::read_lines("__parsing_ex01.Rmd")

starts <- str_detect_indices(flines, "@start")
ends <- str_detect_indices(flines, "@end")

steps <- data.frame(starts, ends)


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
    output_lines <- c(output_lines, stringr::str_split(diffs[[i]], "\\n")[[1]])
    ## don't append more code chunk lines if last one, as none exist
    if (i != length(diffs)) {
      output_lines <- c(output_lines, flines[steps$starts[i+2]:steps$ends[i+2]])
    }
  }
  ## end
  output_lines <- c(output_lines, flines[steps$ends[length(steps$ends)]:length(flines)])
  ## trim all @ based lines
  
  starts <- str_detect_indices(output_lines, "@start")
  ends <- str_detect_indices(output_lines, "@end")
  rm_indices <- c(starts, ends)
  rm_indices <- rm_indices[order(rm_indices)]
  output_lines[-rm_indices]
  
}

writeLines(inject_diffs(flines, steps, diffs), "parsing_ex01.Rmd")
