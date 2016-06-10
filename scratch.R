library(V8)
ct <- v8()
ct$source("./bundle.js")
?ct

start <- "ggplot is great"
changed <- "ggplot2 is the greatest"

result <- ct$call("jsdiff.diffWords", start, changed)
result


start <- clipr::read_clip()
end <- start
