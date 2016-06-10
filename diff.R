#<pre id="result">ggplot(data = df, aes(x = Weight)) +
#  geom_histogram(binwidth = <span style="color:red;"><del>4</del></span><ins>2</ins>)</pre>

# below are before and after things to manually copy
ggplot(data = sd_oral_richpk, aes(x = Weight, group = Gender)) + 
  geom_histogram(binwidth = 4, color = "black", fill = "grey") + 
  theme_bw() + 
  base_theme()

pts <- clipr::read_clip()


ggplot(data = sd_oral_richpk, aes(x = Weight, group = factor(Gender))) + 
  geom_histogram(binwidth = 4, color = "black", fill = "grey") + 
  labs(
    x="Weight (kg)", 
    y = "Count") + 
  theme_bw() + 
  base_theme()
pte <- clipr::read_clip()
  
ggplot(data = sd_oral_richpk, aes(x = Weight)) + 
  geom_histogram() + 
  theme_bw() + 
  base_theme()

PRE_START <- "<pre id=\"result\">" 
PRE_END <- "</pre>"

result <- ct$call("jsdiff.diffWords", paste0(pts, collapse = "\n"), paste0(pte, collapse = "\n"))
result

add_diff_html <- function(x) {
 if(isTRUE(x$removed)) {
   return(paste0("<span style=\"background:rgba(255, 0, 0, 0.5);\"><del>", x$value, "</del></span>"))
 } else if (isTRUE(x$added)) {
   return(paste0("<span style=\"background:rgba(127, 255, 0, 0.5);\"><ins>", x$value, "</ins></span>"))
 } else {
   return(x$value)
 }
}
body <- purrr::by_row(result, add_diff_html, .collate = "row", .to="html")
body_html <- paste0(body$html, collapse="")
cat(paste0(PRE_START, body_html,  PRE_END))
