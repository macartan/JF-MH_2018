# 1. Load libraries
if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(knitr)
library(RWordPress)
library(readr)

# Paths and Login
path2file <- "C:/Users/medina/Dropbox/Liberia_Gender_Clean/oup_rep/"
html.file = paste0(path2file,"20171204_tables_code_post.html")

text = paste(read_lines(html.file, skip = 18, n_max = 901 ), collapse = "\n")

options(WordpressLogin = c(username = 'password'),
        WordpressURL = 'http://www.macartan.nyc/xmlrpc.php')

# create or edit post
newPost(list(description = text, title = "Why do women cooperate more in women\'s groups: Replication code?", categories = c("Code", "Replication")), publish = FALSE)
#editPost(content = text, postid = 723)

