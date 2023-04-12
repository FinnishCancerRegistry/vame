
s1 <- git2r::status()
source("data-raw/sysdata.R")
s2 <- git2r::status()
if (!identical(s1, s2)) {
  git2r::add(path = "R/sysdata.rda")
  git2r::commit(message = "build: run dev/01_data.R")
}
