s1 <- git2r::status()
stopifnot(
  vapply(s1, length, integer(1L)) == 0L
)
usethis::use_mit_license()
unlink(".github", recursive = TRUE, force = TRUE)
usethis::use_github_action(name = "check-release")
desc::desc_normalize()
Sys.sleep(5)
s2 <- git2r::status()

if (!identical(s1, s2)) {
  system2("git", c("add", "--all"))
  system2("git", c("commit", "-m", "\"build: run dev/01_license_and_ci.R\""))
}
