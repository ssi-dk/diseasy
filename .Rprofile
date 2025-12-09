# Set CRAN mirror to p3m on github CI
if (Sys.getenv("CI") == "true" && .Platform$OS.type == "unix" && Sys.info()['sysname'] != "Darwin") {
  options(repos = c("CRAN" = "https://p3m.dev/cran/__linux__/noble/latest"))
}
