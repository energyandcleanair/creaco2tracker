# Build-time dependency installation shared by the production and rr images.
options(repos = c(CRAN = Sys.getenv("CRAN")), Ncpus = 1)
Sys.setenv(PKG_SYSREQS = "false")
options(pkg.sysreqs = FALSE)
stopifnot(pak::sysreqs_is_supported())

check_system_requirements <- function() {
  sysreqs <- pak::sysreqs_check_installed()
  if (any(!sysreqs$installed)) {
    print(sysreqs[!sysreqs$installed, ])
    stop("Add missing system dependencies to the Dockerfile apt-get layer", call. = FALSE)
  }
}

# Check the base library and all planned dependencies before installing R packages.
check_system_requirements()
requirements <- c(
  unlist(pak::pkg_sysreqs(c("devtools", "plotly"), upgrade = FALSE)$packages$system_packages),
  unlist(pak::pkg_sysreqs("deps::.", upgrade = FALSE,
                        dependencies = TRUE)$packages$system_packages)
)
installed <- pak::sysreqs_list_system_packages()
missing <- setdiff(unique(requirements), installed$package[installed$status == "ii"])
if (length(missing) > 0L) {
  stop(paste0("Add missing system dependencies to the Dockerfile apt-get layer: ",
              paste(sort(missing), collapse = ", ")), call. = FALSE)
}

# System packages are managed exclusively in the earlier apt-get layer.
pak::pkg_install(c("devtools", "plotly"), upgrade = FALSE, ask = FALSE)
pak::local_install_deps(dependencies = TRUE, upgrade = FALSE, ask = FALSE)
check_system_requirements()

# Use a fresh process: pak can upgrade packages already loaded by the installer.
packages <- c("creahelpers", "rcrea", "sf", "terra", "magick", "units",
              "arrow", "RPostgres", "devtools")
check <- paste0("for (package in ", paste(deparse(packages), collapse = "\n"),
                ") loadNamespace(package)")
status <- system2(
  command = file.path(R.home("bin"), "Rscript"),
  args = c("--vanilla", "-e", shQuote(check))
)
if (status != 0L) {
  stop("Image dependency load check failed")
}
