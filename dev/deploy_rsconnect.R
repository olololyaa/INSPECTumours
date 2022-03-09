# 1. Set up Rsconnect
# https://docs.rstudio.com/connect/user/connecting/

# 2. Create app.R file in the package's root and add:
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
INSPECTumours::run_app()

# 3. Check all dependencies  (@export to functions, import functions from other packages), use use_package() to change DESCRIPTION

# 4. Run in the package directory:
rsconnect::deployApp()
