# version number: major.minor.patch
usethis::use_version()

# update README.md

# Test package
devtools::test()

devtools::check()
devtools::check_win_devel()
rhub::check()

# Create/update cran comments.md
# create - usethis::use_cran_comments()

# make a package bundle
devtools::build()

# submit a package to CRAN
# https://cran.r-project.org/submit.html

# install from local tar.gz:
devtools::install_local("INSPECTumours_0.1.0.tar.gz")
