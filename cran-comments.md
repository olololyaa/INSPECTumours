## R CMD check results

0 errors | 0 warnings | 1 note

checking package dependencies ... NOTE

Imports includes 25 non-default packages.

Importing from so many packages makes the package vulnerable to any of them becoming unavailable. Move as many as possible to Suggests and use conditionally.

Comment: We have exceeded the limit a bit because we included several packages to customize the shiny app and packages for statistical modelling as well. We hope that it will be possible to leave the number of used packages as is.

* This is a new release.

