## R CMD check results

0 errors | 0 warnings | 1 note

checking package dependencies ... NOTE

Imports includes 25 non-default packages.

Importing from so many packages makes the package vulnerable to any of them becoming unavailable. Move as many as possible to Suggests and use conditionally.

Comment: this is a package to generate shiny tool for statistical modelling. We have used some shiny packages, including rmarkdown shiny shinyalert shinyFeedback shinyjs shinytoastr shinyvalidate, for the shiny interface and several packages for different statistical models.   


* This is a new release.

