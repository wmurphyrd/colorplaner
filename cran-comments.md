## Request for Early Update
This a request to update the colorplaner package shortly after initial
publication. 

I am requesting this update because the published package contains
incomplete versions of the package vignettes. This occurred because I had 
inadvertently left files in /inst/docs/ from previous test building of vignettes
and was unaware that this would prevent the current vignettes from being 
built. 

I have submitted a pull request for the devtools package to 
add a check for this mistake to prevent it from happening to others: 
https://github.com/hadley/devtools/pull/1362

I apologize for the inefficient use of CRAN volunteer and server resources. 
Thank you for considering this early update. 

## Test environments
* local Windows 10 install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There is one NOTE for CRAN incoming feasibility:
* The spelling of the flagged words is as intended
    * bivariate (correct)
    * choropleth (correct)
    * ggplot (package title)

## Downstream dependencies
There are no downstream dependencies for this package
