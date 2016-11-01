## Update Due To Upstream Dependency Changes
Hello. Sorry for another update request so soon, but I received a downstream
depenency failure message, as changes in ggplot2 v2.2.0
were causing this package's build to fail. 
Also made some minor changes to DESCRIPTION Description and Copyright fields
based on feedback from Uwe on a submission for a different package. 

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
