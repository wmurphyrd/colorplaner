
readme <- readLines("vignettes/colorplaner.Rmd")
loc <- grep("^---$", readme)

ins <- c("[![BuildStatus](https://travis-ci.org/wmurphyrd/colorplaner.svg?branch=master)](https://travis-ci.org/wmurphyrd/colorplaner)",
         "",
         "# colorplaner",
         "",
         "To install:",
         "",
         "```{r, eval=FALSE}",
         "devtools::install_github(\"wmurphyrd/colorplaner\")",
         "```",
         "",
         "```{r include = F}",
         "knitr::opts_chunk$set(fig.width = 5, fig.height = 5)",
         "```",
         "",
         "Branch `internalize`: removes dependencies on internal objects in `ggplot2` and passes R CMD check without notes",
         "")

knitr::knit(text = c(ins, readme[seq(loc[2] + 1, length(readme))]),
            output = "readme.md", quiet = TRUE)
