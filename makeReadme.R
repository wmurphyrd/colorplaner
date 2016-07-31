
readme <- readLines("vignettes/colorplaner.Rmd")
loc <- grep("^---$", readme)

news_loc <- grep("^## Usage$", readme)[1] - 1

ins <- c("# colorplaner",
         "",
         "[![BuildStatus](https://travis-ci.org/wmurphyrd/colorplaner.svg?branch=master)](https://travis-ci.org/wmurphyrd/colorplaner)",
         "",
         "```{r include = F}",
         "knitr::opts_chunk$set(fig.width = 5, fig.height = 5)",
         "```",
         "")
news <- readLines("NEWS.md")
news <- gsub("^#", "####", news)
news_ins <- c("",
              "## Installation",
              "",
              "To install:",
              "",
              "```{r, eval=FALSE}",
              "devtools::install_github(\"wmurphyrd/colorplaner\", ref = \"other_projections\")",
              "```",
              "",
              news,
              "")

knitr::knit(text = c(ins, readme[seq(loc[2] + 1, news_loc)],
                     news_ins, readme[seq(news_loc + 1, length(readme))]),
            output = "readme.md", quiet = TRUE)
