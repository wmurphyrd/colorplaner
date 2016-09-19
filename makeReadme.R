# This file is part of colorplaner.
#
# colorplaner is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# colorplaner is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with colorplaner.  If not, see <http://www.gnu.org/licenses/>.
readme <- readLines("vignettes/colorplaner.Rmd")
loc <- grep("^---$", readme)

news_loc <- grep("^## Usage$", readme)[1] - 1

ins <- c("# colorplaner",
         "",
         "[![BuildStatus](https://travis-ci.org/wmurphyrd/colorplaner.svg?branch=master)](https://travis-ci.org/wmurphyrd/colorplaner)",
         "[![Coverage Status](https://coveralls.io/repos/github/wmurphyrd/colorplaner/badge.svg?branch=master)](https://coveralls.io/github/wmurphyrd/colorplaner?branch=master)",
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
              "devtools::install_github(\"wmurphyrd/colorplaner\")",
              "```",
              "",
              news,
              "")

knitr::knit(text = c(ins, readme[seq(loc[2] + 1, news_loc)],
                     news_ins, readme[seq(news_loc + 1, length(readme))]),
            output = "readme.md", quiet = TRUE)
