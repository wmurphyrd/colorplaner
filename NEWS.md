# Version 0.1.3
* Change tests to compensate for ggplot2 2.2.0 changes
* Due to ggplot2 2.2.0 changes, mapping a colorplane at the level of an individual layer will now produce a warning message, but functions normally. See `?colorplaner` for more information. 

# Version 0.1.2
* Fixed an issue that was preventing updated vignettes from building
* Complete versions of `colorplaner` and `other_projections` vignettes 
now included

# Version 0.1.1
* Initial release of the colorplaner package
* Implements two new ggplot2 scales: `scale_color_colorplane` and 
`scale_fill_colorplane`
* Implements one new ggplot2 guide: `guide_colorplane`

