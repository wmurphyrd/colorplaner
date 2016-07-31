# Development branch: other_projections
* Support for providing alternate color space projections through the `color_projection` argument to `scale_fill_colorplane` and `scale_color_colorplane`
* Sample alternate projection `red_blue_projection`
* Documentation for creating alternate projections added at `?color_projections`

# Known Issues and Incomplete Features
* Cannot alter title and axis label positions in guide_colorplane (#2)
* Colors render incorrectly when discrete variables assigned to color/colour/fill (#1) 

# Version 0.0.0.9002
* Removed dependency on `pkg:colorscience` (and all of its imports and depends); replaced with super fast matrix-math YUV-RGB conversions (#4)
* Added missing arguments to `scale_fill_colorplane`: `na.color`, `trans`

# Version 0.0.0.9001
* Removed all dependencies on unexported objects from ggplot2

# Version 0.0.0.9000
* Adds `scale_color_colorplane`, `scale_fill_colorplane`, and `guide_colorplane` extensions to ggplot2
