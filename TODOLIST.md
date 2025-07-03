# Use a function to generate filenames

epFiles <- function(path,commit,page,crop)

Then we can also store the intermediate png files, depending on how easy it is to set up the whole pipeline.

# Specify file paths:

„Path/to/file.pdf:::pages c(1,3:5):::crop x0,0.9 y0,1:::commit sd782h3m55“ = plotspec(filename,…) = .p(filename)

Options:

* Path
* Pages
* Crop
* Commit

Details

* cm to inch conversion in getIMGWiddthHeight
* Fix the row height bug

# Markup of text: 

Same as for file paths

Options:

* Rotate
* Size
* Alignment
* Bold, italic
* Background color is always white

# pdfdiff:

Base on previously defined columns in backend
"diff" = "diff[C1C2]" = diffspec("C1", "C2")

# idempotency:

* Use /tmp/fullPath...
* Reexport png only if input file newer than exported


# Support for ggplot2 objects?

