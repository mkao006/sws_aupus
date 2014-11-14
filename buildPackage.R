## Building the package
## ---------------------------------------------------------------------

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsAupus"))
    unlink("faoswsAupus", recursive = TRUE)

## Build the package
package.skeleton("faoswsAupus", code_files = paste("./codes/",
                           dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsAupus/",
          overwrite = TRUE)
unlink("./faoswsAupus/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsAupus")
## unlink("./faoswsAupus/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsAupus")
system("R CMD build faoswsAupus")
## system("R CMD check --as-cran faoswsAupus")

