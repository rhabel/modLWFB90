# modLWFB90
running LWFBrook90R for FVABW-purposes. 

Will be updated soon. For now, this is how to install from FVA computers.

```{r}
if(!require("remotes")) install.packages("remotes") 
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
Sys.setenv(HTTPS_PROXY=Sys.getenv("HTTP_PROXY")) 

remotes::install_github(repo="pschmidtwalter/LWFBrook90R") 
remotes::install_github(repo="rhabel/modLWFB90", build_vignettes = T)

```

For more info see vignette.
```{r}
browseVignettes(package = NULL, lib.loc = NULL, all = TRUE)
# then look for "modLWFB90". HTML version is nice to read.
```
