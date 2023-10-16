<p align="center">
  <img src="misc/assets/logo.svg" width="50%" alt="loupeR - convert Seurat objects to Loupe files"><br>
  <a href="https://github.com/10XGenomics/loupeR/actions?query=workflow%3Acheck"><img src="https://github.com/10XGenomics/loupeR/actions/workflows/check.yml/badge.svg" alt="Build Status"></a>
  <a href="https://github.com/10XGenomics/loupeR/releases/latest"><img src="https://img.shields.io/badge/latest_release-green" alt="Build Status"></a>
  <a href="https://10xgen.com/EULA"><img src="https://img.shields.io/badge/EULA-purple" alt="EULA"></a>
  <br>
  Convert Seurat objects to 10x Genomics Loupe files.
</p>

<p align="center">
  <a href="#how-to-use">How To Use</a>
  <a href="#installation">Installation</a>
  <a href="#tutorials">Tutorials</a>
  <a href="#troubleshooting">Troubleshooting</a>
</p>

`loupeR` creates a 10x Genomics Loupe file from a Seurat object. 10x Genomics Loupe Browser can visualize single-cell and spatial data from 10x Genomics.  *Only single-cell gene expression datasets are supported*.

## How to Use

Converting a Seurat object to a Loupe file is as simple as the following:

```R
# import the library
library("loupeR")

# convert the SeuratObject named `seurat_obj` to a Loupe file
create_loupe_from_seurat(seurat_obj)
```

Use the function `create_loupe` if you need more control in the clusters and projetions that included in the Loupe file.

```R
# import the library
library("loupeR")

# Gene Expression RNA assay
assay <- seurat_obj[["RNA"]]

# convert the count matrix, clusters, and projections into a Loupe file
create_loupe(
    assay@counts,
    clusters = select_clusters(seurat_obj),
    projections = select_projections(seurat_obj)
)
```

## Installation

### HDF5

Before using `loupeR`, make sure that your system has installed [HDF5](https://www.hdfgroup.org/downloads/hdf5).  The HDF5 organization requires registration before being able to download the installer.  Below are some other more convenient methods for installing HDF5 if you happen to have these package managers installed.

- macOS with [Homebrew](https://brew.sh/) - `brew install hdf5` <br>
- windows with [vcpkg](https://vcpkg.io/en/index.html) - `.\vcpkg install hdf5`

### Installing loupeR from prebuilt bundle

In RStudio, or your R shell, run the following to install this package.

```r
# install dependencies
if (!require("hdf5r")) install.packages("hdf5r")
if (!require("Seurat")) install.packages("Seurat")

# install platform specific source package
os <- sub("Darwin", "macOS", Sys.info()["sysname"])
url <- paste0("https://github.com/10XGenomics/loupeR/releases/latest/download/loupeR_", os, ".tar.gz")
install.packages(url, repos = NULL, type = "source")
```

### Installing loupeR using the `remotes` package

Another installation option is to use the `remotes` package to directly install `loupeR` and its dependencies.  The installed package won't include the prebundled louper executable, so you must invoke the `loupeR::setup()` function which will go and download it.

``` r
remotes::install_github("10XGenomics/loupeR")
loupeR::setup()
```
## Tutorials

* [Notebook](misc/tutorials/5k_mouse_brain.ipynb) with an example dataset can be run from this notebook here: 


## Troubleshooting

For more in depth documentation and support please head to our [support page](https://10xgen.com/louper) or send an email to [support@10xgenomics.com](mailto:support@10xgenomics.com)

Additionally, we have provided utility functions to help gather useful information when contacting support or creating a Github issue.

```R
# import the library
library("loupeR")

# print extra debug information
create_bugreport_from_seurat(seurat_obj)
```
