<p align="center">
  <img src="misc/assets/logo.svg" width="50%" alt="loupeR - convert Seurat objects to Loupe files"><br>
  <a href="https://github.com/10XGenomics/loupeR/actions?query=workflow%3Acheck"><img src="https://github.com/10XGenomics/loupeR/actions/workflows/check.yml/badge.svg" alt="Build Status"></a>
  <a href="https://github.com/10XGenomics/loupeR/releases/latest"><img src="https://img.shields.io/badge/latest_release-green" alt="Build Status"></a>
  <a href="https://10xgen.com/EULA"><img src="https://img.shields.io/badge/EULA-purple" alt="EULA"></a>
  <br>
  Convert Seurat objects to 10x Genomics Loupe files.
</p>

<p align="center">
  ●
  <a href="#how-to-use">How To Use</a>
  ●
  <a href="#installation">Installation</a>
  ●
  <a href="#tutorials">Tutorials</a>
  ●
  <a href="#loupe-browser-compatibility">Loupe Browser Compatibility</a>
  ●
  <a href="#troubleshooting">Troubleshooting</a>
</p>

`loupeR` creates 10x Genomics Loupe files from Seurat objects and other 10x Genomics data in R. 10x Genomics Loupe Browser can visualize single-cell and spatial data from 10x Genomics. _Only single-cell gene expression datasets are supported in LoupeR_.

## How to Use

Converting a Seurat object to a Loupe file is as simple as the following:

```r
# import the library
library("loupeR")

# convert the SeuratObject named `seurat_obj` to a Loupe file
create_loupe_from_seurat(seurat_obj)
```

Use the function `create_loupe` if you need more control in the clusters and projections that included in the Loupe file.

```r
# import the library
library("loupeR")

# Gene Expression RNA assay
assay <- seurat_obj[["RNA"]]

# get counts matrix from either the old or newer formats of assay
counts <- counts_matrix_from_assay(assay)

# convert the count matrix, clusters, and projections into a Loupe file
create_loupe(
    counts,
    clusters = select_clusters(seurat_obj),
    projections = select_projections(seurat_obj)
)
```

Additionally, use the utility function `read_feature_ids_from_tsv` to read the Ensemble ids from the 10x dataset. A Seurat object will only have imported the feature names or ids and attached these as rownames to the count matrix. In order for the Ensemble id links to work correctly within Loupe Browser, one must manually import them and include them.

```r
# import the library
library("loupeR")

# Gene Expression RNA assay
assay <- seurat_obj[["RNA"]]

# A character vector of ensemble ids.
feature_ids <- read_feature_ids_from_tsv("PATH_TO_10X_DATA/features.tsv.gz")

create_loupe_from_seurat(seurat_obj, feature_ids = feature_ids)
```

## Installation

### HDF5

Before using `loupeR`, make sure that your system has installed [HDF5](https://www.hdfgroup.org/downloads/hdf5). The HDF5 organization requires registration before being able to download the installer. Below are some other more convenient methods for installing HDF5 if you happen to have these package managers installed.

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

Another installation option is to use the `remotes` package to directly install `loupeR` and its dependencies. The installed package won't include the prebundled louper executable, so you must invoke the `loupeR::setup()` function which will go and download it.

```r
remotes::install_github("10XGenomics/loupeR")
loupeR::setup()
```

### Automated installation and execution

If you are interested in automating LoupeR installation and execution (and are blocked by interactive license acceptance), please write to [support@10xgenomics.com](mailto:support@10xgenomics.com) for further assistance.

### Customizing the user data directory

By default the louper executable, which is the binary that understands and creates Loupe files, will be stored in the system's `tools::R_user_dir` directory.  You can update this path by setting the environment variable `LOUPER_USER_DATA_DIR` to a directory of your choosing.

## Loupe Browser Compatibility

With new versions of the Loupe Browser, new version of LoupeR need to be released. The table below shows version requirements between the two.

| LoupeR Version | Loupe Browser Version |
| -------------- | --------------------- |
| v1.0.x         | Loupe Browser >= 7.0  |
| v1.1.1         | Loupe Browser >= 8.0  |
| v1.1.2         | Loupe Browser >= 8.1  |

## Tutorials

- [Demo notebook](https://colab.research.google.com/github/10XGenomics/loupeR/blob/main/misc/tutorials/5k_mouse_brain.ipynb) with basic processing of an example 10x dataset [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/10XGenomics/loupeR/blob/main/misc/tutorials/5k_mouse_brain.ipynb)

## Barcode Formatting

Barcodes must be from 10x Genomics exeriments to work with LoupeR. Valid 10x Genomics Single Cell Gene Expression barcodes have the characters ACGT repeated 16 times, followed by an optional GEM well suffix, for example:

```
AAACCCAAGAAATTGC
AAACCCAAGAAATTGC-1
```

Barcodes can also have an additional optional prefix or suffix. These optional prefixes and suffixes must be delineated either by a `:` or a `_`:

```
prefix_AAACCCAAGAAATTGC
AAACCCAAGAAATTGC_suffix
prefix_AAACCCAAGAAATTGC_suffix

prefix:AAACATACAAACAG
AAACATACAAACAG:suffix
prefix:AAACATACAAACAG:suffix

prefix_AAACCCAAGAAATTGC-1
AAACCCAAGAAATTGC-1_suffix
prefix_AAACCCAAGAAATTGC-1_suffix

prefix:AAACCCAAGAAATTGC-1
AAACCCAAGAAATTGC-1:suffix
prefix:AAACCCAAGAAATTGC-1:suffix
```

**Note**: Visium and Xenium barcodes are formatted differently. Visium and Xenium data are currently enabled for use with LoupeR, but **_not_** fully supported. Expression data for these assays can be processed by loupeR, but **_not_** image data.

See `test-validate.R` for further examples of both valid and invalid barcode formatting, as well as `validater.R` for the exact formatting requirements as code.

## Troubleshooting

For more in depth documentation and support please head to our [support page](https://10xgen.com/louper) or send an email to [support@10xgenomics.com](mailto:support@10xgenomics.com)

Additionally, we have provided utility functions to help gather useful information when contacting support or creating a Github issue.

```r
# import the library
library("loupeR")

# print extra debug information
create_bugreport_from_seurat(seurat_obj)
```
