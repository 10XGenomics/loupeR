% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdf5.R
\name{create_hdf5}
\alias{create_hdf5}
\title{Create an hdf5 interchange file}
\usage{
create_hdf5(
  count_mat,
  clusters,
  projections,
  h5path,
  feature_ids,
  seurat_obj_version
)
}
\arguments{
\item{count_mat}{A sparse dgCMatrix as is generated via Matrix::rsparsematrix.
Rows are features, Columns are barcodes.}

\item{clusters}{list of factors that hold information for each barcode}

\item{projections}{list of matrices, all with dimensions (barcodeCount x 2)}

\item{h5path}{path to h5 file}

\item{feature_ids}{optional character vector that specifies the feature ids of the count matrix.
Typically, these are the ensemble ids.}

\item{seurat_obj_version}{optional string that holds the Seurat Object version.
It is useful for debugging compatibility issues.}
}
\value{
TRUE on success, FALSE on error
}
\description{
Create an hdf5 interchange file
}
