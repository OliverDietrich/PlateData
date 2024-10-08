% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import_tekanSpark}
\alias{import_tekanSpark}
\title{Import data from Tekan Spark}
\usage{
import_tekanSpark(file_path = NULL, plate_name = "unspecified")
}
\arguments{
\item{file_path}{Location of file (must be xls or xlsx)}
}
\value{
Tidy data frame with six columns (cycle, time, temp, well, count, mode)
}
\description{
Import plate reader data from an excel sheet using the formatting by
Tekan's Spark.
}
