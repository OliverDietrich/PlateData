% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import_elecsys_concentrations}
\alias{import_elecsys_concentrations}
\title{Import concentration measurements}
\usage{
import_elecsys_concentrations(file_path = NULL, plate_name = NULL)
}
\arguments{
\item{file_path}{File path}
}
\value{
Data.frame
}
\description{
Import ELISA measurements for antibodies screened via Elecsys.
Not well standardized excel spreadsheet with rectangular plate-like shape containing
the antibody names (e.g. 23.068-63H4-IgG) and IgG concentration in ng/mL.
}
