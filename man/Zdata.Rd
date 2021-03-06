% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Zdata.R
\name{Zdata}
\alias{Zdata}
\title{Read a data file from the Znotes repository}
\usage{
Zdata(dname, ...)
}
\arguments{
\item{dname}{the complete name of the .csv file, e.g. \code{"USAFA/hawaii.csv"}}

\item{...}{other arguments to readr::read_csv()}
}
\description{
The \code{{Znotes}} repository on GitHub has an associated web site. Authorized
instructors can add CSV files to that site. These data files can then
be used in the Sandbox or in the R console.
}
\details{
Instructors who wish to upload data should create a GitHub account
for themselves and contact \code{dtkaplan@gmail.com} with their GitHub account name and
a proposed name for the directory where the new data files are to be stored.
To upload data, login to GitHub under your account name and navigate
to \code{github.com/dtkaplan/Znotes}, thence to the \code{docs} directory, thence to the
directory created for storing you data files.
}
\examples{
hawaii <- Zdata("USAFA/hawaii.csv")

}
