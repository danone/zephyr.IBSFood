% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MSP_infraspecies.r
\name{partitionize_MSP}
\alias{partitionize_MSP}
\title{partitionize MSP}
\usage{
partitionize_MSP(binary_msp)
}
\arguments{
\item{binary_msp}{MSP binary matrix}
}
\value{
clusters from fpc::clusterboot
}
\description{
the aim of this function is to find sample cluster base on gene presence/absence per MSP
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[ade4]{dist.binary}}
 \code{\link[fpc]{clusterboot}},\code{\link[fpc]{kmeansCBI}}
}
