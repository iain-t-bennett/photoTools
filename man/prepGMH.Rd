% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepGMH.R
\name{prepGMH}
\alias{prepGMH}
\title{Prepares a location history from google takeout}
\usage{
prepGMH(takeoutjson)
}
\arguments{
\item{takeoutjson}{A "Location History.json" file from google}
}
\value{
tibble containing processed data
}
\description{
Reference: https://www.chipoglesby.com/2018/03/2018-analyzing-google-location-historyII/
}
