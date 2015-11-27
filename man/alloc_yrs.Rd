\name{alloc_yrs}
\alias{alloc_yrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{optimised demand alloction model
%%  ~~function to do ... ~~
}
\description{ optimised demand allocation procedure. looks for shortest travel time and available capacity
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
alloc_yrs(A, cohort, scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{ name of cluster
%%     ~~Describe \code{A} here~~
}
  \item{cohort}{ cohort 'p' for primary or 's' for secondary
%%     ~~Describe \code{cohort} here~~
}
  \item{scenario1}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario1} here~~
}
  \item{scenario2}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario2} here~~
}
  \item{scenario3}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario3} here~~
}
  \item{scenario4}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario4} here~~
}
  \item{scenario5}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario5} here~~
}
  \item{scenario6}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario6} here~~
}
  \item{scenario7}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario7} here~~
}
  \item{scenario8}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario8} here~~
}
  \item{scenario9}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario9} here~~
}
  \item{scenario10}{ filename of scenario (up to 10 scenarios) - exlclude .csv extension
%%     ~~Describe \code{scenario10} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (A, cohort, scenario1, scenario2, scenario3, scenario4, 
    scenario5, scenario6, scenario7, scenario8, scenario9, scenario10) 
{
    library(gdata)
    library(dplyr)
    library(reshape)
    studentseifa = read.xls("studentseifa.xlsx")
    row.names(studentseifa) <- as.vector(unlist(studentseifa[1]))
    traveltime = read.xls("traveltime.xlsx")
    A = deparse(substitute(A))
    cohort = deparse(substitute(cohort))
    if (cohort != "p" & cohort != "s") {
        print("model is missing cohort argument. Please specify p or s (p=primary;s=secondary")
    }
    studentfilename = paste(paste("studentenroll", cohort, sep = ""), 
        ".xlsx", sep = "")
    studentenroll = read.xls(studentfilename)
    row.names(studentenroll) <- as.vector(unlist(studentenroll[1]))
    clusterenroll = studentenroll[which(studentenroll["cluster"] == 
        A), ]
    studentseifa = merge(studentenroll["MB"], studentseifa, by.x = "MB", 
        by.y = "MB")
    clusterseifa = studentseifa[which(studentseifa["cluster"] == 
        A), ]
    schoolfilename = paste(paste("oschool", cohort, sep = ""), 
        ".xlsx", sep = "")
    school = read.xls(schoolfilename)
    row.names(school) <- as.vector(unlist(school[1]))
    oschool = school[which(school["cluster"] == A), ]
    ocapacity = data.frame(oschool[1], oschool[2])
    catchmentlist <- NULL
    averageseifa <- NULL
    averagett <- NULL
    unallocstudent <- NULL
    unalloccap <- NULL
    arguments <- as.character(as.list(match.call()))
    inputfiles <- arguments[4:(length(arguments))]
    for (file in inputfiles) {
        inputs = read.xls(paste(file, ".xlsx", sep = ""))
        row.names(inputs) <- as.vector(unlist(inputs[1]))
        clusterorigin = clusterenroll[1]
        clusterdestination = inputs["MB"]
        colnames(clusterorigin) = "origin"
        colnames(clusterdestination) = "destination"
        origin = merge(clusterorigin, traveltime, by.x = "origin", 
            by.y = "origin")
        clustertraveltime_m = merge(clusterdestination, origin, 
            by.x = "destination", by.y = "destination")
        clustertraveltime_m <- melt(clustertraveltime_m, id = c("origin", 
            "destination"))
        clustertraveltime_m <- cast(clustertraveltime_m, origin ~ 
            destination, mean)
        catchment_m = clustertraveltime_m
        catchment_m[which(sapply(catchment_m, is.numeric) == 
            "TRUE")][catchment_m[which(sapply(catchment_m, is.numeric) == 
            "TRUE")] > 0] <- 0
        firstcol = which(sapply(inputs, is.numeric) == "TRUE")[1]
        seconcol = which(sapply(inputs, is.numeric) == "FALSE")[2]
        inputcapacity = inputs[firstcol:(seconcol - 1)]
        inputwithchange = inputcapacity[which(colSums(inputcapacity) > 
            0)]
        inputwithchange = cbind(X = rownames(inputwithchange), 
            inputwithchange)
        colnames(inputwithchange)[1] = colnames(ocapacity[1])
        inputMB = data.frame(inputs[1], inputs["MB"])
        inputSEIFA = data.frame(inputs[1], inputs["SEIFA"])
        changeyears = colnames(inputwithchange)
        for (i in 2:length(changeyears)) {
            newcapacity = merge(ocapacity, data.frame(inputwithchange[1], 
                inputwithchange[changeyears[i]]), all = "T")
            newcapacity[is.na(newcapacity)] <- 0
            capacityofyear = data.frame(newcapacity[2] + newcapacity[3])
            newcapacityofyear = data.frame(newcapacity[1], capacityofyear)
            newcapacityofyear = merge(newcapacityofyear, inputMB, 
                by.x = colnames(newcapacityofyear)[1], by.y = "cluster.")
            availstudents = data.frame(clusterenroll[1], clusterenroll[changeyears[i]])
            colnames(availstudents)[2] = "students"
            catchment = catchment_m
            clustertraveltime = clustertraveltime_m
            while (sum(newcapacityofyear[2]) && sum(availstudents[2])) {
                clustertraveltime[, sapply(clustertraveltime, 
                  is.numeric)]
                coordinate = which(clustertraveltime == min(clustertraveltime[, 
                  sapply(clustertraveltime, is.numeric)]), arr.ind = TRUE)
                originMBmintravel = as.character(clustertraveltime[coordinate[1, 
                  1], 1])
                destMBmintravel = colnames(clustertraveltime)[coordinate[1, 
                  2]]
                numberofstudents = availstudents[which(availstudents["MB"] == 
                  originMBmintravel), 2]
                schoolsinmbpair <- which(newcapacityofyear["MB"] == 
                  destMBmintravel)
                for (p in schoolsinmbpair) {
                  destschoolcapacity = newcapacityofyear[p, 2]
                  if (numberofstudents < destschoolcapacity) {
                    catchment[coordinate[1, 1], coordinate[1, 
                      2]] = numberofstudents
                    newcapacityofyear[which(newcapacityofyear["MB"] == 
                      destMBmintravel), 2] = destschoolcapacity - 
                      numberofstudents
                    availstudents[which(availstudents["MB"] == 
                      originMBmintravel), 2] = 0
                    clustertraveltime[coordinate[1, 1], coordinate[1, 
                      2]] = 99999
                  }
                  else {
                    catchment[coordinate[1, 1], coordinate[1, 
                      2]] = destschoolcapacity
                    newcapacityofyear[which(newcapacityofyear["MB"] == 
                      destMBmintravel), 2] = 0
                    availstudents[which(availstudents["MB"] == 
                      originMBmintravel), 2] = numberofstudents - 
                      destschoolcapacity
                    clustertraveltime[coordinate[1, 1], coordinate[1, 
                      2]] = 99999
                  }
                }
            }
            averagettbyyear = colSums(catchment[, sapply(catchment, 
                is.numeric)] * clustertraveltime_m[, sapply(clustertraveltime_m, 
                is.numeric)])/colSums(catchment[, sapply(catchment, 
                is.numeric)])
            averagettbyyear = data.frame(averagettbyyear, A, 
                changeyears[i], file)
            averagett <- rbind(averagett, averagettbyyear)
            clusterseifa[2:length(catchment)] = clusterseifa[2]
            colnames(clusterseifa) = colnames(catchment)
            averageseifabyyear = colSums(clusterseifa[, sapply(clusterseifa, 
                is.numeric)] * catchment[, sapply(catchment, 
                is.numeric)])/colSums(catchment[, sapply(catchment, 
                is.numeric)])
            averageseifabyyear = data.frame(averageseifabyyear, 
                A, changeyears[i], file)
            averageseifa <- rbind(averageseifa, averageseifabyyear)
            catchment = data.frame(catchment, A, changeyears[i], 
                file)
            catchmentlist <- rbind(catchmentlist, catchment)
            catchmentlist2 <- melt(catchmentlist, id = c("changeyears.i.", 
                "file", "A", "origin"))
            availstudents = data.frame(availstudents, A, changeyears[i], 
                file)
            unallocstudent <- rbind(unallocstudent, availstudents)
            newcapacityofyear = data.frame(newcapacityofyear, 
                A, changeyears[i], file)
            unalloccap <- rbind(unalloccap, newcapacityofyear)
        }
    }
    write.table(catchmentlist2, "catchment.csv", row.names = FALSE, 
        sep = ",")
    write.table(unallocstudent, "unallocatedstudents.csv", row.names = FALSE, 
        sep = ",")
    write.table(unalloccap, "unallocatedcapacity.csv", row.names = FALSE, 
        sep = ",")
    write.table(averagett, "averagetraveltime.csv", col.names = TRUE, 
        sep = ",")
    write.table(averageseifa, "averageseifa.csv", col.names = TRUE, 
        sep = ",")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
