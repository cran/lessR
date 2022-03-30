\name{dataMach4_lbl}
\docType{data}
\alias{dataMach4_lbl}
\title{VariableLabels: Mach4 Data Set}

\description{For the data of 351 responses to the 20-item Mach IV scale.}

\usage{data(dataMach4_lbl)}
\format{Variable labels, the items of the Christie and Geiss Mach IV Scale\cr
\cr
m01: Never tell anyone the real reason you did something unless it is useful to do so\cr
m02: The best way to handle people is to tell them what they want to hear\cr
m03: One should take action only when sure it is morally right\cr
m04: Most people are basically good and kind\cr
m05: It is safest to assume that all people have a vicious streak and it will come out when they are given a chance\cr
m06: Honesty is the best policy in all cases\cr
m07: There is no excuse for lying to someone else\cr
m08: Generally speaking, people won't work hard unless they're forced to do so\cr
m09: All in all, it is better to be humble and honest than to be important and dishonest\cr
m10: When you ask someone to do something for you, it is best to give the real reasons for wanting it rather than giving reasons which carry more weight\cr
m11: Most people who get ahead in the world lead clean, moral lives\cr
m12: Anyone who completely trusts anyone else is asking for trouble\cr
m13: The biggest difference between most criminals and other people is that the criminals are stupid enough to get caught\cr
m14: Most people are brave\cr
m15: It is wise to flatter important people\cr
m16: It is possible to be good in all respects\cr
m17: Barnum was wrong when he said that there's a sucker born every minute\cr
m18: It is hard to get ahead without cutting corners here and there\cr
m19: People suffering from incurable diseases should have the choice of being put painlessly to death\cr
m20: Most people forget more easily the death of a parent than the loss of their property
}

\source{author}

\references{
Christie, R., & Geis, F. L., (1970).  Studies in Machiavellianism.  New York:  Academic Press.

Hunter, J. E., Gerbing, D. W., and Boster, F. J. (1982). Machiavellian beliefs and personality: The construct invalidity of the Machiavellian dimension. Journal of Personality 
}

\examples{
# Read data and variable labels (items)
d <- Read("Mach4")
l <- Read("Mach4_lbl")

# Convert to factors, i.e., categorical with value labels
d <- factors(m01:m20,
            levels=0:5,
            labels=c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree"))

}

\keyword{datasets}
