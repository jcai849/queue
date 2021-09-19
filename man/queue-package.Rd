\encoding{UTF-8}
\name{queue-package}
\alias{queue-package}
\alias{queue}
\alias{enqueue}
\alias{dequeue}
\alias{as.list.queue}
\alias{format.queue}
\alias{print.queue}
\alias{str.queue}
\docType{package}
\title{
\packageTitle{queue}
}
\description{
\packageDescription{queue}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{queue}
\packageIndices{queue}

NB: \code{enqueue} and \code{dequeue} always return the peek (front) of the queue, performing their respective side effects as well.
}
\author{
\packageAuthor{queue}

Maintainer: \packageMaintainer{queue}
}
\references{
	Donald Knuth. The Art of Computer Programming, Volume 1: Fundamental Algorithms, Third Edition. Addison-Wesley, 1997. ISBN 0-201-89683-4. Section 2.2.1: Stacks, Queues, and Dequeues, pp. 238–243
}
\keyword{ package }
\examples{
	x <- queue()
	str(x)
	enqueue(x, 1)
	enqueue(x, 2)
	str(x)
	dequeue(x)
	dequeue(x)
	dequeue(x)
	str(x)
}
