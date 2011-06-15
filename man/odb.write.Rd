\name{odb.write}
\alias{odb.write}
\title{
  Executes writing SQL queries in an ODB database (INSERT ...)
}

\description{
  Executes an SQL query expecting no output through an "odb" connection.
}

\usage{
  odb.write(odb, sqlQueries, onError = c("warning", "stop"))
}

\arguments{
  \item{odb}{
    An \code{odb} object, as produced by \code{\link{odb.open}}.
}
  \item{sqlQueries}{
    Single or multiple character vector, with queries to be executed ordered in distinct elements. ";" query separation should not be used.
}
  \item{onError}{
    Single character vector, defining how to handle SQL errors (using \code{\link{warning}} or \code{\link{stop}}). If "sqlQueries" contains more than one query, "stop" will prevent remaining queries to be executed, while "warning" won't.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{odb.read}}
}

\examples{
  # New empty .odb file
  odb.create("test.odb", overwrite="do")
  ODB <- odb.open("test.odb")
  
  # New table
  odb.write(ODB, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(ODB, "fruits", c("banana", "pear", "peach"))
  print(odb.tables(ODB))
  
  # Manual insert
  odb.write(ODB, "INSERT INTO fruits VALUES('apple')")
  print(odb.read(ODB, "SELECT * FROM fruits"))
  
  # Remove table
  odb.write(ODB, "DROP TABLE fruits")
  print(odb.tables(ODB))
  
  # Writes to the file and closes the connection
  odb.close(ODB, write=TRUE)
}

