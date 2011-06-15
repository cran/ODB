\name{odb.export}
\alias{odb.export}
\title{
  Exports an ODB database to a SQL file.
}

\description{
  Produces SQL queries describing the structure of the database and its content, and saves it to a file.
}

\usage{
  odb.export(odb, file)
}

\arguments{
  \item{odb}{
    An \code{odb} object, as produced by \code{\link{odb.open}}.
}
  \item{file}{
    Path to the file in which export the SQL queries.
}
}

\value{
  Invisibly returns TRUE if succeeds, raises an error if not.
}

\author{
  Sylvain Mareschal
}

\note{
  It is just a wrapper for the "SCRIPT '[file]'" SQL query, as implemented in HSQLDB.
  
  Column comments and queries won't be exported, as they are not stored in the HSQL database.
}

\examples{
  # New empty .odb file
  odb.create("test.odb", overwrite="do")
  ODB <- odb.open("test.odb")
  
  # New table
  odb.write(ODB, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(ODB, "fruits", c("banana", "pear", "peach"))
  
  # Export to a file
  odb.export(ODB, "fruits.sql")
  
  # Writes to the file and closes the connection
  odb.close(ODB, write=TRUE)
}
