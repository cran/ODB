\name{odb.tables}
\alias{odb.tables}
\title{
  Gets description of every table in an ODB database.
}

\description{
  Gets description of every tables in the database through an "odb" connection : table names, column names, column SQL and R types.
}

\usage{
  odb.tables(odb)
}

\arguments{
  \item{odb}{
    An \code{odb} object, as produced by \code{\link{odb.open}}.
}
}

\value{
  Returns a named list, whith an element for every table in the database. Data.frames are returned by \code{\link{dbColumnInfo}}, updated with comments returned by \code{\link{odb.comments}}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{dbColumnInfo}}, \code{\link{odb.comments}}
}

\examples{
  # New empty .odb file
  odb.create("test.odb", overwrite="do")
  ODB <- odb.open("test.odb")
  
  # New tables
  SQL <- c(
    "CREATE TABLE fruits (
      name VARCHAR(6) PRIMARY KEY,
      color VARCHAR(32)
      )",
    "CREATE TABLE vegetables (
      name VARCHAR(6) PRIMARY KEY,
      color VARCHAR(32)
      )"
    )
  odb.write(ODB, SQL)
  
  # Print tables
  print(odb.tables(ODB))
  
  # Writes to the file and closes the connection
  odb.close(ODB, write=TRUE)
}

