\name{odb.zip}
\alias{odb.zip}
\title{
  Adds files to a ZIP archive
}

\description{
  Adds files to a ZIP archive using the "zip" shell command, available on Unix and recent WIndows Operating Systems.
}

\usage{
  odb.zip(archive, files, from = ".", recursive = TRUE, quiet = FALSE)
}

\arguments{
  \item{archive}{
    Single character value, the path to the ZIP archive to update.
}
  \item{files}{
    Multiple character vector, the paths of files to include. See the Note section.
}
  \item{from}{
    Single character value, the path to append to "files". Uses the working directory as default. See the Note section.
}
  \item{recursive}{
    Single logical value, whether to add content of directories listed in "files" or not.
}
  \item{quiet}{
    Single logical value, whether to print output of the command or not.
}
}

\value{
  Invisibly returns TRUE if succeeds, raises an error if not.
}

\author{
  Sylvain Mareschal
}

\note{
  The distinction between the "files" and "from" arguments gives control on paths inside the ZIP archive. Consider "from" as the path to the root of a locale copy of the archive, and every path element in "files" as directories to create in the archive. While files are retrieved at localtions defined by the concatenation of "from" and "files", only path elements of "files" will be built in the archive. See the example section.
}

\examples{
  # Example files and directories
  dir.create("dir/ect/ory", recursive=TRUE)
  write("test1", "dir/ect/text1.txt")
  write("test2", "dir/ect/ory/text2.txt")
  
  # Addition of files ignoring the "dir/ect" path elements
  from <- "dir/ect"
  files <- c("text1.txt", "ory/text2.txt")
  odb.zip("archive.zip", files=files, from=from, recursive=TRUE)
}
