# Check if a DBI or odb connection is closed
# Author : Sylvain Mareschal
isClosed = function(
		connection
		)
	{
	# Connection check
	if (!inherits(connection, "DBIConnection")) {
		stop("'connection' must inherit from 'DBIConnection', or be an 'odb' connection")
	}
	
	# Test
	closed = tryCatch(
		dbSendUpdate(connection, ""),
		error = function(e) {
			return(grepl("Connection is closed", conditionMessage(e)))
		}
	)
	
	return(!is.null(closed))
}
