# Author : Sylvain Mareschal
odb.queries = function(
		odb,
		queryNames = NULL
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# SAX parsing
	results = xmlEventParse(
		file = paste(odb@directory, "content.xml", sep="/"),
		handlers = list(
			.startElement = function(.name, .attrs, .space, .spaces, .state) {
				# Query tag
				if (names(.space) == "db" && .name == "query") {
					queryName = iconv(.attrs["name"], from="UTF-8")
					if (is.null(queryNames) || queryName %in% queryNames) {
						query = iconv(.attrs["command"], from="UTF-8")
						names(query) = queryName
						.state = c(.state, query)
					}
				}
				return(.state)
			}
		),
		state = character(0),
		useDotNames = TRUE,
		useTagName = FALSE,
		saxVersion = 2
	)
	
	return(results)	
}
