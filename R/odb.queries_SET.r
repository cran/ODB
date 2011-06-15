# Inserts or updates an SQL query stored in an Open Office Base file
# Author : Sylvain Mareschal
"odb.queries<-" = function(
		odb,
		queryNames,
		value
		)
	{
	# Class checks
	if (!is(odb, "odb")) {
		stop("'odb' must be an 'odb' object")
	}
	validObject(odb)
	
	# Args check
	if (length(queryNames) != length(value)) {
		stop("'queryNames' and value must have same lengths")
	}
	if (any(grepl("/", queryNames))) {
		stop("'queryNames' can not contain slashes")
	}
	
	# Encoding
	queryNames = iconv(queryNames, to="UTF-8")
	value = iconv(value, to="UTF-8")
	
	# XML content
	tree = xmlParse(paste(odb@directory, "content.xml", sep="/"))
	
	for(i in 1:length(value)) {
		# Initialization
		path = "/office:document-content/office:body/office:database"
		parent = getNodeSet(tree, path)[[1]]
		nodes = list(
			"queries" = character(0),
			"query" = c("db:name"=queryNames[i])
		)
		
		# Recursive node building
		for(level in 1:length(nodes)) {
			# Node retrieval
			if (length(nodes[[level]]) == 1) {
				xpath = paste(sep="",
					paste("db", names(nodes)[level], sep=":"),
					"[@",
					names(nodes[[level]]),
					"=\"",
					nodes[[level]],
					"\"]"
				)
			} else {
				xpath = paste("db", names(nodes)[level], sep=":")
			}
			
			path = paste(path, xpath, sep="/")
			child = getNodeSet(tree, path)
			
			if (length(child) == 0) {
				# New node
				child = list(
					newXMLNode(
						name = names(nodes)[level],
						attrs = nodes[[level]],
						parent = parent,
						namespace = "db"
					)
				)
			}
			
			parent = child[[1]]
		}
		node = parent
		
		# Updates the node
		xmlAttrs(node) = c("db:command"=value[i], "db:escape-processing"="false")
	}
	
	# Save XML to file
	saveXML(tree, file=paste(odb@directory, "content.xml", sep="/"))
	
	return(odb)
}
