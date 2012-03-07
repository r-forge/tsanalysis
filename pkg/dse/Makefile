# specify any other packages that are prerequisites like this
PREREQ_PACS=setRNG tframe

#  ../Makevars goes up real path not sym link, so
QC=${shell cd ../ ; pwd}

OPTIONAL_PREREQ=src/dsefor.f

include $(QC)/Makevars

default: $(DEFAULT)

include $(QC)/Makerules
