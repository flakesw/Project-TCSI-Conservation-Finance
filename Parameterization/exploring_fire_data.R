#-------------------------------------------------------------------------------
# exploring some available data
#-------------------------------------------------------------------------------
library("sf")

ogrListLayers("./calibration data/wfigs_incidents/Public_EventDataArchive_2020.gdb/Public_EventDataArchive_2020.gdb")

public_event_archive_2020 <- sf::st_read("./calibration data/wfigs_incidents/Public_EventDataArchive_2020.gdb/Public_EventDataArchive_2020.gdb",
                                         layer = "EventPoint")
