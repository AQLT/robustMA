library(zoo)
data_insee <- AQLTools::lectureBDM(c(
	"010756764", # immatriculations_voitures
	"010767578", # IPI Extraction de pétrole brut
	"010768140", # ipi_voitures
	"010776359", # ipc_fioul,
	"010768307" # IPI manuf LS (2008-11-01) LS (2009-01-01)
)
)

file <-"https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2022-11.csv"
data <- AQLThesis::fredmd(file, transform = FALSE, log = TRUE)
data_fred <- list(
	CE16OV = na.omit(data[, "CE16OV"]), #
	RETAILx = na.omit(data[, "RETAILx"]) # LS (2008-10-01) LS (2008-11-01)
)

saveRDS(c(lapply(as.list(data_insee), na.omit), data_fred),
		"data/data.rds")
