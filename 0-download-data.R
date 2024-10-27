library(zoo)
id_bank <- c(
	# Immatriculations de voitures particulières neuves
	immat = "010756764", # immatriculations_voitures
	#IPI Extraction de pétrole brut (NAF rév. 2, niveau groupe, poste 06.1)
	ipi_petrole_brut = "010767578", # IPI Extraction de pétrole brut
	# IPI  Construction de véhicules automobiles (NAF rév. 2, niveau classe, poste 29.10)
	ipi_voitures = "010768140", # ipi_voitures
	# Fioul lourd (Singapour) - FOB - Indice en euros - Base 2010
	i_fioul_lourd = "010776359", # ipc_fioul,
	# IPI Industrie manufacturière (NAF rév. 2, niveau A10, poste CZ)
	ipi_manuf = "010768307" # IPI manuf LS (2008-11-01) LS (2009-01-01)
)
data_insee <- AQLTools::lectureBDM(
	id_bank
)
colnames(data_insee) <- names(id_bank)
file <-"https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2022-11.csv"
data <- AQLThesis::fredmd(file, transform = FALSE, log = TRUE)
data_fred <- list(
	CE16OV = na.omit(data[, "CE16OV"]), #
	RETAILx = na.omit(data[, "RETAILx"]) # LS (2008-10-01) LS (2008-11-01)
)

saveRDS(c(lapply(as.list(data_insee), na.omit), data_fred),
		"data/data.rds")
