# immatriculations_voitures <- AQLTools::lectureBDM("001641574")
immatriculations_voitures <- AQLTools::lectureBDM("010756764")
rjd3x13::regarima(immatriculations_voitures)

# PIC en aout 2019 suivi d'un LS
AQLTools::hc_stocks(immatriculations_voitures)

# Extraction de pétrole brut
ipi061 <- window(AQLTools::lectureBDM("010767578"), end = c(2019, 12))
# AO en juil 1990, TC en mai 2019, LS (2010-06-01)
rjd3x13::regarima(ipi061)
AQLTools::hc_stocks(ipi061)

# AO 1998-08, 1999-08
ipi_voitures <- AQLTools::lectureBDM("010768140")
ipi_voitures <- window(ipi_voitures, end = c(2019, 12))
rjd3x13::regarima(ipi_voitures)
AQLTools::hc_stocks(ipi_voitures)



ipc_fioul <- AQLTools::lectureBDM("010776359")
ipc_fioul <- window(ipc_fioul, end = c(2019, 12))
rjd3x13::regarima(ipc_fioul)
AQLTools::hc_stocks(ipc_fioul)
rjd3x13::regarima(window(AQLTools::lectureBDM("001768736"), end = c(2019, 12)))


data <- window(AQLTools::lectureBDM(
	"010767602", "010768283",
	"010768285", "010768287",
	"010768307"
	), end = c(2019, 12))

plot(window(data[, 5], end = 2015))
rjd3x13::regarima(data[, 5])
rjd3x13::regarima(ipi_c_eu[, "EU28"])
AQLTools::hc_stocks(ipi_c_eu[, "EU28"])

ipi_c_eu <- eurostat::get_eurostat("sts_inpr_m",select_time = "M",
                                   filters = list(nace_r2="C",
                                                  unit = "I15", s_adj = "SCA"))
ipi_c_eu <- ipi_c_eu[as.character(ipi_c_eu$time) >= "1990-01-01", ]
ipi_c_eu <- reshape2::dcast(ipi_c_eu, time ~ geo,  value.var = "values")
ipi_c_eu <- ts(ipi_c_eu[, -1], start = c(1990, 1), frequency = 12)
# # Last date is removed due to NA:
ipi_c_eu <- window(ipi_c_eu, end = c(2019, 12))
ipi_c_eu <- window(ipi_c_eu, end = c(2020, 12))
save(ipi_c_eu,file = "data/ipi_c_eu.rda", version = 2)
