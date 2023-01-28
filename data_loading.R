###############################################################################
###############################################################################
###############################################################################

## loaduji data ---------------------------------------------------------------

setwd(
    paste(
        mother_working_directory,
        "vstupy",
        sep = "/"
    )
)

my_data <- setNames(
    
    object = read.xlsx(
        xlsxFile = "_data_.xlsx",
        sheet = 1,
        startRow = 4,
        colNames = TRUE,
        check.names = FALSE,
        detectDates = TRUE
    ),
    nm = c(
        "jmeno",
        "pohlavi",
        "rok_narozeni",
        "vek",
        "vaha",
        "vyska",
        "BMI",
        "datum_odberu_protilatek",
        "IgG",
        "IgG_hodnota",
        "IgM",
        "IgM_hodnota",
        "bolest_hlavy",
        "bolest_v_krku",
        "bolest_ci_tlak_na_hrudi",
        "bolest_svalu_ci_tela",
        "dusnost",
        "horecka",
        "subfebrilie",
        "kasel",
        "nechutenstvi",
        "prujem",
        "ryma",
        "unava",
        "vyrazka",
        "ztrata_chuti",
        "ztrata_cichu",
        "zvraceni",
        "psychicke_potize",
        "nespavost",
        "jine_potize",
        "intenzita_symptomu",
        "symptomy_od",
        "symptomy_do",
        "pracovni_neschopnost_od",
        "pracovni_neschopnost_do",
        "datum_prukazu_covid"
    )
    
)

setwd(
    mother_working_directory
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





