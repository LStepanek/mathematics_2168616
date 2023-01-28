###############################################################################
###############################################################################
###############################################################################

#### upravuji některé proměnné ------------------------------------------------

for(
    my_variable in colnames(my_data)
){
    
    my_data[, my_variable] <- as.character(
        
        my_data[, my_variable]
        
    )
    
}


for(my_variable in c(
    
    "IgG_hodnota"
    
)){
    
    my_data[, my_variable] <- gsub(
        
        ",",
        "\\.",
        gsub(
            
            "<",
            "",
            my_data[, my_variable]
            
        )
        
    )
    
}


#### měním formáty vybraných proměnných ---------------------------------------

for(
    my_variable in c(
        
        "rok_narozeni",
        "vek",
        "vaha",
        "vyska",
        "BMI"
        
    )
){
    
    my_data[, my_variable] <- as.numeric(
        
        my_data[, my_variable]
        
    )
    
}

for(my_variable in c(
    
    "IgG_hodnota",
    "IgM_hodnota"
    
)){
    
    my_data[, my_variable] <- suppressWarnings(
        as.numeric(
            
            my_data[, my_variable]
            
        )
    )
    
}


#### měním hodnoty kategorických proměnných -----------------------------------

my_data[, "pohlavi"] <- ifelse(
    
    my_data[, "pohlavi"] == "M",
    "muz",
    "zena"
    
)


for(my_variable in c(
    
    "IgG",
    "IgM"
    
)){
    
    my_data[, my_variable] <- ifelse(
        
        my_data[, my_variable] == "P", # hodnotu "H" pro hraniční protilátky
                                       # považujeme ještě za negativní
        "pozitivni",
        "negativni"
        
    )
    
}


for(my_variable in c(
    
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
    "nespavost"
    
)){
    
    my_data[, my_variable] <- ifelse(
        
        my_data[, my_variable] == "1",
        "ano",
        "ne"
        
    )
    
}


my_data[, "intenzita_symptomu"] <- unlist(
    lapply(
        
        my_data[, "intenzita_symptomu"],
        function(i){
            
            switch(
                
                i,
                "0" = {"zadne"},
                "1" = {"mirne"},
                "2" = {"stredni"},
                "3" = {"vyrazne"},
                "4" = {"velmi zavazne"}
                
            )
            
        }
        
    )
)


#### dopočítávám nové proměnné ------------------------------------------------

for(
    my_variable in c(
        
        "datum_odberu_protilatek",
        "symptomy_od",
        "symptomy_do",
        "pracovni_neschopnost_od",
        "pracovni_neschopnost_do",
        "datum_prukazu_covid"
        
    )
){
    
    my_data[, my_variable] <- as.Date(
        
        my_data[, my_variable]
        
    )
    
}


my_data <- data.frame(
    
    my_data,
    "pocet_pritomnych_symptomu" = (
        unname(
            
            apply(
                my_data[
                    ,
                    c(
                        "bolest_hlavy",
                        "bolest_v_krku",
                        "bolest_ci_tlak_na_hrudi",
                        "bolest_svalu_ci_tela",
                        "dusnost",
                        "kasel",
                        "prujem",
                        "ryma",
                        "unava",
                        "vyrazka",
                        "zvraceni"
                    )
                ],
                1,
                function(i){
                    sum(i == "ano")
                }
            )
            
        ) + unlist(
            lapply(
                1:dim(my_data)[1],
                function(i){
                    my_fever <- 0
                    my_subfever <- 0
                    if(
                        ! is.na(my_data[i, "horecka"])
                    ){
                        if(
                            my_data[i, "horecka"] == "ano"
                        ){
                            my_fever <- 1
                        }else{
                            my_fever <- 0
                        }
                    }
                    if(
                        ! is.na(my_data[i, "subfebrilie"])
                    ){
                        if(
                            my_data[i, "subfebrilie"] == "ano"
                        ){
                            my_subfever <- 1
                        }else{
                            my_subfever <- 0
                        }
                    }
                    return(
                        min(
                            1,
                            my_fever + my_subfever
                        )
                    )
                }
            )
        ) + unlist(
            lapply(
                1:dim(my_data)[1],
                function(i){
                    my_taste_loss <- 0
                    my_smell_loss <- 0
                    if(
                        ! is.na(my_data[i, "ztrata_chuti"])
                    ){
                        if(
                            my_data[i, "ztrata_chuti"] == "ano"
                        ){
                            my_taste_loss <- 1
                        }else{
                            my_taste_loss <- 0
                        }
                    }
                    if(
                        ! is.na(my_data[i, "ztrata_cichu"])
                    ){
                        if(
                            my_data[i, "ztrata_cichu"] == "ano"
                        ){
                            my_smell_loss <- 1
                        }else{
                            my_smell_loss <- 0
                        }
                    }
                    return(
                        min(
                            1,
                            my_taste_loss + my_smell_loss
                        )
                    )
                }
            )
        )
    ),
    "delka_trvani_symptomu" = as.numeric(
        
        my_data[, "symptomy_do"] - my_data[, "symptomy_od"]
        
    ),
    "interval_mezi_odberem_protilatek_a_prukazem_covid" = as.numeric(
        
        my_data[, "datum_odberu_protilatek"] -
        my_data[, "datum_prukazu_covid"]
        
    ),
    "interval_mezi_odberem_protilatek_a_zacatkem_symptomu" = as.numeric(
        
        my_data[, "datum_odberu_protilatek"] -
        my_data[, "symptomy_od"]
        
    ),
    stringsAsFactors = FALSE
    
)


#### nakonec pořadí hodnot v datasetu permutuji -------------------------------

set.seed(123)

my_data <- my_data[
    sample(
        x = c(1:dim(my_data)[1]),
        size = dim(my_data)[1],
        replace = FALSE
    )
    ,
]


#### vyřazuji pacienty, u kterých chybí informace o protilátkách --------------

my_data <- my_data[
    
    which(
        !is.na(my_data[, "IgG"]) &
        !is.na(my_data[, "IgM"])
    )
    ,
    
]


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





