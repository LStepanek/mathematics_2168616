###############################################################################
###############################################################################
###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(
    ! grepl(
        "analysis_after_first_revisions$",
        getwd()
    )
){
    
    setwd(choose.dir())
    
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## spouštím sekvenci skriptů --------------------------------------------------

for(my_script in c(
    
    "global_values",           # globální proměnné, konstanty
    "helper_functions",        # definuji pomocné funkce
    "initialization",          # spouštím inicializaci balíčků a souborů
    "data_loading",            # loaduji data
    "data_processing",         # provádím předzpracování dat
    "survival_regression",     # počítám Coxovu regresi
    "logit_regression",        # počítám binární logistickou regresi
    "naive_bayes",             # počítám naivní Bayesův klasifikátor
    "support_vector_machine",  # počítám support vector machine
    "decision_trees",          # počítám rozhodovací stromy
    "random_forests",          # počítám náhodné lesy
    "neural_networks",         # počítám neuronovou síť
    "accuracy_table",          # sestavuji tabulku s předikčními přesnostmi
    "confusion_maps"           # vykresluji konfuzní matice jako heat mapy
    
)){
    
    ## nastavuji mateřskou pracovní složku do rootu ---------------------------
    
    setwd(mother_working_directory)
    
    
    ## iniciální logovací hlášky ----------------------------------------------
    
    flush.console()
    
    cat(
        paste(
            "Právě zahájeno zpracování skriptu '",
            my_script,
            ".R",
            "'.",
            sep = ""
        )
    )
    cat("\n")
    
    
    ## volám daný skript k exekuci ---------------------------------------------
    
    source(
        
        paste(
            my_script,
            ".R",
            sep = ""
        ),
        echo = TRUE,
        encoding = "UTF-8",
        max.deparse.length = Inf   # vypisuji obsah skriptu v plné délce
        
    )
    
    
    ## koncové logovací hlášky ------------------------------------------------
    
    flush.console()
    
    cat(
        paste(
            "Právě dokončeno zpracování skriptu '",
            my_script,
            ".R",
            "'.",
            sep = ""
        )
    )
    cat("\n")
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





