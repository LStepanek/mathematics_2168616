###############################################################################
###############################################################################
###############################################################################

## instaluji a inicializuji balíčky -------------------------------------------

invisible(
    
    lapply(
        
        c(
            
            "ResourceSelection",
            "survival",
            "party",
            "partykit",
            "openxlsx",
            "xtable",
            "nnet",
            "e1071",
            "rpart",
            "rpart.plot",
            "randomForest",
            "e1071",
            "neuralnet"
            
        ),
        
        function(my_package){
            
            if(
                ! my_package %in% rownames(installed.packages())
            ){
                
                install.packages(
                    
                    my_package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                    
                )
                
            }
            
            library(
                
                my_package,
                character.only = TRUE
                
            )
            
        }
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R, bude-li třeba -------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip")


## ----------------------------------------------------------------------------

###############################################################################

## zakládám podsložku "výstupy" -----------------------------------------------

invisible(
    
    lapply(
        
        c(
            "vstupy",
            "vystupy"
        ),
        
        function(my_directory){
            
            if(!file.exists(my_directory)){
                
                dir.create(
                    
                    file.path(
                        
                        mother_working_directory,
                        my_directory
                        
                    )
                    
                )
                
            }
            
        }
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





