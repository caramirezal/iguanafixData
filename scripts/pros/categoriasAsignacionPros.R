## Asignación de categorías a pros


## reading local files
pros <- read.csv("~/data/pros/ACTIVEPROS.csv")
cat <- read.csv("~/data/pros/Categoraspros_2017-10-25_1341.csv")

## column definitions
pros$PLOM <- rep('NO',length(length(pros$COMPANY_ID)))
pros$ELECT <- rep('NO',length(length(pros$COMPANY_ID)))
pros$CARP <- rep('NO',length(length(pros$COMPANY_ID)))
pros$PINT <- rep('NO',length(length(pros$COMPANY_ID)))
pros$IMPER <- rep('NO',length(length(pros$COMPANY_ID)))
pros$ALBAÃ. <- rep('NO',length(length(pros$COMPANY_ID)))
pros$HERRE <- rep('NO',length(length(pros$COMPANY_ID)))
pros$FUMIGA <- rep('NO',length(length(pros$COMPANY_ID)))
pros$INTERF <- rep('NO',length(length(pros$COMPANY_ID)))
pros$CONTRAT <- rep('NO',length(length(pros$COMPANY_ID)))


## Asignación de categorías
for (i in 1:length(cat$company_id)) {
        ## Plomero
        if ( cat$category_id[i] == 122) {
                pros[pros$COMPANY_ID==cat$company_id[i],"PLOM"] <- 'YES'
        } 
        ## Electricista
        if ( cat$category_id[i] == 125) {
                pros[pros$COMPANY_ID==cat$company_id[i],"ELECT"] <- 'YES'
        } 
        ## carpintero
        if ( cat$category_id[i] == 132) {
                pros[pros$COMPANY_ID==cat$company_id[i],"CARP"] <- 'YES'
        } 
        ## Plomero
        if ( cat$category_id[i] == 126) {
                pros[pros$COMPANY_ID==cat$company_id[i],"PINT"] <- 'YES'
        }
        ## impermeablizador
        if ( cat$category_id[i] == 143) {
                pros[pros$COMPANY_ID==cat$company_id[i],"IMPER"] <- 'YES'
        }
        ## albañil
        if ( cat$category_id[i] == 130) {
                pros[pros$COMPANY_ID==cat$company_id[i],"ALBAÃ."] <- 'YES'
        }
        ## herrero
        if ( cat$category_id[i] == 135) {
                pros[pros$COMPANY_ID==cat$company_id[i],"HERRE"] <- 'YES'
        }
        ## Fumigadores (no hay fumigaciones en el último año)
        #if ( cat$category_id[i] == ------- ) {
        #        pros[pros$COMPANY_ID==cat$company_id[i],"FUMIGA"] <- 'YES'
        ## Instalador de interferones
        if ( cat$category_id[i] == 138) {
                pros[pros$COMPANY_ID==cat$company_id[i],"CONTRAT"] <- 'YES'
        }        
        if ( cat$category_id[i] == 138) {
                pros[pros$COMPANY_ID==cat$company_id[i],"CONTRAT"] <- 'YES'
        } 
                
}



write.csv(pros,file = "~/data/pros/prosCategorias.csv",row.names = FALSE)
