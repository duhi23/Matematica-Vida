###################################################
#####     Seguros de vida y supervivencia     #####
###################################################

library("readxl")
library("dplyr")

tabla <- read_excel("TablaMortalidad.xlsx", sheet = 1, col_names = FALSE)
colnames(tabla) <- c("years", "days", "lx", "ly")


# Número de sobrevivientes 
lx <- function(edad, genero){
      if(genero==0){
            res <- as.numeric(tabla[edad,4])
      } else {
            res <- as.numeric(tabla[edad,3])
      }
      return(res)
}

# Probabilidad de supervivencia
tpx <- function(edad, salto, genero){
      return(lx(edad+salto, genero)/lx(edad,genero))
}

# Probabilidad de muerte
tqx <- function(edad, salto, genero){
      return(1-(lx(edad+salto, genero)/lx(edad,genero)))
}

