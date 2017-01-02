#########################################################
#######   Funciones de Probabilidades - Seguros   #######
#########################################################

library("readxl")
library("dplyr")

# Lectura tabla de mortalidad
tabla <- read_excel("TablaMortalidad.xlsx", sheet = 1, col_names = FALSE)
# Nombres columnas
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

# Probabilidad de muerte diferida
utqx <- function(edad, inicio, salto, genero){
      return(tqx(edad, inicio + salto, genero)- tqx(edad, inicio, genero))
}

# Factor financiero - actuarial
nEx <- function(edad, periodo, interes, genero){
      return(tpx(edad, periodo, genero)*(1+interes)^(-periodo))
}

# Seguro de vida temporal
A1xn <- function(edad, omega, interes, genero){
      suma=0
      for(i in 0:(omega-1)){
         suma = suma + utqx(edad,k,1,genero)*(1+interes)^(-periodo)   
      }
      return(suma)
}

# Seguro de supervivencia
Axn1 <- function(edad, periodo, interes, genero){
      return(nEx(edad, periodo, interes, genero))
}

# Seguro Mixto
Ax <- function(edad, periodo, interes, genero){
      return(A1xn(edad, periodo, interes, genero) + Axn1(edad, periodo, interes, genero))
}

# 
