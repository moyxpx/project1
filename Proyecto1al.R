##PREGUNTA 1: Simulación para determinar el tamaño promedio de la cola por día cuando hay 1,2,3,4,5,6,7 servidores



cajeros_tiempo <- function(N=20,k=2,dia=1) {
  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  #Tiempo que tardan los clientes en llegar
  
  tiempodellegada <- sample(entretiempos,N,replace = T,prob = p)
  
  llegadas <- cumsum(tiempodellegada)
  
  # Tiempo de atención al cliete
  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean = 8,sd = 5)))
  
  # Vectores de servicio
  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiempodeservicio <- rep(0,N)
  
  
  # Atencion al cliente por cajero
  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    
  }
  
  # Se ejecuta loop para los clientes pendiente de atencion 
  
  for (i in (k+1):N) {
    
    tiempoentreservicios[i] <- max(llegadas[i],min(tiempoatencion[i-1],tiempoatencion[i-2]))
    if (tiempoentreservicios[i]==tiempoentreservicios[i-1]) {tiempoentreservicios[i] <- max(tiempoatencion[i-3],tiempoatencion[i-2])}
    
    tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
  }
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(clientesllegando=tiempodellegada,totalclientes=llegadas,tiempodeservicio=tiempodeatencion,inicioservicio=tiempoentreservicios,final=tiempoatencion)
  return(df)
  
}

#Función dependiendo del número de cajeros que atiendan


Cajero1_tiempo <- function(dia=1) {
  
  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 1
  
  
  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  
  
  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  
  
  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  
  
  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  
  
  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
    
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    }  
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}


Cajero2_tiempo <- function(dia=1) {
  

  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 2
  
  
  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  
  
  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  
  
  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  
  
  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  
  
  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
    
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    }  else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } 
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}


Cajero3_tiempo <- function(dia=1) {
  

  

  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 3
  

  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  

  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  

  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  

  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  

  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
  
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[2]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[3]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[3] <- tiempoatencion[i]
      
  
      
    } else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } else if (server_time[3] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[3]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[3] <- tiempoatencion[i]
    }
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}




Cajero4_tiempo <- function(dia=1) {
  
  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 4
  

  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  

  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  

  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  

  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  

  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
  
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[2]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[3]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[3] <- tiempoatencion[i]
      
      
    } else if (llegadas[i] >= server_time[4]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[4] <- tiempoatencion[i]
      

      
    } else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } else if (server_time[3] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[3]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[3] <- tiempoatencion[i]
    }else if (server_time[4] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[4]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[4] <- tiempoatencion[i]
    }
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}


Cajero5_tiempo <- function(dia=1) {
  

  
  

  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 5
  
  

  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  

  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  

  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  

  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  

  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
 
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[2]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[3]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[3] <- tiempoatencion[i]
      
      
    } else if (llegadas[i] >= server_time[4]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[4] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[5]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[5] <- tiempoatencion[i]
      
    }else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } else if (server_time[3] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[3]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[3] <- tiempoatencion[i]
    }else if (server_time[4] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[4]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[4] <- tiempoatencion[i]
    }else if (server_time[5] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[5]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[5] <- tiempoatencion[i]
    }
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}



Cajero6_tiempo <- function(dia=1) {
  

  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 6
  
  

  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  

  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  

  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  

  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  

  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
   
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[2]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[3]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[3] <- tiempoatencion[i]
      
      
    } else if (llegadas[i] >= server_time[4]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[4] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[5]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[5] <- tiempoatencion[i]
      
    }else if (llegadas[i] >= server_time[6]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[6] <- tiempoatencion[i]
      
    }else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } else if (server_time[3] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[3]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[3] <- tiempoatencion[i]
    }else if (server_time[4] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[4]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[4] <- tiempoatencion[i]
    }else if (server_time[5] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[5]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[5] <- tiempoatencion[i]
    }else if (server_time[6] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[6]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[6] <- tiempoatencion[i]
    }
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}

Cajero7_tiempo <- function(dia=1) {
  

  
  if (dia==1) p <- c(0.1,0.15,0.1,0.35,0.25,0.05,0)
  if (dia==2) p <- c(0.1,0.1,0.15,0.2,0.35,0.1,0)
  if (dia==3) p <- c(0,0.1,0.1,0.2,0.1,0.25,0.25)
  if (dia==4) p <- c(0,0.15,0.2,0.2,0.15,0.15,0.15) 
  if (dia==5) p <- c(0.15,0.15,0.2,0.2,0.1,0.1,0.1)
  if (dia==6) p <- c(0.2,0.15,0.1,0.5,0.05,0,0)
  if (dia==7) p <- c(0.35,0.25,0.2,0.1,0.1,0,0)
  
  entretiempos <- c(0,1,2,3,4,5,6)  
  
  llegadas <- 0
  tiempollegada <- 0
  k <- 7
  
  

  
  llegadas[1] <- sample(entretiempos,1,replace = T,prob = p)
  N <- 1
  
  while (llegadas[N] < 480) {
    
    N <- N + 1   
    tiempollegada[N] <- sample(entretiempos,1,replace = T,prob = p)
    llegadas[N] <- llegadas[N-1] + tiempollegada[N]
  }
  

  
  tiempodeatencion <- ceiling(abs(rnorm(N,mean=8,sd=5)))
  

  
  tiempoentreservicios <- rep(0,N)
  tiempoatencion <- rep(0,N)
  tiemposervicio <- rep(0,N)
  server_time <- rep(0,k)
  

  
  for (j in 1:k) {
    
    tiempoentreservicios[j] <- llegadas[j]
    tiempoatencion[j] <- tiempoentreservicios[j]+tiempodeatencion[j]
    server_time[j] <- tiempoatencion[j]
    
  }
  

  
  for (i in seq(from=(k+1), to=N, by=1)) {
    
  
    
    if (llegadas[i] >= server_time[1]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[1] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[2]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[2] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[3]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[3] <- tiempoatencion[i]
      
      
    } else if (llegadas[i] >= server_time[4]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[4] <- tiempoatencion[i]
      
    } else if (llegadas[i] >= server_time[5]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[5] <- tiempoatencion[i]
      
    }else if (llegadas[i] >= server_time[6]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[6] <- tiempoatencion[i]
      
    }else if (llegadas[i] >= server_time[7]) {
      
      tiempoentreservicios[i] <- llegadas[i]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i]  
      server_time[7] <- tiempoatencion[i]
      
    }else  if (server_time[1] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[1]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[1] <- tiempoatencion[i]
      
    } else if (server_time[2] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[2]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[2] <- tiempoatencion[i]
      
    } else if (server_time[3] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[3]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[3] <- tiempoatencion[i]
    }else if (server_time[4] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[4]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[4] <- tiempoatencion[i]
    }else if (server_time[5] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[5]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[5] <- tiempoatencion[i]
    }else if (server_time[6] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[6]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[6] <- tiempoatencion[i]
    }else if (server_time[7] == min(server_time)) {
      
      tiempoentreservicios[i] <- server_time[7]
      tiempoatencion[i] <- tiempoentreservicios[i]+tiempodeatencion[i] 
      server_time[7] <- tiempoatencion[i]
    }
    
  }
  
  
  tiempodeespera <- tiempoentreservicios - llegadas
  tiempototal <- tiempoatencion - llegadas
  
  df <- data.frame(entre_llegadas=tiempollegada,totalclientes=llegadas,tiempo_servicio=tiempodeatencion,inicio_servicio=tiempoentreservicios,finalizacion=tiempoatencion)
  return(df)
  
}

#Tamaños promedio por ventanillas
ventanilla1 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero1_tiempo(i)
    ventanilla1 <- rbind(ventanilla1,x)
  }
}

ventanilla2 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero2_tiempo(i)
    ventanilla2 <- rbind(ventanilla2,x)
  }
}

ventanilla3 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7){
    x <- Cajero3_tiempo(i)
    ventanilla3 <- rbind(ventanilla3,x)
  }
} 

ventanilla4 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero4_tiempo(i)
    ventanilla4 <- rbind(ventanilla4,x)
  }
}


ventanilla5 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero5_tiempo(i)
    ventanilla5 <- rbind(ventanilla5,x)
  }
}

ventanilla6 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero6_tiempo(i)
    ventanilla6 <- rbind(ventanilla6,x)
  }
}

ventanilla7 <- 0
for (k in 1:52) {
  x <- 0
  for (i in 1:7) {
    x <- Cajero7_tiempo(i)
    ventanilla7 <- rbind(ventanilla7,x)
  }
}

#Funcion que determina los tiempos promedio por ventanilla dependiendo la cantidad de servidores
resumenparametros <- function(x=Cajero2_tiempo()) {
  
 
  
  c <- 0

  
  N <- length(x$totalclientes)
  tiemposervicio <- rep(0,N) 
  
  tiempodeespera <- x$inicio_servicio-x$totalclientes # tiempo de espera en la cola
  tiempototal <- x$final-x$totalclientes # tiempo total en el banco       
  tiempodeatencion <- x$final-x$inicio_servicio # tiempo de atencion al cliente
  tiempollegada <- x$entre_llegadas # tiempo entre la llegada de cada cliente
  llegadas <- x$totalclientes # numero de clientes
  tiempoatencion <- x$finalizacion # Se termina de atender al cliente
  tiempoentreservicios <- x$inicio_servicio # Empieza a atender al cliente

  
  # Calculos de medias
  
  mediaespera <- mean(tiempodeespera)  
  mediatiempo <- mean (tiempototal) 
  mediaatencion <- mean(tiempodeatencion)
  mediallegada <- mean(tiempollegada) 
  
  # Cantidad de personas que hacen cola
  
  for (k in 1:N) {
    if (llegadas[k] == tiempoentreservicios[k]) {c <- c + 1}
  }
  
  probabilidad1 <- 1- (c/N)
  Pclientes <- round(N*probabilidad1, digits = 0) # Promedio de personas que hacen colas
  
  
  df2 <- data.frame(promediodeespera=mediaespera,probdecola=probabilidad1,promediodeatencion=mediaatencion,promedioenbanco=mediatiempo,promediodellegada=mediallegada,clientesenlacola=Pclientes)
  return(df2)
  
}

#Funcion para pregunta 3


restriccioncajero1 <- function(d=Cajero1_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero1_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}


restriccioncajero2 <- function(d=Cajero2_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero2_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}
restriccioncajero3 <- function(d=Cajero3_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero3_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}
restriccioncajero4 <- function(d=Cajero4_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero4_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}
restriccioncajero5 <- function(d=Cajero5_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero5_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}
restriccioncajero6 <- function(d=Cajero6_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero6_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}
restriccioncajero7 <- function(d=Cajero7_tiempo()) {
  
  count <- rep(0,1000)
  
  for (k in 1:1000) {
    
    d <- Cajero7_tiempo()
    z <- which(d$inicio_servicio -d$totalclientes !=0)
    d <- d[z,]  # Dataframe solo de los que hicieron cola
    
    a <- rep(1000000,5)
    d <- rbind(d,a)
    
    i <- 1
    j <- 0
    c <- rep(0,50)
    
    
    while (i < (length(d$totalclientes)-5)) {
      
      j <- j + 1
      flag <- 0
      minimo <- d$inicio_servicio[i]
      
      while (flag == 0) {
        
        if (d$totalclientes[i+1] < minimo) {
          
          c[j] <- c[j] + 1
          i <- i + 1
          flag <- 0
        }
        
        if (d$totalclientes[i+1] >= minimo) { 
          
          c[j] <- c[j] + 1
          i <- i +1
          flag <- 1
          
        }
      }
    }
    
    c <- c[-which(c==0)]
    c <- c[which(c-20>0)]
    
    count[k] <- sum(c)
    
  }
  
  return(mean(count))
  
}

## RESPUESTAS RPEGUNTA 1
#El tiempo promedio de espera con 1 ventanillero es de 740.88 minutos
resumenparametros(ventanilla1)

#El tiempo promedio de espera con 2 ventanillero es de 254.50 minutos
resumenparametros(ventanilla2)

#El tiempo promedio de espera con 3 ventanillero es de 97 minutos
resumenparametros(ventanilla3)

#El tiempo promedio de espera con 4 ventanillero es de 42 minutos
resumenparametros(ventanilla4)

#El tiempo promedio de espera con 5 ventanillero es de 18 minutos
resumenparametros(ventanilla5)
#El tiempo promedio de espera con 6 ventanillero es de 7 minutos
resumenparametros(ventanilla6)
#El tiempo promedio de espera con 7 ventanillero es de 1.58 minutos
resumenparametros(ventanilla7)

## RESPUESTAS RPEGUNTA 2
##PREGUNTA 2: Si no se quiere que un lciente esté más de Y minutos en cola Cuantos agentes tienen que tener el banco?

#Según la funcion resumenparametros la cantidad de cajeros que debe haber si no se desea que esten mas de 10 minutos es de 5 Cajeros. 

##RESPUESTAPREGUNTA3
#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 1 CAJERO, EN PROMEDIO NO SE ATIENDEN 163 PERSONAS
restriccioncajero1()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 2 CAJERO, EN PROMEDIO NO SE ATIENDEN 137 PERSONAS
restriccioncajero2()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 3 CAJERO, EN PROMEDIO NO SE ATIENDEN 22 PERSONAS
restriccioncajero3()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 4 CAJERO, EN PROMEDIO NO SE ATIENDEN 0 PERSONAS
restriccioncajero4()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 5 CAJERO, EN PROMEDIO NO SE ATIENDEN 0 PERSONAS
restriccioncajero5()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 6 CAJERO, EN PROMEDIO NO SE ATIENDEN 0 PERSONAS
restriccioncajero6()

#SI SE LA CANTIDAD LIMITE DE PERSONAS ES 20 Y SE TIENE 7 CAJERO, EN PROMEDIO NO SE ATIENDEN 0 PERSONAS
restriccioncajero7()

