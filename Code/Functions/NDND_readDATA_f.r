###############################################################################################
#### The Non-Deterministic Network Model (NDND) Mullon et al., 2009 , Planque et al., 2014
#### readDATA function
#### Version v1.0
#### 24.06.19
#### Author : Elliot Sivel, Benjamin Planque and Ulf Lindstrøm
###############################################################################################

##### readData function
# This function takes the NDNDConfig file to find the data files and load them into R
# After loading the data, it assemble them in a NDNDData list object

readDATA<-function(directories,config_file,Data.tag){
  file.names<-scan(config_file,what=character())                     # Reading the elements given by the NDNDConfig file
                                                                     # file.names is object containing the elements of the NDNDConfig file as character             
  setwd(directories$files_dir)
  # Test the presence of the data files needed in the work directory
  
  exist.file<-vector(length=length(file.names)-4)                   # Create an empty vector of length of number of files
                                                                    # "-2" because the two last elements of the NDNDConfig file are the length of simulation and the plotting parameter
                                                                    # We don't need them now
  for (i in (1:(length(file.names)-4))) {                           # Loop on file.names to test if each file is existing in the work directory                
    exist.file[i]<-file.exists(file.names[i])                       # Output a vector of bolean values (FALSE : the file is missing ; TRUE : the file is there)
  }
  
  # Load the files
  # We want the files to be loaded if all files are available
  # We want the function to return an error message if at least one of the files is missing
  if (sum(exist.file)==(length(file.names)-4)){                                                             # Condition for the model to run : the sum of exist.file = length of file.names
                                                                                                            # If the condition is verified :
    Species<-scan(file=file.names[1],what=character(),quote="")                                             # Read species.txt
    PF<-read.table(file.names[2], header=F, sep="\t",stringsAsFactors = F)                                  # Read fluxes.txt
    Coefficients<-as.matrix(read.table(file.names[3], header=F, sep="\t",stringsAsFactors = F))             # Read coefs.txt
    Coefficients_mp<-as.matrix(read.table(file.names[4], header=F, sep="\t",stringsAsFactors = F))          # Read coefs_MP.txt
    Import<-scan(file.names[5], what=numeric())                                                             # Read import.txt
    Export<-scan(file.names[6], what=numeric())                                                             # Read export.txt
  }  else {stop("Missing file")}                                                                            # If the condition is not verified : Print error message       
  
  # Files are now loaded
  # Defining parameters and vectors from NDNDConfig and the 5 data files
  Tmax<-as.numeric(file.names[7])             # Defining the length of the simulation : Tmax
  ns<-as.numeric(length(Species))             # Defining the number of species
  nn<-ns*ns                                   # Defining the number of possible flows in the food web
  
  # Formating Data : give names to the data frames 
  
  colnames(Coefficients)<-Species                                                        # Set the species names as column names in the coefficient dataframe
  rownames(Coefficients)<-c("Biomass","Gama","Kapa","Mu","Rho","Sigma","Bta")            # Set the names of the parameters as row names in the coefficient dataframe
  colnames(Coefficients_mp)<-Species                                                     # Set the species names as column names in the coefficient dataframe
  rownames(Coefficients_mp)<-c("Fmp","Bmp","Blim","M","Catches")                                   # Set the names of the parameters as row names in the coefficient dataframe
  colnames(PF)<-Species                                                                  # Set the species names as column names in the flows matrix
  rownames(PF)<-Species                                                                  # Set the species names as row names in the flows matrix

  # Spliting the Coefficient matrix into single parameters values
  
  Biomass<-Coefficients[1,]               # Row 1 is the initial biomass for each species, it is given in tons per km2
  Gama<-Coefficients[2,]                  # Row 2 is Gama, the potential assimiliation efficiency for each species
  Kapa<-Coefficients[3,]                  # Row 3 is Kapa, the digestability of each species
  Mu<-Coefficients[4,]                    # Row 4 is Mu, the metabolic losses for each species
  Rho<-Coefficients[5,]                   # Row 5 is Rho, the parameter accounting for inertia
  Sgma<-Coefficients[6,]                  # Row 6 is Sigma, the parameter accounting for satiation. It has been renamed Sgma because Sigma is already a function in R
  Bta<-Coefficients[7,]                   # Row 7 is Beta, the value for refuge biomass
  Fmp<-Coefficients_mp[1,]                # Row 1 is Fmp, the value for F on or above Bmp
  Bmp<-Coefficients_mp[2,]                # Row 2 is Bmp, the value of Bmp
  Blim<-Coefficients_mp[3,]               # Row 3 is Blim, the value of Biomass limit for escapement in the harvest control rule
  M<-Coefficients_mp[4,]                  # Row 4 is M, the Natural mortality
  Catches<-Coefficients_mp[5,]
  
  # Import and Export values can be in vectors or matrices
  # If in a vector : replication of the vector for the length of the simulation (Tmax)
  # If in a matrix with nrow > Tmax : Importall = Import[1:Tmax,]
  # If in a matrix with nrow < Tmax : Return an error message
  # If in a matrix with nrow = Tmax : Importall = Import
  # Identical for Export
 
  if (nrow(as.matrix(t(Import)))==1){
    Importall<-matrix(rep(as.matrix(t(Import)),each=Tmax),nrow=Tmax)                       
  } else if (nrow(as.matrix(t(Import)))==Tmax){
    Importall<-Import                                                                       
  } else if (nrow(as.matrix(t(Import))) > Tmax){                      
    Importall<-Import[1:Tmax,]                                                              
    stop("1 < number of rows < Tmax")                                                       
  } else {stop("Import data missing")}                                               
  
  if (nrow(as.matrix(t(Export)))==1){
    Exportall<-matrix(rep(as.matrix(t(Export)),each=Tmax),nrow=Tmax)                        
  } else if (nrow(as.matrix(t(Export)))==1){
    Exportall<-Export                                                                       
  } else if (nrow(as.matrix(t(Export))) > Tmax){
    Exportall<-Export[1:Tmax,]                                                             
  } else if (nrow(as.matrix(t(Export))) < Tmax && nrow(as.matrix(t(Export))) != 1){
    stop("1 < number of rows < Tmax")                                                       
  } else {stop("Export data missing")}                                                      
  
  # Formating Importall and Exportall : setting colnames and rownames
  
  colnames(Importall)<-Species               # Set the species names as column names in Importall matrix
  rownames(Importall)<-1:Tmax                # Set the years as row names in the Importall matrix
  colnames(Exportall)<-Species               # Set the species names as column names in Exportall matrix
  rownames(Exportall)<-1:Tmax                # Set the years as row names in the Exportall matrix
  
  # Rest of the model uses matrix calculation
  # Easier to work with vectors than with matrices
  # Vectorization of the possible flows matrix
  
  PFv=as.vector(t(PF))                  # Transform the possible flows matrix in a single vector of length nn
  
  flows_2<-NULL
  for (i in 1:ns){
    for (j in 1:ns) {
      flux<-paste(rownames(as.matrix(PF[i,])),"-->",colnames(as.matrix(PF[i,]))[j])
      flows_2<-cbind(flows_2,flux)
    }
  }
  flows<-flows_2[which(PFv==1)]
  
  # Defining if the time series are going to be plotted
  
  Plotting<- file.names[8]                   # Binary parameter (0 = no plot ; 1 = plot) 
  
  # Defining the sampling algorithm
  
  ech<-file.names[9]
  
  # Defining the degree of fisheries compensation (alpha)
  
  alfa<-as.numeric(file.names[10])
  
  # Merging all created elements na list
  
  NDNDData<-list(Data.tag = Data.tag,
                 Tmax = Tmax,
                 Species = Species,
                 ns = ns,
                 nn = nn,
                 Biomass = Biomass,
                 Gama = Gama,
                 Kapa = Kapa,
                 Mu = Mu,
                 Rho = Rho,
                 Sgma = Sgma,
                 Bta = Bta,
                 Fmp = Fmp,
                 Bmp = Bmp,
                 Blim = Blim,
                 M = M,
                 Catches=Catches,
                 Importall = Importall,
                 Import = Import,
                 Exportall = Exportall,
                 Export =Export,
                 PF = PF,
                 PFv = PFv,
                 flows = flows,
                 Plotting = Plotting,
                 Alpha = alfa,
                 directories = directories,
                 Sampling_algorithm = ech)
  
  # Set the work directory back to the initial work directory
  setwd(wd)
  
  # Give the elements the function should return
  return(NDNDData)
}
