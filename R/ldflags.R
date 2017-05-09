 LdFlags <- function(){
   os.type <- Sys.info()[['sysname']]
   libpath <- paste0("/lib", Sys.getenv("R_ARCH"))
   libpath <- tools::file_path_as_absolute(base::system.file(libpath, package = "rhdf5" ))
   if(os.type == "Windows")
   {
     lib <- "libhdf5ForBioC-7"
     flags <- paste0("-L",libpath, " -l", lib)
   }else
   {
     lib <- "libhdf5.a"
     lib <- file.path(libpath, lib)
     
     flags <- paste0("-l",lib)
     
   }
    
   cat(flags)
   
 }
