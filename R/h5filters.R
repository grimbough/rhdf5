h5checkFilters <- function(h5id) {
    
    truetype = H5Iget_type(h5id)
    
    if(truetype == "H5I_DATASET") {
        
        pid <- H5Dget_create_plist(h5id)
        on.exit(H5Pclose(pid))
        
    } else if(truetype == "H5I_GENPROP_LST") {
        ## we require this to be a DATASET creation plist
        pid <- h5checktypeAndPLC(h5id, "H5P_DATASET_CREATE")
    }
    
    if( (H5Pget_nfilters(pid) > 0) && (H5Pall_filters_avail(pid) == 0) )
        message("Not all required filters are available")
    
    return(TRUE)
}