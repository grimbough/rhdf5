\name{NEWS}
\title{News for Package 'rhdf5'}

\section{Changes in version 2.14.0}{
  \subsection{NEW FEATURES}{
    \itemize{
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Memory allocation on heap instead of stack for reading large datasets (Thanks to a patch from Jimmy Jia).
    }
  }
}

\section{Changes in version 2.12.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Filenames are expanded with normalizePaths.
      \item New function h5set_extent implemented.
      \item New low level function H5Sset_extent_simple implemented.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Segmentation fault while writing data type names for uncommitted data types.
    }
  }
}

\section{Changes in version 2.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added support for HDF5 property lists.
      \item Added property list arguments to H5Dcreate and H5Dopen.
      \item New function h5readAttributes implemented that reads all HDF5 attributes of one object.
      \item New function h5version implemented.
      \item fillValue parameter added to h5createDataset.
      \item New low level general library functions H5Lcreate_external, H5Fis_hdf5, H5Fget_filesize, H5Fget_name, H5Pcreate, H5Pcopy, H5Pget_class, H5Pclose, H5Pclose_class, H5Pset_char_encoding, H5Pset_create_intermediate_group, H5Pset_chunk_cache, H5Pset_layout, H5Pset_chunk, H5Pget_chunk, H5Pset_deflate, H5Pset_fill_value, H5Pset_fill_time, H5Pset_alloc_time, H5Pequal implemented.
      \item Support for parallel Make (make -j)
    }
  }
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
      \item A warning is shown in high level function (h5read, h5write and others), if an open HDF5 handle already exists for the specified filename.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Error in h5write for 0-length objects, as a consequence of automatic determining chunk size
      \item missing size parameter message in h5createDataset now correctly display
      \item checking for open file identifiers in h5read and h5ls now only searches for file names in open files, groups and datasets.
      \item assignment has now correct pointer target type (void *) in H5Pset_fill_value
    }
  }
}

\section{Changes in version 2.8.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New function h5version implemented.
      \item New low level general library functions H5open, H5close, H5garbage_collect, H5get_libversion, and H5Dset_extent implemented.
    }
  }
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
      \item h5createDataset automatically uses chunking and compression.
      \item Added a warning if chunk size is equal to dimensions for large compressed datasets.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item C-stack overflow when reading large fixed-length strings.
      \item error in i/o with chunksize or blocksize parameters.
      \item compiling errors due to missing int return value.
    }
  }
}

\section{Changes in version 2.6.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item support for logical added
      \item support for reading attributes added (use read.attributes=TRUE)
      \item enabeled compression for data.frame in h5write
    }
  }
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
      \item Use BiocStyles for package vignette
    }
  }
}

\section{Changes in version 2.4.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item support for reading 64-bit integers added
      \item support for reading variable length strings added
      \item support for reading scalar objects added
    }
  }
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
      \item NEWS.Rd added
      \item display of chunksize.pdf as a vignette avoided
    }
  }
}
