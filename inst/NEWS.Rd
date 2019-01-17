\name{NEWS}
\title{News for Package 'rhdf5'}

\section{Changes in version 2.28.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Functions to test and set file locking have added.  These can help
      identify and address problems on Lustre and ZFS file systems.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item Some improvements to performance when selecting subsets
      of datasets using the `index` argument.
    }
  }
}

\section{Changes in version 2.26.0}{

  \subsection{BUG FIXES}{
    \itemize{
      \item Compilation problems on Solaris have been addressed.
    }
  }
}

\section{Changes in version 2.24.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Removed bundled HDF5 library - rhdf5 now depends on Rhdf5lib.
      This updates the version of HDF5 to 1.8.19.
      \item Functions H5Ldelete() and h5delete() added to provide mechanisms
      for removing items from HDF files.
      \item Added argument `native` to many functions, which allows data to be
      treated as row-major rather than column-major, improving portability with
      other programming languages.
      \item Added function H5Sunlimited() allowing creation of extensible 
      datasets - thanks to Brad Friedman 
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item Datasets can now be subset using `[` and a range of values 
      e.g. did[,1:5].
      \item Writing a data.frame that contains factors and setting 
      DataFrameAsCompound=FALSE now works.
      \item Many functions that would leave open file handles after exiting
      under error conditions have been fixed.
      \item Performance improvements in h5read().
    }
  }
}

\section{Changes in version 2.20.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Indexing into spaces with more than .Machine$integer.max
      elements is supported using numeric (rather than integer)
      indexing; this provides exact indexing into spaces with about 51
      bits of precision.
      \item Zero-length indexing is now supported (returning zero-length
      slabs).
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Using \code{bit64conversion = "double"} would always warn
      about loss of precision, but now only warns when precision is
      actually lost.
    }
  }
}

\section{Changes in version 2.18.0}{
  \subsection{NEW FEATURES}{
    \itemize{
	\item The low-level functions H5Pset_libver_bounds and H5Pget_libver_bounds is implemented. Creating files that can only be read by library versions 1.8 or later allows the usage of large attributes and improves performance.
	}
  }
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
        \item Per default all HDF5 files will be created with version 1.8 as lower bound. That means the created files can only be read with HDF5 library versions >= 1.8. This changes allows the usage of large attributes and leads to performance improvements. If one wants to create a file that is readable with the earliest version of HDF5, one has to call H5Fcreate with fapl=h5default("H5P").
	\item Warning messages from the package C code can now be suppressed by the R-function suppressWarnings().
	}
  }
}

\section{Changes in version 2.16.0}{
  \subsection{NEW FEATURES}{
    \itemize{
	\item New access of HDF5 files by file, group and dataset handles. HDF5 groups and datasets can be read and written by the $-operator (e.g. h5f$A) and the [-operator can be used for partial reading and writing of datasets (e.g. h5d[3,,]).
	\item New low level general library function H5Dget_create_plist implemented.
	\item Removed #include <R.H> from external C code. To be compatible with newest C-compilers and current R-devel
	}
  }
}

\section{Changes in version 2.14.0}{
  \subsection{NEW FEATURES}{
    \itemize{
	\item improved handling of error messages: HDF5 error messages are simplified and forwarded to R.
	\item When reading integer valued data, especially 64-integers and unsigned 32-bit integers, overflow values are now replaced by NA's and a warning is thrown in this case.
	\item When coercing HDF5-integers to R-double, a warning is displayed when integer precision is lost.
	\item New low level general library function H5Dget_storage_size implemented.
	}
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Memory allocation on heap instead of stack for reading large datasets (Thanks to a patch from Jimmy Jia).
      \item Some bugs have been fixed for reading large 64-bit integers and unsigned 32-bit integers.
      \item A bug was fixed for reading HDF5 files containing soft links.
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
