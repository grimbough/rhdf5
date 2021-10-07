\name{NEWS}
\title{News for Package 'rhdf5'}

\section{Changes in version 2.38.0}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item Added support for reading attributes where the datatype is either a
      64-bit or unsigned 32-bit integer.
      \item Added many functions for working with file creation property lists.
      (Thanks to @ilia-kats for the contribution, 
      https://github.com/grimbough/rhdf5/pull/95)
      \item Added support for variable length and UTF-8 encoded string datasets.
      (Thanks to Aaron Lun @LTLA for the contribution, 
      https://github.com/grimbough/rhdf5/pull/88)
    }
  }

  \subsection{CHANGES}{
    \itemize{
      \item Documentation switched to roxygen2
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item h5createDataset() now prints a warning if a chunk dimension 
      exceeds the maximum size of that dimension and automatically
      sets the corresponding chunk dimension to the maxiumum.
      (Thanks to Eric Kernfeld @ekernf01 for the report,
      https://github.com/grimbough/rhdf5/issues/97)
    }
  }
}

\section{Changes in version 2.36.0}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item Added additional hyberslab selection functions introduced in 
      HDF5 1.10.7 (\code{H5Scombine_hyperslab}, \code{H5Scombine_select}, 
      \code{H5Sget_select_npoints}).
      \item Support for read access to files in S3 buckets now includes Windows.
      \item Added function \code{h5deleteAttribute()}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Addressed issue where messages printed when loading .Rprofile were
      breaking detection of the rhdf5filters package.
      (https://github.com/grimbough/rhdf5/issues/81)
    }
  }
  
}

\section{Changes in version 2.34.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added support for read access to files in Amazon S3 buckets 
      (currently only available on non-Windows platforms).
      \item Included read and write support for dynamic compression filters 
      distributed in \pkg{rhdf5filters}.
    }
  }
  
  \subsection{CHANGES}{
    \itemize{
      \item All datasets written with h5write() now have the attribute 
      \code{rhdf5-NA.OK} added to them.  This is used to indicate that rhdf5
      was used to create the file and that the user does not need to be 
      informed that specific values will be mapped to \code{NA} in R.
    }
  }
    
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix bug in H5Dget_storage_size() where the wrong C function was 
      called.
      \item \code{NA} values in logical datatypes are now preserved when 
      written and read back into R 
      (https://github.com/grimbough/rhdf5/issues/58).
      \item Fixed error when trying to write a vector containing only empty
      strings (https://github.com/grimbough/rhdf5/issues/60).
      \item \code{h5ls()} and \code{h5dump()} no longer crash when given a 
      file containing recursive or duplicated groups 
      (https://github.com/grimbough/rhdf5/issues/48).
      \item Reading compound datasets with at least one 8-bit integer field
      now works (https://github.com/grimbough/rhdf5/issues/71).
      \item Fixed problem when writing a data.frame containing a column of raw 
      values. These columns were ommitted when creating a compound dataset.
      \item Patch early UNPROTECT() when reading a Enum type that could cause a
      segmentation fault (https://github.com/grimbough/rhdf5/issues/73)
    }
  }
}

\section{Changes in version 2.32.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added support for writing and reading datasets using the R 'raw'
      datatype.
      \item HDF5 functions H5Tset_strbuf and H5Tget_strbuf are now exposed.
    }
  }
  \subsection{CHANGES}{
    \itemize{
      \item R 'logical' values are now stored as 8-bit integers rather than
      32-bit integers.
      (Response to suggestions in https://github.com/grimbough/rhdf5/pull/55)
      \item Default compression level is 6 for all functions, rather than a
      mix of 6 or 7 in different places.
      \item Character vectors written as HDF5 strings are now null padded by 
      default, rather than null terminated. 
      (Response to https://github.com/grimbough/rhdf5/pull/50)
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Writing data.frames with more than one factor column
      no longer leads to memory explosion.
      \item Bug in h5ls() which could lead to failure in printing the dataset 
      dimensions has been fixed.
      \item Patched bug in h5write which left an unclosed HDF5 datatype 
      object when writing strings.
    }
  }
}

\section{Changes in version 2.30.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Functions H5Lmove & H5Lcopy are now exported and accessible.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Source file names are no longer mangled when printing error 
      messages.
      \item NA values in a character() vector can now be written to
      an HDF5 dataset.
    }
  }
}

\section{Changes in version 2.28.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Functions to test and set file locking have added.  These can help
      identify and address problems on Lustre and ZFS file systems.
    }
  }
  
  \subsection{USER VISIBLE CHANGES}{
    \itemize{
      \item Reading a dataset of rank one will now return an R vector, rather 
       than a one dimensional array.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item Large improvements to performance when selecting subsets
      of datasets using the `index` argument.
      \item Resolved limitations where large datasets would exceed HDF5's 4GB
      maximum chunk size.  The default options will now check if this will 
      occur and adjust chunking automatically.
      \item Single-rank datasets larger than 2^31-1 entries can now be read
      into R vectors.
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
