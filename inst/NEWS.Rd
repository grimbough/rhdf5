\name{NEWS}
\title{News for Package 'rhdf5'}

\section{Changes in version 2.58.0}{

  \subsection{New features}{
    \itemize{
      \item `H5Fopen()` now supports opening remote files on S3. Thanks to
      a PR contribution from Michael Sumner (https://github.com/mdsumner).
      \item New \code{h5getAllChunkInfo()} function iterates over all stored 
      chunks of a chunked HDF5 dataset, returning a list with elements 
      \code{offset} (chunk origins as an integer matrix), \code{filter_mask},
      \code{addr} (file byte offsets), and \code{size} (compressed chunk 
      sizes). Requires chunked storage; contiguous datasets raise an error
      Thanks to PR #217 from Michael Sumner (https://github.com/mdsumner).
    }
  }

  \subsection{Deprecations & breaking changes}{
    \itemize{
      \item The `H5Pget_version()` function was removed entirely after being 
      deprecated in version 2.56.0.
      \item It is no longer possible to pass a missing value or "default" to 
      the `bit64conversion` argument in `H5Aread()` and `H5Dread()`.
      \item The `native` argument to `H5Pcreate()` has been removed entirely 
      after being deprecated in version 2.56.0.
    }
  }

}

\section{Changes in version 2.56.0}{

  \subsection{Breaking changes}{
    \itemize{
      \item The default value for the `bit64conversion` argument in `H5Aread()`
      and `H5Dread()` has been set explicitly to `"int"`.
      Relying on a missing value or `"default"` results in a warning and will be
      disallowed in the next release cycle.
      Users should either specify `bit64conversion="int"` explicitly, or omit
      this argument entirely if they wish to use the provided default.
      \item The `native` argument to `H5Pcreate()` now triggers an explicit
      deprecation warning and will be removed in the next release cycle. This
      argument was already documented as defunct (without warning) in Bioconductor
      3.10.
      \item This package now uses HDF5 version 1.14.6 via Rhdf5lib 1.33.3.
    }
  }

  \subsection{Bug fixes}{
    \itemize{
      \item The value ordering (row-major vs column-major) is not correct when
      reading unsigned integers with \code{native=TRUE}. Thanks to Jared Lumpe 
      for the report and detailed reproducible example:
      https://github.com/Huber-group-EMBL/rhdf5/issues/157.
      \item Minor memory protection issues identified by rchk has been resolved.
      This might prevent some rare crashes.
      \item Subsetting and assignment via `[` and `[`<-` operators using a variable
      to define the indices now works as expected. This was due to an issue in the 
      timing of evaluation of the index arguments. Thanks to Michael Schubert and
      Malcolm Perry for the report and reproducible example:
      https://github.com/Huber-group-EMBL/rhdf5/issues/69.
      \item Subsetting with `drop=FALSE` no longer errors. Thanks to Michael
      Schubert for the report:
      https://github.com/Huber-group-EMBL/rhdf5/issues/68.
    }
  }

  \subsection{Minor changes}{
    \itemize{
      \item Documentation for the `name` argument as well as the error message
      when trying to work on a non-existing object now include a hint to use
      `h5ls()` to list the objects in a file. Inspired from a suggestion from
      Frederik Ziebell.
      \item A warning about void pointer arithmetic in H5Aread_helper_ENUM 
      (C code) has been resolved. This increases portability across different 
      compilers.
      \item The \code{H5type} argument in \code{h5createAttribute()}  and 
      \code{h5createDataset()} now behaves more consistently across both 
      functions. Both functions now accept either a HDF5 data type id as a 
      string, or the output of \code{H5Tcopy()}. Thanks to Luke Zappia for
      the report: \url{https://github.com/Huber-group-EMBL/rhdf5/issues/162}.
      \item \code{h5version()} now also reports the version of the Rhdf5lib
      package.
    }
  }

  \subsection{Internal changes}{
    \itemize{
      \item Code coverage reports via codecov have been restored.
      \item The pkgdown documentation website for this package have been restored.
      \item Static analysis via the lintr package is now continuously changed on 
      every changes via GitHub Actions.
      \item Number of skipped tests (when testing, e.g., features only available
      in certain situations) is now explicitly reported.
      \item GitHub Actions now run on ARM64 versions of all operating systems.
      \item rchk is now part of our continuous integration setup to detect potential
      memory issues in the C code.
    }
  }

}

\section{Changes in version 2.54.0}{

  \subsection{CHANGES}{
    \itemize{
      \item Disabled dataset timestamps, which were applied for compound 
      datasets but not other types.  Back ported to 2.52.1.
      (Thanks to Luke Zappia for reporting this 
      https://github.com/Huber-group-EMBL/rhdf5/issues/160)
      \item Exposed HDF5 API functions \code{H5Oget_info()}, \code{H5Fget_intent()}
      \code{H5Dget_num_chunks()}.
      \item Added function \code{h5readTimestamps} to extract object timestamp
      metadata.
      \item The previously deprecated `cset` argument in
      \code{h5createAttribute()} has been removed entirely. Please use the
      `encoding` argument instead.
    }
  }

}

\section{Changes in version 2.52.0}{

  \subsection{CHANGES}{
    \itemize{
      \item Removed used of the non-API `STRING_PTR` function when permuting
      string datasets.
    }
  }

  \subsection{Bug fixes}{
    \itemize{
      \item Fixed overflow when trying to write a large number of very long
      strings.  Backported to 2.50.1. 
      (Thanks to Aaron Lun @LTLA for reporting this
      https://github.com/grimbough/rhdf5/issues/151).
      \item Addressed potential issued caused by undefined compiler behaviour
      when arrays of length zero were being created in C code.  Backported to
      2.50.2 (Thanks to Prof B. Ripley for reporting this.)
    }
  }
}

\section{Changes in version 2.48.0}{

  \subsection{CHANGES}{
    \itemize{
      \item R complex types can now be written to HDF5.  These will be stored as
      a compound datatype with two elements (r & i) representing the real and
      imaginary parts.
      \item Functions \code{H5Screate_simple} and \code{H5Sset_extent_simple} 
      now accept numeric values to the \code{dim} and \code{maxdim} arguments,
      allowing the creation of HDF5 dataspaces larger than R's maximum
      integer value.
      (Thanks to @hpages for reporting this and providing a patch
      https://github.com/grimbough/rhdf5/pull/140).
      \item Messages about the presence of \code{INT_MIN} in datasets created
      outside of rhdf5, and how R will convert them to \code{NA} have been
      moved from \code{H5Dread} to the high-level \code{h5read} function. 
      (See https://github.com/LTLA/scRNAseq/issues/44 for more details).
    }
  }

  \subsection{Bug fixes}{
    \itemize{
      \item Corrected an issue where the function prototype for _h5fileLock()
      differed from the actual implementation.
      \item Addressed a bug where fixed length string attributes would be one
      character shorter than they should be. Backported to rhdf5 2.46.1.
      (Thanks to Aaron Lun @LTLA for reporting this
      https://github.com/grimbough/rhdf5/issues/132). 
      \item Fixed an issue where zero length datasets of uint32, int64 or uint64 
      datatypes could not be read.  This would fail with an error message saying
      there was not enough memory. Backported to rhdf5 2.46.1.
      (Thanks to Aaron Lun @LTLA for reporting this
      https://github.com/grimbough/rhdf5/issues/134). 
    }
  }
}


\section{Changes in version 2.46.0}{

  \subsection{CHANGES}{
    \itemize{
      \item Added support for reading nullable booleans and integers from
      the AnnData specification.  \code{h5read()} will detect these automatically
      an attempt to cooerce them to the appropriate R data format.
    }
  }
}

\section{Changes in version 2.44.0}{

  \subsection{CHANGES}{
    \itemize{
      \item h5closeAll() now accepts objects as arguments to allow closing
      a set of HDF5 identifiers.
      \item Functions \code{H5Teunum_create()} and \code{H5Tenum_insert()}
      have been included.
      \item \code{h5set_extent()} will now test whether a dataset is chunked
      and inform the user if not. This uses the new function 
      \code{H5Dis_chunked()}.
      \item The function \code{H5Pset_filter()} is now exposed to the user.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item Modified how the constant \code{H5S_UNLIMITED} was being passed to
      the HDF5 library.  The previous strategy was not working on the ARM64
      architecture, and leading to failures when trying to change the size of
      a dataset.
      \item Resolved issue when reporting missing filters where R-to-C indexing 
      was being applied twice, resulting in the message:
      "'idx' argument is outside the range of filters set on this property list"
    }
  }

}

\section{Changes in version 2.42.0}{

  \subsection{CHANGES}{
    \itemize{
      \item Function \code{H5Ocopy()} has been included.
      \item UTF-8 encoded character datsets will be marked as having the same
      encoding when read into an R session.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{h5write()} no longer truncates multibyte UTF-8 strings 
      (Thanks to Aaron Lun @LTLA for reporting this and providing a fix,
      https://github.com/grimbough/rhdf5/issues/111). 
    }
  }

}

\section{Changes in version 2.40.0}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item Added H5R functions for working with object and dataset region 
      references.
      \item The HDF5 N-Bit filter has been enabled with via the function
      \code{H5Pset_nbit()}.  This can be combined with 
      \code{H5Tset_precision()} to compress integer and floating-point 
      datasets.
    }
  }
  
  \subsection{CHANGES}{
    \itemize{
      \item Argument 'cset' to \code{h5createAttribute()} and 
      \code{h5writeAttribute()} have been deprecated.  The 'encoding'
      argument should be used going forward.  This ensures consistency with 
      \code{h5create()} and \code{h5write()}.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item The documentation for the 'encoding' argument to 
      \code{h5createDataset()} and \code{h5writeDataset()} stated 'UTF-8' was a 
      valid option, however this would produce an error. This has now been 
      fixed. (Thanks to @ilia-kats for identifying this, 
      https://github.com/grimbough/rhdf5/pull/101)
      \item Fixed bug where an uninitialized value was used in the C code
      underlying \code{h5dump()} potentially causing crashes.
      \item Addressed issue in \code{h5dump()} and \code{h5ls()} that falsely 
      declared there were duplicated groups when used on a file with external
      links (Thanks to @acope3 for reporting this,
      https://github.com/grimbough/rhdf5/issues/107).
    }
  }

}

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
