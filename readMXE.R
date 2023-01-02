####################################################################
#                       function readMXE
#
# An R function to read MXE files produced by the Java application
# MaxEnt.
#
# An mxe-file stores a GIS raster as a gzip-compressed Java data
# stream saved to disk.
#
# This function generalizes the first exploration of a method to
# read mxe files in R published at http://rpubs.com/puddleduck/67084.
# In response to a query from my colleague John Baumgartner at
# Macquarie University, Sydney, Australia, I took another, more
# comprehensive, look at the mxe format and ways to read it into
# R data structures.
#
# Peter D. Wilson
# 11 July 2015
#
# 2023-01-02: Some code tidy-up in preparation for posting to GitHub
#
# This code is placed in the public domain under a GPL3 licence.
#
####################################################################

readMXE <- function(fileName = NULL)
{
  if (is.null(fileName))
  {
    warning("No file name supplied.")
    return(NULL)
  }
  else
  {
    if (!file.exists(fileName))
    {
      warning(paste0("Supplied file name (", fileName, ") not found."))
      return(NULL)
    }
  }
  
  # Establish a file connection object pointing to the mxe file:
  mxe_src <- gzfile(fileName, "rb")
  
  # The first five bytes of the mxe file are made up of the following:
  #
  # byte(s)   value(s)     function
  # 1-2       ac ed        Magic number indicating a stored Java
  #                        object or data values;
  # 3-4       00 05        Version of the Java serialization interface
  #                        used to create the data stream;
  # 5         77 or 7A     Flag indicating if the following block of
  #                        information is "short" or "long" format
  #
  # These data are read into the object javaHeader:
  
  javaHeader <- readBin(mxe_src, "raw", 5, endian = "big")
  
  if (javaHeader[5] == 0x77)
  {
    # A "short" block: the next byte is the length of the entire
    # file minus 5 and is a value fo no interest to us! So,
    # read it to advance the file pointer and move on...
    blockStuff <- readBin(mxe_src, "raw", 1, endian = "big")
    #cat("block\n")
    validHeader <- TRUE
  }
  else
  {
    if (javaHeader[5] == 0x7A)
    {
      # A "long" block": the next 4 bytes represent an
      # integer which always seems to have the value 1024
      # and bears no relationship to the file length. Its
      # function is unknown at this point in time. Again,
      # read to advance the file pointer and move on...
      blockStuff <- readBin(mxe_src, "raw", 4, endian = "big")
      #cat("block long\n")
      validHeader <- TRUE
    }
    else
    {
      warning("This is not a recognized mxe format.")
      validHeader <- FALSE
    }
  }
  
  if (validHeader == TRUE)
  {
    # All files examined by me have had the same
    # structure for parameters of the raster. The
    # parameters and their data format in the data
    # stream are as follows:
    #
    # Parameter  Data format      Meaning
    # xll        64-bit floating  x-coordinate of the
    #            point            lower left EDGE of
    #                             the raster or grid
    #
    # yll        64-bit floating  y-coordinate of the
    #            point            bottom EDGE of the
    #                             raster or grid
    #
    # cellsize   64-bit floating  Size of the SQUARE
    #            point            cells in grid
    #                             coordinates
    #
    # nrow       4-byte integer   Number of rows
    #
    # ncol       4-byte integer   Number of columns
    #
    # nodata     4-byte integer   Value used to
    #                             indicate a cell
    #                             with no data
    #
    # dataType   4-byte integer   Indicates the data
    #                             type which follows
    #                             after this flag:
    #                             1 = 32-floating point;
    #                             2 = signed byte
    #                             (1-byte integer);
    #                             3 = 4-byte integer
    
    xll <- readBin(mxe_src, "numeric", size = 8, endian = "big")
    yll <- readBin(mxe_src, "numeric", 1, endian = "big")
    cellsize <- readBin(mxe_src, "numeric", 1, endian = "big")
    nrow <- readBin(mxe_src, "integer", 1, endian = "big")
    ncol <- readBin(mxe_src, "integer", 1, endian = "big")
    nodata <- readBin(mxe_src, "integer", 1, endian = "big")
    dataType <- readBin(mxe_src, "integer", 1, endian = "big")
    
    if (dataType == 1)
    {
      #cat("32-bit float\n")
      dataType <- "32-bit float"
      data <- readBin(mxe_src, "numeric", size = 4, n = nrow*ncol, endian = "big")
    }
    else
    {
      if (dataType == 2)
      {
        #cat("Signed byte\n")
        dataType <- "Signed byte"
        data <- readBin(mxe_src, "integer", size = 1, n = nrow*ncol, endian = "big")
      }
      else
      {
        if (dataType == 3)
        {
          #cat("4-byte integer\n")
          dataType <- "4-byte integer"
          data <- readBin(mxe_src, "integer", size = 4, n = nrow*ncol, endian = "big")
        }
        else
        {
          #cat("Well I don't know.\n")
          dataType <- "Unknown"
          data <- NULL
        }
      }
    }
    
    # If we got this far then we have something to package and return:
    result <- list(xll = xll,
                   yll = yll,
                   cellsize = cellsize,
                   nrow = nrow,
                   ncol = ncol,
                   nodata = nodata,
                   dataType = dataType,
                   data = data)
  }
  else
  {
    result <- NULL
  }

  # Tidy-up
  close(mxe_src)
  return(result)
}

