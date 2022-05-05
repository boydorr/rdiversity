#' Binary digit.
#'
#' @description Create objects of type binary.
#' @details The binary number is represented by a \emph{logical} vector.
#' The bit order usually follows the same endianess as the byte order.
#' How to read:
#' \itemize{
#' \item Little Endian    (LSB) ---> (MSB)
#' \item Big Endian       (MSB) <--- (LSB)
#' }
#' The Big Endian endianess stores its MSB at the lowest adress.
#' The Little Endian endianess stores its MSB at the highest adress.
#'
#' e.g. b <-binary(8).
#' \itemize{
#' \item "Little Endian" : MSB at b[1] and LSB at b[8].
#' \item "Big Endian" : LSB at b[1] and MSB at b[8].
#' }
#' No floating-point support.
#' @usage binary(n, signed=FALSE, littleEndian=FALSE)
#' @param n length of vector. Number of bits
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @return a vector of class binary of length n. By default filled with zeros(0).
#' @examples
#' b <- rdiversity:::binary(8)
#' summary(b)
#' b <- rdiversity:::binary(16, signed=TRUE)
#' summary(b)
#' b <- rdiversity:::binary(32, littleEndian=TRUE)
#' summary(b)
#' @seealso \link{as.binary} and \link{is.binary}.
binary <- function(n, signed=FALSE, littleEndian=FALSE) {
  x <- logical(n)
  class(x) <- c("binary", "logical")
  attr(x, "signed") <- signed
  attr(x, "littleEndian") <- littleEndian
  if (signed) { x <- fillUpToByte(x) }
  return(x)
}

#' as binary digit.
#'
#' @description Converts an integer (Base10)
#' to a binary (Base2) number. It also converts a logical vector
#' to a binary (Base2) number (see examples).
#' @details The binary number is represented by a logical vector.
#' The bit order usually follows the same endianess as the byte order.
#' No floating-point support. If logic is set to TRUE an integer vector
#' is intepreted as a logical vector (>0 becomes TRUE and 0 becomes FALSE)
#' \itemize{
#' \item Little Endian    (LSB) ---> (MSB)
#' \item Big Endian       (MSB) <--- (LSB)
#' }
#' Auto switch to signed if num < 0.
#' @usage as.binary(x, signed=FALSE, littleEndian=FALSE, size=2, n=0, logic=FALSE)
#' @param x integer or logical vector.
#' @param signed  TRUE or FALSE. Unsigned by default. (two's complement)
#' @param littleEndian if TRUE. Big Endian if FALSE.
#' @param size in Byte. Needed if \bold{signed} is set. (by default 2 Byte)
#' @param n in Bit. Can be set if \bold{unsigned} is set to TRUE. (by default 0 Bit = auto)
#' @param logic If set to TRUE, x is expected as logical vector.
#' @return a vector of class binary.
#' @export
#' @examples
#' as.binary(0xAF)
#' as.binary(42)
#' as.binary(42, littleEndian=TRUE)
#' as.binary(c(0xAF, 0xBF, 0xFF))
#' as.binary(c(2,4,8,16,32), signed=TRUE, size=1)
#' as.binary(-1, signed=TRUE, size=1)
#' as.binary(1:7, n=3)
#' as.binary(sample(2^8,3),n=8)
#' as.binary(c(1,1,0), signed=TRUE, logic=TRUE)
#' as.binary(c(TRUE,TRUE,FALSE), logic=TRUE)
#' @seealso \link{is.binary} and \link{binary}
as.binary <- function(x, signed=FALSE, littleEndian=FALSE, size=2, n=0, logic=FALSE) {
  if (!inherits(x, "binary")) {
    if (logic == FALSE)
    {
      x <- binSeq(x, signed=signed, littleEndian=littleEndian, size=size, n=n)
    } else {
      x <- as.logical(x)
      class(x) <- c("binary", "logical")
      attr(x, "signed") <- signed
      attr(x, "littleEndian") <- littleEndian
      if (signed) x <- fillUpToByte(x)
      else x <- fillUpToBit(x, n)
    }
  } else {
    l <- saveAttributes(x)
    if (signed) {
      l$signed <- TRUE
      #attr(x, "signed") <- TRUE
      x <- fillUpToByte(x)
    } else {
      l$signed <- FALSE
      x <- fillUpToBit(x, n)
      #attr(x, "signed") <- FALSE
    }
    if (littleEndian) {
      l$littleEndian <- TRUE
    } else {
      l$littleEndian <- FALSE
    }
    x <- loadAttributes(x, l)
  }
  return(x)
}

#' is Binary Vector
#'
#' @description test for object "binary".
#' @usage is.binary(x)
#' @param x object to test.
#' @return TRUE or FALSE.
#' @seealso \link{as.binary} and \link{binary}
is.binary <- function(x) {
  return(inherits(x, "binary"))
}

#' Print method for binary number.
#'
#' @description This method prints the binary number.
#' @param x any binary number.
#' @param ... further arguments.
#' @return Output in ones and zeros (binary vector).
#' @seealso \link{summary.binary} provides some additional information.
#' @method print binary
#' @export
print.binary <- function(x, ...) {
  x <- ifelse(x, as.integer(1), as.integer(0))
  attributes(x) <- NULL
  NextMethod(print.default)
}

#' Summary method for binary number.
#'
#' @description This method provides information about the attributes of the binary number.
#' @param object binary number.
#' @param ... further arguments.
#' @return Contains the following information:
#' \itemize{
#' \item Signedness : unsigned or signed
#' \item Endianess : Big-Endian or Little-Endian
#' \item value<0 : negative or positve number
#' \item Size[bit] : Size in bit
#' \item Base10 : Decimal(Base10) number.
#' }
#' @seealso \link{print.binary}
#' @method summary binary
#' @export
summary.binary <- function(object, ...) {
  #I'm not sure if this is the way to do it. just printing a dataframe.
  l <- saveAttributes(object)

  tableHead <- character(3)
  signedness <- character(1)
  endianess <- character(1)
  size <- length(object)
  neg <- logical(1)

  tableHead <- c("Signedness", "Endianess", "value<0", "Size[bit]", "Base10")
  if (l$signed) {
    signedness <- "signed"
    if (l$littleEndian) {
      if (object[length(object)]) neg <- TRUE else neg <- FALSE
    } else {
      if (object[1]) neg <- TRUE else neg <- FALSE
    }
  } else {
    signedness <- "unsigned"
    neg <- FALSE
  }
  if (l$littleEndian) { endianess <- "Little-Endian" } else { endianess <- "Big-Endian" }
  df <- data.frame(signedness, endianess, neg, size, bin2dec(object))
  colnames(df) <- tableHead
  print(df)
}

#' @export
as.raw.binary <- function(x) {
  #b <- as.binary(rawToBits(r)) from raw to binary
  l <- saveAttributes(x)
  x <- fillUpToByte(x, size=bytesNeeded(length(x)))
  xx <- logical(0)

  if (!l$littleEndian) x <- rev(x)
  if (l$littleEndian) {
    x <- as.logical(x)
    dim(x) <- c(4, (2 * bytesNeeded(length(x))))
    for(i in seq(1, (2*bytesNeeded(length(x)) - 1), by = 2)) xx <- c(xx, c(x[,i+1], x[,i]))
    x <- xx
  }
  x <- packBits(x)
  if(!l$littleEndian) x <- rev(x)
  NextMethod(.Generic)
} #rseq = function(x,to=1) NROW(x):to

##' @export
#as.hexmode.binary <- function(x) {
#    x <- bin2dec(x)
#    NextMethod(.Generic)
#}

##' @export
#as.octmode.binary <- function(x) {
#    x <- bin2dec(x)
#    NextMethod(.Generic)
#}

#' @export
as.character.binary <- function(x, ...) {
  x <- as.integer(as.logical(x))
  NextMethod(.Generic, ...)
}

#' @export
as.integer.binary <- function(x, ...) {
  #test for .Machine$integer.max
  x <- bin2dec(x)
  NextMethod(.Generic, ...)
}

#' @export
as.double.binary <- function(x, ...) {
  x <- bin2dec(x)
  NextMethod(.Generic, ...)
}

#' Group Generic Ops
#'
#' Group generic Ops operators
#' @param e1 e1
#' @param e2 e2
#' @method Ops binary
#' @export
Ops.binary <- function(e1, e2) {
  # Group Generic "Ops"
  boolean <- switch(.Generic,  '/' = TRUE, FALSE)
  if (boolean) {
    stop("Division is not supported. For the time being")
  }
  boolean <- switch(.Generic,  '+' =, '-' = TRUE, FALSE)
  if (boolean) {
    l1 <- saveAttributes(e1)
    ret <- NextMethod(.Generic)
    x <- loadAttributes(ret,l1)
    return(x)
  }
  boolean <- switch(.Generic,  '*' =, '^' =, '%%' =, '%/%' = TRUE, FALSE)
  if (boolean) {
    l1 <- saveAttributes(e1)
    l2 <- saveAttributes(e2)
    le1 <- length(e1)
    le2 <- length(e2)
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    if (l1$signed || l2$signed)
    {
      ret <- NextMethod(.Generic)
      ret <- as.binary(ret, signed=TRUE, size=(greater(le1, le2)/byte()))
      x <- loadAttributes(ret, l1)
    } else {
      ret <- NextMethod(.Generic)
      ret <- as.binary(ret)
      x <- loadAttributes(ret, l1)
    }
    return(x)
  }
  boolean <- switch(.Generic,  '&' =, '|' =, 'xor' = TRUE, FALSE)
  if (boolean) {
    l1 <- saveAttributes(e1)
    if (length(e1) < length(e2))
    {
      e1 <- fillUpToBit(e1, value=FALSE, n=length(e2))
    } else if (length(e2) < length(e1)) {
      e2 <- fillUpToBit(e2, value=FALSE, n=length(e1))
    }
    ret <- NextMethod(.Generic)
    x <- loadAttributes(ret, l1)
    return(x)
  }
  boolean <- switch(.Generic,  '!' = TRUE, FALSE)
  if (boolean) {
    l1 <- saveAttributes(e1)
    ret <- NextMethod(.Generic)
    x <- loadAttributes(ret, l1)
    return(x)
  }
  boolean <- switch(.Generic,  '<' =, '<=' =, '>' =, '>=' = TRUE, FALSE)
  if (boolean) {
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    ret <- NextMethod(.Generic)
    return(ret)
  }
}

#' @export
'+.binary' <- function(x, y) {
  binAdd(x, y)
}

#' @export
'-.binary' <- function(x, y) {
  binAdd(x, negate(y))
}

#' @export
'==.binary' <- function(x, y) {
  # attributes are saved @ group generic Ops.
  if (attributes(x)$littleEndian) x <- switchEndianess(x)
  if (attributes(y)$littleEndian) y <- switchEndianess(y)

  if (length(x) > length(y)) {
    delta <- bytesNeeded(length(x))
    if (attributes(y)$signed) {
      y <- fillUpToByte(y, value=TRUE, size=delta)
    } else {
      y <- fillUpToByte(y, value=FALSE, size=delta)
    }
  } else if (length(x) < length(y)) {
    delta <- bytesNeeded(length(y))
    if (attributes(x)$signed) {
      x <- fillUpToByte(x, value=TRUE, size=delta)
    } else {
      x <- fillUpToByte(x, value=FALSE, size=delta)
    }
  }
  return(all(!xor(x, y)))
}

#' @export
'!=.binary' <- function(x, y) {
  # attributes are saved @ group generic Ops.
  !(x == y)
}

#' @export
'[.binary' <- function(x, i, j, drop=TRUE) {
  # this should not become a group generic. This function is an internal generic.
  l <- saveAttributes(x)

  ret <- NextMethod(.Generic, drop=drop)

  x <- loadAttributes(ret,l)
  return(x)
}

#' @export
rev.binary <- function(x) {
  # this should not become a group generic. This function is an internal generic.
  l <- saveAttributes(x)
  #x = x[rseq(x)] rseq = function(x,to=1) NROW(x):to
  x <- x[length(x):1]
  #ret <- NextMethod(.Generic)

  x <- loadAttributes(x,l)
  return(x)
}


# Helper functions

#' loadAttributes
#'
#' Helper function load Attributes
#' @usage loadAttributes(x, l)
#' @param x x
#' @param l l
loadAttributes <- function(x, l) {
  class(x) <- l$class
  attr(x, "signed") <- l$signed
  attr(x, "littleEndian") <- l$littleEndian
  return(x)
}

#' saveAttributes
#'
#' Helper function save Attributes
#' @usage saveAttributes(x)
#' @param x x
saveAttributes <- function(x) {
  if(is.binary(x)) l <- list(class=c("binary","logical"),
                             signed=attr(x,"signed"),
                             littleEndian=attr(x,"littleEndian"))
  else l <- list(class=class(x))
  return(l)
}

#' Fill up to Byte (00000000..)
#'
#' @description Fills up the binary number with zeros(0) or ones(1), to the size in Byte.
#' @details No floating point supported.
#' @usage fillUpToByte(x, size=0, value=FALSE)
#' @param x The binary number to fill up with zeros. (Any binary vector).
#' @param value to fill up with FALSE(0) or fill up with TRUE(1).
#' @param size in Byte. 0 = auto (smallest possible Byte).
#' @return binary number. A binary vector with the desired size.
#' @examples
#' rdiversity:::fillUpToByte(as.binary(c(1,1), logic=TRUE), size=2)
#' rdiversity:::fillUpToByte(as.binary(c(1,0,1), logic=TRUE), size=2, value=FALSE)
#' @seealso \link{fillUpToBit}.
fillUpToByte <- function(x, size=0, value=FALSE) {
  stopifnot(is.binary(x))
  l <- saveAttributes(x)
  if (size == 0 && length(x) %% byte() == 0) return(x)
  if (size > 0 && length(x) >= size * byte()) return(x)

  if (size == 0) {
    append <- binary(((trunc((length(x) / byte())) +1) * byte()) - length(x))
  } else {
    append <- binary(size * byte() - length(x))
  }
  append[1:length(append)] <- value

  if (attributes(x)$littleEndian) {
    x <- c(x,append)
  } else {
    x <- c(append,x)
  }
  x <- loadAttributes(x,l)
  return(x)
}

#' Fill up to bit (000..)
#'
#' @description Fills up the binary number with zeros(0) or ones(1), to the size n in bit.
#' @details No floating point supported.
#' @usage fillUpToBit(x, n, value=FALSE)
#' @param x The binary number to fill up with zeros. (Any binary vector).
#' @param value to fill up with FALSE(0) or fill up with TRUE(1).
#' @param n size in bit.
#' @return binary number. A binary vector with the desired size.
#' @examples
#' rdiversity:::fillUpToBit(as.binary(c(1,1), logic=TRUE), n=4)
#' rdiversity:::fillUpToBit(as.binary(c(1,0,1), logic=TRUE), n=4, value=FALSE)
#' @seealso \link{fillUpToByte}.
fillUpToBit <- function(x, n, value=FALSE) {
  stopifnot(is.binary(x))
  if(missing(n)) stop("n is missing")
  stopifnot(n >= 0)

  l <- saveAttributes(x)
  if (length(x) >= n) return(x)

  append <- binary(n - length(x))
  append[seq(length(append))] <- value

  if (attributes(x)$littleEndian) {
    x <- c(x,append)
  } else {
    x <- c(append,x)
  }
  x <- loadAttributes(x,l)
  return(x)
}

# Helper function to convert decimal to binary
h <- function(x) {
  # Basic error handling: function expects single positive integer
  if(x %% 1 != 0) { stop("Non-integer input to h()") }
  if(x < 0) { stop("Negative input to h()") }

  # Special case
  if(x == 0) { return(c(1, 0)) }

  # Only generate as many comparison digits as we need
  num_digits = floor(log2(x)) + 1

  # Get x %% each power of two; points in the vector where this
  # changes are active. Because my function has bit order reversed
  # versus the original h() function, we need to go num_digits -
  # the active bits. The +2 and append 0 are to match the original
  # h behavior
  c(num_digits + 2 - which(diff(x %% 2^(num_digits:0)) != 0), 0)
}

dec2bin <- function(num, signed=FALSE, littleEndian=FALSE, size=2, n=0) {
  # Very slow with negative numbers. (maybe binAdd)
  if (signed && (((num > ((2^(size*byte())/2)-1))) ||
                 (num < ((-1)*(2^(size*byte())/2)))))
    stop("Out of Range. Please increase the size[Byte]")

  if (signed == FALSE && n > 0 &&
      (((num > ((2^(n))-1))) || (num < (0))))
    stop("Out of Range. Please increase the size n [Bit], or set it to 0=auto")

  l <- list(class=c("binary","logical"),
            signed=signed,
            littleEndian=littleEndian)

  #needed for hex input
  num <- as.numeric(num)

  neg = FALSE
  if (num < 0) {
    l$signed <- TRUE
    neg = TRUE
    num = abs(num)
  }

  #global read only for function h(num)
  ret <- h(num)

  if(l$signed) {
    b <- logical(size*byte())
  } else {
    if (n > 0) {
      b <- logical(n)
    } else {
      b <- logical(max(ret)-1)
    }
  }
  #do some optimization here.
  for (i in seq(length(b))) b[i] <- any(ifelse((ret-1)==i, TRUE, FALSE))

  if (neg)
  {
    b <- !rev(b)
    b <- rev(binAdd(as.binary(b, signed=TRUE, logic=TRUE), as.binary(1, signed=TRUE, logic=TRUE)))
  }
  if(!l$littleEndian) b <- rev(b)
  loadAttributes(b, l)
  return(loadAttributes(b, l))
}

#' Binary sequence
#'
#' @description Binary sequence.
#' @usage binSeq(x, ...)
#' @param x a sequence.
#' @param ... used for dec2bin().
#' @return a sequence list of binary digits.
#' @examples
#' rdiversity:::binSeq(0:4)
#' @seealso \link{binary}
binSeq <- function(x, ...) {
  l <- vector("list", length(x))
  for(i in 1:length(x)) l[[i]] <- dec2bin(x[i], ...)
  if(length(l) == 1){
    return(l[[1]])
  } else {
    return(l)
  }
}

#' Binary Addition (+)
#'
#' @description Adds two binary numbers. (x + y)
#' @details Little-Endian and unsigned is not supported at the moment. No floating point supported.
#' if x or y is signed the return value will also be signed.
#' @usage binAdd(x, y)
#' @param x summand 1 (binary vector)
#' @param y summand 2 (binary vector)
#' @return The sum of x and y. Returns a binary vector.
#' @examples
#' five <- as.binary(5); ten <- as.binary(10);
#' as.numeric(rdiversity:::binAdd(ten, five))
#' rdiversity:::binAdd(as.binary(c(0,1), logic=TRUE), as.binary(c(1,0), logic=TRUE))
#' @seealso base::as.logical , base::is.logical, base::raw
binAdd <- function(x, y) {
  #Should be implemented in C (internal generic). Very slow.
  if (missing(x)) stop("x is missing.")
  if (missing(y)) stop("y is missing.")
  stopifnot(is.binary(x))
  stopifnot(is.binary(y))

  x_signed <- attributes(x)$signed
  x_littleEndian <- attributes(x)$littleEndian
  y_signed <- attributes(y)$signed
  y_littleEndian <- attributes(y)$littleEndian

  l <- list(class=c("binary","logical"),
            signed=FALSE,
            littleEndian=x_littleEndian)

  if (x_signed || y_signed) l$signed <- TRUE
  if (x_littleEndian) y <- switchEndianess(y)
  if (y_littleEndian) x <- switchEndianess(x)

  if (length(x) >= length(y))
  {
    MAX <- length(x)
    a <- rep(FALSE,length(x)-length(y))
    y <- c(a,y)
  } else {
    MAX <- length(y)
    a <- rep(FALSE,length(y)-length(x))
    x <- c(a,x)
  }

  ret = binary(MAX)
  temp = binary(MAX+1)
  ret[MAX] <- xor(x[MAX],y[MAX])
  if ((isTRUE(as.logical(x[MAX])) && isTRUE(as.logical(y[MAX])))) temp[MAX+1] <- TRUE

  if (MAX > 2)
  {
    for(i in (MAX-1):1)
    {
      ret[i] <- xor(x[i],y[i])
      ret[i] <- xor(ret[i],temp[i+2])
      if(((isTRUE(as.logical(x[i])) && isTRUE(as.logical(y[i]))) ||
          (isTRUE(as.logical(x[i])) && isTRUE(as.logical(temp[i+2]))) ||
          (isTRUE(as.logical(y[i])) && isTRUE(as.logical(temp[i+2]))))) temp[i+1] <- TRUE
    }
  }
  if (temp[2] && !l$signed) {
    ret <- c(T,ret)
    #l$signed <- TRUE
    #l$littleEndian <- FALSE
  }
  # reverse big endian.
  return(loadAttributes(ret, l))
}

bin2dec <- function(bin) {
  #could be implemented in C. But it is not that slow.
  signed <- attributes(bin)$signed
  littleEndian <- attributes(bin)$littleEndian

  if(!littleEndian) { bin <- rev(bin) }

  bin <- as.integer(as.logical(bin))
  i = length(bin)-1
  numeric = 0
  first <- TRUE

  for(d in rev(bin))
  {
    if ((signed) & (first)) {
      numeric <- (-1 * d * (2^i))
      i <- i - 1
      first <- FALSE
    } else {
      numeric = (numeric + d * (2^i))
      i <- i - 1
    }
  }
  return(numeric)
}

#' A simple helper function to return the size of one byte
#'
#' @description Used to increase readabilaty
#' @usage byte()
#' @return The size of one byte (8)
#' @seealso \link{fillUpToByte}
byte <- function() {
  return(8)
}

#' Switch Endianess.
#'
#' @description Switch little-endian to big-endian and vice versa.
#' @usage switchEndianess(x, stickyBits=FALSE)
#' @param x binary number. Any binary number.
#' @param stickyBits Bits wont change if set TRUE. Only the attribute will be switched.
#' @return switch little-endian to big-endian and vice versa.
#' @examples
#' x <- as.binary(c(1,1,0,0), logic=TRUE); print(x); summary(x);
#' y <- rdiversity:::switchEndianess(x); print(y); summary(y);
#' y <- rdiversity:::switchEndianess(x, stickyBits=TRUE); print(y); summary(y);
#' @seealso \link{fillUpToByte}.
switchEndianess <- function(x, stickyBits=FALSE) {
  stopifnot(is.binary(x))
  l <- saveAttributes(x)
  l$littleEndian <- !l$littleEndian
  if (stickyBits == TRUE)
  {
    return(loadAttributes(x,l))
  } else {
    return(loadAttributes(rev(x),l))
  }
}

#' Minimum number of "byte" needed to hold n "bit"
#'
#' @description A simple helper function
#' that returns the minimum number of byte needed to hold the amount of n bit.
#' @usage bytesNeeded(n)
#' @param n The number of bit.
#' @return The number of minimum byte needed to hold n bit.
#' @examples
#' ten <- as.binary(10)
#' rdiversity:::bytesNeeded(length(ten))
#' @seealso \link{fillUpToByte} or \link{byte}
bytesNeeded <- function(n) {
  stopifnot(all(is.numeric(n) || is.na(n)))
  stopifnot(all(n >= 0 || is.na(n)))

  ifelse(n %% byte() == 0, n %/% byte(), n %/% byte() + 1)
}

#' Binary Negation (!)
#'
#' @description Negates the binary number x. Negation x -> -x or -x -> x
#' @details An »unsigned« number will be returned as »signed« regardless of whether the value is negative.
#' No floating point supported.
#' @usage negate(x)
#' @param x The number to be negated. A binary vector is expected.
#' @return The negated number of x. Returns a binary vector with signed=TRUE
#' @examples
#' summary(rdiversity:::negate(as.binary(5, signed=TRUE)))
#' summary(rdiversity:::negate(as.binary(-5, signed=TRUE)))
#' summary(rdiversity:::negate(as.binary(5, signed=FALSE)))
#' @seealso \link{switchEndianess} or \link{fillUpToByte}.
negate <- function(x) {
  stopifnot(is.binary(x))
  signed <- attributes(x)$signed
  if (all(!x)) return(binary(length(x)))
  #if (!signed) warning("Trying to negate an unsigned digit. treated as signed value. Returns a signed value")
  littleEndian <- attributes(x)$littleEndian

  if (littleEndian) x <- rev(x)

  if (length(x) %% byte() != 0) {
    MAX <- (trunc((length(x)/byte())) +1) * byte()
    a <- rep(FALSE, MAX - length(x))
    a <- as.binary(a, littleEndian=littleEndian, logic=TRUE)
    # 'c.binary'(x,y) needs to be implemented
    x <- as.binary(c(a,x), logic=TRUE)
  }
  x <- !x
  x <- binAdd(as.binary(x, logic=TRUE),as.binary(TRUE, logic=TRUE))

  if (littleEndian) x <- rev(x)
  attr(x,"signed") <- TRUE
  attr(x,"littleEndian") <- littleEndian
  class(x) <- c("binary", "logical")
  return(x)
}

greater <- function(e1, e2) {
  if (e1 > e2)
  {
    return(e1)
  } else {
    return(e2)
  }
}
