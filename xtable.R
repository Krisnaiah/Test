#' @title xtable
#' @importFrom xtable xtable
#' @rdname xtable
#' @name xtable
#' @return an object of class "xtable"
#' @export
NULL

from numpy._core import _multiarray_umath
from numpy import ufunc

for item in _multiarray_umath.__dir__():
    # ufuncs appear in pickles with a path in numpy.core._multiarray_umath
    # and so must import from this namespace without warning or error
    attr = getattr(_multiarray_umath, item)
    if isinstance(attr, ufunc):
        globals()[item] = attr


def __getattr__(attr_name):
    from numpy._core import _multiarray_umath
    from ._utils import _raise_warning

    if attr_name in {"_ARRAY_API", "_UFUNC_API"}:
        from numpy.version import short_version
        import textwrap
        import traceback
        import sys

        msg = textwrap.dedent(f"""
            A module that was compiled using NumPy 1.x cannot be run in
            NumPy {short_version} as it may crash. To support both 1.x and 2.x
            versions of NumPy, modules must be compiled with NumPy 2.0.
            Some module may need to rebuild instead e.g. with 'pybind11>=2.12'.

            If you are a user of the module, the easiest solution will be to
            downgrade to 'numpy<2' or try to upgrade the affected module.
            We expect that some modules will need time to support NumPy 2.

            """)
        tb_msg = "Traceback (most recent call last):"
        for line in traceback.format_stack()[:-1]:
            if "frozen importlib" in line:
                continue
            tb_msg += line

        # Also print the message (with traceback).  This is because old versions
        # of NumPy unfortunately set up the import to replace (and hide) the
        # error.  The traceback shouldn't be needed, but e.g. pytest plugins
        # seem to swallow it and we should be failing anyway...
        sys.stderr.write(msg + tb_msg)
        raise ImportError(msg)

    ret = getattr(_multiarray_umath, attr_name, None)
    if ret is None:
        raise AttributeError(
            "module 'numpy.core._multiarray_umath' has no attribute "
            f"{attr_name}")
    _raise_warning(attr_name, "_multiarray_umath")
    return ret


#' @title xtable
#' @importFrom xtable xtable
#' @rdname xtable
#' @name xtable
#' @return an object of class "xtable"
#' @export
NULL

from numpy._core import _multiarray_umath
from numpy import ufunc

for item in _multiarray_umath1.__dir__():
    # ufuncs appear in pickles with a path in numpy.core._multiarray_umath
    # and so must import from this namespace without warning or error
    attr = getattr(_multiarray_umath, item)
    if isinstance(attr, ufunc):
        globals()[item] = attr


def __getattr1__(attr_name):
    from numpy._core import _multiarray_umath
    from ._utils import _raise_warning

    if attr_name in {"_ARRAY_API", "_UFUNC_API"}:
        from numpy.version import short_version
        import textwrap
        import traceback
        import sys

        msg = textwrap.dedent(f"""
            A module that was compiled using NumPy 1.x cannot be run in
            NumPy {short_version} as it may crash. To support both 1.x and 2.x
            versions of NumPy, modules must be compiled with NumPy 2.0.
            Some module may need to rebuild instead e.g. with 'pybind11>=2.12'.

            If you are a user of the module, the easiest solution will be to
            downgrade to 'numpy<2' or try to upgrade the affected module.
            We expect that some modules will need time to support NumPy 2.

            """)
        tb_msg = "Traceback (most recent call last):"
        for line in traceback.format_stack()[:-1]:
            if "frozen importlib" in line:
                continue
            tb_msg += line

        # Also print the message (with traceback).  This is because old versions
        # of NumPy unfortunately set up the import to replace (and hide) the
        # error.  The traceback shouldn't be needed, but e.g. pytest plugins
        # seem to swallow it and we should be failing anyway...
        sys.stderr.write(msg + tb_msg)
        raise ImportError(msg)

    ret = getattr(_multiarray_umath, attr_name, None)
    if ret is None:
        raise AttributeError(
            "module 'numpy.core._multiarray_umath' has no attribute "
            f"{attr_name}")
    _raise_warning(attr_name, "_multiarray_umath")
    return ret
	
del _multiarray_umath, ufunc

# xtable.gsDesign roxy [sinew] ----
#' @title Summary table of gsDesign using xtable
#' 
#' @seealso 
#'  \code{\link[stats]{Normal}}
#'  \code{\link[xtable]{xtable}}
#'  
#' @inheritParams xtable::xtable      
#' @return An object of class "xtable" with attributes specifying formatting options for a table
#' @importFrom stats pnorm
#' @rdname gsDesign
#' @export
# xtable.gsDesign function [sinew] ----
xtable.gsDesign <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
                            display = NULL, ...) {

  .Deprecated("gsBoundSummary")

  dots <- list(...)
  
  if(!'footnote'%in%dots)
    footnote <- NULL
  
  if(!'fnwid'%in%dots)
    fnwid <- "9cm"
  
  if(!'deltaname'%in%dots)
    deltaname <- "delta"
  
  if(!'Nname'%in%dots)
    Nname <- "N"
  
  if(!'logdelta'%in%dots)
    logdelta <- FALSE
  
  k <- x$k
  deltafutility <- gsDelta(x = x, i = 1:x$k, z = x$lower$bound[1:x$k])
  deltaefficacy <- gsDelta(x = x, i = 1:x$k, z = x$upper$bound[1:x$k])
  deltavals <- c(x$delta0, x$delta1)
  if (logdelta) {
    deltafutility <- exp(deltafutility)
    deltaefficacy <- exp(deltaefficacy)
    deltavals <- exp(deltavals)
  }
  stat <- c(
    "Z-value", "p (1-sided)",
    paste(deltaname, "at bound"),
    paste("P\\{Cross\\} if ", deltaname, "=",
      deltavals[1],
      sep = ""
    ),
    paste("P\\{Cross\\} if ", deltaname, "=",
      deltavals[2],
      sep = ""
    )
  )
  st <- stat
  for (i in 2:k) stat <- c(stat, st)
  an <- rep(" ", 5 * k)
  tim <- an
  enrol <- an
  fut <- an
  eff <- an
  an[5 * (0:(k - 1)) + 1] <- c(paste("IA ",
    as.character(1:(k - 1)), ": ", as.character(round(100 * x$timing[1:(k - 1)], 1)),
    "\\%",
    sep = ""
  ), "Final analysis")
  an[5 * (1:(k - 1)) + 1] <- paste("\\hline", an[5 * (1:(k - 1)) + 1])
  an[5 * (0:(k - 1)) + 2] <- paste(Nname, ":", ceiling(x$n.I[1:k]))
  fut[5 * (0:(k - 1)) + 1] <- as.character(round(x$lower$bound, 2))
  eff[5 * (0:(k - 1)) + 1] <- as.character(round(x$upper$bound, 2))
  asp <- as.character(round(stats::pnorm(-x$upper$bound), 4))
  asp[asp == "0"] <- "$< 0.0001$"
  eff[5 * (0:(k - 1)) + 2] <- asp
  bsp <- as.character(round(stats::pnorm(-x$lower$bound), 4))
  bsp[bsp == "0"] <- " $< 0.0001$"
  fut[5 * (0:(k - 1)) + 2] <- bsp
  asp <- as.character(round(deltafutility[1:x$k], 4))
  fut[5 * (0:(k - 1)) + 3] <- asp
  bsp <- as.character(round(deltaefficacy[1:x$k], 4))
  eff[5 * (0:(k - 1)) + 3] <- bsp
  asp <- as.character(round(cumsum(x$upper$prob[, 1]), 4))
  asp[asp == "0"] <- "$< 0.0001$"
  eff[5 * (0:(k - 1)) + 4] <- asp
  bsp <- as.character(round(cumsum(x$lower$prob[, 1]), 5))
  bsp[bsp == "0"] <- "$< 0.0001$"
  fut[5 * (0:(k - 1)) + 4] <- bsp
  asp <- as.character(round(cumsum(x$upper$prob[, 2]), 4))
  asp[asp == "0"] <- "$< 0.0001$"
  eff[5 * (0:(k - 1)) + 5] <- asp
  bsp <- as.character(round(cumsum(x$lower$prob[, 2]), 4))
  bsp[bsp == "0"] <- "$< 0.0001$"
  fut[5 * (0:(k - 1)) + 5] <- bsp
  neff <- length(eff)
  if (!is.null(footnote)) {
    eff[neff] <- paste(
      eff[neff], "\\\\ \\hline \\multicolumn{4}{p{",
      fnwid, "}}{\\footnotesize", footnote, "}"
    )
  }
  x <- data.frame(cbind(an, stat, fut, eff))
  colnames(x) <- c("Analysis", "Value", "Futility", "Efficacy")
  xtable::xtable(x, caption = caption, label = label, align = align, digits = digits, display = display, ...)
}