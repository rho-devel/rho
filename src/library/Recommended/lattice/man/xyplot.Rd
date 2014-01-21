\name{B_00_xyplot}
\title{Common Bivariate Trellis Plots}
\alias{xyplot}
\alias{xyplot.formula}
\alias{barchart}
\alias{barchart.formula}
\alias{barchart.numeric}
\alias{barchart.default}
\alias{bwplot}
\alias{bwplot.formula}
\alias{bwplot.numeric}
\alias{dotplot}
\alias{dotplot.formula}
\alias{dotplot.numeric}
\alias{dotplot.default}
\alias{stripplot}
\alias{stripplot.formula}
\alias{stripplot.numeric}
\usage{
xyplot(x, data, \dots)
dotplot(x, data, \dots)
barchart(x, data, \dots)
stripplot(x, data, \dots)
bwplot(x, data, \dots)

\method{xyplot}{formula}(x,
       data,
       allow.multiple = is.null(groups) || outer,
       outer = !is.null(groups),
       auto.key = FALSE,
       aspect = "fill",
       panel = lattice.getOption("panel.xyplot"),
       prepanel = NULL,
       scales = list(),
       strip = TRUE,
       groups = NULL,
       xlab,
       xlim,
       ylab,
       ylim,
       drop.unused.levels = lattice.getOption("drop.unused.levels"),
       \dots,
       lattice.options = NULL,
       default.scales,
       default.prepanel = lattice.getOption("prepanel.default.xyplot"),
       subscripts = !is.null(groups),
       subset = TRUE)

\method{dotplot}{formula}(x,
        data,
        panel = lattice.getOption("panel.dotplot"),
        default.prepanel = lattice.getOption("prepanel.default.dotplot"),
        \dots)

\method{barchart}{formula}(x,
         data,
         panel = lattice.getOption("panel.barchart"),
         default.prepanel = lattice.getOption("prepanel.default.barchart"),
         box.ratio = 2,
         \dots)

\method{stripplot}{formula}(x,
          data,
          panel = lattice.getOption("panel.stripplot"),
          default.prepanel = lattice.getOption("prepanel.default.stripplot"),
          \dots)

\method{bwplot}{formula}(x,
       data,
       allow.multiple = is.null(groups) || outer,
       outer = FALSE,
       auto.key = FALSE,
       aspect = "fill",
       panel = lattice.getOption("panel.bwplot"),
       prepanel = NULL,
       scales = list(),
       strip = TRUE,
       groups = NULL,
       xlab,
       xlim,
       ylab,
       ylim,
       box.ratio = 1,
       horizontal = NULL,
       drop.unused.levels = lattice.getOption("drop.unused.levels"),
       \dots,
       lattice.options = NULL,
       default.scales,
       default.prepanel = lattice.getOption("prepanel.default.bwplot"),
       subscripts = !is.null(groups),
       subset = TRUE)

}

\description{
  This help page documents several commonly used high-level Lattice
  functions.  \code{xyplot} produces bivariate scatterplots or
  time-series plots, \code{bwplot} produces box-and-whisker plots,
  \code{dotplot} produces Cleveland dot plots, \code{barchart} produces
  bar plots, and \code{stripplot} produces one-dimensional scatterplots.
  All these functions, along with other high-level Lattice functions,
  respond to a common set of arguments that control conditioning,
  layout, aspect ratio, legends, axis annotation, and many other details
  in a consistent manner.  These arguments are described extensively in
  this help page, and should be used as the reference for other
  high-level functions as well.

  For control and customization of the actual display in each panel, the
  help page of the respective default panel function will often be more
  informative.  In particular, these help pages describe many arguments
  commonly used when calling the corresponding high-level function but
  are specific to them.
}

% designed mainly for two continuous variates (though factors can be
% supplied as well, in which case they will simply be coerced to
% numeric), which produces Conditional Scatter plots.

% The others are useful when one of the variates is a factor or a
% shingle.  Most of these arguments are also applicable to other high
% level functions in the lattice package, but are only documented here.


\arguments{
  \item{x}{
    All high-level function in \pkg{lattice} are generic.  \code{x} is
    the object on which method dispatch is carried out.

    For the \code{"formula"} methods, \code{x} must be a formula
    describing the primary variables (used for the per-panel display)
    and the optional conditioning variables (which define the subsets
    plotted in different panels) to be used in the plot.  Conditioning
    is described in the \dQuote{Details} section below.  

    For the functions documented here, the formula is generally of the
    form \code{y ~ x | g1 * g2 * \dots} (or equivalently, \code{y ~ x |
    g1 + g2 + \dots}), indicating that plots of \code{y} (on the y-axis)
    versus \code{x} (on the x-axis) should be produced conditional on
    the variables \code{g1, g2, \dots}.  Here \code{x} and \code{y} are
    the primary variables, and \code{g1, g2, \dots} are the conditioning
    variables.  The conditioning variables may be omitted to give a
    formula of the form \code{y ~ x}, in which case the plot will
    consist of a single panel with the full dataset.  The formula can
    also involve expressions, e.g., \code{sqrt()}, \code{log()}, etc.
    See the \code{data} argument below for rules regarding evaluation of
    the terms in the formula.

    With the exception of \code{xyplot}, the functions documented here
    may also be supplied a formula of the form \code{ ~ x | g1 * g2 *
    \dots}.  In that case, \code{y} defaults to \code{names(x)} if
    \code{x} is named, and a factor with a single level otherwise.

    Cases where \code{x} is not a formula is handled by appropriate
    methods.  The \code{numeric} methods are equivalent to a call with
    no left hand side and no conditioning variables in the formula.  For
    \code{barchart} and \code{dotplot}, non-trivial methods exist for
    tables and arrays, documented at \code{\link{barchart.table}}.

    The conditioning variables \code{g1, g2, \dots} must be either
    factors or shingles.  Shingles provide a way of using numeric
    variables for conditioning; see the help page of
    \code{\link{shingle}} for details. Like factors, they have a
    \code{"levels"} attribute, which is used in producing the
    conditional plots.  If necessary, numeric conditioning variables are
    converted to shingles using the \code{shingle} function; however,
    using \code{\link{equal.count}} may be more appropriate in many
    cases.  Character variables are coerced to factors.

    %% As an extension of the interpretation described above, 
    % A special case is when the left and/or right sides of the
    % Formulas containing a \sQuote{+} sign in the specification of
    % primary variables before the conditioning variables) contain a \sQuote{+} sign, e.g.,
    % \code{y1 + y2 ~ x | a*b}.

    \bold{Extended formula interface:} As a useful extension of the
    interface described above, the primary variable terms (both the LHS
    \code{y} and RHS \code{x}) may consist of multiple terms separated
    by a \sQuote{+} sign, e.g., \code{y1 + y2 ~ x | a * b}.  This
    formula would be taken to mean that the user wants to plot both
    \code{y1 ~ x | a * b} and \code{y2 ~ x | a * b}, but with the
    \code{y1 ~ x} and \code{y2 ~ x} superposed in each panel.  The two
    groups will be distinguished by different graphical parameters.
    This is essentially what the \code{groups} argument (see below)
    would produce, if \code{y1} and \code{y2} were concatenated to
    produce a longer vector, with the \code{groups} argument being an
    indicator of which rows come from which variable.  In fact, this is
    exactly what is done internally using the \code{\link{reshape}}
    function. This feature cannot be used in conjunction with the
    \code{groups} argument.

    To interpret \code{y1 + y2} as a sum, one can either set
    \code{allow.multiple=FALSE} or use \code{I(y1+y2)}.

    A variation on this feature is when the \code{outer} argument is set
    to \code{TRUE}. In that case, the plots are not superposed in each
    panel, but instead separated into different panels (as if a new
    conditioning variable had been added).

    \bold{Primary variables:} The \code{x} and \code{y} variables should
    both be numeric in \code{xyplot}, and an attempt is made to coerce
    them if not. However, if either is a factor, the levels of that
    factor are used as axis labels. In the other four functions
    documented here, exactly one of \code{x} and \code{y} should be
    numeric, and the other a factor or shingle. Which of these will
    happen is determined by the \code{horizontal} argument --- if
    \code{horizontal=TRUE}, then \code{y} will be coerced to be a factor
    or shingle, otherwise \code{x}. The default value of
    \code{horizontal} is \code{FALSE} if \code{x} is a factor or
    shingle, \code{TRUE} otherwise. (The functionality provided by
    \code{horizontal=FALSE} is not S-compatible.)

    Note that the \code{x} argument used to be called \code{formula} in
    earlier versions (when the high-level functions were not generic and
    the formula method was essentially the only method).  This is no
    longer allowed.  It is recommended that this argument not be named
    in any case, but instead be the first (unnamed) argument.
  }
  \item{data}{
    For the \code{formula} methods, a data frame (or more precisely,
    anything that is a valid \code{envir} argument in
    \code{\link{eval}}, e.g., a list or an environment) containing values
    for any variables in the formula, as well as \code{groups} and
    \code{subset} if applicable.  If not found in \code{data}, or if
    \code{data} is unspecified, the variables are looked for in the
    environment of the formula.  For other methods (where \code{x} is
    not a formula), \code{data} is usually ignored, often with a warning
    if it is explicitly specified.
  }
  \item{allow.multiple}{
    Logical flag specifying whether the extended formula interface
    described above should be in effect.  Defaults to \code{TRUE}
    whenever sensible.
  }
  \item{outer}{
    Logical flag controlling what happens with formulas using the
    extended interface described above (see the entry for \code{x} for
    details).  Defaults to \code{FALSE}, except when \code{groups} is
    explicitly specified or grouping does not make sense for the default
    panel function.
  }
  \item{box.ratio}{
    Applicable to \code{barchart} and \code{bwplot}.  Specifies the
    ratio of the width of the rectangles to the inter-rectangle space.
    See also the \code{box.width} argument in the respective default
    panel functions.
  }
  \item{horizontal}{
    Logical flag applicable to \code{bwplot}, \code{dotplot},
    \code{barchart}, and \code{stripplot}. Determines which of \code{x}
    and \code{y} is to be a factor or shingle (\code{y} if TRUE,
    \code{x} otherwise).  Defaults to \code{FALSE} if \code{x} is a
    factor or shingle, \code{TRUE} otherwise. This argument is used to
    process the arguments to these high-level functions, but more
    importantly, it is passed as an argument to the panel function,
    which is expected to use it as appropriate.

    A potentially useful component of \code{scales} in this case may be
    \code{abbreviate = TRUE}, in which case long labels which would
    usually overlap will be abbreviated. \code{scales} could also
    contain a \code{minlength} argument in this case, which would be
    passed to the \code{abbreviate} function.
  }

  %   \item{jitter.data}{
  %     logical specifying whether the values should be jittered by adding a
  %     random noise in stripplot.  This is actually an argument of
  %     \code{panel.stripplot}.
  %   }
  %   \item{factor}{
  %     numeric controlling amount of jitter as in \code{\link{jitter}}.
  %   }

  \bold{Common arguments: } The following arguments are common to all
  the functions documented here, as well as most other high-level
  Trellis functions. These are not documented elsewhere, except to
  override the usage given here.

  \item{panel}{
    Once the subset of rows defined by each unique combination of the
    levels of the grouping variables are obtained (see
    \dQuote{Details}), the corresponding \code{x} and \code{y} variables
    (or other variables, as appropriate, in the case of other high-level
    functions) are passed on to be plotted in each panel. The actual
    plotting is done by the function specified by the \code{panel}
    argument.  The argument may be a function object or a character
    string giving the name of a predefined function.  Each high-level
    function has its own default panel function, named as
    \dQuote{\code{panel.}} followed by the name of the corresponding
    high-level function (e.g., \code{\link{panel.xyplot}},
    \code{\link{panel.barchart}}, etc).

    Much of the power of Trellis Graphics comes from the ability to
    define customized panel functions.  A panel function appropriate for
    the functions described here would usually expect arguments named
    \code{x} and \code{y}, which would be provided by the conditioning
    process.  It can also have other arguments. It is useful to know in
    this context that all arguments passed to a high-level Lattice
    function (such as \code{xyplot}) that are not recognized by it are
    passed through to the panel function. It is thus generally good
    practice when defining panel functions to allow a \code{\dots}
    argument. Such extra arguments typically control graphical
    parameters, but other uses are also common. See documentation for
    individual panel functions for specifics.

    Note that unlike in S-PLUS, it is not guaranteed that panel
    functions will be supplied only numeric vectors for the \code{x} and
    \code{y} arguments; they can be factors as well (but not
    shingles).  Panel functions need to handle this case, which in most
    cases can be done by simply coercing them to numeric.

    Technically speaking, panel functions must be written using Grid
    graphics functions.  However, knowledge of Grid is usually not
    necessary to construct new custom panel functions, as there are
    several predefined panel functions which can help; for example,
    \code{panel.grid}, \code{panel.loess}, etc.  There are also some
    grid-compatible replacements of commonly used traditional graphics
    functions useful for this purpose.  For example, \code{lines} can be
    replaced by \code{llines} (or equivalently, \code{panel.lines}).
    Note that traditional graphics functions like \code{lines} will not
    work in a lattice panel function.

    One case where a bit more is required of the panel function is when
    the \code{groups} argument is not \code{NULL}.  In that case, the
    panel function should also accept arguments named \code{groups} and
    \code{subscripts} (see below for details).  A useful panel function
    predefined for use in such cases is \code{\link{panel.superpose}},
    which can be combined with different \code{panel.groups} functions
    to determine what is plotted for each group.  See the
    \dQuote{Examples} section for an interaction plot constructed in
    this way.  Several other panel functions can also handle the
    \code{groups} argument, including the default ones for
    \code{xyplot}, \code{barchart}, \code{dotplot}, and
    \code{stripplot}.

    Even when \code{groups} is not present, the panel function can have
    \code{subscripts} as a formal argument.  In either case, the
    \code{subscripts} argument passed to the panel function are the
    indices of the \code{x} and \code{y} data for that panel in the
    original \code{data}, BEFORE taking into account the effect of
    the \code{subset} argument.  Note that \code{groups} remains
    unaffected by any subsetting operations, so
    \code{groups[subscripts]} gives the values of \code{groups} that
    correspond to the data in that panel.

    This interpretation of \code{subscripts} does not hold when the
    extended formula interface is in use (i.e., when
    \code{allow.multiple} is in effect).  A comprehensive description
    would be too complicated (details can be found in the source code of
    the function \code{latticeParseFormula}), but in short, the extended
    interface works by creating an artificial grouping variable that is
    longer than the original data frame, and consequently,
    \code{subscripts} needs to refer to rows beyond those in the
    original data.  To further complicate matters, the artificial
    grouping variable is created after any effect of \code{subset}, in
    which case \code{subscripts} may have no relationship with
    corresponding rows in the original data frame.

    One can also use functions called \code{\link{panel.number}} and
    \code{\link{packet.number}}, representing panel order and packet
    order respectively, inside the panel function (as well as the strip
    function or while interacting with a lattice display using
    \code{\link{trellis.focus}} etc).  Both provide a simple integer
    index indicating which panel is currently being drawn, but differ in
    how the count is calculated.  The panel number is a simple
    incremental counter that starts with 1 and is incremented each time
    a panel is drawn.  The packet number on the other hand indexes the
    combination of levels of the conditioning variables that is
    represented by that panel.  The two indices coincide unless the
    order of conditioning variables is permuted and/or the plotting
    order of levels within one or more conditioning variables is altered
    (using \code{perm.cond} and \code{index.cond} respectively), in
    which case \code{packet.number} gives the index corresponding to the
    \sQuote{natural} ordering of that combination of levels of the
    conditioning variables.

    \code{\link{panel.xyplot}} has an argument called \code{type} which
    is worth mentioning here because it is quite frequently used (and as
    mentioned above, can be passed to \code{xyplot} directly).  In the
    event that a \code{groups} variable is used,
    \code{\link{panel.xyplot}} calls \code{\link{panel.superpose}},
    arguments of which can also be passed directly to \code{xyplot}.
    Panel functions for \code{bwplot} and friends should have an
    argument called \code{horizontal} to account for the cases when
    \code{x} is the factor or shingle.
  }

  % \item{panel.groups}{
  %     useful mostly for \code{xyplot} and \code{densityplot}. Applies when
  %     \code{panel} is \code{panel.superpose} (which happens by default in
  %     these cases if \code{groups} is non-null)
  %   }

  \item{aspect}{
    This argument controls the physical aspect ratio of the panels,
    which is usually the same for all the panels. It can be specified as
    a ratio (vertical size/horizontal size) or as a character string.
    In the latter case, legitimate values are \code{"fill"} (the
    default) which tries to make the panels as big as possible to fill
    the available space; \code{"xy"}, which computes the aspect ratio
    based on the 45 degree banking rule (see \code{\link{banking}}); and
    \code{"iso"} for isometric scales, where the relation between
    physical distance on the device and distance in the data scale are
    forced to be the same for both axes.

    If a \code{prepanel} function is specified and it returns components
    \code{dx} and \code{dy}, these are used for banking calculations.
    Otherwise, values from the default prepanel function are used.  Not
    all default prepanel functions produce sensible banking
    calculations.
  }

  \item{groups}{
    A variable or expression to be evaluated in \code{data}, expected to
    act as a grouping variable within each panel, typically used to
    distinguish different groups by varying graphical parameters like
    color and line type.  Formally, if \code{groups} is specified, then
    \code{groups} along with \code{subscripts} is passed to the panel
    function, which is expected to handle these arguments.  For high
    level functions where grouping is appropriate, the default panel
    functions can handle grouping.

    It is very common to use a key (legend) when a grouping variable is
    specified.  See entries for \code{key}, \code{auto.key} and
    \code{\link{simpleKey}} for how to draw a key.
  }

  \item{auto.key}{
    A logical, or a list containing components to be used as arguments
    to \code{\link{simpleKey}}.  \code{auto.key=TRUE} is equivalent to
    \code{auto.key=list()}, in which case \code{\link{simpleKey}} is
    called with a set of default arguments (which may depend on the
    relevant high-level function).  Most valid components to the
    \code{key} argument can be specified in this manner, as
    \code{\link{simpleKey}} will simply add unrecognized arguments to
    the list it produces.

    \code{auto.key} is typically used to automatically produce a
    suitable legend in conjunction with a grouping variable.  If
    \code{auto.key=TRUE}, a suitable legend will be drawn if a
    \code{groups} argument is also provided, and not otherwise.  In list
    form, \code{auto.key} will modify the default legend thus produced.
    For example, \code{auto.key=list(columns = 2)} will create a legend
    split into two columns (\code{columns} is documented in the entry
    for \code{key}).

    More precisely, if \code{auto.key} is not \code{FALSE},
    \code{groups} is non-null, and there is no \code{key} or
    \code{legend} argument specified in the call, a key is created with
    \code{simpleKey} with \code{levels(groups)} as the first
    (\code{text}) argument. (Note: this may not work in all high-level
    functions, but it does work for the ones where grouping makes sense
    with the default panel function).  If \code{auto.key} is provided as
    a list and includes a \code{text} component, then that is used
    instead as the text labels in the key, and the key is drawn even if
    \code{groups} is not specified.

    Note that \code{simpleKey} uses the default settings (see
    \code{\link{trellis.par.get}}) to determine the graphical parameters
    in the key, so the resulting legend will be meaningful only if the
    same settings are used in the plot as well.  The \code{par.settings}
    argument may be useful to temporarily modify the default settings
    for this purpose.

    One disadvantage to using \code{key} (or even \code{simpleKey})
    directly is that the graphical parameters used in the key are
    absolutely determined at the time when the \code{"trellis"} object
    is created. Consequently, if a plot once created is
    re-\code{plot}-ted with different settings, the original parameter
    settings will be used for the key even though the new settings are
    used for the actual display. However, with \code{auto.key}, the key
    is actually created at plotting time, so the settings will match.
  }
  \item{prepanel}{
    A function that takes the same arguments as the \code{panel}
    function and returns a list, possibly containing components named
    \code{xlim}, \code{ylim}, \code{dx}, and \code{dy} (and less
    frequently, \code{xat} and \code{yat}).  The return value of a
    user-supplied prepanel function need not contain all these
    components; in case some are missing, they are replaced by the
    component-wise defaults.

    The \code{xlim} and \code{ylim} components are similar to the high
    level \code{xlim} and \code{ylim} arguments (i.e., they are usually
    a numeric vector of length 2 defining a range, or a
    character vector representing levels of a factor).  If the
    \code{xlim} and \code{ylim} arguments are not explicitly specified
    (possibly as components in \code{scales}) in the high-level call,
    then the actual limits of the panels are guaranteed to include the
    limits returned by the prepanel function.  This happens globally if
    the \code{relation} component of \code{scales} is \code{"same"}, and
    on a per-panel basis otherwise.

    The \code{dx} and \code{dy} components are used for banking
    computations in case \code{aspect} is specified as \code{"xy"}.  See
    documentation of \code{\link{banking}} for details.

    If \code{xlim} or \code{ylim} is a character vector (which is
    appropriate when the corresponding variable is a factor), this
    implicitly indicates that the scale should include the first
    \code{n} integers, where \code{n} is the length of \code{xlim} or
    \code{ylim}, as the case may be.  The elements of the character
    vector are used as the default labels for these \code{n} integers.
    Thus, to make this information consistent between panels, the
    \code{xlim} or \code{ylim} values should represent all the levels of
    the corresponding factor, even if some are not used within that
    particular panel.

    In such cases, an additional component \code{xat} or \code{yat} may
    be returned by the \code{prepanel} function, which should be a
    subset of \code{1:n}, indicating which of the \code{n} values
    (levels) are actually represented in the panel.  This is useful when
    calculating the limits with \code{relation="free"} or
    \code{relation="sliced"} in \code{scales}.

    The prepanel function is responsible for providing a meaningful
    return value when the \code{x}, \code{y} (etc.) variables are
    zero-length vectors.  When nothing else is appropriate, values of NA
    should be returned for the \code{xlim} and \code{ylim} components.
  }

  \item{strip}{
    A logical flag or function.  If \code{FALSE}, strips are not drawn.
    Otherwise, strips are drawn using the \code{strip} function, which
    defaults to \code{strip.default}.  See documentation of
    \code{\link{strip.default}} to see the arguments that are available
    to the strip function.  This description also applies to the
    \code{strip.left} argument (see \code{\dots} below), which can be
    used to draw strips on the left of each panel (useful for wide short
    panels, e.g., in time-series plots).
  }

  \item{xlab}{
    Character or expression (or a \code{"grob"}) giving label(s) for the
    x-axis.  Generally defaults to the expression for \code{x} in the
    formula defining the plot.  Can be specified as \code{NULL} to omit
    the label altogether.  Finer control is possible, as described in
    the entry for \code{main}, with the modification that if the
    \code{label} component is omitted from the list, it is replaced by
    the default \code{xlab}.
  }
  \item{ylab}{
    Character or expression (or \code{"grob"}) giving label for the
    y-axis.  Generally defaults to the expression for \code{y} in the
    formula defining the plot.  Finer control is possible, see entries
    for \code{main} and \code{xlab}.
  }

  \item{scales}{
    Generally a list determining how the x- and y-axes (tick marks and
    labels) are drawn.  The list contains parameters in
    \code{name=value} form, and may also contain two other lists called
    \code{x} and \code{y} of the same form (described below).
    Components of \code{x} and \code{y} affect the respective axes only,
    while those in \code{scales} affect both.  When parameters are
    specified in both lists, the values in \code{x} or \code{y} are
    used.  Note that certain high-level functions have defaults that are
    specific to a particular axis (e.g., \code{bwplot} has
    \code{alternating=FALSE} for the categorical axis only); these can
    only be overridden by an entry in the corresponding component of
    \code{scales}.

    As a special exception, \code{scales} (or its \code{x} and \code{y}
    components) can also be a character string, in which case it is
    interpreted as the \code{relation} component.

    The possible components are :

    \describe{
      \item{\code{relation}}{
        A character string that determines how axis limits are
        calculated for each panel.  Possible values are \code{"same"}
        (default), \code{"free"} and \code{"sliced"}.  For
        \code{relation="same"}, the same limits, usually large enough to
        encompass all the data, are used for all the panels.  For
        \code{relation="free"}, limits for each panel is determined by
        just the points in that panel.  Behavior for
        \code{relation="sliced"} is similar, except that the length (max
        - min) of the scales are constrained to remain the same across
        panels.

        The determination of what axis limits are suitable for each
        panel can be controlled by the \code{prepanel} function, which
        can be overridden by \code{xlim}, \code{ylim} or
        \code{scales$limits} (except when \code{relation="sliced"}, in
        which case explicitly specified limits are ignored with a
        warning).  When \code{relation} is \code{"free"}, \code{xlim} or
        \code{ylim} can be a list, in which case it is treated as if its
        components were the limit values obtained from the prepanel
        calculations for each panel (after being replicated if
        necessary).
      }
      \item{\code{tick.number}}{
        An integer, giving the suggested number of intervals between
        ticks.  This is ignored for a factor, shingle, or character
        vector, for in these cases there is no natural rule for leaving
        out some of the labels. But see \code{xlim}.
      }
      \item{\code{draw}}{
        A logical flag, defaulting to \code{TRUE}, that determines
        whether to draw the axis (i.e., tick marks and labels) at all.
      }
      \item{\code{alternating}}{
        Usually a logical flag specifying whether axis labels should
        alternate from one side of the group of panels to the other.
        For finer control, \code{alternating} can also be a vector
        (replicated to be as long as the number of rows or columns per
        page) consisting of the following numbers
        \itemize{
          \item 0: do not draw tick labels
          \item 1: bottom/left
          \item 2: top/right
          \item 3: both.
        }
        \code{alternating} applies only when \code{relation="same"}.
        The default is \code{TRUE}, or equivalently, \code{c(1, 2)}
      }
      \item{\code{limits}}{
        Same as \code{xlim} and \code{ylim}.
      }
      \item{\code{at}}{
        The location of tick marks along the axis (in native
        coordinates), or a list as long as the number of panels
        describing tick locations for each panel.
      }
      \item{\code{labels}}{
        Vector of labels (characters or expressions) to go along with
        \code{at}. Can also be a list like \code{at}.
      }
      \item{\code{cex}}{
        A numeric multiplier to control character sizes for axis labels.
        Can be a vector of length 2, to control left/bottom and
        right/top labels separately.
      }
      \item{\code{font}, \code{fontface}, \code{fontfamily}}{
        Specifies the font to be used for axis labels.
      }
      \item{\code{lineheight}}{
	Specifies the line height parameter (height of line as a
	multiple of the size of text); relevant for multi-line labels.
	(This is currently ignored for \code{\link{cloud}}.)
      }
      \item{\code{tck}}{
        Usually a numeric scalar controlling the length of tick marks.
        Can also be a vector of length 2, to control the length of
        left/bottom and right/top tick marks separately.
      }
      \item{\code{col}}{
        Color of tick marks and labels.
      }
      \item{\code{rot}}{
        Angle (in degrees) by which the axis labels are to be rotated.
        Can be a vector of length 2, to control left/bottom and
        right/top axes separately.
      }
      \item{\code{abbreviate}}{
        A logical flag, indicating whether to abbreviate the labels
        using the \code{\link{abbreviate}} function.  Can be useful for
        long labels (e.g., in factors), especially on the x-axis.
      }
      \item{\code{minlength}}{
        Argument passed to \code{\link{abbreviate}} if
        \code{abbreviate=TRUE}.
      }
      \item{\code{log}}{
        Controls whether the corresponding variable (\code{x} or
        \code{y}) will be log transformed before being passed to the
        panel function.  Defaults to \code{FALSE}, in which case the
        data are not transformed.  Other possible values are any number
        that works as a base for taking logarithm, \code{TRUE} (which is
        equivalent to 10), and \code{"e"} (for the natural logarithm).
        As a side effect, the corresponding axis is labeled differently.
        Note that this is in reality a transformation of the data, not
        the axes.  Other than the axis labeling, using this feature is
        no different than transforming the data in the formula; e.g.,
        \code{scales=list(x = list(log = 2))} is equivalent to \code{y ~
          log2(x)}.

	See entry for \code{equispaced.log} below for details on how to
	control axis labeling.

        % Note that in this case the values passed
        % to the panel function are already transformed, so all
        % computations done inside the panel function will be affected
        % accordingly. For example, \code{panel.lmline} will fit a line
        % to the transformed values.
      }
      \item{\code{equispaced.log}}{
	A logical flag indicating whether tick mark locations should be
	equispaced when \sQuote{log scales} are in use.  Defaults to
	\code{TRUE}.

	Tick marks are always labeled in the original (untransformed)
	scale, but this makes the choice of tick mark locations
	nontrivial.  If \code{equispaced.log} is \code{FALSE}, the
	choice made is similar to how log scales are annotated in
	traditional graphics.  If \code{TRUE}, tick mark locations are
	chosen as \sQuote{pretty} equispaced values in the transformed
	scale, and labeled in the form \code{"base^loc"}, where
	\code{base} is the base of the logarithm transformation, and
	\code{loc} are the locations in the transformed scale.

	See also \code{xscale.components.logpower} in the
	\pkg{latticeExtra} package.
      }
      \item{\code{format}}{
        The \code{format} to use for POSIXct variables. See
        \code{\link{strptime}} for description of valid values.
      }
      \item{\code{axs}}{
        A character string, \code{"r"} (default) or \code{"i"}.  In the
        latter case, the axis limits are calculated as the exact data
        range, instead of being padded on either side. (May not always
        work as expected.)
      }
    }

    Note that much of the function of \code{scales} is accomplished by
    \code{pscales} in \code{\link{splom}}.
  }
  \item{subscripts}{
    A logical flag specifying whether or not a vector named
    \code{subscripts} should be passed to the panel function.  Defaults
    to \code{FALSE}, unless \code{groups} is specified, or if the panel
    function accepts an argument named \code{subscripts}.  This argument
    is useful if one wants the subscripts to be passed on even if these
    conditions do not hold; a typical example is when one wishes to
    augment a Lattice plot after it has been drawn, e.g., using
    \code{\link{panel.identify}}.
  }
  \item{subset}{
    An expression that evaluates to a logical or integer indexing
    vector.  Like \code{groups}, it is evaluated in \code{data}.  Only
    the resulting rows of \code{data} are used for the plot.  If
    \code{subscripts} is \code{TRUE}, the subscripts provided to the
    panel function will be indices referring to the rows of \code{data}
    prior to the subsetting.  Whether levels of factors in the data
    frame that are unused after the subsetting will be dropped depends
    on the \code{drop.unused.levels} argument.
  }
  \item{xlim}{
    Normally a numeric vector (or a DateTime object) of length 2 giving
    left and right limits for the x-axis, or a character vector,
    expected to denote the levels of \code{x}.  The latter form is
    interpreted as a range containing c(1, length(xlim)), with the
    character vector determining labels at tick positions
    \code{1:length(xlim)}.

    \code{xlim} could also be a list, with as many components as the
    number of panels (recycled if necessary), with each component as
    described above.  This is meaningful only when
    \code{scales$x$relation} is \code{"free"}, in which case these are
    treated as if they were the corresponding limit components returned
    by prepanel calculations.
  }
  \item{ylim}{ Similar to \code{xlim}, applied to the y-axis. }
  \item{drop.unused.levels}{
    A logical flag indicating whether the unused levels of factors will
    be dropped, usually relevant when a subsetting operation is
    performed or an \code{\link{interaction}} is created.  Unused levels
    are usually dropped, but it is sometimes appropriate to suppress
    dropping to preserve a useful layout.  For finer control, this
    argument could also be list containing components \code{cond} and
    \code{data}, both logical, indicating desired behavior for
    conditioning variables and primary variables respectively.  The
    default is given by \code{lattice.getOption("drop.unused.levels")},
    which is initially set to \code{TRUE} for both components.  Note
    that this argument does not control dropping of levels of the
    \code{groups} argument.
  }
  \item{default.scales}{
    A list giving the default values of \code{scales} for a particular
    high-level function.  This is rarely of interest to the end-user,
    but may be helpful when defining other functions that act as a
    wrapper to one of the high-level Lattice functions.
  }
  \item{default.prepanel}{
    A function or character string giving the name of a function that
    serves as the (component-wise) fallback prepanel function when the
    \code{prepanel} argument is not specified, or does not return all
    necessary components.  The main purpose of this argument is to
    enable the defaults to be overridden through the use of
    \code{\link{lattice.options}}.
  }
  \item{lattice.options}{
    A list that could be supplied to \code{\link{lattice.options}}.
    These options are applied temporarily for the duration of the call,
    after which the settings revert back to what they were before.  The
    options are retained along with the object and reused during
    plotting.  This enables the user to attach options settings to the
    trellis object itself rather than change the settings globally.  See
    also the \code{par.settings} argument described below for a similar
    treatment of graphical settings.
  }
  \item{\dots}{
    Further arguments, usually not directly processed by the high-level
    functions documented here, but instead passed on to other
    functions. Such arguments can be broadly categorized into two types:
    those that affect all high-level Lattice functions in a similar
    manner, and those that are meant for the specific panel function
    being used.

    The first group of arguments are processed by a common, unexported
    function called \code{trellis.skeleton}.  These arguments affect all
    high-level functions, but are only documented here (except to
    override the behaviour described here).  All other arguments
    specified in a high-level call, specifically those neither described
    here nor in the help page of the relevant high-level function, are
    passed unchanged to the panel function used.  By convention, the
    default panel function used for any high-level function is named as
    \dQuote{\code{panel.}} followed by the name of the high-level
    function; for example, the default panel function for \code{bwplot}
    is \code{panel.bwplot}.  In practical terms, this means that in
    addition to the help page of the high-level function being used, the
    user should also consult the help page of the corresponding panel
    function for arguments that may be specified in the high-level call.

    The effect of the first group of common arguments are as follows:

    \describe{
      \item{\code{as.table}:}{
        A logical flag that controls the order in which panels should be
        displayed: if \code{FALSE} (the default), panels are drawn left to
        right, bottom to top (as in a graph); if \code{TRUE}, left to
        right, top to bottom (as in a table).
      }

      \item{\code{between}:}{
        A list with components \code{x} and \code{y} (both usually 0 by
        default), numeric vectors specifying the space between the
        panels (units are character heights). \code{x} and \code{y} are
        repeated to account for all panels in a page and any extra
        components are ignored. The result is used for all pages in a
        multi page display.  In other words, it is not possible to use
        different \code{between} values for different pages.
      }

      \item{\code{key}:}{
        A list that defines a legend to be drawn on the plot.  This list
        is used as an argument to the \code{\link{draw.key}} function,
        which produces a \code{"grob"} (grid object) eventually plotted
        by the print method for \code{"trellis"} objects.  The structure
        of the legend is constrained in the ways described below.

        Although such a list can be and often is created explicitly, it
        is also possible to generate such a list using the
        \code{\link{simpleKey}} function; the latter is more convenient
        but less flexible.  The \code{auto.key} argument can be even
        more convenient for the most common situation where legends are
        used, namely, in conjunction with a grouping variable.  To use
        more than one legend, or to have arbitrary legends not
        constrained by the structure imposed by \code{key}, use the
        \code{legend} argument.

        The position of the key can be controlled in either of two
        possible ways. If a component called \code{space} is present,
        the key is positioned outside the plot region, in one of the
        four sides, determined by the value of \code{space}, which can
        be one of \code{"top"}, \code{"bottom"}, \code{"left"} and
        \code{"right"}.  Alternatively, the key can be positioned inside
        the plot region by specifying components \code{x}, \code{y} and
        \code{corner}. \code{x} and \code{y} determine the location of
        the corner of the key given by \code{corner}, which is usually
        one of \code{c(0,0)}, \code{c(1,0)}, \code{c(1,1)} and
        \code{c(0,1)}, which denote the corners of the unit square.
        Fractional values are also allowed, in which case \code{x} and
        \code{y} determine the position of an arbitrary point inside (or
        outside for values outside the unit interval) the key.

        \code{x} and \code{y} should be numbers between 0 and 1, giving
        coordinates with respect to the \dQuote{display area}.
        Depending on the value of the \code{"legend.bbox"} option (see
        \code{\link{lattice.getOption}}), this can be either the full
        figure region (\code{"full"}), or just the region that bounds
        the panels and strips (\code{"panel"}).

        The key essentially consists of a number of columns, possibly
        divided into blocks, each containing some rows.  The contents of the
        key are determined by (possibly repeated) components named
        \code{"rectangles"}, \code{"lines"}, \code{"points"} or
        \code{"text"}.  Each of these must be lists with relevant graphical
        parameters (see later) controlling their appearance.  The \code{key}
        list itself can contain graphical parameters, these would be used if
        relevant graphical components are omitted from the other components.

        The length (number of rows) of each such column (except
        \code{"text"}s) is taken to be the largest of the lengths of the
        graphical components, including the ones specified outside (see
        the entry for \code{rep} below for details on this). The
        \code{"text"} component must have a character or expression
        vector as its first component, to be used as labels.  The length
        of this vector determines the number of rows.

        The graphical components that can be included in \code{key} and
        also in the components named \code{"text"}, \code{"lines"},
        \code{"points"} and \code{"rectangles"} (as appropriate) are:

        \itemize{
          \item \code{cex=1} (text, lines, points)
          \item \code{col="black"} (text, rectangles, lines, points)
          \item \code{alpha=1} (text, rectangles, lines, points)
          \item \code{fill="transparent"} (lines, points)
          \item \code{lty=1} (lines)
          \item \code{lwd=1} (lines, points)
          \item \code{font=1} (text, points)
          \item \code{fontface} (text, points)
          \item \code{fontfamily} (text, points)
          \item \code{pch=8} (lines, points)
          \item \code{adj=0} (text)
          \item \code{type="l"} (lines)
          \item \code{size=5} (rectangles, lines)
          \item \code{height=1} (rectangles)
	  \item \code{lineheight=1} (text)
          \item \code{angle=0} (rectangles, but ignored)
          \item \code{density=-1} (rectangles, but ignored)
        }

        In addition, the component \code{border} can be included inside
        the \code{"rect"} component to control the border color of the
        rectangles; when specified at the top level, \code{border}
        controls the border of the entire key (see below).

        \code{angle} and \code{density} are unimplemented.  \code{size}
        determines the width of columns of rectangles and lines in
        character widths. \code{type} is relevant for lines; \code{"l"}
        denotes a line, \code{"p"} denotes a point, and \code{"b"} and
        \code{"o"} both denote both together. \code{height} gives
        heights of rectangles as a fraction of the default.

        Other possible components of \code{key} are:

        \describe{
          \item{\code{reverse.rows}}{
            Logical flag, defaulting to \code{FALSE}.  If \code{TRUE},
            all components are reversed \emph{after} being replicated
            (the details of which may depend on the value of
            \code{rep}).  This is useful in certain situations, e.g.,
            with a grouped \code{barchart} with \code{stack = TRUE} with
            the categorical variable on the vertical axis, where the
            bars in the plot will usually be ordered from bottom to top,
            but the corresponding legend will have the levels from top
            to bottom unless \code{reverse.rows = TRUE}.  Note that in
            this case, unless all columns have the same number or rows,
            they will no longer be aligned.
          }
          \item{\code{between}}{
            Numeric vector giving the amount of space (character widths)
            surrounding each column (split equally on both sides).
          }
          \item{\code{title}}{
            String or expression giving a title for the key.
          }
          \item{\code{rep}}{
            Logical flag, defaults to \code{TRUE}.  By default, it is
            assumed that all columns in the key (except the
            \code{"text"}s) will have the same number of rows, and all
            components are replicated to be as long as the longest. This
            can be suppressed by specifying \code{rep=FALSE}, in which
            case the length of each column will be determined by
            components of that column alone.
          }
          \item{\code{cex.title}}{
            Zoom factor for the title.
          }
          \item{\code{lines.title}}{
            The amount of vertical space to be occupied by the title in
            lines (in multiples of itself).  Defaults to 2.
          }
          \item{\code{padding.text}}{
            The amount of space (padding) to be used above and below
            each row containing text, in multiples of the default, which
            is currently \code{0.2 * "lines"}.  This padding is in
            addition to the normal height of any row that contains text,
            which is the minimum amount necessary to contain all the
            text entries.
          }
          \item{\code{background}}{
            Background color for the legend.  Defaults to the global
            background color.
          }
          \item{\code{alpha.background}}{
            An alpha transparency value between 0 and 1 for the
            background.
          }
          \item{\code{border}}{
            Either a color for the border, or a logical flag.  In the
            latter case, the border color is black if \code{border} is
            \code{TRUE}, and no border is drawn if it is \code{FALSE}
            (the default).
          }
          \item{\code{transparent=FALSE}}{
            Logical flag, whether legend should have a transparent
            background.
          }
          \item{\code{just}}{
            A character or numeric vector of length one or two giving
            horizontal and vertical justification for the placement of
            the legend.  See \code{\link[grid:grid.layout]{grid.layout}}
            for more precise details.
          }
          \item{\code{columns}}{
            The number of column-blocks (drawn side by side) the legend
            is to be divided into.
          }
          \item{\code{between.columns}}{
            Space between column blocks, in addition to \code{between}.
          }
          \item{\code{divide}}{
            Number of point symbols to divide each line when \code{type} is
            \code{"b"} or \code{"o"} in \code{lines}.
          }
        }
      }
      \item{\code{legend}:}{
        The legend argument can be useful if one wants to place more
        than one key.  It also allows the use of arbitrary
        \code{"grob"}s (grid objects) as legends.

        If used, \code{legend} must be a list, with an arbitrary number
        of components.  Each component must be named one of
        \code{"left"}, \code{"right"}, \code{"top"}, \code{"bottom"}, or
        \code{"inside"}.  The name \code{"inside"} can be repeated, but
        not the others.  This name will be used to determine the
        location for that component, and is similar to the \code{space}
        component of \code{key}.  If \code{key} (or \code{colorkey} for
        \code{\link{levelplot}} and \code{\link{wireframe}}) is
        specified, their \code{space} component must not conflict with
        the name of any component of \code{legend}.

        Each component of \code{legend} must have a component called
        \code{fun}.  This can be a \code{"grob"}, or a function (or the
        name of a function) that produces a \code{"grob"} when called.
        If this function expects any arguments, they must be supplied as
        a list in another component called \code{args}.  For components
        named \code{"inside"}, there can be additional components called
        \code{x}, \code{y} and \code{corner}, which work in the same way
        as for \code{key}.
      }

      \item{\code{page}:}{
        A function of one argument (page number) to be called after
        drawing each page.  The function must be
        \sQuote{grid-compliant}, and is called with the whole display
        area as the default viewport.
      }
      \item{\code{xlab.top}, \code{ylab.right}:}{
	Labels for the x-axis on top, and y-axis on the right.  Similar
	to \code{xlab} and \code{ylab}, but less commonly used.
      }
      \item{\code{main}:}{
        Typically a character string or expression describing the main
        title to be placed on top of each page.  Defaults to
        \code{NULL}.

        \code{main} (as well as \code{xlab}, \code{ylab} and \code{sub})
        is usually a character string or an expression that gets used as
        the label, but can also be a list that controls further details.
        Expressions are treated as specification of LaTeX-like markup as
        described in \code{\link{plotmath}}.  The label can be a vector,
        in which case the components will be spaced out horizontally (or
        vertically for \code{ylab}).  This feature can be used to
        provide column or row labels rather than a single axis label.

        When \code{main} (etc.) is a list, the actual label should be
        specified as the \code{label} component (which may be unnamed if
        it is the first component).  The label can be missing, in which
        case the default will be used (\code{xlab} and \code{ylab}
        usually have defaults, but \code{main} and \code{sub} do not).
        Further named arguments are passed on to
        \code{\link[grid:grid.text]{textGrob}}; this can include
        arguments controlling positioning like \code{just} and
        \code{rot} as well as graphical parameters such as \code{col}
        and \code{font} (see \code{\link[grid:gpar]{gpar}} for a full
        list).

        \code{main}, \code{sub}, \code{xlab}, \code{ylab},
        \code{xlab.top}, and \code{ylab.right} can also be arbitrary
        \code{"grob"}s (grid graphical objects).
      }
      \item{\code{sub}:}{
        Character string or expression (or a list or \code{"grob"}) for
        a subtitle to be placed at the bottom of each page.  See entry
        for \code{main} for finer control options.
      }
      \item{\code{par.strip.text}:}{
        A list of parameters to control the appearance of strip text.
        Notable components are \code{col}, \code{cex}, \code{font}, and
        \code{lines}.  The first three control graphical parameters
        while the last is a means of altering the height of the strips.
        This can be useful, for example, if the strip labels (derived
        from factor levels, say) are double height (i.e., contains
        \code{"\\n"}-s) or if the default height seems too small or too
        large.

        Additionally, the \code{lineheight} component can control the
        space between multiple lines.  The labels can be abbreviated
        when shown by specifying \code{abbreviate = TRUE}, in which case
        the components \code{minlength} and \code{dot} (passed along to
        the \code{\link{abbreviate}} function) can be specified to
        control the details of how this is done.
      }
      \item{\code{layout}:}{
        In general, a conditioning plot in Lattice consists of several
        panels arranged in a rectangular array, possibly spanning
        multiple pages. \code{layout} determines this arrangement.

        \code{layout} is a numeric vector of length 2 or 3 giving the
        number of columns, rows, and pages (optional) in a multipanel
        display.  By default, the number of columns is the number of
        levels of the first conditioning variable and the number of rows
        is the number of levels of the second conditioning variable.  If
        there is only one conditioning variable, the default layout
        vector is \code{c(0,n)}, where \code{n} is the number of levels
        of the given vector.  Any time the first value in the layout
        vector is 0, the second value is used as the desired number of
        panels per page and the actual layout is computed from this,
        taking into account the aspect ratio of the panels and the
        device dimensions (via \code{\link{par}("din")}).  If \code{NA}
        is specified for the number of rows or columns (but not both),
        that dimension will be filled out according to the number of
        panels.

	The number of pages is by default set to as many as is required
        to plot all the panels, and so rarely needs to be specified.
        However, in certain situations the default calculation may be
        incorrect, and in that case the number of pages needs to be
        specified explicitly.

	%% Example: xyplot(yield~year|site, barley, layout = c(0, 5))
	%% OK with some par("din"), but not others (e.g., square)
	
	%% NO LONGER TRUE: In general, giving a high value of
        %% \code{layout[3]} is not wasteful because blank pages are
        %% never created.

      }
      \item{\code{skip}:}{
        A logical vector (default \code{FALSE}), replicated to be as
        long as the number of panels (spanning all pages).  For elements
        that are \code{TRUE}, the corresponding panel position is
        skipped; i.e., nothing is plotted in that position.  The panel
        that was supposed to be drawn there is now drawn in the next
        available panel position, and the positions of all the
        subsequent panels are bumped up accordingly.  This may be useful
        for arranging plots in an informative manner.
      }

      \item{\code{strip.left}:}{
        \code{strip.left} can be used to draw strips on the left of each
        panel, which can be useful for wide short panels, as in
        time-series (or similar) plots.  See the entry for \code{strip}
        for detailed usage.
      }

      \item{\code{xlab.default}, \code{ylab.default}:}{
        Fallback default for \code{xlab} and \code{ylab} when they are
        not specified.  If \code{NULL}, the defaults are parsed from the
        Trellis formula.  This is rarely useful for the end-user, but
        can be helpful when developing new Lattice functions.
      }

      \item{\code{xscale.components}, \code{yscale.components}:}{
        Functions that determine axis annotation for the x and y axes
        respectively.  See documentation for
        \code{\link{xscale.components.default}}, the default values of
        these arguments, to learn more.
      }

      \item{\code{axis}:}{
        Function responsible for drawing axis annotation.  See
        documentation for \code{\link{axis.default}}, the default value
        of this argument, to learn more.
      }

      \item{\code{perm.cond}:}{
        An integer vector, a permutation of \code{1:n}, where \code{n}
        is the number of conditioning variables.  By default, the order
        in which panels are drawn depends on the order of the
        conditioning variables specified in the formula.
        \code{perm.cond} can modify this order.  If the trellis display
        is thought of as an \code{n}-dimensional array, then during
        printing, its dimensions are permuted using \code{perm.cond} as
        the \code{perm} argument does in \code{\link{aperm}}.
      }
      \item{\code{index.cond}:}{
        Whereas \code{perm.cond} permutes the dimensions of the
        multidimensional array of panels, \code{index.cond} can be used
        to subset (or reorder) margins of that array.  \code{index.cond}
        can be a list or a function, with behavior in each case
        described below.

        The panel display order within each conditioning variable
        depends on the order of their levels.  \code{index.cond} can be
        used to choose a \sQuote{subset} (in the R sense) of these
        levels, which is then used as the display order for that
        variable.  If \code{index.cond} is a list, it has to be as long
        as the number of conditioning variables, and the \code{i}-th
        component has to be a valid indexing vector for
        \code{levels(g_i)}, where \code{g_i} is the \code{i}-th
        conditioning variable in the plot (note that these levels may
        not contain all levels of the original variable, depending on
        the effects of the \code{subset} and \code{drop.unused.levels}
        arguments).  In particular, this indexing may repeat levels, or
        drop some altogether.  The result of this indexing determines
        the order of panels within that conditioning variable. To keep
        the order of a particular variable unchanged, the corresponding
        component must be set to \code{TRUE}.

        Note that the components of \code{index.cond} are interpreted in
        the order of the conditioning variables in the original call,
        and is not affected by \code{perm.cond}.

        Another possibility is to specify \code{index.cond} as a
        function.  In this case, this function is called once for each
        panel, potentially with all arguments that are passed to the
        panel function for that panel.  (More specifically, if this
        function has a \code{\dots} argument, then all panel arguments
        are passed, otherwise, only named arguments that match are
        passed.)  If there is only one conditioning variable, the levels
        of that variable are then sorted so that these values are in
        ascending order.  For multiple conditioning variables, the order
        for each variable is determined by first taking the average over
        all other conditioning variables.

        Although they can be supplied in high-level function calls
        directly, it is more typical to use \code{perm.cond} and
        \code{index.cond} to update an existing \code{"trellis"} object,
        thus allowing it to be displayed in a different arrangement
        without re-calculating the data subsets that go into each panel.
        In the \code{\link{update.trellis}} method, both can be set to
        \code{NULL}, which reverts these back to their defaults.
      }
      \item{\code{par.settings}:}{
        A list that could be supplied to \code{\link{trellis.par.set}}.
        When the resulting object is plotted, these options are applied
        temporarily for the duration of the plotting, after which the
        settings revert back to what they were before.  This enables the
        user to attach some display settings to the trellis object
        itself rather than change the settings globally.  See also the
        \code{lattice.options} argument described above for a similar
        treatment of non-graphical options.
      }
      \item{\code{plot.args}:}{
        A list containing possible arguments to
        \code{\link{plot.trellis}}, which will be used by the
        \code{plot} or \code{print} methods when drawing the object,
        unless overridden explicitly.  This enables the user to attach
        such arguments to the trellis object itself.  Partial matching
        is not performed.
      }
    }
  }
}


\value{
  The high-level functions documented here, as well as other high-level
  Lattice functions, return an object of class \code{"trellis"}.  The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  subsequently update components of the object, and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.
}

\details{
  The high-level functions documented here, as well as other high-level
  Lattice functions, are generic, with the \code{formula} method usually
  doing the most substantial work.  The structure of the plot that is
  produced is mostly controlled by the formula (implicitly in the case
  of the non-formula methods).  For each unique combination of the
  levels of the conditioning variables \code{g1, g2, \dots}, a separate
  \dQuote{packet} is produced, consisting of the points \code{(x,y)} for
  the subset of the data defined by that combination.  The display can
  be thought of as a three-dimensional array of panels, consisting of one
  two-dimensional matrix per page.  The dimensions of this array are
  determined by the \code{layout} argument.  If there are no
  conditioning variables, the plot produced consists of a single packet.
  Each packet usually corresponds to one panel, but this is not strictly
  necessary (see the entry for \code{index.cond} above).

  The coordinate system used by \pkg{lattice} by default is like a
  graph, with the origin at the bottom left, with axes increasing to the
  right and top.  In particular, panels are by default drawn starting
  from the bottom left corner, going right and then up, unless
  \code{as.table = TRUE}, in which case panels are drawn from the top
  left corner, going right and then down.  It is possible to set a
  global preference for the table-like arrangement by changing the
  default to \code{as.table=TRUE}; this can be done by setting
  \code{lattice.options(default.args = list(as.table = TRUE))}.  Default
  values can be set in this manner for the following arguments:
  \code{as.table}, \code{aspect}, \code{between}, \code{page},
  \code{main}, \code{sub}, \code{par.strip.text}, \code{layout},
  \code{skip} and \code{strip}.  Note that these global defaults are
  sometimes overridden by individual functions.

  The order of the panels depends on the order in which the conditioning
  variables are specified, with \code{g1} varying fastest, followed by
  \code{g2}, and so on. Within a conditioning variable, the order
  depends on the order of the levels (which for factors is usually in
  alphabetical order).  Both of these orders can be modified using the
  \code{index.cond} and \code{perm.cond} arguments, possibly using the
  \code{\link[lattice:update.trellis]{update}} (and other related)
  method(s).
}

\note{
  Most of the arguments documented here are also applicable for the
  other high-level functions in the \pkg{lattice} package. These are not
  described in any detail elsewhere unless relevant, and this should be
  considered the canonical documentation for such arguments.

  Any arguments passed to these functions and not recognized by them
  will be passed to the panel function. Most predefined panel functions
  have arguments that customize its output. These arguments are
  described only in the help pages for these panel functions, but can
  usually be supplied as arguments to the high-level plot.
}

\references{
  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data Visualization
    with R}, Springer.  \url{http://lmdvr.r-forge.r-project.org/}
}

\seealso{
  \code{\link{Lattice}} for an overview of the package, as well as
  \code{\link{barchart.table}},
  \code{\link{print.trellis}},
  \code{\link{shingle}},
  \code{\link{banking}},
  \code{\link{reshape}},
  \code{\link{panel.xyplot}},
  \code{\link{panel.bwplot}},
  \code{\link{panel.barchart}},
  \code{\link{panel.dotplot}},
  \code{\link{panel.stripplot}},
  \code{\link{panel.superpose}},
  \code{\link{panel.loess}},
  \code{\link{panel.average}},
  \code{\link{strip.default}},
  \code{\link{simpleKey}}
  \code{\link{trellis.par.set}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\examples{
require(stats)

## Tonga Trench Earthquakes

Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(lat ~ long | Depth, data = quakes)
update(trellis.last.object(),
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),
       aspect = "iso")

## Examples with data from `Visualizing Data' (Cleveland, 1993) obtained
## from http://cm.bell-labs.com/cm/ms/departments/sia/wsc/

EE <- equal.count(ethanol$E, number=9, overlap=1/4)

## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
       },
       aspect = "xy")

## Extended formula interface 

xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))


## user defined panel functions

states <- data.frame(state.x77,
                     state.name = dimnames(state.x77)[[1]],
                     state.region = state.region)
xyplot(Murder ~ Population | state.region, data = states,
       groups = state.name,
       panel = function(x, y, subscripts, groups) {
           ltext(x = x, y = y, labels = groups[subscripts], cex=1,
                 fontfamily = "HersheySans")
       })

## Stacked bar chart

barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))

bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")

dotplot(variety ~ yield | year * site, data=barley)

## Grouped dot plot showing anomaly at Morris

dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)

stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")

## Interaction Plot

xyplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
       type = "a",
       auto.key =
       list(space = "right", points = FALSE, lines = TRUE))

## longer version with no x-ticks

\dontrun{
bwplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
       panel = "panel.superpose",
       panel.groups = "panel.linejoin",
       xlab = "treatment",
       key = list(lines = Rows(trellis.par.get("superpose.line"),
                  c(1:7, 1)),
                  text = list(lab = as.character(unique(OrchardSprays$rowpos))),
                  columns = 4, title = "Row position"))
}

}
\keyword{hplot}
