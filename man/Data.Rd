% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\name{Data}
\alias{Data}
\title{Abstract class Data}
\description{
As abstract, a \link{Data} object cannot be stored on any layer. Instead, extended
\link{TrainData} or \link{TestData} objects can be stored on a layer.
}
\seealso{
\link{TrainData} and \link{TestData}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Data-new}{\code{Data$new()}}
\item \href{#method-Data-print}{\code{Data$print()}}
\item \href{#method-Data-getIndSubset}{\code{Data$getIndSubset()}}
\item \href{#method-Data-impute}{\code{Data$impute()}}
\item \href{#method-Data-getVarSubset}{\code{Data$getVarSubset()}}
\item \href{#method-Data-getSetDiff}{\code{Data$getSetDiff()}}
\item \href{#method-Data-getDataFrame}{\code{Data$getDataFrame()}}
\item \href{#method-Data-setDataFrame}{\code{Data$setDataFrame()}}
\item \href{#method-Data-getCompleteData}{\code{Data$getCompleteData()}}
\item \href{#method-Data-getId}{\code{Data$getId()}}
\item \href{#method-Data-getData}{\code{Data$getData()}}
\item \href{#method-Data-getIndCol}{\code{Data$getIndCol()}}
\item \href{#method-Data-clone}{\code{Data$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-new"></a>}}
\if{latex}{\out{\hypertarget{method-Data-new}{}}}
\subsection{Method \code{new()}}{
Constructor of class Data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$new(id, ind_col, data_frame)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{\code{character} \cr
Object ID.}

\item{\code{ind_col}}{\code{character} \cr
Column name containing individual IDs.}

\item{\code{data_frame}}{\code{data.frame} \cr
\code{data.frame} containing data.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-print"></a>}}
\if{latex}{\out{\hypertarget{method-Data-print}{}}}
\subsection{Method \code{print()}}{
Printer
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{\code{any}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getIndSubset"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getIndSubset}{}}}
\subsection{Method \code{getIndSubset()}}{
Retrieve a data subset for a given variable name and values, a data subset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getIndSubset(var_name, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{var_name}}{\code{character} \cr
Variable name of interest.}

\item{\code{value}}{\code{vector} \cr
Values of interest.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The data subset is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-impute"></a>}}
\if{latex}{\out{\hypertarget{method-Data-impute}{}}}
\subsection{Method \code{impute()}}{
Imputes missing values in modality-specific predictions.
Only mode and median based imputations are actually supported.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$impute(impute_fct, impute_param, target_name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{impute_fct}}{\code{character} \cr
An imputation function to use instead of median or mode imputation. Not yet implemented!}

\item{\code{impute_param}}{\code{list} \cr}

\item{\code{target_name}}{\code{character} \cr
Name of the target variable.
The list of parameters to call the imputation function.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new object with the predicted values is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getVarSubset"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getVarSubset}{}}}
\subsection{Method \code{getVarSubset()}}{
Retrieve a subset of variables from data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getVarSubset(var_name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{var_name}}{\code{character} \cr
Variable names of interest.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The data subset is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getSetDiff"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getSetDiff}{}}}
\subsection{Method \code{getSetDiff()}}{
For the given variable name, non existing values in the
current dataset are returned.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getSetDiff(var_name, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{var_name}}{\code{character} \cr
Variable name of interest.}

\item{\code{value}}{\code{vector} \cr
Values of interest.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The subset difference is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getDataFrame"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getDataFrame}{}}}
\subsection{Method \code{getDataFrame()}}{
Getter of the \code{data.frame}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getDataFrame()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The \code{data.frame} of the current object is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-setDataFrame"></a>}}
\if{latex}{\out{\hypertarget{method-Data-setDataFrame}{}}}
\subsection{Method \code{setDataFrame()}}{
Set a new \code{data.frame} to the current object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$setDataFrame(data_frame)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_frame}}{\code{data.frame}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The current object is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getCompleteData"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getCompleteData}{}}}
\subsection{Method \code{getCompleteData()}}{
Getter of the complete dataset without missing values.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getCompleteData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The complete dataset is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getId"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getId}{}}}
\subsection{Method \code{getId()}}{
Getter of the current object ID.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getId()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The current object ID is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getData"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getData}{}}}
\subsection{Method \code{getData()}}{
Getter of the current Data. This function is re-implemented
by \link{TrainData} and \link{TestData}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Do not use on this class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-getIndCol"></a>}}
\if{latex}{\out{\hypertarget{method-Data-getIndCol}{}}}
\subsection{Method \code{getIndCol()}}{
Getter of the individual column variable.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$getIndCol()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Data-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Data-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Data$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
