% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cronLog.R, R/getLog.R, R/setCronjobValues.R
\name{cronLog}
\alias{cronLog}
\alias{logErrors}
\alias{getLog}
\alias{setCronjobValues}
\title{Logging}
\usage{
cronLog(msg, level = "info", app = getOption("crontabRjobValues")$name)

logErrors(expr)

getLog(levels, jobs, start_date, end_date)

setCronjobValues(name, desc, logLevel = "info", textLevel = "none")
}
\arguments{
\item{msg}{The message to log}

\item{level}{The loglevel to log to}

\item{app}{The app to which to associate the log entry}

\item{expr}{an expression to be evaluated with crontabR logging.}

\item{levels}{A list of logging levels to return from the log}

\item{jobs}{A list of cronjob names to return from the log}

\item{start_date}{The earliest date for which to return log entries}

\item{end_date}{The latest date for which to return log entries}

\item{name}{The name of the currently active script}

\item{desc}{The description of the currently active script}
}
\description{
Log to the rAutomate Logging System
}
\details{
\code{crontabR} contains a logging system that will automatically log most errors
and warnings to a log file saved in \code{~/.crontabR/logs/log}. In addition, you
may use the \code{cronLog} function to log additional information to the log. The
log can be one of six different levels; error, warn, info, verbose, debug, or silly.

\code{logErrors} isn't meant to be used by users. Each script that is automated
with \code{crontabR} is wrapped in this function so that errors, warnings, and
messages are automatically logged. Errors are logged at level \code{error},
warnings are logged at level \code{warn}, and messages are logged at level
\code{verbose}. In addition, any value the script returns will be logged at
level \code{info}.

\code{getLog} reads and optionally filters the \code{crontabR} log.

\code{setCronjobValues} and \code{clearCronjobValues} are automatically added
to scripts processed by \code{crontabR} so that the loggin system will be able
to assign the correct name to the log entries. There shouldn't be any need for
a user to use these functions.
}
