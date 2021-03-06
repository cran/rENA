% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ena.conversations.R
\name{ena.conversations}
\alias{ena.conversations}
\title{Find conversations by unit}
\usage{
ena.conversations(
  set,
  units,
  units.by = NULL,
  codes = NULL,
  conversation.by = NULL,
  window = 4,
  conversation.exclude = c()
)
}
\arguments{
\item{set}{[TBD]}

\item{units}{[TBD]}

\item{units.by}{[TBD]}

\item{codes}{[TBD]}

\item{conversation.by}{[TBD]}

\item{window}{[TBD]}

\item{conversation.exclude}{[TBD]}
}
\value{
list containing row indices representing conversations
}
\description{
Find rows of conversations by unit
}
\details{
[TBD]
}
\examples{
data(RS.data)

codeNames = c('Data','Technical.Constraints','Performance.Parameters',
              'Client.and.Consultant.Requests','Design.Reasoning',
              'Collaboration');

accum = ena.accumulate.data(
  units = RS.data[,c("Condition","UserName")],
  conversation = RS.data[,c("Condition","GroupName")],
  metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre",
                        "CONFIDENCE.Post","C.Change")],
  codes = RS.data[,codeNames],
  model = "EndPoint",
  window.size.back = 4
);
set = ena.make.set(
  enadata = accum,
  rotation.by = ena.rotate.by.mean,
  rotation.params = list(accum$meta.data$Condition=="FirstGame",
                         accum$meta.data$Condition=="SecondGame")
);
ena.conversations(set = RS.data,
  units = c("FirstGame.steven z"), units.by=c("Condition","UserName"),
  conversation.by = c("Condition","GroupName"),
  codes=codeNames, window = 4
)

}
