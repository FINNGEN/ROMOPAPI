


#' Send feedback to the API server
#'
#' @param feedback A character string containing the feedback to send.
#'
#' @return NULL
#'
#' @export
sendFeedback <- function(feedback) {

  #
  # VALIDATE
  #
  feedback |> checkmate::assertCharacter()
  #
  # FUNCTION
  #
  ParallelLogger::logInfo("FEEDBACK: ", feedback)
}