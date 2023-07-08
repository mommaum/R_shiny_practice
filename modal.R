create_modal <- function(title, message) {
  modalDialog(
    title = title,
    message,
    easyClose = TRUE,
    footer = tagList(actionButton("closeModal", "닫기"))
  )
}
