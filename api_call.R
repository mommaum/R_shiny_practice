dotenv::load_dot_env()

client_id <- Sys.getenv("CLIENT_ID")
client_secret <- Sys.getenv("CLIENT_SECRET")

headers <- c("X-Naver-Client-Id" = client_id,
             "X-Naver-Client-Secret" = client_secret)

send_post_request <- function(jsonData) {
  url <- "https://openapi.naver.com/v1/datalab/search"
  response <-
    POST(url, add_headers(.headers = headers), body = jsonData)
  content <- content(response, "text")
  return(content)
}
