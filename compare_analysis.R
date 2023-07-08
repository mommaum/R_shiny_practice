source("api_call.R")

compare_process_data <-
  function(jsonData) {
    response <- send_post_request(jsonData)

    parsed_data <- fromJSON(response)
    parsed_data_frame <- as.data.frame(parsed_data$results$data)
    parsed_data_frame$period <- as.Date(parsed_data_frame$period)
    parsed_data_frame$period.1 <-
      as.Date(parsed_data_frame$period.1)

    print(parsed_data_frame)

    graph <- ggplot(parsed_data_frame, aes(x = period)) +
      geom_line(aes(y = ratio, color = "1번")) +
      geom_line(aes(y = ratio.1, color = "2번")) +
      xlab("date") +
      ylab("search volume") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))

    scatter_plot <-
      ggplot(parsed_data_frame, aes(x = ratio, y = ratio.1)) +
      geom_point() +
      xlab("X축") +
      ylab("Y축") +
      ggtitle("두 열의 산포도") +
      coord_fixed(ratio = max(parsed_data_frame$ratio) / max(parsed_data_frame$ratio.1))

    correlation <-
      cor(parsed_data_frame$ratio, parsed_data_frame$ratio.1)

    if (correlation >= -0.3 && correlation <= 0.3) {
      correlation <- paste(correlation, "상관관계가 없습니다.")
    } else if ((correlation >= -0.7 &&
      correlation < -0.3) ||
      (correlation > 0.3 && correlation <= 0.7)) {
      correlation <- paste(correlation, "약한 상관관계가 있습니다.")
    } else {
      correlation <- paste(correlation, "강한 상관관계가 있습니다.")
    }

    return(list(
      scatter_plot = scatter_plot,
      graph = graph,
      correlation = correlation
    ))
  }
