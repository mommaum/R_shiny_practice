source("api_call.R")
source("utils.R")

analysis_single_data <-
  function (jsonData) {
    parsed_data_frame <-
      jsonData %>% send_post_request() %>% fromJSON() %>% {
        .$results$data
      } %>% as.data.frame() %>% mutate(period = as.Date(period))
    
    adf_result <-
      adf.test(parsed_data_frame$ratio,
               alternative = "stationary",
               k = 4)
    
    graph <-
      ggplot(parsed_data_frame, aes(x = period, y = ratio)) + geom_line() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))
    
    if (adf_result$p.value < 0.05 && adf_result$statistic < 0) {
      trend_analysis_result <- "검정 결과 추세가 존재하지 않는 것으로 판단됩니다."
      
    } else {
      trend_analysis_result <-
        "검정 결과 추세가 존재하는 것으로 판단됩니다. 추세선이 그래프에 나타납니다."
      graph <- graph + geom_smooth(method = "lm", se = FALSE)
    }
    
    return(list(trend_analysis_result = trend_analysis_result, graph = graph))
  }
