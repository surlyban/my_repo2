# my_repo2

      rm(list = ls(all=T))


      #삼성전자와 삼성바이오로직스의 일별 수익률 비교

      library(dplyr)
      library(stringr)
      library(lubridate)

      # stock_data <- function(code, startdate, enddate){
      #   
      #   code <- code[2:7]
      # 
      #   first = paste0("https://api.finance.naver.com/siseJson.naver?symbol=", code) 
      #   second = paste0("&requestType=1&startTime=", startdate)
      #   third = paste0("&endTime=", enddate)
      #   fourth = paste0("&timeframe=day")
      #   url <- paste0(first, second, third, fourth)
      #   
      #   data <- readLines(url, warn = FALSE)
      #   
      #   data2 <- 
      #     gsub("\t|\\[|\\]","",data) |> 
      #     strsplit(", ")
      #   
      #   column_names <- 
      #     data2[[2]] |> 
      #     trimws() |> 
      #     str_remove_all("'")
      #   
      #   data3 <- 
      #     data2[-2]
      #   
      #   data4 <- 
      #     data3[lengths(data3) != 0]
      #   
      #   data5 <-lapply(data4,
      #                  setNames,
      #                  column_names) |> 
      #             bind_rows() |> 
      #             mutate(날짜 = str_remove_all(날짜, "\\\""))
      #   
      #   data5$날짜 <- ymd(data5$날짜)
      #   
      #   return(data5)
      # }
      # 
      # biosam <- stock_data(207940, 20220304, 20230611)
      # 
      # biosam
      # 
      # samsung <- stock_data(005930, 20220304, 20230611)
      # samsung

      url <- "https://api.finance.naver.com/siseJson.naver?symbol=005930&requestType=1&startTime=20220312&endTime=20230517&timeframe=day"

      data <- readLines(url, warn = FALSE)

      data2 <- 
        gsub("\t|\\[|\\]","",data) |> 
        strsplit(", ")

      column_names <- 
        data2[[2]] |> 
        trimws() |> 
        str_remove_all("'")

      data3 <- 
        data2[-2]

      data4 <- 
        data3[lengths(data3) != 0]

      data5 <-lapply(data4,
                     setNames,
                     column_names) |> 
        bind_rows() |> 
        mutate(날짜 = str_remove_all(날짜, "\\\""))

      data5$날짜 <- ymd(data5$날짜)

      samsung <- data5

      url <- "https://api.finance.naver.com/siseJson.naver?symbol=207940&requestType=1&startTime=20220312&endTime=20230517&timeframe=day"

      data <- readLines(url, warn = FALSE)

      data2 <- 
        gsub("\t|\\[|\\]","",data) |> 
        strsplit(", ")

      column_names <- 
        data2[[2]] |> 
        trimws() |> 
        str_remove_all("'")

      data3 <- 
        data2[-2]

      data4 <- 
        data3[lengths(data3) != 0]

      data5 <-lapply(data4,
                     setNames,
                     column_names) |> 
        bind_rows() |> 
        mutate(날짜 = str_remove_all(날짜, "\\\""))

      data5$날짜 <- ymd(data5$날짜)

      biosam <- data5

      head(samsung)
      tail(samsung)

      plot(biosam[,5])
      plot(samsung[,5])

      samsung.rtn <- ROC(as.numeric(samsung$종가))
      samsung.rtn

      biosam.rtn <- ROC(as.numeric(biosam$종가))
      biosam.rtn

      #Date
      date<-samsung$날짜
      date

      #return-risk ratio
      mean(biosam.rtn)/sd(biosam.rtn)
      mean(samsung.rtn)/sd(samsung.rtn)

      #Plot the returns of biosam and samsung
      dev.new()
      par(mfrow=c(1,1), mai=c(0.5,0.5,0.5,0.5))
      plot(date, biosam.rtn, type="l", col="blue", ylab="Return", xlab="")
      lines(date,samsung.rtn, type="l", col="red")
      legend("topright", legend = c("biosam Return", "samsung Return"), 
             cex=1.5, lty=c(1,1), col=c("blue","red"), box.lty = 0)


      dev.new()
      par(mfrow=c(2,1), mai=c(0.7,0.7,0.7,0.7))
      plot(date, biosam.rtn, pch=12, cex=0.5, col="blue", ylab="Return", xlab="")
      plot(date, samsung.rtn, pch=12, cex=0.5, col="red", ylab="Return", xlab="")
      abline(v=mean(biosam.rtn))
      abline(h=mean(samsung.rtn))

      cor(biosam.rtn, samsung.rtn)

      #Scatter Plot
      data<-data.frame(biosam.rtn, samsung.rtn)
      colnames(data)<-c("삼성바이오로직스", "삼성전자")
      dev.new()
      pairs(data, col="blue", pch=16, cex=1.25, cex.axis=1.25)


