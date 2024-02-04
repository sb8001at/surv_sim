library(shiny)

function(input, output, session) {

  output$survplot <- renderPlot({
    surv <- if(input$dist == "exp"){
      rexp(input$n, rate = input$rate) |> round()+1
    } else if(input$dist == "weibull"){
      rweibull(input$n, shape = input$shape, scale = 1/input$rate) |> round()+1
    } else if(input$dist == "gamma"){
      rgamma(input$n, shape = input$shape, scale = 1/input$rate) |> round()+1
    }
    
    survtext <- if(input$dist == "exp"){
      paste("rate=", input$rate,", censor ratio=", input$ratio_censor, ", N=", input$n)
    } else if(input$dist == "weibull"){
      paste("shape=", input$shape, "scale=", 1/input$rate,", censor ratio=", input$ratio_censor, ", N=", input$n)
    } else if(input$dist == "gamma"){
      paste("shape=", input$shape, "scale=", 1/input$rate,", censor ratio=", input$ratio_censor, ", N=", input$n)
    }
    
    d <- data.frame(surv, censor = rbinom(input$n, size=1, prob=1-input$ratio_censor))
    
    sfit <- survfit(Surv(surv, censor)~1, data=d)
    
    d_dist <- data.frame(
      x=1:(sfit$time |> max()), 
      cdf_ = if(input$dist == "exp"){
        pexp(1:(sfit$time |> max()), rate=input$rate)
      } else if(input$dist == "weibull"){
        pweibull(1:(sfit$time |> max()), shape = input$shape, scale = 1/input$rate)
      } else if(input$dist == "gamma"){
        pgamma(1:(sfit$time |> max()), shape = input$shape, scale = 1/input$rate)
      },
      pdf_ = if(input$dist == "exp"){
        dexp(1:(sfit$time |> max()), rate=input$rate)
      } else if(input$dist == "weibull"){
        dweibull(1:(sfit$time |> max()), shape = input$shape, scale = 1/input$rate)
      } else if(input$dist == "gamma"){
        dgamma(1:(sfit$time |> max()), shape = input$shape, scale = 1/input$rate)
      }
    )
    
    p_pdf <- 
      d_dist |> 
      ggplot(aes(x=x, y=pdf_, ymin=0, ymax=pdf_, color="#F8766D", fill="#F8766D"))+
      geom_ribbon()+
      theme_bw()+
      theme(legend.position="none")+
      labs(y="確率分布", x="Time")
    
    p_cdf <- 
      d_dist |> 
      ggplot(aes(x=x, y=cdf_, ymin=0, ymax=cdf_, color="#F8766D", fill="#F8766D"))+
      geom_ribbon()+
      theme_bw()+
      theme(legend.position="none")+
      labs(y="累積確率", x="Time")
    
    p_surv <- sfit |> 
      ggsurvfit(linewidth = 1) +
      add_confidence_interval()+
      add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75)+
      add_censor_mark(size=3)+
      labs(caption=survtext)
    
    dist_survreg <- if(input$dist == "exp"){
      "exponential"
    } else {
      "weibull"
    }
    
    reg <- survreg(Surv(surv, censor)~1, data=d, dist=dist_survreg)
    rate_calculated <- 1/reg$coefficients |> exp()
    scale_calculated <- reg$coefficients |> exp()
    shape_calculated <- 1 / reg$scale
    
    survtext2 <- if(input$dist == "exp"){
      paste("rate=", rate_calculated)
    } else if(input$dist == "weibull"){
      paste("shape=", shape_calculated, "scale=", scale_calculated)
    } else if(input$dist == "gamma"){
      paste("shape=", shape_calculated, "scale=", scale_calculated)
    }
    
    p_haz <- data.frame(
      x = 1:(sfit$time |> max()),
      y = hazard_f(1:(sfit$time |> max()), 1/scale_calculated, shape_calculated)
    ) |> 
      ggplot(aes(x=x, y=y))+
      geom_line(linewidth=1)+
      expand_limits(y=0)+
      theme_bw()+
      labs(y="ハザード", x="Time", title="survregで計算したハザード", caption=survtext2)
    
    (p_surv | p_pdf) / (p_haz | p_cdf)
  })
}
