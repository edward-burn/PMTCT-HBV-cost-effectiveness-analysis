#### CLEAR WORKSPACE -----
rm(list=ls())

#### PACKAGES -----
# n.b these may need to be installed first
library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(dplyr)
library(triangle)


#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),

# title ------ 
 # shown across tabs
titlePanel("Prevention of mother to child transmission (PMTCT) of Hepatitis B virus (HBV) in South Africa: 
             a cost-effectiveness analysis"),

# Pages along the side -----  
navlistPanel(

# Introduction -----  
tabPanel("Introduction", 
         	 tabsetPanel(type = "tabs",
# backgound tab ------  	         
tabPanel("Background",
	  mainPanel(
	    tags$h2("heading"),
	      tags$h3("intro, link to paper, etc"),
	    tags$h4("more text")
	                 )
	                 ), 
# strategies tab ------  	
tabPanel("Strategies",
	  mainPanel(
	       tags$h3("S1: Current practice"),
	       tags$h4("No pregnant woman is screened for HBsAg and 
	                therefore no HBV treatment is given perinatallyed."),
	       tags$h3("S2: Antiviral prophylaxis for all women who 
	                test HBsAg positive"),
	       tags$h4("Pregnant women are screened for HBsAg; 
	                those who test positive are treated with TDF 
	                from 28 weeks' gestation to 4 weeks post-partumdelivery."), 
	       tags$h3("S3: Antiviral prophylaxis of women 
	                who are HBsAg positive and HBeAg positive"),
         tags$h4("Pregnant women are screened for HBsAg, those who test positive are screened for HBeAg. Only those who are HBeAg positive are treated with TDF from 28 weeks' 
	                gestation to 4 weeks post-partumdelivery."
	                   )
	                 )),
# model structure tab ------  	
tabPanel("Model structure",
	 mainPanel(
		   #tags$img(src="decision_tree.png")
		 htmlOutput("picture")   
	         )
	                 ))),     

# inputs ------ 
tabPanel("Model inputs",	     
  tabsetPanel(type = "tabs",

# Study cohort ------            
tabPanel("Study cohort",	     
	  mainPanel(
	      tags$h4("Number of mothers",
		    numericInput('cohort.n', 
		                 '',
		                 value=10000, step=50,
                     min = 0, max = 10000))
	          )
),

# probabilities tab ------ 
tabPanel("Probabilities",
		      tabsetPanel(type = "tabs",
# p1 -----
tabPanel("P1",   
	 tags$h4("Probability of 
            mother being HBsAg+"), 
	 numericInput('p1', 
		            'Mean', 
		             value=0.036,step=0.1,
                 min = 0, max = 1),
	 numericInput('p1.prob.low', 
		            'Lower limit', 
		             value=0.031,step=0.1,
                 min = 0, max = 1),
	 numericInput('p1.prob.high', 
		            'Upper limit', 
		             value=0.074,step=0.1,
                 min = 0, max = 1)
),

# p2 -----		          
tabPanel("P2",   
	 tags$h4("Mother who is HBsAg+ being HBeAg+"), 
	 numericInput('p2', 
		            'Mean', 
		            value=0.23,step=0.1,
                 min = 0, max = 1),
	 numericInput('p2.prob.low', 
		            'Lower limit', 
		             value=0.167,step=0.1,
                 min = 0, max = 1),
	numericInput('p2.prob.high', 
		           'Upper limit', 
		            value=0.429,step=0.1,
                 min = 0, max = 1)
		         ),
		         
# p3 -----		         
tabPanel("P3",
	 tags$h4("Mother who is HBsAg+ and HBeAg+ having child who is HBsAg+"), 
	 tags$h5("Without treatment"),
	 numericInput('s1.p3', 
		            'Mean', 
		             value=0.383,step=0.1,
                 min = 0, max = 1),
	 numericInput('s1.p3.prob.low', 
		            'Lower limit', 
		             value=0.07,step=0.1,
                 min = 0, max = 1),
	 numericInput('s1.p3.prob.high', 
		            'Upper limit', 
		             value=0.744,step=0.1,
                 min = 0, max = 1),
	tags$hr(tags$h5("With treatment"),
	numericInput('s2.p3', 
		           'Mean', 
		            value=0.029,step=0.01,
                 min = 0, max = 1),
	numericInput('s2.p3.prob.low', 
		           'Lower limit', 
		            value=0,step=0.01,
                 min = 0, max = 1),
	numericInput('s2.p3.prob.high', 
		           'Upper limit', 
		            value=0.052,step=0.01,
                 min = 0, max = 1))
		          ),
# p4 ----
tabPanel("P4",
	tags$h4("Mother who is HBsAg+ and HBeAg- 
	         having child who is HBsAg+"), 
	tags$h5("Without treatment"),
  numericInput('s1.p4', 
		           'Mean', 
		           value=0.048, step=0.1,
               min = 0, max = 1),
	numericInput('s1.p4.prob.low', 
		           'Lower limit', 
		           value=0,step=0.1,
               min = 0, max = 1),
	numericInput('s1.p4.prob.high', 
		           'Upper limit', 
		            value=0.142,step=0.1,
                min = 0, max = 1),
  tags$hr(tags$h5("With treatment"),
  tags$h5("*Assumed to be 
         		equal to P3 with treatment")
         ))
		     )),
		
		
# costs tab ------ 
tabPanel("Costs",
	numericInput('test.cost.hbsag', 
		           'Test cost: HBsAg', 
		            value=9.1,
		           step=0.1,
                 min = 0, max = 1),
	numericInput('test.cost.hbeag', 
		           'Test cost: HBeAg', 
		            value=9.1,step=0.1,
                 min = 0, max = 1),
	numericInput('treatment.cost', 
		                       'Treatment cost', 
		                       value=9.92,
		                       step=0.1,
                 min = 0, max = 1))

)),


# output title tab ------ 
tabPanel("Model outputs",
		tabsetPanel(type = "tabs",
# summary table------ 
tabPanel("Summary",
   sidebarLayout(
   sidebarPanel("",
     numericInput('exp.wtp.summary', 
                  'Expected willingness to pay  
                   per infection avoided',
                   5000),
     downloadButton("download.summary.table", 
                    "Download table")
             ),
mainPanel("",
   tableOutput("summary.table"),
   tags$hr(tags$h4(
   textOutput("txtout.summary.table")))
         ))
                           ),
# CE Plane------ 
tabPanel("Cost-effectiveness plane",
   sidebarLayout(
   sidebarPanel("",
   numericInput('exp.wtp.ce_plane', 
                'Expected willingness to pay  
                 per infection avoided',
                 5000),
   checkboxInput("show.wtp.ce_plane", 
                 "Add expected willingness to pay
                  per infection avoided to plot", 
                  FALSE)
             ),
mainPanel("",
          plotOutput("ce.plane")
          ))),
# CEAC------ 
tabPanel("Cost-effectiveness 
          acceptability curve",
   sidebarLayout(
   sidebarPanel("",
      numericInput('exp.wtp.ceac', 
                   'Expected willingness to pay 
                    per infection avoided',
                    5000),
   checkboxInput("show.wtp.ceac", 
                 "Add expected willingness to 
                  pay per infection avoided
                  to plot", 
                  FALSE),
   numericInput('max.wtp.ceac', 
                'Maximum willingness to pay per 
                infection avoided',
                10000)
               ),
  mainPanel("",
            plotOutput("ceac")
                    )))
    	)),
# notes ----
tabPanel("Other",
     tabsetPanel(type = "tabs",    
# citation -----
tabPanel("Citation",
  mainPanel(  
    tags$h3("To cite estimates from this calculator"),
    tags$h3("...."),
    tags$h3("To cite the paper upon which 
             the tool is based"),
    tags$h3("...."))),
# Acknowledgements -----
tabPanel("Acknowledgements",
        tags$h3("....")
         ),
# Disclaimer -----
tabPanel("Disclaimer",
         tags$h3("....")
         ),
# Contact -----
tabPanel("Contact",
   tags$h3("For any questions, 
            bug reports or 
            suggestions relating to this 
            tool please email 
            edward.burn@ndorms.ox.ac.uk")) 
         )
         )
))


#### SERVER ------
server <-	function(input, output, session) {

#src ="https://github.com/edward-burn/econ-eval-hep-b/blob/master/images/decision_tree.png"
src ="https://github.com/edward-burn/PMTCT-HBV-cost-effectiveness-analysis/blob/master/model%20outline.png"
#src = "https://theWeb/aPictureSomewhere.jpg"
output$picture<-renderText({c('<img src="',src,'">')})
  
  
# get probabilities   -----
probs<-
reactive({
  
  n.sims<-1000
  p1.prob <- rtriangle(n.sims, 
            input$p1.prob.low,
            input$p1.prob.high, 
            input$p1) 
  p2.prob <- rtriangle(n.sims, 
            input$p2.prob.low,
            input$p2.prob.high, 
            input$p2) 
  
  s1.p3.prob <- rtriangle(n.sims, 
            input$s1.p3.prob.low,
            input$s1.p3.prob.high, 
            input$s1.p3) 
  s1.p4.prob <- rtriangle(n.sims, 
            input$s1.p4.prob.low,
            input$s1.p4.prob.high, 
            input$s1.p4) 
  s2.p3.prob <- rtriangle(n.sims, 
                          input$s2.p3.prob.low,
                          input$s2.p3.prob.high,
                          input$s2.p3)
                          
  
  # list of probabilities
  # deterministic and probabilistic
  list(
       ## deterministic
       #common probs
       p1=input$p1,
       p2=input$p2,
       #s1 probs
       s1.p3=input$s1.p3,
       s1.p4=input$s1.p4,
       # s2 probs
       s2.p3=input$s2.p3,
       s2.p4=input$s2.p3,
       # s3 probs
       s3.p3= input$s2.p3,# same as prob s2.p3
       s3.p4= input$s1.p4,# same as prob s1.p4
       ## probabilistic
       #common probs
       p1.prob=p1.prob,
       p2.prob=p2.prob,
       #s1 probs
       s1.p3.prob=s1.p3.prob,
       s1.p4.prob=s1.p4.prob,
       # s2 probs
       s2.p3.prob=s2.p3.prob,
       s2.p4.prob=s2.p3.prob, # same as prob s2.p3
       # s3 probs
       s3.p3.prob= s2.p3.prob,# same as prob s2.p3
       s3.p4.prob= s1.p4.prob # same as prob s1.p4
       )
})

# get transitions -----
tranistions<-
reactive({
  a<-probs()
  
  list(
    #strategy 1
  #  deterministic
  s1.O1=input$cohort.n*
            a$p1*a$p2*a$s1.p3,
       s1.O2= input$cohort.n*
              a$p1*a$p2* (1-a$s1.p3),
       s1.O3= input$cohort.n*
            a$p1*(1-a$p2)*a$s1.p4,
       s1.O4= input$cohort.n*
            a$p1*(1-a$p2)* (1-a$s1.p4),
       s1.O5= input$cohort.n*
            (1-a$p1),
       s1.O1.prob=input$cohort.n*
            a$p1.prob*a$p2.prob*a$s1.p3.prob,
       s1.O2.prob= input$cohort.n*
              a$p1.prob*a$p2.prob* (1-a$s1.p3.prob),
       s1.O3.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)*a$s1.p4.prob,
       s1.O4.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)* (1-a$s1.p4.prob),
       s1.O5.prob= input$cohort.n*
            (1-a$p1.prob),
  
  #strategy 2
  #  deterministic
  s2.O1=input$cohort.n*
            a$p1*a$p2*a$s2.p3,
       s2.O2= input$cohort.n*
              a$p1*a$p2* (1-a$s2.p3),
       s2.O3= input$cohort.n*
            a$p1*(1-a$p2)*a$s2.p4,
       s2.O4= input$cohort.n*
            a$p1*(1-a$p2)* (1-a$s2.p4),
       s2.O5= input$cohort.n*
            (1-a$p1),
       s2.O1.prob=input$cohort.n*
            a$p1.prob*a$p2.prob*a$s2.p3.prob,
       s2.O2.prob= input$cohort.n*
              a$p1.prob*a$p2.prob* (1-a$s2.p3.prob),
       s2.O3.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)*a$s2.p4.prob,
       s2.O4.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)* (1-a$s2.p4.prob),
       s2.O5.prob= input$cohort.n*
            (1-a$p1.prob),

  #strategy 3
  #  deterministic
  s3.O1=input$cohort.n*
            a$p1*a$p2*a$s3.p3,
       s3.O2= input$cohort.n*
              a$p1*a$p2* (1-a$s3.p3),
       s3.O3= input$cohort.n*
            a$p1*(1-a$p2)*a$s3.p4,
       s3.O4= input$cohort.n*
            a$p1*(1-a$p2)* (1-a$s3.p4),
       s3.O5= input$cohort.n*
            (1-a$p1),
       s3.O1.prob=input$cohort.n*
            a$p1.prob*a$p2.prob*a$s3.p3.prob,
       s3.O2.prob= input$cohort.n*
              a$p1.prob*a$p2.prob* (1-a$s3.p3.prob),
       s3.O3.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)*a$s3.p4.prob,
       s3.O4.prob= input$cohort.n*
            a$p1*(1-a$p2.prob)* (1-a$s3.p4.prob),
       s3.O5.prob= input$cohort.n*
            (1-a$p1.prob))
})



# get costs ----- 
costs<-
reactive({
  
a<-tranistions()

test.cost.hbsag<-input$test.cost.hbsag
test.cost.hbeag<-input$test.cost.hbeag
treatment.cost<-input$treatment.cost



s2.test.cost<-input$cohort.n*test.cost.hbsag
# all mothers (all cohort) are tested


# det
s2.treatment.cost<-treatment.cost*
                     (a$s2.O1+
                        a$s2.O2+
                        a$s2.O3+a$s2.O4)
# those mothers who are HBsAg positive are treated

s2.total.cost<-s2.test.cost+
                    s2.treatment.cost

# prob
s2.treatment.cost.prob<-treatment.cost*
                     (a$s2.O1.prob+
                        a$s2.O2.prob+
                        a$s2.O3.prob+a$s2.O4.prob)
# those mothers who are HBsAg positive are treated

s2.total.cost.prob<-s2.test.cost+
                    s2.treatment.cost.prob

# strategy 3
# test cost
# all mothers (all cohort) are tested for HBsAg
 # mothers HBsAg positive are tested for HBeAg


s3.test.cost.HBsAg<-input$cohort.n*
            test.cost.hbsag

s3.test.cost.HBeAg<-test.cost.hbeag*
                   (a$s3.O1+a$s3.O2+a$s3.O3+a$s3.O4)

s3.test.cost.HBeAg.prob<-test.cost.hbeag*
                   (a$s3.O1.prob+a$s3.O2.prob+
                      a$s3.O3.prob+a$s3.O4.prob)

s3.test.cost<-s3.test.cost.HBsAg+
                  s3.test.cost.HBeAg
s3.test.cost.prob<-s3.test.cost.HBsAg+
                   s3.test.cost.HBeAg.prob
 
 

s3.treatment.cost<-treatment.cost*
                      (a$s2.O1+a$s2.O2)
s3.treatment.cost.prob<-treatment.cost*
                      (a$s2.O1.prob+a$s2.O2.prob)

s3.total.cost<-s3.test.cost+
                     s3.treatment.cost
s3.total.cost.prob<-s3.test.cost.prob+
                     s3.treatment.cost.prob


list(s2.total.cost=s2.total.cost,
     s3.total.cost=s3.total.cost,
     s2.total.cost.prob=s2.total.cost.prob,
     s3.total.cost.prob=s3.total.cost.prob)
  
 })
 

# summarise infections ----- 
summary.infections<-
reactive({
  a<-tranistions()

  s1.infections.mean<-a$s1.O1+a$s1.O3
  s1.infections.low<-quantile((a$s1.O1.prob+a$s1.O3.prob),
                               0.025)
  s1.infections.high<-quantile((a$s1.O1.prob+a$s1.O3.prob),
                               0.975)
  
  s2.infections.mean<-a$s2.O1+a$s2.O3
  s2.infections.low<-quantile((a$s2.O1.prob+a$s2.O3.prob),
                               0.025)
  s2.infections.high<-quantile((a$s2.O1.prob+a$s2.O3.prob),
                               0.975)
  s2.infections.avoided.mean<-(a$s1.O1+a$s1.O3)-(a$s2.O1+a$s2.O3)
  s2.infections.avoided.low<-quantile((a$s1.O1.prob+a$s1.O3.prob)-(a$s2.O1.prob+a$s2.O3.prob),
                               0.025)
  s2.infections.avoided.high<-quantile((a$s1.O1.prob+a$s1.O3.prob)-(a$s2.O1.prob+a$s2.O3.prob),
                               0.975)
  
  s3.infections.mean<-a$s3.O1+a$s3.O3
  s3.infections.low<-quantile((a$s3.O1.prob+a$s3.O3.prob),
                               0.025)
  s3.infections.high<-quantile((a$s3.O1.prob+a$s3.O3.prob),
                               0.975)
  s3.infections.avoided.mean<-(a$s1.O1+a$s1.O3)-(a$s3.O1+a$s3.O3)
  s3.infections.avoided.low<-quantile((a$s1.O1.prob+a$s1.O3.prob)-(a$s3.O1.prob+a$s3.O3.prob),
                                0.025)
  s3.infections.avoided.high<-quantile((a$s1.O1.prob+a$s1.O3.prob)-(a$s3.O1.prob+a$s3.O3.prob),
                                0.975)  
  
  
  list(s1.infections.mean=s1.infections.mean,
       s1.infections.low=s1.infections.low,
       s1.infections.high=s1.infections.high,
       s2.infections.mean=s2.infections.mean,
       s2.infections.low=s2.infections.low,
       s2.infections.high=s2.infections.high,
       s2.infections.avoided.mean=s2.infections.avoided.mean,
       s2.infections.avoided.low=s2.infections.avoided.low,
       s2.infections.avoided.high=s2.infections.avoided.high,
       s3.infections.mean=s3.infections.mean,
       s3.infections.low=s3.infections.low,
       s3.infections.high=s3.infections.high,
       s3.infections.avoided.mean=s3.infections.avoided.mean,
       s3.infections.avoided.low=s3.infections.avoided.low,
       s3.infections.avoided.high=s3.infections.avoided.high)
  
  
})



# summarise costs -----
summary.costs<-
reactive({
  
  a<-costs()
  
  s2.total.cost.mean<-a$s2.total.cost
   s2.total.cost.low<-quantile(a$s2.total.cost.prob, 0.025)
   s2.total.cost.high<-quantile(a$s2.total.cost.prob, 0.975) 

  s3.total.cost.mean<-a$s3.total.cost
   s3.total.cost.low<-quantile(a$s3.total.cost.prob, 0.025)
   s3.total.cost.high<-quantile(a$s3.total.cost.prob, 0.975) 

   
   list(s2.total.cost.mean=s2.total.cost.mean,
        s2.total.cost.low=s2.total.cost.low,
        s2.total.cost.high=s2.total.cost.high,
        s3.total.cost.mean=s3.total.cost.mean,
        s3.total.cost.low=s3.total.cost.low,
        s3.total.cost.high=s3.total.cost.high)
  
})




# get summary table -----  
for.summary.table<- reactive({

    
    a<-summary.costs()
     b<-summary.infections()

  # table with decision rules 
# table of infections
infections<-  data.frame(s1.infections= b$s1.infections.mean,
         s2.infections= b$s2.infections.mean,
         s3.infections= b$s3.infections.mean) 
infections<-data.frame(infections=t(infections))
infections<-cbind(strategy=c("S1", "S2", "S3"),
                 infections)
row.names(infections)<-c(1:3)
infections$strategy<-as.character(infections$strategy)

infections.conf<-
  data.frame(s1.infections.conf= paste0(sprintf("%.2f",b$s1.infections),
                              " (",
                              sprintf("%.2f",b$s1.infections.low),
                              " to ",
                              sprintf("%.2f",b$s1.infections.high),
                              ")"),
         s2.infections.conf= paste0(sprintf("%.2f",b$s2.infections),
                              " (",
                              sprintf("%.2f",b$s2.infections.low),
                              " to ",
                              sprintf("%.2f",b$s2.infections.high),
                              ")"),
         s3.infections.conf= paste0(sprintf("%.2f",b$s3.infections),
                              " (",
                              sprintf("%.2f",b$s3.infections.low),
                              " to ",
                              sprintf("%.2f",b$s3.infections.high),
                              ")")) 
infections.conf<-data.frame(infections.conf=t(infections.conf))
infections.conf<-cbind(strategy=c("S1", "S2", "S3"),
                  infections.conf)
row.names(infections.conf)<-c(1:3)
infections.conf$strategy<-as.character(infections.conf$strategy)







cost<-
       data.frame(s1.cost= 0,
         s2.cost= a$s2.total.cost.mean,
         s3.cost= a$s3.total.cost.mean) %>%
  select(s1.cost,s2.cost,s3.cost)
cost<-data.frame(cost=t(cost))
cost<-cbind(strategy=c("S1", "S2", "S3"),
                  cost)
row.names(cost)<-c(1:3)
cost$strategy<-as.character(cost$strategy)




cost.conf<-
  data.frame(s1.cost.conf= 0,
         s2.cost.conf= paste0(sprintf("%.2f",a$s2.total.cost),
                              " (",
                              sprintf("%.2f",a$s2.total.cost.low),
                              " to ",
                              sprintf("%.2f",a$s2.total.cost.high),
                              ")"),
         s3.cost.conf= paste0(sprintf("%.2f",a$s3.total.cost),
                              " (",
                              sprintf("%.2f",a$s3.total.cost.low),
                              " to ",
                              sprintf("%.2f",a$s3.total.cost.high),
                              ")")) 
cost.conf<-data.frame(cost.conf=t(cost.conf))
cost.conf<-cbind(strategy=c("S1", "S2", "S3"),
                  cost.conf)
row.names(cost.conf)<-c(1:3)
cost.conf$strategy<-as.character(cost.conf$strategy)



 # combine
summary.table<-infections %>%
  full_join(infections.conf,
            by="strategy")
summary.table<-summary.table %>%
  full_join(cost,
            by="strategy")
summary.table<-summary.table %>%
  full_join(cost.conf,
            by="strategy")


# arrange by infections (high to low)
summary.table<-summary.table %>%
  arrange(desc(infections))


 # incremental effects and ICER
 # calculation depends on whether
 # 2nd is dominated by 3
 # eg whether 3 costs more or less than 2
 summary.table$ICER<-NA

#dominated?
if(summary.table$cost[2]>
    summary.table$cost[3]) {
  summary.table$ICER[2]<-paste0("Dominated (by strategy ",
                             summary.table$strategy[3],
                             ")")

  summary.table$ICER[3]<-sprintf("%.f",
                        summary.table$cost[3]/
        (summary.table$infections[1]-
           summary.table$infections[3]))

summary.table$ICER[3]<-paste0(summary.table$ICER[3],
                             " (compared to strategy ",
                             summary.table$strategy[1],
                             ")")

  }

#if 2 is extendedly dominated
if(summary.table$cost[2]<=
    summary.table$cost[3]){ 
 if(
  (summary.table$cost[2]/
        (summary.table$infections[1]-
           summary.table$infections[2])) >
   (summary.table$cost[3]/
        (summary.table$infections[1]-
           summary.table$infections[3]))
  ) {

summary.table$ICER[2]<-paste0("Extendedly dominated (by strategy ",
                             summary.table$strategy[3],
                             ")")
  summary.table$ICER[3]<-sprintf("%.2f",
                        summary.table$cost[3]/
        (summary.table$infections[1]-
           summary.table$infections[3]))

summary.table$ICER[3]<-paste0(summary.table$ICER[3],
                             " (compared to strategy ",
                             summary.table$strategy[1],
                             ")")
        }}





# extendedly dominated?
if(summary.table$cost[2]<=
    summary.table$cost[3]){ 
if(
  (summary.table$cost[2]/
        (summary.table$infections[1]-
           summary.table$infections[2])) <=
   (summary.table$cost[3]/
        (summary.table$infections[1]-
           summary.table$infections[3]))
  ) {

summary.table$ICER[2]<-sprintf("%.2f",
                        summary.table$cost[2]/
        (summary.table$infections[1]-
           summary.table$infections[2]))
  summary.table$ICER[3]<-sprintf("%.2f",
                        summary.table$cost[3]/
        (summary.table$infections[2]-
           summary.table$infections[3]))

summary.table$ICER[2]<-paste0(summary.table$ICER[2],
                             " (compared to strategy ",
                             summary.table$strategy[1],
                             ")")
summary.table$ICER[3]<-paste0(summary.table$ICER[3],
                             " (compared to strategy ",
                             summary.table$strategy[2],
                             ")")
        }}



summary.table<-summary.table %>%
  mutate(Strategy=strategy,
         Infections=paste(sprintf("%.2f",infections),
                          infections.conf),
         Cost=paste(sprintf("%.2f",cost),
                    cost.conf)) %>%
  select(Strategy,
         Infections,
         Cost,
         ICER) 
summary.table$ICER[1]<-"-"
summary.table$Cost[1]<-0
summary.table
  
# add probability cost-effective
#summary.table$'Probability cost-effective'<-input$exp.wtp.summary
#summary.table
#exp.wtp.summary


a<-tranistions()
b<-costs()

using.wtp<-input$exp.wtp.summary


nmb.s1.s2.s3<-data.frame(
  s1=(using.wtp*(-(a$s1.O1.prob+a$s1.O3.prob)))-
                  0,
  s2=(using.wtp*(-(a$s2.O1.prob+a$s2.O3.prob)))-
                  b$s2.total.cost.prob,
  s3=(using.wtp*(-(a$s3.O1.prob+a$s3.O3.prob)))-
                 b$s3.total.cost.prob)

nmb.s1.s2.s3$cost.effective<-
  colnames(nmb.s1.s2.s3)[
    apply(nmb.s1.s2.s3,1,which.max)]

working.probability.cost.effective<-
   data.frame(prop.table(table(nmb.s1.s2.s3$cost.effective)))  %>% 
   tidyr::spread(Var1, Freq) %>% 
   mutate(wtp=using.wtp)


# add value of zero if never cost-effective
if (is.null(working.probability.cost.effective$s1) == TRUE) {
  working.probability.cost.effective$s1<-0 }
if (is.null(working.probability.cost.effective$s2) == TRUE) {
  working.probability.cost.effective$s2<-0 }
if (is.null(working.probability.cost.effective$s3) == TRUE) {
  working.probability.cost.effective$s3<-0 }



p.ce<-rbind(
data.frame(Strategy="S1",
          p.ce=paste0(sprintf("%.2f",working.probability.cost.effective$s1*100),
          "%")
          ),
data.frame(Strategy="S2",
          p.ce=paste0(sprintf("%.2f",working.probability.cost.effective$s2*100),
                      "%")
          ),
data.frame(Strategy="S3",
          p.ce=paste0(sprintf("%.2f",working.probability.cost.effective$s3*100),
              "%")
          
          ))


summary.table<-summary.table %>% 
  full_join(p.ce, by="Strategy") #%>% 
  #rename('Probability cost-effective'=p.ce)

names(summary.table)[names(summary.table) == 
        "p.ce"] <- paste0("Probability cost-effective",
                         " (WTP: ",input$exp.wtp.summary,
                         ")")
summary.table
  })
  

# produce summary table -----
output$summary.table<- renderTable({
a<-for.summary.table() 
a
})




# download summary table ------
output$download.summary.table <- downloadHandler(
    filename = function(){"summary_table.csv"},
    content = function(fname){
      write.csv(for.summary.table() , 
                fname, 
                row.names = F)
     # write.csv(data.frame(a=c(1:10), b=c(1:10)), fname)
    })
# produce text for below summary table -----
output$txtout.summary.table <- renderText({  
  
  paste0("The incremental cost effectiveness ratio
                 (ICER) gives the incremental cost per infection
                 avoided. Decision rules were applied. A 
                 strategy is 'dominated' if another strategy 
                 is expected to lead to fewer infections 
                 for a lower cost. A strategy is 'extendedly 
                 dominated' if there is another strategy which 
                 leads to fewer infections and has a lower 
                 ICER. Probability cost-effective is based 
         on an expected willingness to pay (WTP) per 
         infection avoided of ",
         input$exp.wtp.summary, ".")
  
  
})

# download summary table---
  

# CE plane ------

 output$ce.plane<- renderPlot({

     a<-tranistions()
     b<-costs()

  s2.infections.avoided<-
 (a$s1.O1+a$s1.O3)-(a$s2.O1+a$s2.O3)
 s2.infections.avoided.prob<-
 (a$s1.O1.prob+a$s1.O3.prob)-(a$s2.O1.prob+a$s2.O3.prob)
  s3.infections.avoided<-
 (a$s1.O1+a$s1.O3)-(a$s3.O1+a$s3.O3)
 s3.infections.avoided.prob<-
 (a$s1.O1.prob+a$s1.O3.prob)-(a$s3.O1.prob+a$s3.O3.prob)

 
s2.total.cost<-b$s2.total.cost
s2.total.cost.prob<-b$s2.total.cost.prob

s3.total.cost<-b$s3.total.cost
s3.total.cost.prob<-b$s3.total.cost.prob

 s2<-
   data.frame(
   infections.avoided=s2.infections.avoided,
   cost=s2.total.cost,
   strategy="S2",
   type="deterministic")
 s2.prob<-
   data.frame(
   infections.avoided=s2.infections.avoided.prob,
   cost=s2.total.cost.prob,
   strategy="S2",
   type="probabilistic")
 
 s3<-
   data.frame(
   infections.avoided=s3.infections.avoided,
   cost=s3.total.cost,
   strategy="S3",
   type="deterministic")
 s3.prob<-
   data.frame(
   infections.avoided=s3.infections.avoided.prob,
   cost=s3.total.cost.prob,
   strategy="S3",
   type="probabilistic")
 

a<- ggplot()+
    geom_point(data=rbind(s2.prob,
       s3.prob),
        aes(infections.avoided,
                   cost,
                   colour=strategy), 
               alpha=0.7, shape=24)+
    geom_point(data=rbind(s2,
       s3),
                aes(x=infections.avoided, 
                   y=cost, 
                   fill=strategy),
                   colour="black",  
               stroke=1.2, size=4, shape=24)+
  scale_x_continuous(limits=c(0,NA))+
 #scale_y_continuous(limits=c(0,NA))+
 theme_minimal(base_size = 18)+ 
    theme(legend.title=element_blank())+
          scale_colour_manual(values=c(#"#FF0000",
            "#00A08A", "#F2AD00"))+
           scale_fill_manual(values=c(#"#FF0000",
             "#00A08A", "#F2AD00"))+
 xlab("Infections avoided") +
  ylab("Incremental cost")
 
 
 
 if(input$show.wtp.ce_plane==TRUE) {
a<- a  + 
   geom_abline(intercept = 0,
               slope = input$exp.wtp.ce_plane, 
               linetype="dashed")
}

 a
 
 })

# produce CEAC -----
 
  output$ceac<- renderPlot({
 
 
#NMB = (wtp x E) - C
# n.b. we effect is number of infections avoided
# strategy 1 has a nmb of zero 
      # (cost is zero and infections avoided is zero)
# n.b. when calculating nmb we use -E as infections are a negtive consequence


wtp<- seq(0,input$max.wtp.ceac, 
          by=input$max.wtp.ceac/1000)
 # will need to do this for many values of wtp 

a<-tranistions()
b<-costs()
  

probability.cost.effective<-NULL
for(i in 1:length(wtp)) {
using.wtp<-wtp[i]

nmb.s1.s2.s3<-data.frame(
  s1=(using.wtp*(-(a$s1.O1.prob+a$s1.O3.prob)))-
                  0,
  s2=(using.wtp*(-(a$s2.O1.prob+a$s2.O3.prob)))-
                  b$s2.total.cost.prob,
  s3=(using.wtp*(-(a$s3.O1.prob+a$s3.O3.prob)))-
                 b$s3.total.cost.prob)

nmb.s1.s2.s3$cost.effective<-
  colnames(nmb.s1.s2.s3)[
    apply(nmb.s1.s2.s3,1,which.max)]

working.probability.cost.effective<-
   data.frame(prop.table(table(nmb.s1.s2.s3$cost.effective)))  %>% 
   tidyr::spread(Var1, Freq) %>% 
   mutate(wtp=using.wtp)


# add value of zero if never cost-effective
if (is.null(working.probability.cost.effective$s1) == TRUE) {
  working.probability.cost.effective$s1<-0 }
if (is.null(working.probability.cost.effective$s2) == TRUE) {
  working.probability.cost.effective$s2<-0 }
if (is.null(working.probability.cost.effective$s3) == TRUE) {
  working.probability.cost.effective$s3<-0 }


working.probability.cost.effective<-working.probability.cost.effective %>% 
  select(s1, s2, s3, wtp)

probability.cost.effective<-rbind(probability.cost.effective,
  working.probability.cost.effective)
 }


# plot ceac
#reshape long
a<-probability.cost.effective %>% 
  tidyr::gather(strategy, probability.ce, -wtp)

a$strategy<-ifelse(a$strategy=="s1", 
                   "S1", a$strategy)
a$strategy<-ifelse(a$strategy=="s2", 
                   "S2", a$strategy)
a$strategy<-ifelse(a$strategy=="s3", 
                   "S3", a$strategy)

a<-a %>% 
  ggplot() + 
  geom_line(aes(x=wtp, y=probability.ce, 
                group=strategy, 
                colour=strategy))+
 theme_minimal(base_size = 18)+
    theme(legend.title=element_blank())+
  ylab("Probability cost-effective")+
  xlab("Willingness to pay per infection avoided")+
  scale_y_continuous(labels=percent)+
        scale_colour_manual(values=c("#FF0000", "#00A08A", "#F2AD00"))

if(input$show.wtp.ceac==TRUE) {
a<- a  + geom_vline(xintercept = input$exp.wtp.ceac,
             linetype="dashed")}


a 
})
 



}


#### RUN APP ----
shinyApp(ui = ui, server = server)


