
# Risk table
riskHightoLow <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
risk <- data.frame(Risk_Category=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life'))
brksrisk <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
clrsrisk <- c("firebrick","#F0E442","#009E73","#0072B2")

# Each parameter risk table and colors
pHRiskTable <- list(
  Data = data.frame(Risk_Category=c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'),pH=c("< 6","6 - 9","> 9")),
  ColNames = c('Risk Category','pH (unitless)'),
  StyleEqual1 = c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'),
  StyleEqual2 = c('#F0E442','#009E73'),
  brks = c(0,6,9),
  clrs = c("gray","#F0E442","#009E73","#F0E442")
)

DORiskTable <- list(
  Data = cbind(risk,DO=c('< 7','> 7, < 8','> 8, < 10','> 10')),
  ColNames = c('Risk Category','DO (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,7,8,10),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

TNRiskTable <- list(
  Data = cbind(risk,TN=c('> 2','> 1, < 2','> 0.5, < 1','< 0.5')),
  ColNames = c('Risk Category','Total Nitrogen (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.5,1,2),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TPRiskTable <- list(
  Data = cbind(risk,TP=c('> 0.1','> 0.05, < 0.1','> 0.02, < 0.05','< 0.02')),
  ColNames = c('Risk Category','Total Phosphorus (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.02,0.05,0.1),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TotHabRiskTable <- list(
  Data = cbind(risk,TotalHabitat=c('< 100','> 100, < 130','> 130, < 150','> 150')),
  ColNames = c('Risk Category','Total Habitat (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,100,130,150),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

LRBSRiskTable <- list(
  Data = cbind(rbind(risk,'Medium Probability of Stress to Aquatic Life'),LRBS=c('< -1.5','> -1.5, < -1.0','> -0.5, < -1.0','> -0.5, < 0.5','> 0.5')),
  ColNames = c('Risk Category','Relative Bed Stability (unitless)'),
  StyleEqual1 = c(riskHightoLow,'Medium Probability of Stress to Aquatic Life'),
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2","#F0E442"),
  brks = c(-3,-1.5,-1,-0.5,0.5),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2","#F0E442")
)

MetalsCCURiskTable <- list(
  Data = cbind(risk,MetalsCCU=c('> 2.0','> 1.5, < 2.0','> 0.75, < 1.5','< 0.75')),
  ColNames = c('Risk Category','Metals CCU (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.75,1.5,2.0),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

SpCondRiskTable <- list(
  Data = cbind(risk,SpCond=c('> 500','> 350, < 500','> 250, < 350','< 250')),
  ColNames = c('Risk Category','Specific Conductivity (uS/cm)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,250,350,500),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TDSRiskTable <- list(
  Data = cbind(risk,TDS=c('> 350','> 250, < 350','> 100, < 250','< 100')),
  ColNames = c('Risk Category','Total Dissolved Solids (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,100,250,350),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSulfateRiskTable <- list(
  Data = cbind(risk,DSulfate=c('> 75','> 25, < 75','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Sulfate (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,10,25,75),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DChlorideRiskTable <- list(
  Data = cbind(risk,DChloride=c('> 50','> 25, < 50','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Chloride (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,10,25,50),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DPotassiumRiskTable <- list(
  Data = cbind(risk,DPotassium=c('> 10','> 2, < 10','> 1, < 2','< 1')),
  ColNames = c('Risk Category','Dissolved Potassium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,1,2,10),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSodiumRiskTable <- list(
  Data = cbind(risk,DSodium=c('> 20','> 10, < 20','> 7, < 10','< 7')),
  ColNames = c('Risk Category','Dissolved Sodium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,7,10,20),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)


DOsettingsCDF <- list(
  annotate("rect", xmin=10, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2") ,
  annotate("rect",xmin=8, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73") ,
  annotate("rect",xmin=7, xmax=8, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442") ,
  annotate("rect",xmin=-Inf, xmax=7, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

pHsettingsCDF <- list(
  annotate("rect", xmin=6, xmax=9, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73"),
  annotate("rect",xmin=9, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-Inf, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442" ))

SpCondsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=250, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=250, xmax=350, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=350, xmax=500, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=500, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TDSsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=100, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=100, xmax=250, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=250, xmax=350, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=350, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DSulfatesettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=10, xmax=25, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=25, xmax=75, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=75, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DChloridesettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=10, xmax=25, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=25, xmax=50, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=50, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DPotassiumsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=1, xmax=2, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=2, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=10, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DSodiumsettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=7, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=7, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=10, xmax=20, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TNsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.5, xmax=1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=1, xmax=2, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=2, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TPsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=.02, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=.02, xmax=.05, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=.05, xmax=.1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=.1, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TotHabsettingsCDF <- list(
  annotate("rect", xmin=150, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=130, xmax=150, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=100, xmax=130, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-Inf, xmax=100, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

LRBSsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=-1.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick"),
  annotate("rect",xmin=-1.5, xmax=-1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-1, xmax=-0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73"),
  annotate("rect",xmin=-0.5, xmax=0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"))

MetalsCCUsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=0.75, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.75, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=1.5, xmax=2.0, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=2.0, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

VSCIsettingsCDF <- list(
  geom_vline(xintercept = 60, color="red", size=1))
