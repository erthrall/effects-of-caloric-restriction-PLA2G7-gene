# forest plot for linear regression
# forest plot will plot beta estimate, 95% CI 
require(ggplot2)
require(readxl)
require(dplyr)
data<-read_xlsx('forestplot_linregcont.xlsx',sheet=2)



# format data 
data<-data %>% 
  mutate(significant=ifelse(`P-value`<0.05,TRUE, FALSE)) %>% 
  mutate(fill_var = ifelse(significant, Domain, NA)) %>% 
  mutate(Domain=as.factor(Domain)) %>% 
  arrange(Domain)

# plot 
ggplot(data, aes(x=Estimate, y=Trait, color=Domain, fill=fill_var)) + 
  geom_point(shape=21)+
  geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI), height=0.1)+
  facet_wrap(~Domain, scales="free")+
  scale_color_manual(
    values=c(
      "Blood chemistry"="lightblue",
      "Immune"="pink",
      "Impedence"="brown",
      "Measurements"="darkgreen",
      "NMR"="purple",
      "Telomeres"="darkgrey")
  )+
  scale_fill_manual(values=c(
    "Blood chemistry"="lightblue",
    "Immune"="pink",
    "NMR"="purple"),
  na.value = "white",   # fill non-significant polygons as white
  na.translate = FALSE  # do not create legend key for NA
) +
  geom_vline(xintercept=0, linetype="dashed", colour="darkgray")+
  guides(fill="none")+
  theme_minimal()


ggsave("linreg_continuous.jpeg")

## binary 
# plot ORs

data<-read_xlsx('forestplot_linregcont.xlsx',sheet=2)

data<-data %>% 
  mutate(significant=ifelse(`P-value`<0.05,TRUE, FALSE)) 

# plot 
ggplot(data, aes(x=OR, y=trait)) + 
  geom_point(shape=21)+
  geom_errorbarh(aes(xmin=OR_lowerCI,xmax=OR_upperCI), height=0.1)+
  geom_vline(xintercept=1, linetype="dashed", colour="lightgray")+
  theme_minimal() +
  ylab("Disease Diagnosis") 
ggsave("linreg_binary.jpeg")


rvtest<-read.table("pheno.tsv", header=TRUE)

