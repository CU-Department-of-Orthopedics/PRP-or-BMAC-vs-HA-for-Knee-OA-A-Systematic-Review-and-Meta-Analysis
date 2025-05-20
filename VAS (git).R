## NMA - First Run (VAS)
library(readxl)
library(esc)
library(netmeta)

dat.vas <- ...

dat_ef.vas <- esc_mean_sd(
  grp1m = dat.vas$Mean1, 
  grp2m = dat.vas$Mean2, 
  grp1sd = dat.vas$SD1, 
  grp2sd = dat.vas$SD2, 
  grp1n = dat.vas$n1, 
  grp2n = dat.vas$n2
)

dat.vas$ef <- dat_ef.vas$es
dat.vas$ef_se <- dat_ef.vas$se

## Analysis 

m.netmeta.vas <- netmeta(
  reference.group = "BMAC",
  TE = ef, # Difference
  seTE = ef_se,
  treat1 = Tr1,
  treat2 = Tr2,
  studlab = Study,
  data = dat.vas,
  sm = "SMD",
  fixed = TRUE,
  random = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = 1
)

summary(m.netmeta.vas)

decomp.design(m.netmeta.vas)

netsplit(m.netmeta.vas) %>% forest(., show = "all",   label.left = "Favors Trt.1",
                                   label.right = "Favors Trt.2")

plot(direct.evidence.plot(m.netmeta.vas)$plot)

netrank(m.netmeta.vas, small.values = "good")

netgraph(m.netmeta.vas)

forest(
  m.netmeta.vas,
  drop.reference.group = TRUE,
  smlab = paste("VAS: HA & PRP vs. BMAC \n",
                "(Common Effects Model)"),
  label.left = "Favors HA/PRP",
  label.right = "Favors BMAC"
)


## Four + figures 
## Forest plots of pairwise differences 

## Pairwise Meta-Analysis 
library(metafor)

ma1.vas <- rma(ef, ef_se, data = dat.vas)

summary(ma1.vas)

forest(
  x = ma1.vas,
  slab = paste0(dat.vas$Study, " (", dat.vas$Tr1, " vs ", dat.vas$Tr2, ")"),
  addfit = FALSE,
  header = paste("Comparison of HA, PRP, BMAC \n",
                 "- VAS"),
  xlab = "Effect Size (Mean Difference)"
)

funnel(ma1.vas,
       xlab = "Effect Size (Mean Difference)")
