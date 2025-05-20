## NMA - First Run (WOMAC)
library(readxl)
library(esc)
library(netmeta)

dat.womac <- ...

dat_ef.wom <- esc_mean_sd(
  grp1m = dat.womac$Mean1, 
  grp2m = dat.womac$Mean2, 
  grp1sd = dat.womac$SD1, 
  grp2sd = dat.womac$SD2, 
  grp1n = dat.womac$n1, 
  grp2n = dat.womac$n2
)

dat.womac$ef <- dat_ef.wom$es
dat.womac$ef_se <- dat_ef.wom$se

## Analysis 

m.netmeta.wom <- netmeta(
  reference.group = "BMAC",
  TE = ef, # Difference
  seTE = ef_se,
  treat1 = Tr1,
  treat2 = Tr2,
  studlab = Study,
  data = dat.womac,
  sm = "SMD",
  fixed = T,
  random = F,
  details.chkmultiarm = TRUE,
  tol.multiarm = 1
  )

m.wom.sum <- summary(m.netmeta.wom)

m.wom.sum$common

decomp.design(m.netmeta.wom)

plot(direct.evidence.plot(m.netmeta.wom)$plot)

netrank(m.netmeta.wom, small.values = "good")

library(kableExtra)

netsplit_wom <- netsplit(m.netmeta.wom)

netsplit_wom$common
kable(as.data.frame(netsplit(m.netmeta.wom))) %>% kable_classic(html_font = "cambria")

netsplit(m.netmeta.wom) %>% forest(  label.left = "Favors Trt.1",
                                     label.right = "Favors Trt.2")

netgraph(m.netmeta.wom)

forest(
  m.netmeta.wom,
  drop.reference.group = TRUE,
  smlab = paste("WOMAC: HA & PRP vs. BMAC \n",
                "(Common Effects Model)"),
  label.left = "Favors HA/PRP",
  label.right = "Favors BMAC"
  )


## Four + figures 
## Forest plots of pairwise differences 

## Pairwise Meta-Analysis 
library(metafor)

ma1.wom <- rma(ef, ef_se, data = dat.womac)

summary(ma1.wom)

forest(
  x = ma1.wom,
  slab = paste0(dat.womac$Study, " (", dat.womac$Tr1, " vs ", dat.womac$Tr2, ")"),
  addfit = FALSE,
  header = paste("Comparison of HA, PRP, BMAC \n",
                 "- WOMAC"),
  xlab = "Effect Size (Mean Difference)"
  )

funnel(ma1.wom,
       xlab = "Effect Size (Mean Difference)")
