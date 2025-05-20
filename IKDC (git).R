## NMA - First Run (VAS)
library(readxl)
library(esc)
library(netmeta)

dat.IKDC <- ...

dat_ef.IKDC <- esc_mean_sd(
  grp1m = dat.IKDC$Mean1, 
  grp2m = dat.IKDC$Mean2, 
  grp1sd = dat.IKDC$SD1, 
  grp2sd = dat.IKDC$SD2, 
  grp1n = dat.IKDC$n1, 
  grp2n = dat.IKDC$n2
)

dat.IKDC$ef <- dat_ef.IKDC$es
dat.IKDC$ef_se <- dat_ef.IKDC$se

## Analysis 

m.netmeta.IKDC <- netmeta(
  reference.group = "BMAC",
  TE = ef, # Difference
  seTE = ef_se,
  treat1 = Tr1,
  treat2 = Tr2,
  studlab = Study,
  data = dat.IKDC,
  sm = "SMD",
  fixed = TRUE,
  random = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = 1
)

summary(m.netmeta.IKDC)

decomp.design(m.netmeta.IKDC)

netsplit(m.netmeta.IKDC) %>% forest(., show = "all",   label.left = "Favors Trt.2",
                                    label.right = "Favors Trt.1")

plot(direct.evidence.plot(m.netmeta.IKDC)$plot)

netrank(m.netmeta.IKDC, small.values = "good")

netgraph(m.netmeta.IKDC)

forest(
  m.netmeta.IKDC,
  drop.reference.group = TRUE,
  smlab = paste("IKDC: HA & PRP vs. BMAC \n",
                "(Common Effects Model)"),
  label.left = "Favors HA/PRP",
  label.right = "Favors BMAC"
)

## Four + figures 
## Forest plots of pairwise differences 

## Pairwise Meta-Analysis 
library(metafor)

ma1.IKDC <- rma(ef, ef_se, data = dat.IKDC)

summary(ma1.IKDC)

forest(
  x = ma1.IKDC,
  slab = paste0(dat.IKDC$Study, " (", dat.IKDC$Tr1, " vs ", dat.IKDC$Tr2, ")"),
  addfit = FALSE,
  header = paste("Comparison of HA, PRP, BMAC \n",
                "- IKDC"),
  xlab = "Effect Size (Mean Difference)"
)

funnel(
  x = ma1.IKDC,
  xlab = "Effect Size (Mean Difference)"
  )
