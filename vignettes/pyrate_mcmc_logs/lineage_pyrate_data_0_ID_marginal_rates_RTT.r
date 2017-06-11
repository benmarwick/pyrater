# 1 files combined:
# 	C:\Users\bmarwick\Downloads\PyRate_Cultural_Evolution-master\pyrater\vignettes\pyrate_mcmc_logs/lineage_pyrate_data_0_ID_marginal_rates.log

# 95% HPDs calculated using code from Biopy (https://www.cs.auckland.ac.nz/~yhel002/biopy/)

file_path=paste0(getwd(), '/', quote(lineage_pyrate_data_0_ID_marginal_rates))


pdf(file=paste0(file_path, '_RTT.pdf'),width=0.6*9,height=16.8)
par(mfrow=c(4,1))
library(scales)
L_hpd_m95=c(0.226709056041, 0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041,0.226709056041)
L_hpd_M95=c(29.7174158458, 29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,29.7174158458,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,35.5240053481,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,42.8993218218,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138,36.6773311138)
M_hpd_m95=c(0.109604259604, 0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109604259604,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.109731389621,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582,0.122530156582)
M_hpd_M95=c(0.253021844809, 0.253021844809,0.253021844809,0.253021844809,0.253021844809,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.219988899918,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.689087288665,0.722961655113,0.722961655113,0.722961655113,0.722961655113,0.722961655113,0.742584253561,0.742584253561,0.742584253561,0.742584253561,0.742584253561,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093,0.832360858093)
R_hpd_m95=c(0.104178899459, 0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459,0.104178899459)
R_hpd_M95=c(29.4974269459, 29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,29.4974269459,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,35.3633916367,36.5677268542,36.5677268542,36.5677268542,36.5677268542,36.5677268542,36.5677268542,36.5677268542,36.5677268542,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.7504972218,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,42.0669609637,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494,36.192597494)
L_mean=c(22.9613201502, 22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,22.9613201502,24.4326438487,24.4326438487,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,25.6899150556,27.262077211,27.262077211,27.262077211,27.262077211,27.262077211,27.262077211,27.262077211,27.262077211,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,29.1707102591,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396,27.6608606396)
M_mean=c(0.155941500945, 0.155941500945,0.155941500945,0.155941500945,0.155941500945,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.148558927827,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.146197212544,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.202717647065,0.211457719496,0.211457719496,0.211457719496,0.211457719496,0.211457719496,0.211457719496,0.234187862946,0.234187862946,0.234187862946,0.234187862946,0.234187862946,0.234187862946,0.234187862946,0.292762868163,0.292762868163,0.332357045028,0.332357045028,0.332357045028,0.451333969849,0.451333969849,0.451333969849,0.480106833433,0.480106833433,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.567480519948,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519,0.517933561519)
R_mean=c(22.8053786493, 22.8053786493,22.8053786493,22.8053786493,22.8053786493,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,22.8127612224,24.2840849208,24.2840849208,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,25.5437178431,27.0593595639,27.0593595639,27.0593595639,27.0593595639,27.0593595639,27.0593595639,27.0593595639,27.0593595639,28.967992612,28.967992612,28.9592525396,28.9592525396,28.9592525396,28.9592525396,28.9592525396,28.9592525396,28.9365223961,28.9365223961,28.9365223961,28.9365223961,28.9365223961,28.9365223961,28.9365223961,28.8779473909,28.8779473909,28.838353214,28.838353214,28.838353214,28.7193762892,28.7193762892,28.7193762892,28.6906034256,28.6906034256,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,28.6032297391,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.0933801197,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781,27.1429270781)
trans=0.5
age=(0:(119-1))* -1
plot(age,age,type = 'n', ylim = c(0, 47.1892540039), xlim = c(-124.95,5.95), ylab = 'Speciation rate', xlab = 'Ma',main='lineage' )
polygon(c(age, rev(age)), c(L_hpd_M95, rev(L_hpd_m95)), col = alpha("#4c4cec",trans), border = NA)
lines(rev(age), rev(L_mean), col = "#4c4cec", lwd=3)
plot(age,age,type = 'n', ylim = c(0, 0.915596943903), xlim = c(-124.95,5.95), ylab = 'Extinction rate', xlab = 'Ma' )
polygon(c(age, rev(age)), c(M_hpd_M95, rev(M_hpd_m95)), col = alpha("#e34a33",trans), border = NA)
lines(rev(age), rev(M_mean), col = "#e34a33", lwd=3)
plot(age,age,type = 'n', ylim = c(-0.114596789405, 47.0255469439), xlim = c(-124.95,5.95), ylab = 'Net diversification rate', xlab = 'Ma' )
abline(h=0,lty=2,col="darkred")
polygon(c(age, rev(age)), c(R_hpd_M95, rev(R_hpd_m95)), col = alpha("#504A4B",trans), border = NA)
lines(rev(age), rev(R_mean), col = "#504A4B", lwd=3)
plot(age,age,type = 'n', ylim = c(0, max(1/M_mean, na.rm = TRUE)), xlim = c(-124.95,5.95), ylab = 'Longevity (Myr)', xlab = 'Ma' )
lines(rev(age), rev(1/M_mean), col = "#504A4B", lwd=3)
n <- dev.off()