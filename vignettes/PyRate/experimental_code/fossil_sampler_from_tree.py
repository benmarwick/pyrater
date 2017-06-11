import argparse, os,sys
from os.path import basename
from numpy import *
import numpy as np
import os

# fossilSampler .
#os.chdir(r'C:\Users\oskar\Documents\Dropbox\PyRate_Age-Dependency_and_Beyond\Toy_Datasets_TreeSimGM\BAT_simulator\testingzone')
#os.chdir('/Users/daniele/Dropbox-personal/Dropbox/PyRate_Age-Dependency_and_Beyond/Toy_Datasets_TreeSimGM/BAT_simulator/testingzone')

p = argparse.ArgumentParser() #description='<input file>') 
p.add_argument('-v', action='version', version='%(prog)s')
p.add_argument('-d', type=str,help="Load SE table",metavar='<1 input file>',default="")

args = p.parse_args()

output_wd = os.path.dirname(args.d)
if output_wd=="": output_wd= self_path

filename=os.path.splitext(basename(args.d))[0]

# reading simulated file
simi = np.loadtxt(fname=args.d, skiprows=1)
TS=simi[:,2]
TE=simi[:,3]

# simulatiom name
output = filename.split("_")[0]

##  'q_rate' IS THE PRESERVATION RATE, WE SHOULD TEST WITH VALUES OF E.G. 0.25, 0.5, 1, 3
q_rate = round(random.uniform(0.5, 1.5),3)
rho=1
## SIMULAITON FUNCTIONS
def write_to_file(f, o):
	sumfile = open(f , "wb") 
	sumfile.writelines(o)
	sumfile.close()

# N=repeat(0,len(TS)) ## DANIELE im no sure about this....

def resample_simulation(TS,TE, beta_par=3,q=1,rho=rho,minFO=0,verbose=1):
	# uniform sample
	n = TS-TE
	N = np.random.poisson(q*n)
	if q==0: N =np.random.poisson(3*n)
	if minFO>0: N=N+1
	
	if rho <1:
		eliminate=int((1-rho)*len(N))
		DEL=np.random.choice(len(N), eliminate, replace=False)
		N[DEL]=0
		
	FAbeta, LObeta=zeros(len(n)), zeros(len(n))
	all_records=list()
	
	for i in range(len(n)): #looping over species
		
		if TE[i]==0: 
			## setting extinctiom of extant taxa as getting extinct at the present
			rW = np.random.weibull(float(filename.split("_")[2]),1)*float(filename.split("_")[3])
			#with the following while we avoid extinction times that are before the present
			while rW <= TS[i]: rW = np.random.weibull(float(filename.split("_")[2]),1)*float(filename.split("_")[3])

			m=TS[i]-rW # m is equal to the future projected time to live....
			n_DA=(TS[i]-m)
			N[i]=np.random.poisson(q*n_DA) ##DANIELE, I can not follow this up...		
			if q==0: N[i] =np.random.poisson(3*n_DA)
			samples= TS[i] - np.random.beta(beta_par,beta_par,N[i])*n_DA # + m
			samples=samples[samples>0] # avoid negative
			if len(samples)>0:
				samples=np.concatenate((samples,array([0]))) #, axis=1)
			else: samples = []
			#print samples
		elif N[i]>0:
			samples=np.random.beta(beta_par,beta_par,N[i]) *n[i] +TE[i]
		else: samples=[] # no record

		if len(samples)>0: 
			samples = np.sort(samples)[::-1]
			FAbeta[i]=max(samples)
			LObeta[i]=min(samples)
			all_records.append(samples)
		
	if q==0: # trend 95-05%
		x=all_records
		NTAX=list()
		for i in range(len(x)):
			s_max=max(x[i])
			rnd = np.random.uniform(0,1,len(x[i]))
			th=array(.95*(1-x[i]/s_max) +.05)
			in2 = (rnd<th).nonzero()[0]
			sub=x[i][in2] #) [I]
			if len(x[i])==1 and x[i][0]==0: NTAX.append(x[i])
			else:
				if min(x[i])==0:
					if len(sub)==0: sub=np.append(sub, 0)
					elif min(sub)>0: sub=np.append(sub, 0)
				if len(sub)>=1: NTAX.append(sub)
		all_records=NTAX
	
	g,a,b,e,f="no. records:\n", "\nts (true):\n","\nte (true):\n","\nts (obs):\n","\nte (obs):\n"
	for i in range(len(TS)):
		g += "%s\t" % (N[i])	
		a += "%s\t" % (TS[i])	
		b += "%s\t" % (TE[i])	
		e += "%s\t" % (FAbeta[i])	
		f += "%s\t" % (LObeta[i])
	data="\ndata (beta=%s)\n%s"  % (beta_par, all_records)		
	print "\n real length:  %s " % (round(sum(TS)-sum(TE), 2))
	print " beta length:    %s " % (round(sum(FAbeta)-sum(LObeta), 2)), "beta_par", beta_par,"q", q ,"rho", rho,"minFO", minFO
	no_data=0.
	for i in range(len(all_records)):
		if min(all_records[i])==0 and len(all_records[i])==1: no_data+=1.
	
	
	a1= "\n\nTotal species: %s - %s observed (%s); %s extant; %s no data)" % \
	(len(TE), int(len(all_records)-no_data), round((len(all_records)-no_data)/len(TE),2),len(TE[TE==0]), no_data)

	a1+="\ntotal length: %s (observed: %s, beta: %s, minFO: %s, q: %s, rho: %s)\n" % \
	(sum(TS-TE), sum(FAbeta-LObeta), beta_par, minFO, q, rho)
	
	o=''.join([a1,g,a,b,e,f]) # a0,

	return [all_records, o]



# RUN SIMULATION
sim_data = resample_simulation(TS,TE,q=q_rate)
all_records = sim_data[0]

# names log files

Nextinct=sum(Next == 0 for row in all_records for Next in row)
Nextant=len(all_records)- Nextinct

output_name = output +"_"+str(Nextant)+"_"+str(Nextinct)+"_"+str(rho)+"_"+str(q_rate)+"_"+filename.split("_")[1]+"_"+filename.split("_")[2]+"_"+filename.split("_")[3]


#print "\n\n", all_records, len(all_records)
#filtering sizes
if Nextinct >= 20 and Nextinct <=300 and Nextant <=300:
	data="#!/usr/bin/env python\nfrom numpy import * \n\n"
	d="\nd=[sim_data]"
	names="\nnames=['%s']" % (output_name)
	data += "\nsim_data = %s"  % (all_records)
	taxa_names="\ntaxa_names=["
	for i in range(len(all_records)): 
		taxa_names+= "'sp_%s'" % (i)
		if i<len(all_records)-1: taxa_names +=","
	taxa_names += "]\ndef get_taxa_names(): return taxa_names\n"                     
	f="\ndef get_data(i): return d[i]\ndef get_out_name(i): return names[i]"
	all_d=data+d+names+taxa_names+f
	#write_to_file(r"\fossils\%s.py" % output, all_d) 	
	#write_to_file(r"\fossils\%s_summary.txt" % output, sim_data[1]) 	
	write_to_file(output_wd+"/fossils/%s.py" % output, all_d) 	
	write_to_file(output_wd+"/fossils/%s_summary.txt" % output, sim_data[1]) 	
else:
	print("Skipping "+ filename + " with " + str(Nextant) +"extant and "+ str(Nextinct) +"extinct")
quit()
