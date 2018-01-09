from matplotlib.pyplot import *
from numpy import *
from sys import argv



if len(argv)<=1:
	print("usage: %s  <filename>"%argv[0])
	exit(">>>>exit<<<<")

filename = argv[1]

print('load file ...', filename)
D = loadtxt(filename)

peds = unique(D[:,1])

for ped in peds:
	if ped not in range(10):
		continue
	if ped%10 == 0:
		print("ped = %d"%ped)
	d = D[D[:,1] == ped]
	x = d[:,2]
	y = d[:,3]
	plot(x, y, "-", color='r',lw=2)

    
xlabel("x [m]")
ylabel("y [m]")
name = filename.split(".")[0]
title(name)
for end in ['png', 'pdf', 'eps']:
    print("save figure as <%s.%s>"%(name,end))
    savefig("%s.%s"%(name, end))

show()
