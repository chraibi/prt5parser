from matplotlib.pyplot import *
from numpy import *
from sys import argv



if len(argv)<=1:
    print "usage: %s  <filename>"%argv[0]
    exit(">>>>exit<<<<")
filename = argv[1]

print 'load file ...', filename 
D = loadtxt(filename)

peds = unique(D[:,1])
frames = unique(D[:, 0])
for frame in frames:
    d = D[D[:,0] == frame]
    x = d[:,2]
    y = d[:,3]
    plot(x, y, "o", color='b',ms=10)
    xlim([-1100,1200])
    ylim([-200,700])
    
    savefig("%.3d.png"%frame )
    print "%.3d.png"%frame
    clf()
