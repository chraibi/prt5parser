import matplotlib.pyplot as plt
import numpy as np
from sys import argv



if len(argv)<=1:
    print ("usage: %s  <filename>"%argv[0])
    exit(">>>>exit<<<<")
filename = argv[1]

print ("load file %s..."%filename) 
D = np.loadtxt(filename)

frames = np.unique(D[:, 0])
for frame in frames:
    d = D[D[:,0] == frame]
    x = d[:,2]
    y = d[:,3]
    plt.plot(x, y, "o", color='b',ms=10)
    plt.xlim([-1100,1200])
    plt.ylim([-200,700])
    
    plt.savefig("%.3d.png"%frame )
    print ("%.3d.png"%frame)
    plt.clf()
