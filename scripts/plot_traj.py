from matplotlib.pyplot import *
from numpy import *
from sys import argv
from mpl_toolkits.mplot3d import Axes3D




if len(argv)<=1:
        print("usage: %s  <filename>"%argv[0])
        exit(">>>>exit<<<<")

filename = argv[1]

print('load file ...', filename)
D = loadtxt(filename)

peds = unique(D[:,0])
fig = figure()

z = np.sort(D[:, 4])

_3D = 0
if z[0] != z[-1]:
    ax = fig.add_subplot(111, projection='3d')
    print("3D")
    _3D = 1
    
for ped in peds:
    if ped%10 == 0:
        print("ped = %.3d"%ped)
    d = D[D[:,0] == ped]
    x = d[:,2]
    y = d[:,3]

    if _3D:
        z = d[:, 4]
        plot(x, y, z, "-", color='r',lw=0.2)
    else:
        plot(x, y, "-", color='r',lw=0.2)
        
xlabel("x [m]")
ylabel("y [m]")
name = filename.split(".")[0]
title(name)
for end in ['png', 'pdf', 'eps']:
    print("save figure as <%s.%s>"%(name,end))
    savefig("%s.%s"%(name, end))

show()
