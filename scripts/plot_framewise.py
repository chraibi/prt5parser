# plots the trajectories framewise and produce png files

from sys import argv
import os
import glob
import matplotlib.pyplot as plt
import numpy as np


if len(argv) <= 1:
    print("usage: %s  <filename>"%argv[0])
    exit(">>>>exit<<<<")
filename = argv[1]

print("load file %s..."%filename)
D = np.loadtxt(filename)

frames = np.unique(D[:, 0])
xmin = np.min(D[:, 2])
xmax = np.max(D[:, 2])
ymin = np.min(D[:, 3])
ymax = np.max(D[:, 3])

for frame in frames:
    d = D[D[:, 0] == frame]
    x = d[:, 2]
    y = d[:, 3]
    plt.plot(x, y, "o", color='b', ms=10)
    plt.xlim([xmin-0.5, xmax+0.5])
    plt.ylim([ymin-0.5, ymax+0.5])

    plt.savefig("%.3d.png"%frame)
    print("%.3d.png"%frame)
    plt.clf()

make_movie = r'ffmpeg -y -framerate 16 -pattern_type  glob -i "*.png" -r 25 -f mpeg -vcodec mpeg1video -ar 48000 -b:v 5000k -b:a 128k -acodec mp2 -ar 44100 -ac 1  demo.mpg'

try:
    err = os.system(make_movie)
except err:
    print("command does not exist. err = %d"%err)

png_files = glob.glob("*.png")

if png_files:
    print("remove png files")
    
for f in png_files:
    os.remove(f)
