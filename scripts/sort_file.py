# sort a file with 5 columns:
# index (int) | frame (int)  | xPos (float) | yPos (float) | zPos (float)
# The output file is sorted with respect to the index.

# EXAMPLE 
# input <-- "file.txt" | output --> "s_file.txt"
# id fr  x    y    z
# 1  0   1.1  2.2  1.0
# 1  1   1.3  2.3  1.0
# 1  2   1.6  2.9  1.0
# 2  0   14.4 2.0  1.0
# 2  1   15.0 3.9  1.0
# 2  2   16.7 5.6  1.0
# .  .   .    .    .
######################################################################################
import numpy as np
from sys import argv
import os

if len(argv) != 2:
    print "\tUsage: python %s <file_to_sort>\n"%argv[0]
    print "---> Going to exit <---\n"
    exit(-1)

filename = argv[1] # input file
if not os.path.exists(filename):
    print "ERROR: file <%s> does not exist.\n"%filename
    print "---> Going to exit <---\n"
    exit(-1)
#basename = os.path.basename(file)

out_file = "s_" + filename #basename 

print "sort_file: %s"%(filename)

d = np.loadtxt(filename)
# read the columns of the matrix d
ids = d[:,0]
frames = d[:,1]
x = d[:,2]
y = d[:,3]
z = d[:,4]
# convert lists to matrix, so that we can use the transpose of a vector
frames = np.matrix(frames)
ids = np.matrix(ids)
x = np.matrix(x)
y = np.matrix(y)
z = np.matrix(z)
# make from the vectors a matrix
data = np.array( np.hstack( (ids.T, frames.T, x.T, y.T, z.T) ) )
# sort matrix by first column (index of pedestrians), then by second column (frames)
data = data[ np.lexsort( (data[:,1], data[:,0]) ) ]     
# save matrix in out_file
np.savetxt(out_file, data, fmt = ('%d\t' , '%d\t' , '%.2f\t' , '%.2f', '%.2f') )



