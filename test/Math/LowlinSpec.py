import numpy as np
import numpy.linalg as npl

s1 = 9
v2 = np.array([23,53])
w2 = np.array([74,49])
v3 = np.array([93,95,34])
w3 = np.array([73,27,51])
v4 = np.array([94,23,43,30])
w4 = np.array([62,88,23,98])

m22 = np.array([[43,65],[23,83]])
n22 = np.array([[54,77],[46,72]])
m33 = np.array([[43,22,65],[23,83,54],[49,29,55]])
n33 = np.array([[23,90,30],[29,33,65],[50,23,66]])
m44 = np.array([[88,25,71,38],[55,50,30,27],[11,73,52,60],[33,69,29,52]])
n44 = np.array([[84,40,59,13],[51,84,50,39],[32,74,40,91],[30,60,31,70]])

if __name__ == "__main__":
    for mat1,mat2,vec1,vec2 in ((m22,n22,v2,w2),(m33,n33,v3,w3),(m44,n44,v4,w4)):
        print(s1*vec1)
        print(vec1*s1)
        print(vec1/s1)
        print(npl.norm(vec1))
        print(vec1/npl.norm(vec1))
        print(np.dot(vec1,vec2))
        print(vec1.max())
        print(vec1.min())
        print(np.sum(vec1))
        print(np.dot(mat1,mat2))
        print(np.dot(mat1,vec1))
        print(npl.norm(mat1))
        print(np.transpose(mat1))
        print(np.diagonal(mat1))
        print(np.trace(mat1))
        print(npl.det(mat1))
        print(npl.norm(np.dot(npl.inv(mat1),mat1)))
        print(" ")
