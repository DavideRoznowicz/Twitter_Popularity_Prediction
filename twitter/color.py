import numpy as np
import cv2 as cv
from matplotlib import pyplot as plt
img = cv.imread('129168397_714202876183863_4207819796983216597_n.jpg',0)
hist_img = cv.calcHist([img],[0],None,[256],[0,256])

