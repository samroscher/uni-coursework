'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 7: Computer Vision Basics
'''

# pip install opencv-python
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import cv2
import pathlib
pathlib.Path('models').mkdir(parents=True, exist_ok=True) 
pathlib.Path('plots').mkdir(parents=True, exist_ok=True) 

###############################################################################
# If you obtain the error message:
# QObject::moveToThread: Current thread (0x31a16f0) is not the object's thread (0x2eaf780).
# Cannot move to target thread (0x31a16f0)
# 
# qt.qpa.plugin: Could not load the Qt platform plugin "xcb" in "/home/feurerm/sync_dir/teaching/2023_SoSe_Python/miniconda/envs/lecturepython/lib/python3.11/site-packages/cv2/qt/plugins" even though it was found.
# This application failed to start because no Qt platform plugin could be initialized. Reinstalling the application may fix this problem.
#
# Please try one the following two alternative backends for displaying figures
# in Python:
# matplotlib.use("TKAgg")
# This require TKInter
# matplotlib.use("WebAgg")
# This requires the package `tornado`
#
###############################################################################

#%% ------------------------------------------------------------------------------------
# BLOCK 1: Reading and Displaying Images
# --------------------------------------
print('#'*50)
print('########## Reading and Displaying Images ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Read the image 'files/flowers.jpg' using cv2.imread()
print('---------- EX-01 ----------')
 
img = cv2.imread('files/flowers.jpg')

#%% ------------------------------------------------------------------------------------
# EX02: Show the image in the console. To this end, use plt.imshow()
print('---------- EX-02 ----------')
plt.imshow(img)

#%% ------------------------------------------------------------------------------------
# EX03: How is the image represented in Python?
# Output the object type and the data type of the image
print('---------- EX-03 ----------')

img_type  = type(img)
img_dtype = img.dtype

print(img_type, img_dtype)
# It is a NumPy array with integers => we can use everything we learned in the NumPy lecture!

#%% ------------------------------------------------------------------------------------
# EX04: Output the image dimensions 
print('---------- EX-04 ----------')

img_shape = img.shape
print(img_shape)

#%% ------------------------------------------------------------------------------------
# EX05: Output the size of the image's array 
print('---------- EX-05 ----------')

img_size = img.size
print(img_size)

#%% ------------------------------------------------------------------------------------
# EX06: Make a copy of the image
print('---------- EX-06 ----------')

img_copy = img.copy()
print(img_copy)

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Small Changes on Images
# --------------------------------
print('#'*50)
print('########## Small Changes on Images ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Output the three color values of an arbitrary pixel
print('---------- EX-01 ----------')

pixel = img[200, 300]
print(pixel)

#%% ------------------------------------------------------------------------------------
# EX02: Output the blue value of the same pixel 
# Note: cv2 shows BGR by default
print('---------- EX-02 ----------')

pixel_blue = pixel[0]
print(pixel_blue) 

#%% ------------------------------------------------------------------------------------
# EX03: Change the color values of an arbitrary pixel and output the new values
# You can view the modified image
# Make sure to not change the original image!
# Note: color values are whole numbers from 0 up to and including 255 
# (8 bit = integer). (0,0,0) is black, (255, 255, 255) is white.
print('---------- EX-03 ----------')
img_copy[200, 300] = [0, 250, 0]
plt.imshow(img_copy)

#%% ------------------------------------------------------------------------------------
# EX04: Cut out part of the image and display the partition in the console
print('---------- EX-04 ----------')
img_part = img_copy[150:250, 250:350]
plt.imshow(img_part)

#%% ------------------------------------------------------------------------------------
# EX05: Change the size (i.e., the number of pixels) of the image with cv2.resize() 
# and display the resized image in the console
print('---------- EX-05 ----------')
img_small = cv2.resize(img, (64, 48))
plt.imshow(img_small)

#%% ------------------------------------------------------------------------------------
# EX06: Writing / Painting something on the image:
# Use cv2.rectangle() to draw a rectangle on the image in an arbitrary color.
# Note: The rectangle should match the image dimensions
# Make sure to not change the original image!
print('---------- EX-06 ----------')
img_rect = img.copy()
img_rect = cv2.rectangle(
    img_rect,
    pt1=(250, 250),
    pt2=(350, 400),
    color=(255, 0, 0),thickness=5
)
img_rect = cv2.cvtColor(img_rect, cv2.COLOR_BGR2RGB)
plt.imshow(img_rect)

#%% ------------------------------------------------------------------------------------
# EX07: Draw a circle on the image with cv2.circle().
# Note: The circle should match the image dimensions
print('---------- EX-07 ----------')
img_circle = cv2.cvtColor(img.copy(), cv2.COLOR_BGR2RGB)
img_circle = cv2.circle(img_circle, center=(320, 280), radius=30, color=(0, 0, 255), thickness=3)
plt.imshow(img_circle)

#%% ------------------------------------------------------------------------------------
# EX08: Write a text on the image with cv2.putText() in an arbitrary color.
print('---------- EX-08 ----------')
# cv2.putText(img_to_draw_on, text = ..., org = (50, 200), fontFace = cv2.FONT_HERSHEY_DUPLEX, fontScale = 1, color = ..., thickness = 4, lineType = cv2.LINE_AA)
img_text = cv2.cvtColor(img.copy(), cv2.COLOR_BGR2RGB)
img_text = cv2.putText(
    img_text,
    'I love pink!',
    org=(25, 350),
    fontFace = cv2.FONT_HERSHEY_SCRIPT_COMPLEX,
    fontScale = 1,
    color =(0, 0, 255),
    thickness=1)
plt.imshow(img_text)

#%% ------------------------------------------------------------------------------------
# BLOCK 3: Color Spaces
# ---------------------
print('#'*50)
print('########## Color Spaces ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Change the image from BGR to RGB and check the result
print('---------- EX-01 ----------')

img_rgb = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
plt.imshow(img_rgb)
plt.show()

#%% ------------------------------------------------------------------------------------
# EX02: Change the image to black and white and check the result
print('---------- EX-02 ----------')

img_gray = cv2.cvtColor(img_rgb, cv2.COLOR_RGB2GRAY)
plt.imshow(img_gray, cmap = 'gray')
plt.show()

#%% ------------------------------------------------------------------------------------
# BLOCK 4: Flipping, Rotating, and Blurring
# -----------------------------------------
print('#'*50)
print('########## Flipping, Rotating, and Blurring ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: # Rotate the image 46° counterclockwise and output the result
# Note: Helps for the functions can be found at:
#   https://docs.opencv.org/2.4/modules/imgproc/doc/geometric_transformations.html
#   https://docs.opencv.org/2.4/modules/core/doc/operations_on_arrays.html#
#   https://docs.opencv.org/2.4/modules/imgproc/doc/filtering.html?highlight=blur#
print('---------- EX-01 ----------')
(h, w) = img_rgb.shape[:2]
center = (w / 2, h / 2)
angle = 46
M = cv2.getRotationMatrix2D(center, angle, scale=1)
img_rotated = cv2.warpAffine(img_rgb, M, (w, h))
plt.imshow(img_rotated)

#%% ------------------------------------------------------------------------------------
# EX02: Flip the image vertically with cv2.flip() and output the result
print('---------- EX-02 ----------')
img_flip1 = cv2.flip(img_rgb, 1)
plt.imshow(img_flip1)

#%% ------------------------------------------------------------------------------------
# EX03: Flip the image horizontally with cv2.flip() and output the result
print('---------- EX-03 ----------')
img_flip0 = cv2.flip(img_rgb, 0)
plt.imshow(img_flip0)

#%% ------------------------------------------------------------------------------------
# EX04: Use cv2.blur() to blur the image and output the result
# Try several kernel sizes.
print('---------- EX-04 ----------')
img_blur_mid = cv2.blur(img_rgb, ksize=(5, 5))
plt.imshow(img_blur_mid)

img_blur_high = cv2.blur(img_rgb, ksize=(15, 15))
plt.imshow(img_blur_high)
#%% ------------------------------------------------------------------------------------
# EX05: Blur the image by applying the functions:
# - cv2.GaussianBlur()
# - cv2.medianBlur()
# - cv2.bilateralFilter()
print('---------- EX-05 ----------')
img_blur_gauss = cv2.GaussianBlur(img_rgb, ksize=(9, 9), sigmaX=3)
plt.imshow(img_blur_gauss)

img_blur_med = cv2.medianBlur(img_rgb, ksize=7)
plt.imshow(img_blur_med)

img_blur_bil = cv2.bilateralFilter(img_rgb, d=7, sigmaColor=75, sigmaSpace=75)
plt.imshow(img_blur_bil)

#%% ------------------------------------------------------------------------------------
# EX06: Apply erosion kernels to the image with cv2.erode() and check the result. 
# Try several kernels.
print('---------- EX-06 ----------')
kernel = np.ones((3, 3), np.uint8)
img_eroded = cv2.erode(img_rgb, kernel, iterations=3)
plt.imshow(img_eroded)

#%% ------------------------------------------------------------------------------------
# EX07: Apply dilation kernels to the image and and output the result.
# Try several kernels and cv2.dilate()
print('---------- EX-07 ----------')
img_dilated = cv2.dilate(img_rgb, kernel, iterations=3)
plt.imshow(img_dilated)

#%% ------------------------------------------------------------------------------------
# EX08: Write a function that automatically augments images.
# The function should return the augmented image.
# The change method should be randomly selected for each function call.
print('---------- EX-08 ----------')

def get_aug_img(img):
    options = [
        cv2.flip(img, flipCode=0),
        cv2.blur(img, ksize=(7, 7)),
        cv2.GaussianBlur(img, ksize = (7, 7), sigmaX = 0),
        cv2.erode(img, np.ones((3, 3), np.uint8), iterations = 3),
        cv2.dilate(img, np.ones((3, 3), np.uint8), iterations = 3),
        cv2.cvtColor(img, cv2.COLOR_BGR2HLS),
        cv2.flip(img, flipCode=1)
    ]

    idx = np.random.randint(low=0, high=len(options))
    return options[idx]

img = cv2.imread('files/flowers.jpg')
img_aug = get_aug_img(img)
plt.imshow(img_aug)

#%% ------------------------------------------------------------------------------------
# BLOCK 5: Saving Images
# ----------------------
print('#'*50)
print('########## Saving Images ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Save one of the newly created images with cv2.imwrite() in the data folder.
# Help for this:
# https://docs.opencv.org/2.4/modules/highgui/doc/reading_and_writing_images_and_video.html#
print('---------- EX-01 ----------')
cv2.imwrite('files/flowers_aug.jpg', img_aug)
