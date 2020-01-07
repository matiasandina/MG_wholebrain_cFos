import pandas as pd
import numpy as np
import cv2
import matplotlib.pyplot as plt
import matplotlib
import matplotlib.patches as patches
import math


#polygon_filename = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/contours/2019_08_19__0002_16_16.csv"

#dapi_filename = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/2019_08_19__0002_16_16.tif"

contour_df = pd.read_csv(polygon_filename)


img = cv2.imread(dapi_filename, -1)

masked_image = np.zeros((img.shape), dtype=np.uint8)

# Find SSp
df = contour_df[contour_df['parent'].str.contains("SSp")]

# scale
df2 = scale_df(df.copy(), img)

display_all_contours(img, df2, "contour.ID")

for each_acronym in np.unique(df2.parent):
    df_sub = df2[df2.parent == each_acronym]

    # do proper selections
    df_left = df_sub[["xlT", "ylT"]]
    df_right = df_sub[["xrT", "ylT"]]

    # get the bounding rectangle
    #bbox = bounding_box(np.array(contour_df.drop('contour.ID', 1)))
    bbox_left = bounding_box(np.array(df_left))
    bbox_right = bounding_box(np.array(df_right))


    # Create axis
    fig, ax = plt.subplots(1)

    # Display the image
    ax.imshow(img)

    # add polygon
    poly = patches.Polygon(np.array([df2.xlT, df2.ylT]).T,
                           fill=False)
    ax.add_patch(poly)

    poly = patches.Polygon(np.array([df2.xrT, df2.yrT]).T,
                           fill=False)
    ax.add_patch(poly)

    # Create a Rectangle patch
    rect = patches.Rectangle(bbox_left[0], bbox_left[1], bbox_left[2],
                             linewidth=1, edgecolor='r', facecolor='none')

    # Add the patch to the Axes
    ax.add_patch(rect)

    # generate random numbers simulating cells
    fos = np.random.rand(1000, 2) * 1000
    fos = pd.DataFrame(fos, columns=["x", "y"])

    # transform
    fos.x = fos.x + math.ceil(bbox_left[1])
    fos.y = fos.y + math.ceil(bbox_left[2])

    # remember coordinates are the other way
    ax.scatter([fos.x], [fos.y], marker='o')

# region crop ####
# Round to get integers
# this kind of crop removes the border itself
# In principle, the is rounding should not affect
x = math.floor(bbox[0][0])
y = math.floor(bbox[0][1])
width = math.ceil(bbox[1])
height = math.ceil(bbox[2])
# Do the actual crop
# Remember that y and x are flipped
crop_img = masked_image[y:y + height, x:x + width]
# endregion



def display_all_contours(img, df, grouping_var):
    # display with matplotlib

    # Create figure and axes
    fig, ax = plt.subplots(1)

    # Display the image
    ax.imshow(img)

    # split by contour
    grouped_frame = df.groupby(grouping_var)
    li = [grouped_frame.get_group(x) for x in grouped_frame.groups]

    # for every contour
    for i in range(len(li)):
        poly = patches.Polygon(np.array([li[i].xrT, li[i].yrT]).T,
                               fill=False)
        ax.add_patch(poly)

    for i in range(len(li)):
        poly = patches.Polygon(np.array([li[i].xlT, li[i].ylT]).T,
                               fill=False, color="white")
        ax.add_patch(poly)

    # we don't block code from executing
    plt.show(block=False)
    # but we do some sleep, so that the plot renders
    # time.sleep(5)
    return("Displaying " + str(len(np.unique(df[grouping_var]))) + " contours.")

def scale_df(df, img):
    # region Do scaling math ######

    ## cols divided the width on R program
    ## we changed this to go from the original to the "small" version fed to wholebrain
    scaling_factor = img.shape[1]/1000

    # This is the magic rescaling factor inside wholebrain
    # We multiply by this to get the correct scaling
    # it should be only one scaling factor per plate (it might throw errors if more than 1 scaling factor is present)
    resize_parameter = float(np.unique(df.scale_factor))


    # right hemisphere
    # TODO: add parameter from resize_and_pad in R to this script or keep it as is?
    # the 100 is because we used resize_and_pad with pad = 200
    df.xrT = scaling_factor * (df.xrT * resize_parameter - 100)
    df.yrT = scaling_factor * (df.yrT * resize_parameter - 100)
    # left hemisphere
    df.xlT = scaling_factor * (df.xlT * resize_parameter - 100)
    df.ylT = scaling_factor * (df.ylT * resize_parameter - 100)

    return(df)

# region Helper functions #####
def bounding_box(points):
    x_coordinates, y_coordinates = zip(*points)

    x_min = min(x_coordinates)
    y_min = min(y_coordinates)
    width = max(x_coordinates) - x_min
    height = max(y_coordinates) - y_min

    return [(x_min, y_min), width, height]


