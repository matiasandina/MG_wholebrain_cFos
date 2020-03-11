import pandas as pd
import numpy as np
import cv2
import matplotlib.pyplot as plt
import matplotlib
import matplotlib.patches as patches
import math
import PIL
import os
import re
import time

# Read the polygons
#polygon_filename = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/contours/2019_08_19__0001_16.csv"

#dapi_filename = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/2019_08_19__0001_16.tif"

# read polygons
#poly_df = pd.read_csv(polygon_filename)

# do scaling first, then
# debug masking
# contour_df = poly_df[poly_df.parent_roi_number == 1][["contour.ID", "xlT", "ylT"]]


# region Helper functions #####
def bounding_box(points):
    x_coordinates, y_coordinates = zip(*points)

    x_min = min(x_coordinates)
    y_min = min(y_coordinates)
    width = max(x_coordinates) - x_min
    height = max(y_coordinates) - y_min

    return [(x_min, y_min), width, height]

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

def mask_and_crop(img, contour_df, contour_tag, img_filename, roi_name, display_crop):
    # create black canvas
    mask = np.zeros(img.shape, dtype=np.uint8)
    # fill the ROI so it doesn't get wiped out when the mask is applied
    # channel_count = img.shape[2]  # i.e. 3 or 4 depending on your image
    # make canvass
    # we will fill each polygon that we are presented with (in case there's more than one)

    if(contour_tag == "right"):
        for poly in np.unique(contour_df["contour.ID"]):
            # subset
            sub_df = contour_df[contour_df["contour.ID"] == poly]
            # fillConvexPoly does not overwrite
            # explicitly burn into the mask
            # mind the np.array(..., "int32") is wrapped in [] because that's how fillPoly likes it
            mask = cv2.fillPoly(mask, [np.array(sub_df[["xrT", "yrT"]], 'int32')], 1)

    if(contour_tag == "left"):
        for poly in np.unique(contour_df["contour.ID"]):
            # subset
            sub_df = contour_df[contour_df["contour.ID"] == poly]
            # burn into the mask
            # explicitly burn into the mask
            # mind the np.array(..., "int32") is wrapped in [] because that's how fillPoly likes it
            mask = cv2.fillPoly(mask, [np.array(sub_df[["xlT", "ylT"]], 'int32')], 1)

    # apply the mask
    masked_image = img * mask

    # get the bounding rectangle
    bbox = bounding_box(np.array(contour_df.drop('contour.ID', 1)))

    # region display #####
    if display_crop:
        # Create axis
        fig, ax = plt.subplots(1)

        # Display the image
        ax.imshow(masked_image)

        # add polygon
        if contour_tag == "left":
            poly = patches.Polygon(np.array([contour_df.xlT, contour_df.ylT]).T,
                                   fill=False)
            ax.add_patch(poly)

        if contour_tag == "right":
            poly = patches.Polygon(np.array([contour_df.xrT, contour_df.yrT]).T,
                                   fill=False)
            ax.add_patch(poly)

        # Create a Rectangle patch
        rect = patches.Rectangle(bbox[0], bbox[1], bbox[2],
                                 linewidth=1, edgecolor='r', facecolor='none')

        # Add the patch to the Axes
        ax.add_patch(rect)
    # endregion

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

    # region Save ######

    path_sep = os.path.split(img_filename)

    root_folder = path_sep[0]
    nombre = clean_filename(img_filename)

    # try to get whether they tagged the channel you are using
    channel = re.search("_c[0-9]", img_filename)
    if channel is None:
        channel = ""
    else:
        channel = channel[0]
        #channel = re.sub("_", "", channel)
        # images are stored within a c0 or c1 channel, so we remove that from root_folder
        # this would be the equivalent of coming to '..' from the folder
        root_folder = os.path.dirname(root_folder)

    output_file = os.path.join(root_folder, nombre + "_" +
                               contour_tag + '_parent_roi_' +
                               str(roi_name) + channel + '.tif')

    pil_image = PIL.Image.fromarray(crop_img)

    pil_image.save(output_file)
    return(print("Image saved at:\n" + output_file))


def clean_filename(filename):
    if not isinstance(filename, str):
        # map basename on the list
        available_files = list(np.unique(list(map(os.path.basename, filename))))
        # remove the .tif
        available_files = [re.sub(".tif", "", x) for x in available_files]
        # remove the "small"
        available_files = [re.sub("small_", "", x) for x in available_files]
        # remove any channel info
        available_files = [re.sub("_c[0-9]", "", x) for x in available_files]
        return(available_files)

    else:
        filename = os.path.basename(filename)
        # remove the .tif
        available_files = [re.sub(".tif", "", filename)]
        # remove the "small"
        available_files = [re.sub("small_", "", x) for x in available_files]
        # remove any channel info
        available_files = [re.sub("_c[0-9]", "", x) for x in available_files]
        # return the string
        return(available_files[0])

# endregion

# main function
def find_and_crop(img_filename, df, grouping_var, display_crop=False, display_contours=False):


    # Read the image
    # -1 loads as-is so if it will be 3 or 4 channel as the original
    # not really working like as supposed for my multi-channel images, but feeding single channel in the pipeline
    img = cv2.imread(img_filename, -1)
    # resize to fit screen
    #imS = imutils.resize(img, width=1280)

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

    # endregion

    # np.unique(df[df["parent_roi_number"] == 1]["contour.ID"])

    # double condition here
    # df[(df.parent_roi_number == 1) & (df["contour.ID"] == 24)]

    if display_contours:
        display_all_contours(img, df, "contour.ID")

    # region select contour #####
    for k in np.unique(df[grouping_var]):
        # get the rows we care about
        target_rows = df[grouping_var] == k

        # get the left hemisphere for contour(s) k
        left = df[target_rows][["contour.ID", "xlT", "ylT"]]
        # get the right hemisphere for contour(s) k
        right = df[target_rows][["contour.ID", "xrT", "yrT"]]


        # make the roi_name
        if(grouping_var == "parent_roi_number"):
            roi_name = np.unique(df[target_rows].parent)[0]
        else:
            # we presume we are using individual contours (we tag with acronym)
            roi_name = np.unique(df[target_rows].acronym)[0]

        # Call mask and crop function
        # left hemisphere
        mask_and_crop(img, contour_df=left,  contour_tag="left",
                      img_filename=img_filename, roi_name=roi_name, display_crop=display_crop)
        # right hemisphere
        mask_and_crop(img, contour_df=right, contour_tag="right",
                      img_filename=img_filename, roi_name=roi_name, display_crop=display_crop)

        # endregion

    # Print some messages
    return("done :)")


#find_and_crop(dapi_filename, poly_df, "parent_roi_number", True)

if __name__ == '__main__':
    print('This program is being run by itself in command line')
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-img_filename", help="image filename")
    parser.add_argument("-df_path", help="path to csv contours")
#    parser.add_argument("-contour_key_path", help="path to csv with key of contours names and regions per file")
    parser.add_argument("-grouping_var", help="name of the variable to use as grouping for contours")
    parser.add_argument("-display_crop", help="boolean to display crops", default=False,
    # this is needed for transforming str to bool
    type=lambda x: (str(x).lower() == 'true'))
    parser.add_argument("-display_contours", help="boolean to display all contours", default=False,
    type=lambda x: (str(x).lower() == 'true'))
    args = parser.parse_args()
    
    # read data frame
    args.df = pd.read_csv(args.df_path)
#    args.contour_key = pd.read_csv(args.contour_key_path)

    # Call the function
    find_and_crop(img_filename=args.img_filename, df=args.df,
                  grouping_var =args.grouping_var,
                  display_crop=args.display_crop, display_contours= args.display_contours)

    # important if we want to show at the end and preventing closing
    if args.display_contours:
        plt.show()

    if args.display_crop:
        plt.show()


else:
    print(__name__ + ' is being imported from another module')

