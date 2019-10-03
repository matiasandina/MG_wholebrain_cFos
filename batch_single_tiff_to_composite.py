import os
from listdir_fullpath import *
from PIL import Image
import tifffile
from tqdm import tqdm
import argparse
import re
import struct

# This function needs work...

# we are producing a stack, not a composite
# so we have to call imageJ anyhow...see
# https://stackoverflow.com/questions/50258287/how-to-specify-colormap-when-saving-tiff-stack



# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("-input_dir_c0", help="c0 image folder")
parser.add_argument("-input_dir_c1", help="c1 image folder")
parser.add_argument("-input_dir_c2", help="c2 image folder", required=False, default=None)
args = parser.parse_args()

input_dir_c0 = args.input_dir_c0
input_dir_c1 = args.input_dir_c1
input_dir_c2 = args.input_dir_c2

# for debug
# input_dir_c0 = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/c0_crops"
# input_dir_c1 = "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/c1_crops"

if input_dir_c2 is None:
    dirs = [input_dir_c0, input_dir_c1]
else:
    dirs = [input_dir_c0, input_dir_c1, input_dir_c2]

# Get the root folder where things will be saved to
root_folder = os.path.dirname(dirs[0])

# make the folder composites
save_folder = os.path.join(root_folder, "composites")
if not os.path.exists(save_folder):
    print("Creating folder : >> " + save_folder)
    os.makedirs(save_folder)

# get the tif files in the folder
# because we transpose, it will be a np.array of n by num channels
file_list = np.array([listdir_fullpath(x, file_extension=".tif") for x in dirs]).T

pbar = tqdm(total=len(file_list))
for file_group in file_list:
    pbar.set_description("Processing %s group" % os.path.basename(file_group[0]))

    # read individual files
    img0 = tifffile.imread(file_group[0])
    img1 = tifffile.imread(file_group[1])
    if len(file_group) == 3:
        img2 = tifffile.imread(file_group[2])


    if len(file_group) == 2:
        composite = np.array([img0, img1]).astype(np.uint16)

    elif len(file_group) == 3:
        composite = np.array([img0, img1, img2]).astype(np.uint16)


    else:
        print("This program does not currently support more than 3 channels")

    # get the filename
    nombre = os.path.basename(file_group[0])

    # sub the c0 for "composite"
    nombre = re.sub("c0", "composite", nombre)

    output_file = os.path.join(save_folder, nombre)

    # save
    tifffile.imsave(output_file, composite)

    print("Composite image saved at:\n" + output_file)

    pbar.update(1)
pbar.close()

# debug
# import matplotlib.pyplot as plt
# fig = plt.figure()

# plt.subplot(1, 2, 1)
# plt.imshow(img0)

# plt.subplot(1, 2, 2)
# plt.imshow(img1)


