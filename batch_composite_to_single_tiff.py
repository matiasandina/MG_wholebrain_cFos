import os
from listdir_fullpath import *
import PIL.Image
import tifffile
from tqdm import tqdm
import argparse

# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("-input_dir", help="image folder")
args = parser.parse_args()

input_dir = args.input_dir

# get the tif files in the folder
file_list = listdir_fullpath(input_dir, file_extension='.tif')

pbar = tqdm(total=len(file_list))
for ind_file in file_list:
    pbar.set_description("Processing %s" % ind_file)
    # read individual file
    img = tifffile.imread(ind_file)
    # TODO: tif structure might be a problem
    # for some reason, the channels go at the beginning instead of at the end
    # this might be related to why opencv can't actually read it
    # tifffile was the only library that could...

    number_of_channels = img.shape[0]

    for channel in range(number_of_channels):
        channel_folder = "c"+str(channel)
        channel_folder = os.path.join(os.path.split(ind_file)[0], channel_folder)

        # create dir
        if not os.path.exists(channel_folder):
            os.makedirs(channel_folder)

        # subset the image
        c = img[channel, :, :]

        # get the filename
        nombre = os.path.splitext(os.path.basename(ind_file))[0]

        output_file = os.path.join(channel_folder, nombre + "_c"+ str(channel) + '.tif')

        pil_image = PIL.Image.fromarray(c)

        pil_image.save(output_file)
        # #print("Image saved at:\n" + output_file)

    pbar.update(1)
pbar.close()


