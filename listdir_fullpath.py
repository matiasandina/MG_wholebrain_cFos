# This function returns the full path
# It tries to be an analogous of list.files in R...still work to do

import os
import numpy as np

# We will use this helper function to get the full path of the video files
def listdir_fullpath(root_dir, file_extension=None, exclude_dir = True):

    # Get everything
    if file_extension is None:
        a = [os.path.join(root_dir, files) for files in os.listdir(root_dir)]
    else:
        a = [os.path.join(root_dir, files) for files in os.listdir(root_dir) if files.endswith(file_extension)]

    if len(a) > 0:
        if exclude_dir:
            files_to_keep = np.bitwise_not(list(map(os.path.isdir, a)))
            a = np.array(a)[files_to_keep]
            a = a.tolist()

    return sorted(a)

