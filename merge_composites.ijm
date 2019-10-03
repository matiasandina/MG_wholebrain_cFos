@File(label="input directory") input
// This macro will open a stacked tiff image (n channel)
// and merge it into a n channel composite
// CAUTION: it will overwrite the original
// provide path from console as 'input="/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/composites/test"'
// MIND THE COMMAS ON THE PATH!!!!!

//input = getDirectory("Choose an input directory");
//print(input);
// We overwrite the input
output = input;
//output = getDirectory("Choose an output directory");
processFolder(input);


function processFolder(dir) {
list = getFileList(dir);
print(list.length);

for (i=0; i<list.length; i++) {
	print(list[i]);
	print(i);
if(endsWith(list[i], ".tif")) { //add the file ending for your images
print(".tif file, making compoiste");
processFile(dir, output, list[i]);
// change this if you want it recursive to all folders
// normally not a good idea, unpredictable
//} else if(endsWith(list[i], "/") && !matches(output, ".*" + substring(list[i], 0, lengthOf(list[i])-1) + ".*")) {
//if the file encountered is a subfolder, go inside and run the whole process in the subfolder
//processFolder(""+dir+list[i]);
} else {
//if the file encountered is not an image nor a folder just print the name in the log window
print("Not a tif, moving on");
print(dir + File.separator + list[i]);
}
}
}


function processFile(inputFolder, output, file) {
open(inputFolder + File.separator + file);
title = getTitle();
run("Make Composite", "display=Composite");
saveAs("tiff", output + File.separator + file);
close();
}
