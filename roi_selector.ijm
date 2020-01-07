// this script will help us select 2 polygonal regions on each image for S1DZ

input = getDirectory("Choose an input directory");
output = getDirectory("Choose an output Directory")

processFolder(input);


////////////    Function definitions here           ////////////////////////////////

function processFolder(dir) {
list = getFileList(dir);
//print(list.length);

for (i=0; i<list.length; i++) {
	print(list[i]);
	print(i);
if(endsWith(list[i], ".tif")) { //add the file ending for your images
print("Open file");
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



function processFile(dir, output, file){

open(inputFolder + file);

roiNumber = getNumber("How many ROIs in this image?", 2);
for (i = 0; i < roiNumber; i++) {

Dialog.create("Brain side");
Dialog.addChoice("Side of the brain:", newArray("left", "right"));
Dialog.show();
mySide = Dialog.getChoice();

setTool("polygon");
waitForUser("Draw a polygon and click OK...");
run("Convex Hull");

h = getHeight();
toScaled( h );
name = getTitle();

getSelectionCoordinates( x, y );

for(q=0; q<lengthOf(x); q++) {
setResult("X", q, x[q]);
setResult("Y", q, y[q]);
setResult("Side", q, mySide);
}
updateResults();
saveAs("Measurements", output + file.nameWithoutExtension + "_Measurements_" + mySide + ".csv");
selectWindow("Results");
close("Results");


// print some with 
print("Done, moving on");
}


	
}


