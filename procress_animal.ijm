/*
 * This macro will help modify an animal's wholebrain sections
 * Then it saves the composite to a new folder
 * It supports 2 channels (red + blue) or 3 channels (red + green + blue)
 */

#@ File (label = "Input directory", style = "directory") input
#@ File (label = "Output directory", style = "directory") output
#@ String (label = "File suffix", value = ".tif") suffix


AnalyzeAnimal(input);

function AnalyzeAnimal(input) {
// Should get the .tiff list

	list = getFileList(input);
	list = Array.sort(list);


// TODO make the list able to be started at any i_start from the file list 
// The problem is how to get the index of the file we want 
// This is the best I can do for now

var i_start = getNumber("What image do you want to start on? First image is no. 0", 0);

if(i_start > list.length){

 exit("Error: There is a total of " + list.length - 1 + " images.\nChoose a lower number");

}

	for (i = i_start; i < list.length; i++){


// Warn the user about how many files are left
// Ask for input about doing analysis

// continue_analysis = getBoolean("This is the file no." + i+1 + " of " + list.length + " files in input folder.\nContinue?");	

// stop the program if user wants to leave

//if(continue_analyis == false){
//   	exit("Alright, see you nect time!");
//	}

// print the path that we are trying to open
// print("Trying to open " + input + File.separator + list[i]);

// open file
open(input + File.separator + list[i]);

// run the adjust
run("Color Balance...");	
waitForUser("User action required", "Adjust color balance in the image, \nApply changes, then press OK");



setTool("polygon");

waitForUser("User action required", "Select polygon around region \nThereafter, press OK");
	// Select a point and print the coords
	coord = Roi.getCoordinates(x,y);

 
   // check number of entries in each list 
   if(x.length < 5){

    close();
   	exit("Error: Select a polygon with at least 6 vertices");
   	
   }
   	
     
	// clear to black outside selection
    setBackgroundColor(0, 0, 0);
    run("Clear Outside");


// Make RGB
run("Select None");
run("Stack to RGB");

// close the composite
close(list[i]);
	

// Rotate image
/////////////////////////////////////////////

// run("To Bounding Box");
// run("Select All");
run("Rotate... ");

satisfied_with_rotation = false;

while(satisfied_with_rotation == false){

satisfied_with_rotation = getBoolean("Are you satisfied with the rotation?");	

if(satisfied_with_rotation == true){
 
 // do nothing
 
 } else {
run("Undo"); 

// rotate doesn't seem to work here so...
close();
waitForUser("User action required", "Really sorry...\nImageJ sucks and can't figure out this one. \nStart again");

// roll back counter
i = i - 1;


// exit the while loop
satisfied_with_rotation = true;

}	
	
}
	
// Call the flipping function
flipping_images = true;

while (flipping_images){
flipImage();

satisfied_flipping = getBoolean("Have you finished flipping?");	

	if(satisfied_flipping == true){
  
  		flipping_images = false;
  		print("Finished image " + i + 1 + " of " + list.length + " Continue!");
  		run("Select None");

	}
}


// use it to make the output name
saveAs("Tiff", output + File.separator + list[i]);
close();

}


}


function flipImage(){

// Flip horizontally or vertically

Dialog.create("Flip image");
Dialog.addCheckbox("Flip Horizontally", false); 
Dialog.addCheckbox("Flip Vertically", false); 


Dialog.show();

flip_horizontal = Dialog.getCheckbox(); 
flip_vertical = Dialog.getCheckbox();


if (flip_horizontal) run("Flip Horizontally");
if (flip_vertical) run("Flip Vertically");

	
}

