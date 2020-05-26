## Requirements

We need to make sure people have imageMagick

On macOS

```
brew install imagemagick
```

## match_image_to_atlas

People will try to close the windows. Because we are only opening the windows once and using the window by their name. It can create conflicts.

TODO: maybe check whether window has been opened ?

## improvements 

have the ability to quickly scan and discard one particular section.  
For example, this can be useful when sections are broken.

~~Speed. pull_atlas gets **really really** slow when too many vectors have to be drawn.~~ 
A faster version has been implemented.
