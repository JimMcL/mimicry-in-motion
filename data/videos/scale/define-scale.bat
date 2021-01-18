@echo off
@rem Usage : define-scale.bat <scale-file>
@rem
@rem You will be prompted to enter a real-world distance, and drag out a line on the image with the specified distance. The scale will be displayed in the dialog, with the option of copying it to the cliupbopard in a format suitable for adding to the command line

set TR=C:\Jim\uni\apps\YetAnotherTracker\YetAnotherTracker.bat 

@REM Luckily, all videos with associated scale images have the same size, 1920x1080
%TR%  -v --view-scale ? --resize 1920 %*
