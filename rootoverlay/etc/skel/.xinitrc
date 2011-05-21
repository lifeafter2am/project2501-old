#!/bin/bash
 
# Load resources
 
xrdb -merge .Xresources
 
# Set Wallpaper
feh --bg-fill '/home/pptmstr/wallpaper.jpg' &

# Fire up apps
urxvtd &
 
exec xmonad

