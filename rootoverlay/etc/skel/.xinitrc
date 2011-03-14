#!/bin/bash
 
# Load resources
 
xrdb -merge .Xresources
 
# Set Wallpaper
cat ~./fehbg &

# Fire up apps
urxvtd &
 
exec xmonad

