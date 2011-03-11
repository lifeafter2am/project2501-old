#!/bin/bash
 
# Load resources
 
xrdb -merge .Xresources
 
# Set Dual or Single Screen Wallpapers
nitrogen --restore &

# Fire up apps
urxvtd &
 
exec xmonad

