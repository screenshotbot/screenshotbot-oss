# PixelDiff

A visual image comparison tool for pixel-perfect difference
detection. Designed for use with Screenshot tests.

## Overview

PixelDiff is an open source GUI application for comparing two images
side-by-side and highlighting pixel-level differences. It's designed
for developers, designers, and QA professionals who need to visually
validate changes between image versions.

## Features

- **Side by Side Comparison**: View original and updated images simultaneously
- **Difference Highlighting**: Visual overlay showing exactly which pixels have changed
- **Interactive Navigation**: Pan and zoom to examine differences in detail
- **Multiple View Modes**: Toggle between previous image, updated image, and difference overlay
- **Zoom to Changes**: Automatically focus on areas with detected differences
- **Pixel-Level Analysis**: Hover over pixels to see exact color values and changes
- **Transparency Support**: Proper handling of images with alpha channels

## Usage

TODO


## View Modes

- **Previous**: Shows only the original image
- **Diff**: Highlights changed pixels in red overlay (default)
- **Updated**: Shows only the new image

## Controls

### Mouse Controls
- **Click & Drag**: Pan around the image
- **Mouse Hover**: Display pixel color information

### Keyboard Shortcuts
- **V**: Toggle between Previous/Updated views
- **+ / -**: Zoom in/out at cursor position

### Interface Elements
- **View Radio Panel**: Switch between Previous, Diff, and Updated modes
- **Zoom to Change Button**: Automatically center on first detected difference
- **Status Bar**: Shows pixel information and color values

## Known Bugs

- **MouseWheel does not work** Our GUI library does not propagate
  mouse wheel events so you currently can't zoom with mouse wheel. Use
  +/- to zoom. We have a workaround that we hope to release in the next version.
  
- **Zoom animations are sometimes janky** We think this should be easy to fix by
  the next version.

## Implementation

PixelDiff is implemented in Common Lisp using the CAPI (Common
Application Programmer Interface) for cross-platform GUI
functionality. The core comparison algorithm performs pixel-by-pixel
analysis to detect and highlight differences with sub-pixel accuracy.

## License

Open source - see LICENSE file for details.

