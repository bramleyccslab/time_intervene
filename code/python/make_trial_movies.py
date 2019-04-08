"""convert physics data to movie"""

#make_movies.py

import json
import math
import colorsys
import os
import glob

import gizeh as gz
import numpy as np
import moviepy.editor as mpy
from scipy import misc
from os import listdir

#Set constants
RAD = 25
W = 400
H = 400
# H_outer = 500
N_OBJ=3


#Load data
with open('cc_example_data.json') as data_file:    
    events = json.load(data_file)

image = misc.imread('cursor.jpg')
#image_small = misc.imresize(image, 25)
#colors = [(1,1,0,),(0,0,0)]

#print colors, np.random.rand(N_OBJ, 3)
labels = ['A','B','C']

centers = [(150, 400-286.6),(150, 400-113.4),(286.6, 200)]
#[(200, 400-286.6),(400-286.6, 250),(400-113.4, 250)]

def make_frame(t):
    
    frame = int(math.floor(t*60))
    print(frame)

    #Essentially pauses the action if there are no more frames and but more clip duration
    if frame >= len(events["A"]):
        frame = len(events["A"])-1

    #White background
    surface = gz.Surface(W,H, bg_color=(1,1,1))            

    #Pucks
    for label, center in zip(labels, centers):

        xy = [center[0], center[1]]

        if events[label][frame]==1:
            #Event
            ball = gz.circle(r=RAD, fill=(1,1,0), stroke=(0,0,0), stroke_width=1).translate(xy)
            ball.draw(surface)

            # text = gz.text(label, fontfamily="Helvetica", 
            # fontsize=25, fontweight='bold', fill=(0.2,0.2,0.2), xy=xy) #, angle=Pi/12
            # text.draw(surface)
        elif events[label][frame]==2:
            #Action/intervention
            ball = gz.circle(r=RAD, fill=(1,1,0), stroke=(0,0,0), stroke_width=6).translate(xy)
            ball.draw(surface)

            text = gz.text('+', fontfamily="Helvetica",  fontsize=35, fontweight='bold', fill=(0,0,0), xy=xy) #, angle=Pi/12
            text.draw(surface)

            # timeline = gz.rectangle(251,201, fill=gz.ImagePattern(image), xy=xy)
            # timeline.draw(surface)
        elif events[label][frame]==0:
            #Inactive
            ball = gz.circle(r=RAD, fill=(.7,.7,.7), stroke=(0,0,0), stroke_width=1).translate(xy)
            ball.draw(surface)
                    #Letters
            # text = gz.text(label, fontfamily="Helvetica", 
            # fontsize=25, fontweight='bold', fill=(0.2,0.2,0.2), xy=xy) #, angle=Pi/12
            # text.draw(surface)


    #Mouse cursor
    # cursor_xy = np.array([this_events['mouse']['x'][frame]*RATIO, this_events['mouse']['y'][frame]*RATIO])
    # cursor = gz.text('+', fontfamily="Helvetica",  fontsize=25, fill=(0,0,0), xy=cursor_xy) #, angle=Pi/12
    # cursor.draw(surface)
    
    return surface.get_npimage()  

#Create the clip
duration = len(events['A'])/60
clip = mpy.VideoClip(make_frame, duration=duration)#, fps=60?\

#Create the filename (adding 0s to ensure things are in a nice alphabetical order now)
writename = '../../movies/cc_example.mp4'
print(writename)

#Write the clip to file
clip.write_videofile(writename, fps=24)#
#clip.write_gif("balls.gif",fps=15,opt="OptimizePlus")
