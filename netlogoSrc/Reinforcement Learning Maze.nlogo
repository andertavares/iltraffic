patches-own[reward Qlist]
breeds [walker]
globals[ave-reward r-episode episode]

walker-own [Qsa Hlist]

to setup ;--this builds the maze that the walker has to learn to get through
ca
  let x screen-edge-x 
  let y screen-edge-y
  ;--patches are the individual states: set up color, reward, and inital Q(s,a) values
  ask patches [ set pcolor green - 1]
  ask patches with [pxcor = x or pxcor = (- x) or pycor = y or pycor = (- y)][set pcolor blue]   
  ask patches with [pycor = -1][set pcolor blue]
  ask patch -4 -4 [set pcolor green - 2]
  ask patch -1 2 [set pcolor blue ask random-one-of neighbors4 [set pcolor blue]]
  ask patch 1 1 [set pcolor blue ask random-one-of neighbors4 [set pcolor blue]]
  ask patch -2 -3 [set pcolor blue ask random-one-of neighbors4 [set pcolor blue]]
  ask random-one-of patches with [pycor = -1 and pxcor != (- x) and pxcor != x]
    [set pcolor green - 1 ask patches with [pycor = pycor-of myself + 1 or pycor = pycor-of myself - 1 and pxcor = pxcor-of myself]
      [if pcolor != pcolor-of myself
        [set pcolor pcolor-of myself]
      ]
    ]
  ask patches [ifelse pcolor > 70 [set reward ( -10)][set reward 0]]
  ask patch 4 4 [set pcolor green set reward 10] 
  ask patches [set plabel reward] 
  ;--initailize Q(s,a) values for the patches
  ask patches [set Qlist [0 0 0 0]] ;--this list stores the reward values of the state-action mapping
  ;--intialize walker
  create-custom-walker 1 [ set shape "ant 2" set color red - 1 setxy -4 -4 set size .5 set Hlist [90 180 270 0]]
end

to go
  set episode 1
  while [episode <= num-episodes]
    [set r-episode []
    ask walker [setxy -4 -4]
    pen-down
      set Qsa 0
      while [reward-of patch-here = 0]
      [
      ;--from current state find maximun action
      ;let Qnew max Qlist ;--find the max value of Qsa from this patch
      ;--better search
      let Qnew 1
      let Qmax 0
      let dirp 0
      
      let rand random-float 1
      ifelse rand <= exploration-%
        [
        let dir random-one-of Hlist ;--pick random direction
        set dirp position dir Hlist ;--find dir's position in the Hlist array
        set Qmax max Qlist ;--get max from the Qlist values of the current patch
        set Qnew item dirp Qlist ;--find the value in Qlist with the same position as in the Hlist
        ]
      [
      while [Qnew != Qmax]
      [
      let dir random-one-of Hlist ;--pick random direction
      set dirp position dir Hlist ;--find dir's position in the Hlist array
      set Qmax max Qlist ;--get max from the Qlist values of the current patch
      set Qnew item dirp Qlist ;--find the value in Qlist with the same position as in the Hlist
      ]
      ]
      set heading item dirp Hlist
      let r reward-of patch-ahead 1
      set r-episode lput r r-episode
      ;-- Q-learning update function
      let QQnew max Qlist-of patch-ahead 1
      set Qnew Qnew + step-size * (r + discount * QQnew - Qnew) ;--perform Q-Learning
      set Qnew precision Qnew 3
      set Qlist replace-item dirp Qlist Qnew
      fd 1
      ]
      let lng length r-episode
      let lngsum sum r-episode
      set ave-reward lngsum / lng
      set-current-plot "Ave Reward Per Episode"
      plot ave-reward
      print "--------episode " + episode
    set episode episode + 1
    pen-erase
    ]
end
@#$#@#$#@
GRAPHICS-WINDOW
204
10
764
591
5
5
50.0
1
10
1
1
1
0
1
1
1

CC-WINDOW
5
605
773
700
Command Center
0

BUTTON
2
10
65
43
NIL
setup
NIL
1
T
OBSERVER
T
NIL

SLIDER
2
77
174
110
step-size
step-size
0
1
1.0
0.1
1
NIL

SLIDER
2
110
174
143
discount
discount
0
1
0.5
0.1
1
NIL

PLOT
3
176
203
326
Ave Reward Per Episode
episode
reward
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -2674135 true

SLIDER
2
44
174
77
num-episodes
num-episodes
0
100
100
1
1
NIL

BUTTON
65
10
120
43
NIL
go
NIL
1
T
TURTLE
T
NIL

BUTTON
121
10
187
43
Clear Path
cd
NIL
1
T
OBSERVER
T
NIL

SLIDER
2
142
174
175
exploration-%
exploration-%
0
0.3
0.0
0.01
1
NIL

@#$#@#$#@
WHAT IS IT?
-----------
This model implements Q-learning (Watkins 1989) a one-step temporal difference algorithm in the area of reinforcement learning, a branch of artificial intelligence and machine learning. 


HOW IT WORKS
------------
The agent (ant) moves to a high value patch, receives a reward, and updates the previous patches learned values with the received reward using the following algorithm:

Q(s,a) = Q(s,a) + step-size * [reward + discount * max(Q(s’,a’)) – Q(s,a)] 

The agent keeps moving until it hits a blue patch with a -10pts reward or the goal patch with +10pts reward, which results in a new episode and resetting of the agent to the starting position.


HOW TO USE IT
-------------
The buttons and sliders control the setup and all the parameters inside the algorithm. The graph provides the average reward on obtained per episode. The step-size parameter is the amount old values are updated towards new values. Discount is the present value worth of future rewards. Exploration-% is the amount moves the agent takes towards a non-optimum patch, which can help the agent explore more of the maze and not get stuck in local optimums.

THINGS TO NOTICE
----------------
The average reward in the graph increases over the number of episodes that the agent has trained on, which shows the learning process of the agent.


THINGS TO TRY
-------------
Experiment with the algorithm parameters such as step-size, discount, and exploration-%.


EXTENFDING THE MODEL
-------------
Implement different reward schemes allowing more direct and optimal paths, such as -1pts for every move the agent makes forcing the agent to find a more direct approach to the goal square.


CREDITS AND REFERENCES
----------------------
Written by Joe Roop (Spring 2006): Joseph.Roop@asdl.gatech.edu
Graduate Research Assistant 
Aerospace Systems Design Laboratory (ASDL): http://www.asdl.gatech.edu/
Georgia Institute of Technology


References:
1.	Sutton, R. S., Barto, A .G. (1998) Reinforcement Learning: An Introduction. MIT Press
2.	Watkins, C. J. C. H. (1989) Learning from Delayed Rewards. Ph.D. thesis, Cambridge University. 

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

ant 2
true
0
Polygon -7500403 true true 150 19 120 30 120 45 130 66 144 81 127 96 129 113 144 134 136 185 121 195 114 217 120 255 135 270 165 270 180 255 188 218 181 195 165 184 157 134 170 115 173 95 156 81 171 66 181 42 180 30
Polygon -7500403 true true 150 167 159 185 190 182 225 212 255 257 240 212 200 170 154 172
Polygon -7500403 true true 161 167 201 150 237 149 281 182 245 140 202 137 158 154
Polygon -7500403 true true 155 135 185 120 230 105 275 75 233 115 201 124 155 150
Line -7500403 true 120 36 75 45
Line -7500403 true 75 45 90 15
Line -7500403 true 180 35 225 45
Line -7500403 true 225 45 210 15
Polygon -7500403 true true 145 135 115 120 70 105 25 75 67 115 99 124 145 150
Polygon -7500403 true true 139 167 99 150 63 149 19 182 55 140 98 137 142 154
Polygon -7500403 true true 150 167 141 185 110 182 75 212 45 257 60 212 100 170 146 172

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mouse top
true
0
Polygon -7500403 true true 144 238 153 255 168 260 196 257 214 241 237 234 248 243 237 260 199 278 154 282 133 276 109 270 90 273 83 283 98 279 120 282 156 293 200 287 235 273 256 254 261 238 252 226 232 221 211 228 194 238 183 246 168 246 163 232
Polygon -7500403 true true 120 78 116 62 127 35 139 16 150 4 160 16 173 33 183 60 180 80
Polygon -7500403 true true 119 75 179 75 195 105 190 166 193 215 165 240 135 240 106 213 110 165 105 105
Polygon -7500403 true true 167 69 184 68 193 64 199 65 202 74 194 82 185 79 171 80
Polygon -7500403 true true 133 69 116 68 107 64 101 65 98 74 106 82 115 79 129 80
Polygon -16777216 true false 163 28 171 32 173 40 169 45 166 47
Polygon -16777216 true false 137 28 129 32 127 40 131 45 134 47
Polygon -16777216 true false 150 6 143 14 156 14
Line -7500403 true 161 17 195 10
Line -7500403 true 160 22 187 20
Line -7500403 true 160 22 201 31
Line -7500403 true 140 22 99 31
Line -7500403 true 140 22 113 20
Line -7500403 true 139 17 105 10

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

tank
true
0
Rectangle -7500403 true true 144 0 159 105
Rectangle -6459832 true false 195 45 255 255
Rectangle -16777216 false false 195 45 255 255
Rectangle -6459832 true false 45 45 105 255
Rectangle -16777216 false false 45 45 105 255
Line -16777216 false 45 75 255 75
Line -16777216 false 45 105 255 105
Line -16777216 false 45 60 255 60
Line -16777216 false 45 240 255 240
Line -16777216 false 45 225 255 225
Line -16777216 false 45 195 255 195
Line -16777216 false 45 150 255 150
Polygon -7500403 true true 90 60 60 90 60 240 120 255 180 255 240 240 240 90 210 60
Rectangle -16777216 false false 135 105 165 120
Polygon -16777216 false false 135 120 105 135 101 181 120 225 149 234 180 225 199 182 195 135 165 120
Polygon -16777216 false false 240 90 210 60 211 246 240 240
Polygon -16777216 false false 60 90 90 60 89 246 60 240
Polygon -16777216 false false 89 247 116 254 183 255 211 246 211 237 89 236
Rectangle -16777216 false false 90 60 210 90
Rectangle -16777216 false false 143 0 158 105

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
