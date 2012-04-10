globals
  [
  x 
  y
  sim-time ;--the clock of sim needed for fire-rate of agent
  episode ;--the number of runs/trials that the agent is on
  runs-since-setup ;--
  ]
  
breeds [agent weapon]

weapon-own 
  [
  target
  intelligent ;--it's not right now
  ]

agent-own 
  [
  ;----physical attributes----
  intelligent ;--true/false if true then agent can make up state-action list and learn
  x-start ;--intial place at the beggining of episode
  y-start ;--
  myStealth
  Team ;--says what team each agent is on
  dead ;--true/false
  num-hit ;--number of hits an agent can take before dead, gets decreased
  target ;--single agent
  target-list ;--list of agents for targeting
  detection-list ;--holds the agents that were detected from the last state-action, used to alert when time to make a decision based on enemy dist
  num-wep ;--number of shots/weapons an agent has
  num-wep-total ;--total number of weapons doesn't change
  fire-rate ;--used to decide how often agent can shoot uses mod on sim-time
  radius ;--serves for both detection and weapon dist radius/range
  direction ;--used to hold data for the direction in the evade routine
  speed ;--speed the agent can move at 
  reward-hit ;--the number that is sent to the learning agent
  reward-killed ;--the number that is sent to the learning agent
  reward-wep-use ;--the number that is sent to the learning agent
  reward-else ;--use to set the reward for every other state that doesn't have a reward
  make-decision ;--true/false, used to let agent know when can jump out of actions and select new one
  mission-target ;--the target that the agents are trying to kill
  
  
  ;----Q-Learning vars----
  r ;--the value of reward that is given by the other agent, used for Q-Learning alg
  r-list-epi ;--the average reward the agent has until dead, or reaching an other end state
  r-ave-epi ;--total reward over many states
  state-target-list ;--close-target-list ;--holds the controller, sam1, and sam2 based on distances
  state-action-list ;--holds all state-action values ever visited be the agent
  state ;--holds the state vector in string form and is used for comparison 122211220012
  Q-list ;--holds the state-action values for the current state [0 0 0 0 0 0 0 0]
  state-pos ;--holds the position of the state within the state-action-list
  Q-list-pos ;--holds the position of the Q-list within the state-action-list
  Q ;--holds the state-action value in the list that is selected
  Q-pos ;--holds the position of the Q inside the Q-list
  Qmax ;--holds the max value inside the Q-list
  state-old ;--
  Q-list-old ;--
  state-pos-old ;--
  Q-list-pos-old ;--
  Q-old ;--
  Q-pos-old ;--
  ]
  
to setup
  ca
  random-seed rand-seed ;--can change this only used for repeatability
  set runs-since-setup 0
  print "-----setup-----"
  set x screen-edge-x 
  set y screen-edge-y
  ask patches 
    [
    ;set pcolor white
    if pxcor = x or pxcor = (- x) or pycor = y or pycor = (- y)
      [];set pcolor gray]
    ]
  set-default-shape weapon "line" ;--so when you hatch one it knows what shape to be
  cct-agent 1 ;--create the intelligent agent
    [
    set x-start (- x + 1) ;random (- x + 1)
    set y-start (- y + 1) ;random (- y + 1)
    set Team "team1"
    set color blue
    initialize-agent Team ;--color Team
    set reward-hit (-0) ;--put in as when I get hit
    set reward-killed (-200)
    set reward-wep-use (-2) ;--cause reward = -3 for -4 
    set reward-else -0 
    if stealth = true
      [set reward-else -0]
    if scenario = "random"
      [
      set xcor x-start
      set ycor y-start 
      ]
    ]
  cct-agent 0 ;--create the intelligent agent
    [
    set x-start (- x + 1) ;random (- x + 1)
    set y-start (- y + 1) ;random (- y + 1)
    set Team "team1"
    set color green
    initialize-agent Team ;--color Team
    set reward-hit (-0) ;--put in as when I get hit
    set reward-killed (-200)
    set reward-wep-use (-2) ;--cause reward = -3 for -4 
    set reward-else -0 
    if scenario = "random"
      [
      set xcor x-start
      set ycor y-start 
      ]
    ]
  cct-agent SAM-sites ;--create the hand coded sam sites
    [
    set x-start random (x - 1)
    set y-start random (y - 1)
    set Team "team2"
    set color red
    initialize-agent Team ;--color Team
    set reward-hit (-0) ;--put in as when I get hit and if someone hits you they get + these points
    set reward-killed (-2) ;--this contradicts above line 
    set reward-wep-use (-0)
    set reward-else -0 
    if scenario = "random"
      [
      set xcor x-start
      set ycor y-start 
      ]
    ]
  cct-agent 1 ;--make this one the mission-target agent
    [
    set x-start random (x - 1)
    set y-start random (y - 1) 
    set Team "team2"
    set color red
    initialize-agent Team ;--color
    set reward-hit (-0) ;--put in as when I get hit
    set reward-killed (-1000)
    set reward-wep-use (-0)
    set reward-else -1
    if scenario = "random"
      [
      set xcor x-start
      set ycor y-start 
      ]
    ]
  ask agent 
    [
    get-scenario
    ifelse min-one-of agent with [color = red][reward-killed] = self
      [set label "target" ];set label-color black]
      [set label who ];set label-color black] ;--display agent number
    ]
  ask agent with [intelligent = true][without-interruption [load-state-action-file]] ;--load existing data from file 
end

to print-profile
  print "*****game profile*****"
  print "runs-since-setup " + runs-since-setup;--need number of runs total
  print "alpha " + step-size;--alpha
  print "gamma " + discount;--gamma
  print "e-greed " + exploration-%;--exploration-%
  print "rand-seed " + rand-seed;--rand-seed
  print "scenario " + scenario;--scenario 
  print "blue agents " + count agent with [Team = "team1"]
  print "red agents " + count agent with [Team = "team2"]
  ask agent 
    [
    print "------agent # " + who + "------"
    print "x = " + x-start + " y = " + y-start
    print "reward-hit " + reward-hit
    print "reward-killed " + reward-killed
    print "reward-wep-use " + reward-wep-use
    print "reward-else "  + reward-else
    print "total-num-wep " + num-wep-total
    print "radius " + radius
    print "speed " + speed 
    ]
    
end

;----used to setup the agents----
to initialize-agent [inTeam]
 
  if inTeam = "team1"
    [
    set shape "airplane"
    set dead false 
    set intelligent true
    set num-hit 1
    set myStealth stealth
    ifelse myStealth = true
      [
      set num-hit 99999
      set reward-hit 0
      ]
      []
    set target-list []
    set num-wep 1
    set num-wep-total num-wep
    set fire-rate 1
    set radius 2.5
    set speed .5
    ifelse intelligent = true 
      [set make-decision true]
      [set mission-target nobody]
    set detection-list []
    if scenario = "random"
      [  
      set xcor x-start
      set ycor y-start 
      ]
    ]
    if inTeam = "team2"
    [   
    set shape "x"
    set dead false 
    set intelligent false
    set num-hit 1
    set target-list []
    set num-wep 1
    set num-wep-total num-wep
    set fire-rate 1
    set radius 2
    set speed 0
    ifelse intelligent = true 
      [set make-decision true]
      [
      set mission-target nobody
      set make-decision false
      ]
    set detection-list []
    ]
end
;-------end set up of agents----

;=======Main Run Function=========
to go ;--the agents keep going as long as they have not reached the end of their episodes
  print "--------New set of Episodes--------"
  set episode 1 ;--a global counter
  while [episode <= num-episodes] ;--compare to global slider bar
    [
    ask weapon [die] ;--clear the weapons off the screen for the next episode
    print "---Episode " + episode + "---"
    set sim-time 1
    ask agent 
      [
      initialize-agent Team-of self;--set all phys parameters to same as setup
      get-scenario ;--x, y layout for the agents
      ;without-interruption 
       ; [
        if intelligent = true ;--if intelligent, set the mission target
          [
          set mission-target min-one-of agent with [color != color-of myself][reward-killed] ;--mission target is the target with max rewrds 
          set r-list-epi [] ;--list to hold the reward recieved after each stat-action, held by each agent only useful for the intelligent ones
          ]
        get-target-list ;--set up target-list all agents get a list of agents with opposite color and sort them by distance length 3 for intelligent agent
        total-state-action ;--if intelligent, set state-action and append to current state-action-list, i.e. "1222122011" [0 0 0 0 0 0 0]
       ; ] 
      ] 
    let done false
    while [done = false] ;--this contains the end conditions     
      [
      ask agent
        [
        if done = false
          [
          ;print " agent # " + who
          take-action ;--agents do the appropiate actions, sets end state, gets rewards
          perform-make-decision ;-- contains Q-Learning, if you are this current agent and have not been interrupted by another agent then perform Q-Learning 
          set done get-end-state
          ]
        ]
      set sim-time sim-time + 1 
      ]
    ;print "end of episode....."
    ask agent
      [
      if intelligent = true
        [
        ifelse empty? r-list-epi = true
          [set r-ave-epi 0]
          [set r-ave-epi mean r-list-epi]
        ;show "number of actions for episode " + length r-list-epi
        ;show "reward list for episode " + r-list-epi 
        ]
      ]
    set episode episode + 1
    set runs-since-setup runs-since-setup + 1
    set-plot
    ]
    ask agent with [intelligent = true]
      [
      show "State-Action-List " + state-action-list
      show "Number of states " + length state-action-list / 2
      ]
    print "total runs since setup " + runs-since-setup   
end
;=======End Main Run Function======

to perform-make-decision
  if make-decision = true and intelligent = true 
  [
  without-interruption
    [
    get-old-to-new ;--sets old states = new states
    get-target-list ;--set up target-list all agents get a list of agents with opposite color and sort them by distance length 3 for intelligent agent
    total-state-action ;--if intelligent, set state-action and append to current state-action-list, i.e. "1222122011" [0 0 0 0 0 0 0]
    ;--perform Q-Learning
    ;show "--old state-action " + state-old + "-" + Q-list-old
    ;show "new state-action " + state + "-" + Q-list 
    ;show "reward " + r
    set Q-old Q-old + step-size * (r + (discount * Qmax) - Q-old)
    set Q-old precision Q-old 4 
    ;--update the state-action-list
    set Q-list-old replace-item Q-pos-old Q-list-old Q-old 
    set state-action-list replace-item Q-list-pos-old state-action-list Q-list-old 
    ;show "updated old state-action " + state-old + "-" + Q-list-old
    set r-list-epi lput r r-list-epi
    ]
  ]
end


;------Action Section-----------------------------------------------------------------------------------
to take-action
  move
  evade
  fire ;without-interruption [fire]
end

to move
  if dead = false and speed != 0 and empty? target-list = false and make-decision = false
    [
    if intelligent = true and Q-pos > 1 and Q-pos <= 4 ;--if not intelligent just executes the function
      [
      if Q-pos = 2
        [set target item 0 target-list]
      if Q-pos = 3
        [set target item 1 target-list]
      if Q-pos = 4
        [set target item 2 target-list]
      ;show "move to -----" + target
      while [speed <= distance-nowrap target and make-decision = false and dead = false]
        [
        set heading towards-nowrap target
        ifelse agent-path = true
          [
          pen-down
          fd speed / 2
          pen-up
          fd speed / 2
          ]
          [fd speed]
        ask agent [detect]
        ]
      if speed <= distance-nowrap target and make-decision = true and dead = false
        [set r reward-else]
      set r reward-else ;--give reward for nothing happening
      if dead = false
        [set make-decision true]
      ;perform-make-decision ;--do Q-Learning 
      ]
    if intelligent = false
      [
      show who
      set target item 0 target-list
      while [speed <= distance-nowrap target and dead = false]
        [
        set heading towards-nowrap target
        fd speed
        
        ]
      ]
    ]
end

to evade
  if dead = false and speed != 0 and empty? target-list = false and make-decision = false
    [
    if intelligent = true and Q-pos <= 1 ;--if not intelligent just executes the function
      [
      let dir "N/A"
      ifelse Q-pos = 0 ;--two different turnning styles
;        [set direction (- 15) set dir "left"]
;        [set direction 15 set dir "right"]
        [set heading heading - 90 set direction 10 set dir "left"] ;--change this number to decrease/increase amount of steps the evade routine takes
        [set heading heading + 90 set direction (- 10) set dir "right"]
      ;show "evade -----" + dir
      let turns abs (round (90 / direction))
      while [turns > 0 and make-decision = false and dead = false]
        [
        if (abs pxcor-of patch-ahead speed = screen-edge-x)
          [set heading (- heading) set make-decision true]
        if (abs pycor-of patch-ahead speed = screen-edge-y)
          [ set heading (180 - heading) set make-decision true ]
        set heading heading + direction
        ifelse agent-path = true
          [
          pen-down
          fd speed / 2
          pen-up
          fd speed / 2
          ]
          [fd speed]
        set turns turns - 1
        ask agent [without-interruption[detect]] ;without-interruption
        ]
      ;show "evade to " + dir
      if dead = false
        [
        set r reward-else  
        set make-decision true
        ]
      ;perform-make-decision ;--do Q-learning 
      ]
    if intelligent = false
      [
      ;--can't evade right now, no need
      ]
    ] 
end

to fire
  if dead = false and empty? target-list = false and num-wep > 0
  [
  if intelligent = true and Q-pos > 4 and make-decision = false and dead = false
    [ 
    if Q-pos = 5
      [set target item 0 target-list]
    if Q-pos = 6
      [set target item 1 target-list]
    if Q-pos = 7
      [set target item 2 target-list]
    ;show "fire at -----" + target
    set num-wep num-wep - 1
    set r reward-wep-use ;--reward for firing a weapon
    ifelse distance-nowrap target <= radius
      [
      hatch-weapon 1 ;--if agent hits target
        [
        set intelligent false
        set heading towards-nowrap target
        set size distance-nowrap target
        jump size / 2
        ]
      ask target [without-interruption[hit]] ;-- make sure the target is not dead when giving out reward
      ]
      [
      hatch-weapon 1 ;--if agent misses, out of range
        [
        set intelligent false
        set color color - 2
        set heading towards-nowrap target
        set size radius-of myself
        jump radius-of myself / 2
        ]
      set make-decision true
      ]
    ]
  if intelligent = false and distance-nowrap first target-list <= radius and sim-time mod fire-rate = 0 and dead-of first target-list = false and dead = false;--if you are a hand coded agent then can only shoot when have a target in range
    [
    set target first target-list
    if myStealth-of target = false
    [
    ;show "fire at -----" + target
    set num-wep num-wep - 1
    hatch-weapon 1 ;--if agent hits target
      [
      set intelligent false
      set heading towards-nowrap target
      set size distance-nowrap target
      jump size / 2
      ]
    ask target [without-interruption[hit]]
    ]
    ]
  ]
end

to hit ;--is asked by the agent that fires weapon, so the agent being hit has to perfrom this function
  ;--performed by the target (agent being hit)
  set num-hit num-hit - 1
  if intelligent = true and dead = false ;--target
    [
    ifelse intelligent-of myself = false and dead = false;--shooter
      [
      ;print "non-intelligent killing intelligent"
      set r r + reward-hit ;--give intel agent points for getting hit
     ;print myself + " hit " + self + ":  reward to " + self + " " + r 
      if num-hit < 1
        [
        set r r + reward-killed ;--add the reward of being killed to the reward of getting hit
        ;print myself + " killed " + self + ":  reward to " + self + " " + r 
        set dead true
        set shape "fire"
        ]
      set make-decision true
      perform-make-decision      
      ]
      [print "Intelligent killing intelligent!"] ;--add more if have intelligent fighting intelligent
    ]
  if intelligent = false and dead = false ;--target
    [
    ifelse intelligent-of myself = true and dead = false;--shooter
      [
      ;print "intelligent killing non-intelligent"
      set r-of myself  r-of myself + (- reward-hit) ;--give intel agent that killed me points for getting hit
      ;print myself + " hit " + self + ":  reward to " + myself + " " + r-of myself 
      if num-hit < 1
        [
        set r-of myself r-of myself + (- reward-killed)
        ;print myself + " killed " + self + ":  reward to " + myself + " " + r-of myself 
        set dead true
        set shape "fire"
        ]
     set make-decision-of myself true
     ask myself [perform-make-decision]
      ]
      [print "non-intelligent killing non-intelligent"] ;--add more if needed
    ]
  ;print "hit reward of shooter " + r-of myself + " " + myself + "  hit reward of target " + r + " " + self 
end

to detect ;--just notifies the intelligent agent if distances between agents merit decision making
  if dead = false
    [
    let list-temp2 []
    let list-temp1 values-from agent with [Team != Team-of myself and distance-nowrap myself <= radius][self]
    ;show "inside my radius " + list-temp1
    if intelligent = true and list-temp1 != detection-list
      [
      set detection-list list-temp1
      set make-decision true ;--ask self that agents have come in/out of my detection
      ]
    if intelligent = false and list-temp1 != detection-list
      [
      ifelse length list-temp1 > length detection-list
        [
        foreach list-temp1 
          [
          if member? ? detection-list = false
            [set list-temp2 lput ? list-temp2]
          ]
        set detection-list list-temp1
        ]
        [
        foreach detection-list  
          [
          if member? ? list-temp1 = false
            [set list-temp2 lput ? list-temp2]
          ]
        set detection-list list-temp1
        ]
      foreach detection-list
        [ask ? [set make-decision true]] ;--ask all the intelligent agents that come in/out of my detection
      ]
    ;show "agents that are new in/out radius " + detection-list
    ]
end 

;-----End Action Section----------------------------------------------------------------------------------------------

;----to get scenario------
to get-scenario
  if scenario = "random"
      [  
      set xcor x-start
      set ycor y-start 
      ]
  if scenario = "linear"
    [
    let blue-count count agent with [Team = "team1"]
    let total-count count agent
    if Team = "team2" 
      [
      if who = blue-count 
        [setxy 0 0]
      if who = 2
        [setxy (x / 3) (y / 3) ]
      if who = total-count - 1 
        [setxy (x * (2 / 3)) (y * (2 / 3)) ]
      if who >= total-count 
        [
        set xcor x-start
        set ycor y-start
        ]
      ]
     if Team = "team1" 
       [
       set xcor x-start
       set ycor y-start
       set heading 45
       ]
    ]
end
;----to get scenario------

;--------get the max state-action value with e-greedy-------
to choose-state-action-value ;--gets the Q, Qmax, Q-list-pos. only need to pass in Q-list
  let rnd random-float 1 ;--generate a random number
  ;print rnd
  set Qmax max Q-list ;--find the max value from the Q-list list
  ifelse rnd < exploration-% ;--this allows for exploration of the policy space at level set by global slider bar
    [
    ;print "exploration"
    set Q-pos random length Q-list ;--choose a random position out of the Q-list
    set Q item Q-pos Q-list ;--find the position of the random value picked from the action-list
    ;print "Q " + Q
    ]
    [
    ;print "exploitation"
    set Q 9999999 ;--default setting to ensure the while loop is entered once
    while [Q != Qmax] ;--if not exploring make sure always take the max of current-action
      [
      set Q-pos random length Q-list
      set Q item Q-pos Q-list ;--find the position of the random value picked from the action-list
     ;print "Q " + Q
      ]
    ]
end
;--------end get the max state-action value with e-greedy---


;---------get the target-list together----------
to get-target-list ;--makes a list of all agents not your color 
  set target-list values-from agent with [Team != Team-of myself][self] 
  set target-list sort-by [distance-nowrap ?1 < distance-nowrap ?2] target-list
  if mission-target != nobody 
    [
    set target-list fput mission-target target-list ;--if you have a mission-target put it at the beginning
    set target-list remove-duplicates target-list
    if length target-list > 3
      [set target-list sublist target-list 0 3 ];--get the first three items for the state-action-list composition
    ]
end
;---------end get the target-list together-------

;---------get the state-list together--------------
to-report get-state ;--function puts together the state i.e. "122231100" 
  if intelligent = true and empty? target-list = false
    [
    let this-state transform-health self
    set this-state this-state + "" + transform-weapons self
    set this-state this-state + "" + transform-tech self
    set this-state this-state + "" + transform-relative-dist
    foreach target-list
      [
      set this-state this-state + "" + transform-health ?
      set this-state this-state + "" + transform-weapons ?
      set this-state this-state + "" + transform-dist self ?
      set this-state this-state + "" + transform-angle self ?
      ]
    report this-state
    ]
   report "not intelligent"
end

to-report transform-tech [inAgent]
  ifelse myStealth-of inAgent = false
    [report 0]
    [report 1]
end

to-report transform-health [inAgent] ;--to call need to set function equal to a variable
  ifelse dead-of inAgent = true 
    [report 0]
    [report 1]
end

to-report transform-weapons [inAgent]
  if num-wep-total-of inAgent = 0 or num-wep-of inAgent = 0
    [report 0]
  ifelse (num-wep-of inAgent) / (num-wep-total-of inAgent) <= .4
    [report 1]
    [report 2]
end

to-report transform-dist [intelAgent inAgent]
  let dist 0
  ask intelAgent [set dist distance-nowrap inAgent]
  if dist > radius-of intelAgent and dist <= radius-of inAgent
    [report 0]
  if dist <= radius-of intelAgent and dist <= radius-of inAgent 
    [report 1] 
  if dist > radius-of intelAgent and dist > radius-of inAgent 
    [report 2] 
  if dist <= radius-of intelAgent and dist > radius-of inAgent 
    [report 3]  
end

to-report transform-relative-dist ;--should always be length 3 as long as you start with at least 3, as the agents will just be dead but still in the game
  if length target-list = 3
    [
    if distance-nowrap item 0 target-list > distance-nowrap item 2 target-list
      [report 0]
    if distance-nowrap item 0 target-list <= distance-nowrap item 2 target-list and distance-nowrap item 0 target-list > distance-nowrap item 1 target-list
      [report 1]
    if distance-nowrap item 0 target-list <= distance-nowrap item 1 target-list
      [report 2]
    ]
  if length target-list = 2
    [
    if distance-nowrap item 0 target-list > distance-nowrap item 1 target-list
      [report 0]
    if distance-nowrap item 0 target-list <= distance-nowrap item 1 target-list
      [report 2]
    ]
  if length target-list = 1
    [report 2]
end

to-report transform-angle [intelAgent inAgent]
  let angle subtract-headings heading-of intelAgent towards-nowrap inAgent
  if angle < 180 and angle > 0  
    [report 1]
  if angle = 0 or angle = 180
    [report 0]
  if angle < 0 
    [report 2]
end
;---------end get the state-list together----------

;---------get the state-action list together--------
to get-state-action ;--sets the Q-list, Q-list-pos, state-pos, and appends the state and the action to the state-action-list 
  if intelligent = true
    [
    ifelse position state state-action-list = false ;--if no have to create the new actions for the state
      [
      set state-action-list lput state state-action-list ;--state to state-action-list
      set Q-list [0 0 1 0 0 1 0 0]  ;--make sure it has 8 actions initialize the action values to list after current-state
      set state-action-list lput Q-list state-action-list ;--append Q-list values to list
      set state-pos position state state-action-list
      set Q-list-pos state-pos + 1 ;--set the position of the Q-list in the state-action-list
      ]
      [
      set state-pos position state state-action-list ;--find the position 
      set Q-list-pos state-pos + 1
      set Q-list item Q-list-pos state-action-list ;--retrieve the Q-list from the state-action-list
      ]
    ]
    ;show "state-action-list from get-state-action" + state-action-list
end
;---------end get the state-action list together----

;------set up plot-------------
to set-plot    
  let num count agent with [intelligent = true]
  if num = 1
    [
    set-current-plot "Ave Reward Per Episode"
    set-current-plot-pen word "agent " (num - 1)
    set-plot-pen-color blue
    plot r-ave-epi-of min-one-of agent with [who = num - 1][who]
    ]
  if num = 2
    [
    set-current-plot "Ave Reward Per Episode"
    set-current-plot-pen word "agent " (num - 2)
    set-plot-pen-color blue
    plot r-ave-epi-of min-one-of agent with [who = num - 2][who]
    set-current-plot-pen word "agent " (num - 1)
    set-plot-pen-color green
    plot r-ave-epi-of min-one-of agent with [who = num - 1][who]
    ]
  if num = 3
    [
    set-current-plot "Ave Reward Per Episode"
    set-current-plot-pen word "agent " (num - 3)
    set-plot-pen-color blue
    plot r-ave-epi-of min-one-of agent with [who = num - 3][who]
    set-current-plot-pen word "agent " (num - 2)
    set-plot-pen-color green
    plot r-ave-epi-of min-one-of agent with [who = num - 2][who]
    set-current-plot-pen word "agent " (num - 1)
    set-plot-pen-color gray
    plot r-ave-epi-of min-one-of agent with [who = num - 1][who]
    ]
  if num = 4
    [
    set-current-plot "Ave Reward Per Episode"
    set-current-plot-pen word "agent " (num - 4)
    set-plot-pen-color blue
    plot r-ave-epi-of min-one-of agent with [who = num - 4][who]
    set-current-plot-pen word "agent " (num - 3)
    set-plot-pen-color green
    plot r-ave-epi-of min-one-of agent with [who = num - 3][who]
    set-current-plot-pen word "agent " (num - 2)
    set-plot-pen-color gray
    plot r-ave-epi-of min-one-of agent with [who = num - 2][who]
    set-current-plot-pen word "agent " (num - 1)
    set-plot-pen-color orange
    plot r-ave-epi-of min-one-of agent with [who = num - 1][who]
    
    ]
end
;------end set up plot---------


;------set old values to new----
to get-old-to-new ;--set old states to new states befroe getting new new state
  set state-old state
  set state-pos-old state-pos
  set Q-list-old Q-list
  set Q-list-pos-old Q-list-pos
  set Q-old Q
  set Q-pos-old Q-pos
end 
;------set old values to new----

;------collect end-state-----
to-report get-end-state ;--observer context if true episode over used in inner while, composed of dead intelligent, dead mission-target, and if any of the agents have no weapons left
  let dead-list []
  let agent-list values-from agent with [intelligent = true][self]
  foreach agent-list
    [
    if (dead-of ? = true) or (dead-of mission-target-of ? = true) or (num-wep-of ? < 1)
      [
      set dead-list lput ? dead-list
      ]   
    ]
  ;show dead-list
  ifelse length dead-list = 0
    [report false]
    [report true]
end
;------end collect end-state-

;------Total-State------
to total-state-action
  if intelligent = true and make-decision = true ;--if intelligent, set state-action and append to current state-action-list, i.e. "1222122011" [0 0 0 0 0 0 0]
  [
  ;without-interruption 
    ;[
    set state get-state ;--set the state, which is a string
    get-state-action ;--set the Q-list for the state, will also set Q-list-pos, state-pos, and appends the state and the action to the state-action-list 
    choose-state-action-value ;--find the action to take, sets 
    set make-decision false ;--so that the agent will jump into actions
    ;]
  ]
end
;------End Total-State--

;--------load the state-action file-------
to load-state-action-file ;--want to load all saved state-actions actions are loaded at the start of the 
  file-close
  set state-action-list []
  file-open breed + ".rtf"
  let var "N/A"
  set var file-read set runs-since-setup file-read
  while [file-at-end? = false]
      [set state-action-list lput file-read state-action-list]  
  file-close
  show "state-action-list from load file " + state-action-list
  show "number of total runs " + runs-since-setup
end
;--------end load the state-action file---

;---to clear state-action-file------
to clear-state-action-file
  ifelse turtle 0 != nobody and intelligent-of turtle 0 = true
    [
    ask turtle 0
      [
      file-close 
      ;--now write
      let file-name breed + ".rtf"
      if file-exists? file-name = true 
        [file-delete file-name] ;--this just makes sure that duplicate data is not appended
      set runs-since-setup 0
      file-open file-name
      file-write "runs-since-setup" file-print runs-since-setup
      file-close
      ]
    ]
    [print "cannot use this function now: see clear-state-action-file function"]
  print "*******cleared file:*********"  
end
;---to clear state-action-file------

;-------write the state-action file-------
to write-state-action-file  ;--linked to global button only opens the saved state-action-list and if current state matches the loaded list then replace the spot in the loaded list
  if intelligent = true
    [
    without-interruption 
      [
      ;--first open and collect old data
      let file-state-action-list []
      file-close ;--open the existing file and append to current state-action-list
      file-open breed + ".rtf"
      let var "N/A"
      let temp 0
      set var file-read set temp file-read
      set temp runs-since-setup - temp 
      ifelse temp < 0 
        [set runs-since-setup 0 print "error with number of runs"]
        [set runs-since-setup runs-since-setup + temp ] 
      while [file-at-end? = false]
          [
          set file-state-action-list lput file-read file-state-action-list
          ]
      ;show "file-state-action-list " + file-state-action-list  
      without-interruption
        [
        foreach file-state-action-list
          [
          if position ? file-state-action-list mod 2 = 0 
            [
            if member? ? state-action-list = false
              [
              let pos position ? file-state-action-list
              set state-action-list lput item pos file-state-action-list state-action-list
              set state-action-list lput item (pos + 1) file-state-action-list state-action-list
              ] 
            ]
          ]
        ] 
      show "state-action-list from write file " + state-action-list
      show "number of total runs " + runs-since-setup 
      file-close
      file-close 
      ;--now write
      let file-name breed + ".rtf"
      if file-exists? file-name = true 
        [file-delete file-name] ;--this just makes sure that duplicate data is not appended
      file-open file-name
      file-write var file-print runs-since-setup
      without-interruption
        [ 
        foreach state-action-list 
          [
            if position ? state-action-list mod 2 = 0 ;--find all the indexes of the state postions
            [
              let pos position ? state-action-list ;--find the index
              set pos pos + 1 ;--increase the index by 1
              let val item pos state-action-list ;--get the value of the index found
              file-write ?  ;--print out on a new line to the file, writes as a string
              file-print val
            ]
          ]
        ]
     file-close
     ]
   ]
end
;-------end write the state-action file-------

@#$#@#$#@
GRAPHICS-WINDOW
198
10
708
541
12
12
20.0
1
12
1
1
1
0
1
1
1

CC-WINDOW
5
555
717
650
Command Center
0

BUTTON
126
10
189
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
111
174
144
num-episodes
num-episodes
1
10001
351
10
1
NIL

BUTTON
133
77
196
110
NIL
go
NIL
1
T
OBSERVER
T
NIL

SLIDER
1
326
173
359
exploration-%
exploration-%
0
0.5
0.03
0.01
1
NIL

SLIDER
1
359
173
392
step-size
step-size
0
1
0.5
0.1
1
NIL

SLIDER
1
392
173
425
discount
discount
0
1
0.9
0.1
1
NIL

PLOT
503
385
703
535
Ave Reward Per Episode
Episode
Ave Reward
0.0
10.0
0.0
10.0
true
false
PENS
"agent 0" 1.0 0 -13345367 true
"agent 1" 1.0 0 -16777216 true
"agent 2" 1.0 0 -10899396 true
"agent 3" 1.0 0 -2674135 true

SLIDER
1
294
173
327
rand-seed
rand-seed
0
1000
1000
1
1
NIL

BUTTON
2
43
103
76
Clear Plots
clear-plot\ncd
NIL
1
T
OBSERVER
T
NIL

SWITCH
1
214
117
247
agent-path
agent-path
0
1
-1000

BUTTON
2
10
125
43
Clear/Create File
clear-state-action-file
NIL
1
T
OBSERVER
T
NIL

CHOOSER
0
248
138
293
scenario
scenario
"random" "linear"
1

BUTTON
2
77
131
110
Save Memory File
write-state-action-file
NIL
1
T
TURTLE
T
NIL

SWITCH
1
179
104
212
stealth
stealth
1
1
-1000

SLIDER
2
145
174
178
SAM-sites
SAM-sites
2
5
2
1
1
NIL

@#$#@#$#@

WHAT IS IT?
-----------
This model implements Q-learning (Watkins 1989) a one-step temporal difference algorithm in the area of reinforcement learning.


HOW IT WORKS
------------
The agent (strike aircraft, blue) has the ability to sense the state of the game in the form of health, distances, and number of weapons. After sensing the state and receiving a reward the agent can choose from 8 different actions to manipulate the state space such as evading left or right, flying towards a SAM, and firing a weapon towards the SAM. The following Q-Learning algorithm is used:

Q(s,a) = Q(s,a) + step-size * [reward + discount * max(Q(s’,a’)) – Q(s,a)] 

The agent keeps makes moves until it runs out of weapons, dies, or kills the ‘target’ SAM site. The rewards are -2pts for weapons use, -200pts for dying, and +1000pts for killing the ‘target’ SAM. The agent also has the option of turning on the stealth technology, which allows the agent the ability to not be seen by the SAM sites.


HOW TO USE IT
-------------
The buttons and sliders control the setup and all the parameters inside the algorithm. The graph provides the average reward on obtained per episode. The step-size parameter is the amount old values are updated towards new values. Discount is the present value worth of future rewards. Exploration-% is the amount moves the agent takes towards a non-optimum patch, which can help the agent explore more tactics and not get stuck in local optimums. 


THINGS TO NOTICE
----------------
The average reward in the graph increases over the number of episodes that the agent has trained on, which shows the learning process of the agent. With the stealth technology enabled does the agent perform different tactics? 


THINGS TO TRY
-------------
Experiment with the algorithm parameters such as step-size, discount, and exploration-%. Also, investigate the environmental parameters.


EXTENDING THE MODEL
-------------
Implement different reward schemes allowing more direct and optimal paths, such as -1pts for every move the agent makes forcing the agent to find a more direct approach to the ‘target’ SAM. Add a more robust exploration routine. The model is set up for multi-agent learning however, more advanced cooperation vs self-interest algorithms need to be implemented to help solve the unstable environment that multi-agent learning can cause.


TROUBLE SHOOTING
--------------
This model requires an outside file (“agent.rtf”) in order to store the learned tactics. If an error is seen for “LOAD-STATE-ACTION-FILE” click the “Clear/Create File” button and the “agent.rtf” file will be created and the file will work as long as there is permission to write in the directory where the model is stored. 


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

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

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
