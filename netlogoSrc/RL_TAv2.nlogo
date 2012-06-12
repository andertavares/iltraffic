;TODO calcular a ocupacao otima em cada via a cada passo - feito
;TODO armazenar ocupacao media (atual e otima) - feito
;TODO plotar capacidade das vias - feito
;TODO distribuicao proporcional nao deve considerar motoristas finalizados - feito
;BUGFIX distribuicao proporcional de motoristas (esta zerando quando motoristas ja passaram pela intersecao) - feito
;BUGFIX distribuicao proporcional tem que ser incrementada a cada passo, e nao setada - feito
;BUGFIX historico e' zerado quando os 2 valores sao iguais - feito
;TODO atualizar historico - feito
;HEISENBUGFIX algumas vezes o agente fica com melhor via = -1 - feito
;TODO truncar historico entre 60:140? - feito
;TODO atualizar pontuacao - aparentemente feito
;TODO colorir arestas de acordo com ocupacao media - feito
;TODO historico / ocupacao percentual ou fracionario? - noneed
;TODO plotar tempos de viagem por par OD - feito
;BUGFIX historico limitado em 60 - FEITO
;TODO forcar motoristas a so 1 par OD - feito
;TODO forcar motoristas teimosos
;TODO implementar um enroute? para retirar escolha gulosa -feito
;TODO ver questao da ponderacao da previsao - feito
;BUGFIX nao esta plotando ocupacao media - corrigido
;BUGFIX ett calculado errado para 3 motoristas - corrigido
;TODO mexer no maxQForNextState??? (tirar o zero quando chega ao fim)
;BUG tabela Q atualizada simultaneamente para todos?!?
;TODO poder salvar e carregar todos os parametros do experimento - OK
;--file-read somente aceita numeros
;TODO armazenar tempo medio de viagem para cada motorista - OK
;TODO potencializar efeito de vias ocupadas?
;TODO mostrar media de 'n' ultimos episodios?
;TODO implementar 'in-route?' com uma busca no grafo

; ROTAS MINIMAS:
; 1-8: 2-8-15 / 2-9-17 / 3-12-19 / 3-11-17 / 3-10-15
; 1-9: 3-12-20 / 3-11-18 / 2-9-18 / 
; 1-10: 3-12-21
; 2-8: 6-15
; 2-9: 5-12-20 / 5-11-18 / 6-13-18 / 6-14-20
; 2-10: 5-12-21 / 6-14-21
; 3-8: 8-15 / 9-17
; 3-9: 9-18
; 3-10: 9-17-23 / 9-18-24 / 7-12-21 / 8-15-23


extensions [ array ]

globals [
  num-intersections 
  num-roads 
  ideal-segment-length
  od-pairs
  avg-occ-dev ;average deviation of the difference of "optimal" # of cars and actual # of cars
  
  dijkstra-distances     ;; shortest path distance from initial node to all other nodes
  dijkstra-directions    ;; which direction to head in for shortest path --> the 'previous' array
  nodes-visited          ;; total number of nodes visited by all walkers
]

breed [ intersections intersection ]
breed [ drivers driver ]

directed-link-breed [ roads road ]

intersections-own [ 
   x y
   id
]

roads-own [
  road-id  ;identificador da via
  capacity ;capacidade [130:250]
  num-drv  ;numero de motoristas
  opt-num-drv ;numero proporcional de motoristas
  
  avg-num-drv ;ocupacao real media
  avg-opt-drv ;ocupacao proporcional media
  
  fftt  ;free-flow travel time
  travel-time ;tempo de viagem
  
  history ;historico de ocupacao
]

drivers-own [
  current-route ;lista de road-id descrevendo a rota atual
  route-weight  ;peso da rota a ser calculado na chegada
  actual-tt     ;tempo de viagem real do motorista
  average-tt    ;tempo medio de viagem do motorista
  
  optimal-route ;rota otima (menor #nos ate o destino)
  expected-tt   ;tempo de viagem esperado (considera os motoristas no mesmo par OD
  
  roads-weight  ;lista com o peso das rotas TODO: sera' necessario?
  
  origin        ;ID do no' de origem
  destination   ;ID do no' de destino
  current-node  ;ID do no' atual
  
  current-road  ;via atual (a via mesmo, nao o ID)
  
  q-table       ;tabela Q
  
  travel-times ;list of travel-times for every road
]

to setup 
    clear-all
    ;setup-network "mgta2"
    ;setup-network "inputs/6x6-papers.txt"
    setup-network "mgta2"
    setup-drivers
end

to setup-std-scenario
  setup
  let inputfile "inputs/MGTA_compare2.txt"
  
  load-setup-from-file inputfile
end


to go
  reset-road-network
  
  while [drivers-not-arrived] [
    step
  ]
  
  experience-travel-times
  update-q-table 
  update-roads-visual
  
  ;updates the avg-occ-dev
  set avg-occ-dev ((avg-dev-episode - avg-occ-dev) / (ticks + 1)) + avg-occ-dev
  
  do-plots
  
  prepare-next-episode
  
  tick
end

to step 
  distribute-drivers-proportionally
  choose-next-road
  advance
end

;initializes the drivers
to setup-drivers
  
  ;monta a lista com os pares OD, a partir dos valores selecionados pelos usuarios
  set od-pairs []
  
  ;REMOVE AFTER TESTING V2V -- better: parametrize in file
  ;set od-pairs [[1 9]]
  ;set od-pairs [[1 25]] ;this one is for the 5x5 scenario
  ;set od-pairs [[1 36]] ;this one is for the 6x6 scenario
  ;set od-pairs [[1 4]] ;for Pigou's example and queue-test
  set od-pairs read-from-string od-pairs-list
  
  create-drivers num-drivers [
    let od-pair one-of od-pairs
    
    set origin item 0 od-pair ;1 + random 3  ;origem em nos 1, 2 ou 3
    set destination item 1 od-pair ;8 + random 3 ;destino em 8, 9 ou 10
    
    setup-driver origin destination
  ]
  
  calculate-ett
end

;configures drivers' attributes
to setup-driver [the-origin the-destination] 
  
  set origin the-origin
  set destination the-destination
  
  set current-node origin
  set current-route []
  set optimal-route calculate-opt-route origin destination
  
  set q-table n-values num-roads [0]
  
  set roads-weight n-values num-roads [1] ;incializa os pesos de todas as rotas como 1 (nao influencia a escolha)
  
  let myorig origin
  let mydest destination
  move-to one-of intersections with [id = myorig]
  face-nowrap one-of intersections with [id = mydest]
  
  set average-tt 0
  
  set shape "car"
  set size 3
    
end

; creates drivers according to a trips file definition
; previously created drivers are deleted
to load-drivers-from-file
  let infile user-file
  
  ask drivers [die]
  
  file-open infile
  
  set od-pairs []
  while [not file-at-end?] [
    let orig file-read
    let dest file-read
    let num-commuters file-read
    
    create-drivers num-commuters [
      setup-driver orig dest
    ]

  ]
  
  file-close
  set num-drivers count drivers
  
  ;calculate the expected travel time (should it be moved to setup-driver?)
  calculate-ett
end

;clear all statistics and resets original values of entities attributes 
to reset-experiment
  ask drivers [
    setup-driver origin destination
  ]
  
  ask roads [
    
    set num-drv  0    ;numero de motoristas
    set opt-num-drv 0 ;numero proporcional de motoristas
    
    set avg-num-drv 0;ocupacao real media
    set avg-opt-drv 0;ocupacao proporcional media
    
    set travel-time 0
  ]
  
  set avg-occ-dev 0
  
  set-current-plot "drivers-per-road-linechart"
  clear-plot
   
  reset-ticks
end

to distribute-drivers-proportionally
  ask intersections [
    let drv-at-intersection count drivers-here with [current-node != destination];desconta os motoristas que ja chegaram
   
    let total-capacity sum [capacity] of my-out-links
    
    ask my-out-links [
      let proportion capacity / total-capacity
      set opt-num-drv opt-num-drv + (proportion * drv-at-intersection)
    ]

    
  ]
end
;    atualiza o historico de ocupacao das vias, inserindo a ocupacao da rodada atual
to update-history 
  ask roads [

    let occupation (num-drv / capacity) * 100
    
    if occupation < 60 [set occupation 60]
    if occupation > 140 [set occupation 140]
    
    set history remove-item (length history - 1) history
    set history fput occupation history
    if length history = 1 [show (word "error " history)]
  ]
end

to choose-next-road 
  ask drivers [
    if current-node = destination [stop] ;nao faz escolhas se ja tiver chegado
    
    ifelse random-float 1 < epsilon 
      [choose-road-randomly]
      [choose-road-greedily]
  ]
end

;calcula o tempo de viagem esperado para cada motorista
to calculate-ett
  
  ask drivers [
    let drv-same-od 0
    let exp-drv 0
    let ett 0
  
    let myorig origin
    let mydest destination
    
    set drv-same-od count drivers with [origin = myorig and destination = mydest]
    
    set exp-drv drv-same-od  - 50  + random 101 ;adiciona ruido +-50
    
;TODO colocar alfa e beta aqui?
    foreach optimal-route [
      ask ? [set ett ett + fftt * (1 + exp-drv / capacity) ^ 2]
   ;   ask ? [set ett ett + fftt * (1 + travel-time-alpha * (exp-drv / capacity) ^ travel-time-beta)]
    ]
    set expected-tt ett
  ]
end

; reinicia a rede de trafego: coloca os motoristas na origem, reseta as rotas
; reseta os tempos de viagem das vias e a ocupacao 
to reset-road-network 
  reset-drivers
  reset-roads
end

;reinicia o estado dos motoristas, posicionando-os na origem e resetando as rotas
to reset-drivers
    ask drivers [
     
      set current-node origin
      set current-route []
      
      ;TODO atualizar os pesos das rotas
      ;set roads-weight n-values num-roads [1]
      let myorig origin
      let mydest destination
      move-to one-of intersections with [id = myorig]
      face-nowrap one-of intersections with [id = mydest]
    ]
end

;reinicia o estado das vias (num-motoristas; num-proporcional; tempo-de-viagem)
to reset-roads
  ask roads [ 
    set num-drv  0
    set opt-num-drv 0
    set travel-time 0
  ]
end

to choose-road-randomly
  let chosen-road nobody
;show "epsilon"
  ;a partir da intersecao atual...
  let node-id current-node
  let myorig origin
  let mydest destination
  
  ask intersections with [id = node-id] [
      
    while [chosen-road = nobody] [
      set chosen-road one-of my-out-links
      if not road-in-route chosen-road myorig mydest
      [ set chosen-road nobody ]
    ]
  ]
  
          
  set current-road chosen-road  
  ask current-road [set num-drv num-drv + 1]
end


;faz o motorista escolher a proxima via a ser usada 
;de maneira gulosa, isto e, escolhe a via que tem o maior valor Q
to choose-road-greedily
    
  ;ask drivers[
    
  ;configura variaveis para serem usadas dentro dos asks
  let node-id current-node
  let myorig origin
  let mydest destination
  let the-q-table q-table
  
  ;inicializa melhor via e maior valor Q encontrado
  let best-road-id -1
  let maxQ -100000000 ;inicializa maiorQ com valor pequeno
  
  ;a partir da intersecao atual...
  ask intersections with [id = node-id] [
    
    ;let will-reach-dest false
    
    ;...analisa todos os links de saida para achar o melhor
    ask my-out-links [
      
      ;se ja encontrou aresta que leva ao destino, nao procura mais
      ;if will-reach-dest [stop] 
      if (not in-route myorig mydest) [stop]
      
      let thisQ item (road-id - 1) the-q-table
      
      ; testa se a via atual e' a melhor ate o momento (maior valor Q)
      if thisQ > maxQ [
        set maxQ thisQ
        set best-road-id road-id
      ]
      
    ] ;ask my-out-links    
  ] ;ask intersections
  
  
  set current-road one-of roads with [road-id = best-road-id]
  
  ask current-road [set num-drv num-drv + 1]
 ; ]
end

;diz se a via pertence ou nao 'a uma rota entre orig e dest
to-report in-route [orig dest]
  
  let forbidden []
  
  if dest = 8  [ set forbidden [18 20 21 22 23 24] ]
  if dest = 9  [ set forbidden [21 23 24] ]
  
  report not member? road-id forbidden
end

;diz se a via identificada por rid pertence ou nao 'a uma rota entre orig e dest
to-report road-in-route [the-road orig dest]
  let inroute? false
  ask the-road [
    set inroute? in-route orig dest
  ]
  report inroute?
end

;faz o motorista chegar ao destino da via escolhida e experimentar o tempo de viagem
to advance
  ask drivers [
    if current-node = destination [stop]
    
    ;adiciona a via atual 'a rota
    set current-route lput current-road current-route
    
    ;avanca para a proxima intersecao
    set current-node [id] of [end2] of current-road
    let nid current-node
    move-to one-of intersections with [id = nid]
    
  ]
end

;calcula o tempo de viagem gasto pelos motoristas em cada via e atualiza o vetor de pesos da rotas
to experience-travel-times
  ask roads [
    
    set travel-time fftt * (1 + travel-time-alpha * (num-drv / capacity) ^ travel-time-beta)

    if num-drv > 0 [ ;TODO porque num-drv > 0 ???
      set avg-num-drv ((num-drv - avg-num-drv) / (ticks + 1)) + avg-num-drv
      set avg-opt-drv ((opt-num-drv - avg-opt-drv) / (ticks + 1)) + avg-opt-drv
    ]
;    avgNumDrivers = ((getNumDrivers() - getAvgNumDrivers()) / (Experiment.getInstance().getTimestep() + 1)) + getAvgNumDrivers();
  ]
  
  ask drivers [
    let attR 0 ;inicializa o tempo de viagem do motorista
    
    ;calcula o custo da rota
    foreach current-route [
      ask ? [set attR attR + travel-time]
    ]
    
    set actual-tt attR
    set average-tt ((actual-tt - average-tt) / (ticks + 1)) + average-tt
    
    ;set route-weight actual-tt / expected-tt
    
    ;atualiza o vetor de pesos de rota por aresta
    foreach current-route [
      let rw roads-weight
      let att actual-tt
      let w route-weight
      
      ask ? [
        let rid road-id
        set rw replace-item (rid - 1) rw w
      ;replace-item 0 [1 3 3] 3
      ]
      set roads-weight rw
    ]
  ]
end

;sets up variables for the next episode
to prepare-next-episode
  ;set q-learning-alpha q-learning-alpha * decay
  set epsilon epsilon * decay
end

to-report avg-dev-episode
  let total-dev 0
  ask roads [
    set total-dev total-dev + abs (num-drv - opt-num-drv)
  ]
  
  report total-dev / count roads
end

to-report avg-dev
  
end


;atualiza a tabela Q, a partir das recompensas recebidas pelas vias
;utilizadas na ultima viagem
;A recompensa dada pela viagem em cada via e: R = -tt * W (w e' o peso da rota)
to update-q-table

  ;para cada via...
  ask roads [
    if num-drv = 0 [stop]
    
    let rid road-id
    let rhist history
    let rocc capacity / num-drv 
    
    let road-tt travel-time
    
    ;para cada motorista
    ask drivers [  
      
      let found-in-route false
      foreach current-route [
        ask ? [ if road-id = rid [set found-in-route true] ]
      ]
      if not found-in-route [stop]
      
      ;calcula a recompensa
      let reward 0
      let rew-tt -1 * road-tt 
      let rew-occ rocc - 1;^ 2 ;(1 / rocc) - 1; cap/num-drv - 1
      
      
      if reward-by = "travel-time" [set reward rew-tt]
      if reward-by = "occupation" [set reward rew-occ]
      if reward-by = "both" [set reward (tt-weight * rew-tt) + ((1 - tt-weight) * rew-occ)]
      
      
      ;TODO: normalizar recompensas
      ;show (word rid " -> " reward)
      
      let oldQ item (rid - 1) q-table
      
      
      let newQ (1 - q-learning-alpha) * oldQ + q-learning-alpha * (reward + q-learning-gamma * (maxQForNextState rid)) 
      
      ;show (word rid ": " oldQ " - " newQ "(" maxQForNextState rid ")" )
      
      set q-table replace-item (rid - 1) q-table newQ
    ]
  ]
end

; retorna o maior valor Q possivel para as proximas acoes disponiveis 
; a partir do no de destino da via recebida
; se o no de destino for o ultimo da viagem, retorna zero
to-report maxQForNextState [rid]
  let nextIntersection [end2] of one-of roads with [road-id = rid]
  
  ;show (word nextIntersection " -- " destination)

  if [id] of nextIntersection = destination [report 0]
  
  let maxQ -1000000
  let myorig origin
  let mydest destination
  let the-q-table q-table
  
  ask nextIntersection [
    ask my-out-links [
      let this-q item (road-id - 1) the-q-table 
      if in-route myorig mydest and this-q > maxQ [
        set maxQ this-q
      ]
    ]
  ]
  
  report maxQ
  
end

;retorna a rota otima entre nos origem-destino
to-report calculate-opt-route [org dest]
  
  let rid-list []
  
  if org = 1 and dest = 8 [ set rid-list [1 6 15] ]
  if org = 1 and dest = 9 [ set rid-list [3 12 20] ]
  if org = 1 and dest = 10 [ set rid-list [3 12 21] ]
  
  if org = 2 and dest = 8 [ set rid-list [6 15] ]
  if org = 2 and dest = 9 [ set rid-list [6 13 18] ]
  if org = 2 and dest = 10 [ set rid-list [6 15 23] ]

  if org = 3 and dest = 8 [ set rid-list [9 17] ]
  if org = 3 and dest = 9 [ set rid-list [9 18] ]
  if org = 3 and dest = 10 [ set rid-list [9 18 24] ]
  
  
  let opt-route []
  
  foreach rid-list [
    set opt-route lput one-of roads with [road-id = ?] opt-route
  ]
  
  report opt-route
end

; Prediz a ocupacao na via dado uma estrategia (o preditor) e o historico.
; A previsao e' dada pela formula:
; p(t) = x(t - 1) * a(t - 1) + x(t - 2) * a(t -2) +..
;      ... + x(t - MEMORY-SIZE) * a(t - MEMORY-SIZE)
; onde p(t) e' a previsao no tempo t, x(t) e a ocupacao da via no tempo t,
; a(t) e' o peso para o tempo t e MEMORY-SIZE e' o tamanho do historico
to-report predict-occupation [predictor occ-history]
  let prediction sum (map [?1 * ?2] predictor occ-history)
  if prediction < 60 [set prediction  60]
  if prediction > 140 [set prediction  140]

  report prediction
end
  

;diz se todos os motoristas ja chegaram nos seus destinos
to-report drivers-not-arrived
  let all-arrived true
  ask drivers [
    if current-node != destination [ set all-arrived false ]
  ]
  report not all-arrived
end


;configures the network
to setup-network [network-file]
  clear-turtles
  
  set avg-occ-dev 0
    
  file-open network-file 
  set num-intersections file-read
  set num-roads file-read
  set ideal-segment-length file-read

  repeat num-intersections  [ 
    create-intersections 1 [
      set id file-read ;id-counter

      set xcor file-read / 1.1
      set ycor file-read / 1.1
      
      update-node-visual
    ]
  ]
  repeat num-roads  [
    let r-id file-read
    let id1 file-read
    let id2 file-read
    let the-capacity file-read
    ask intersections with [ id = id1 ]  [
      create-roads-to intersections with [id = id2] [
        set road-id r-id
        
        set num-drv 0
        set opt-num-drv 0
        set avg-num-drv 0
        set avg-opt-drv 0
        
        set fftt 5
        ;if no capacity was informed in the file, assign the default 
        ifelse the-capacity = false [
          set capacity roads-capacity + random (cap-randomness + 1)
        ]
        [;else
          set capacity the-capacity
        ]
        set label road-id
        set label-color red
      ]
    ]
  ]
  file-close  
  
  setup-roads-thickness
end

to save-cap 
  file-open user-new-file
  ask roads [
    file-print (word road-id " " capacity)
  ]
  file-close
end

to load-cap
  file-open user-file
  
  repeat num-roads [
    let rid file-read
    let cap file-read
    
    ask roads with [road-id = rid] [set capacity cap]
  ]
  file-close
  
  calculate-ett
  setup-roads-thickness
end

;calculates the shortest path between orig and dest 
;using the travel times as the edges weight
to-report calculate-min-z-route [orig dest]
  let initial-node one-of intersections with [id = orig]
    
  initialise-distances [who] of initial-node
  
  let nodes-to-visit []
  ask initial-node [
    set nodes-to-visit other intersections;; this is the list of nodes yet to have been visited
  ]
  ;performs dijkstra using the q-values as the graph weights
  perform-dijkstra initial-node nodes-to-visit q-table;travel-times
  
  ;now dijkstra-directions is the 'previous' vector
  let previous array:to-list dijkstra-directions
  let curr-node dest
  let the-route []
  
  while [curr-node != orig] [
    set the-route fput curr-node the-route
    set curr-node 1 + item (curr-node - 1) previous
  ]
  report the-route
end

;initialise the dijkstra distances array and the 'previous' array as well
to initialise-distances [initial-node]
  set dijkstra-distances array:from-list n-values num-intersections [1000000] ;; "infinity"
  set dijkstra-directions array:from-list n-values num-intersections [nobody]
  array:set dijkstra-distances initial-node 0 ;; set distance to 0 for initial node
end

;; calculates the distance array for the Dijkstra algorithm using the initial node
;; as the focal point -- z-values contains the weight of each edge
to perform-dijkstra [initial-node nodes-to-visit weights]

  set nodes-visited 0
  
  initialise-distances [who] of initial-node
  
  let visited-set [] ;; this is the list of nodes that have been visited
  let unvisited-set nodes-to-visit ;; this is the list of nodes yet to have been visited
     
  let curr-node initial-node
  while [count unvisited-set > 0]
  [
   
    ask curr-node
    [
      set nodes-visited nodes-visited + 1
      set visited-set fput who visited-set
      set unvisited-set other unvisited-set

      ask out-link-neighbors
      [
        ;let dist-thru-here (array:item dijkstra-distances [who] of curr-node) + 1 ;cost of next hop
        let dist-thru-here (array:item dijkstra-distances [id - 1] of curr-node) + dist-between curr-node self weights;cost of next hop
        let dist-thru-to-there array:item dijkstra-distances (id - 1)
        
        if (dist-thru-here < dist-thru-to-there) [ 
          array:set dijkstra-distances who dist-thru-here
          array:set dijkstra-directions who [who] of curr-node 
        ]
      ]
      
      ;; set the curr-node to the remaining unvisited node that has the smallest distance to the initial node      
      set curr-node min-one-of unvisited-set [array:item dijkstra-distances who]
    ]
  ]
  
  ;; print array:to-list dijkstra-distances

end

to-report dist-between [node-a node-b weights]
  let rid [road-id] of one-of roads with [end1 = node-a and end2 = node-b]
  report item (rid - 1) weights
end

; shows the 'previous' array
to dump-dijkstra-directions
  show dijkstra-directions
  let i 0
  foreach array:to-list dijkstra-directions
  [
    if (? != nobody) [ type " from: " type i type " thru: " type ? type " dist: " print array:item dijkstra-distances i ]
    set i i + 1
  ]
end

;configures the nodes appearence
to update-node-visual
    set shape "circle 2"
    set size ideal-segment-length / 3
    set color 5
    ask intersections [ set label id ]
end

to do-plots
  plot-travel-times
  plot-roads-data
  plot-tt-per-od
end

to plot-roads-data
  plot-drivers-per-road-column-chart
  ;plot-roads-occupation
end

to-report avg-travel-time
  let total-att 0
  
  ask drivers [
    set total-att total-att + actual-tt
  ]
  
  report total-att / num-drivers
end

to plot-travel-times
  let total-att 0
  let total-ett 0
  
  ask drivers [
    set total-att total-att + actual-tt
    set total-ett total-ett + expected-tt
  ]
  
  let avg-att total-att / num-drivers
  let avg-ett total-ett / num-drivers
  
  set-current-plot "avg-travel-time"
  
  set-current-plot-pen "actual"
  plot avg-att
  
  set-current-plot-pen "expected"
  plot avg-ett
end

to plot-tt-per-od 
  let counter 1
  let origins [1 2 3]
  let dests [8 9 10]

  set-current-plot "travel-time-per-od"
  
  clear-plot

  foreach origins [
    let orig ?
    foreach dests [
      let dest ?
      
      set-current-plot-pen "actual"
      ifelse average?
      [plotxy counter (avg-att-per-od orig dest)]
      [plotxy counter (att-per-od orig dest)]
      
      set-current-plot-pen "expected"
      plotxy (counter) (ett-per-od orig dest)
      
      set counter counter + 1
    ]
  ]

end

;reports the average of driver's travel time on this episode from a given OD pair
to-report att-per-od [orig dest]
  let total-tt 0
  
  ask drivers with [origin = orig and destination = dest] [
    set total-tt total-tt + actual-tt
  ]
  if total-tt = 0 [report 0]
  report total-tt / count drivers with [origin = orig and destination = dest]
end

;reports the average driver's average travel time from a given OD pair
to-report avg-att-per-od [orig dest]
  let total-avgtt 0
  
  ask drivers with [origin = orig and destination = dest] [
    set total-avgtt total-avgtt + average-tt
  ]
  if total-avgtt = 0 [report 0]
  ;show total-avgtt
  report total-avgtt / count drivers with [origin = orig and destination = dest]
end

to-report ett-per-od [orig dest]
  let total-tt 0
  
  ask drivers with [origin = orig and destination = dest] [
    set total-tt total-tt + expected-tt
  ]
  if total-tt = 0 [report 0]
  report total-tt / count drivers with [origin = orig and destination = dest]
end

to plot-drivers-per-road-column-chart
  set-current-plot "drivers-per-road"
  clear-plot
  set-current-plot-pen "actual"
  
  ask roads [
    ifelse avg?
    [plotxy road-id avg-num-drv]
    [plotxy road-id num-drv]
  ]
  
  
  set-current-plot-pen "proportional"
  ask roads [
    ifelse avg?
    [plotxy road-id avg-opt-drv]
    [plotxy road-id opt-num-drv]
  ]
  
  if plot-capacity? [  
    set-current-plot-pen "capacity"
    ask roads [
      plotxy (road-id + .5) capacity
    ]
  ]
  
  set-current-plot "inst-drv-per-road"
  clear-plot
  set-current-plot-pen "actual"
  
  ask roads [
    plotxy road-id num-drv
  ]
  
  set-current-plot-pen "proportional"
  
  ask roads [
    plotxy road-id opt-num-drv
  ]
end

; plots the drivers-per-road linechart
; for networks above 24 links, does not work because chart was created w/ 24 pens only
to plot-roads-occupation
  set-current-plot "drivers-per-road-linechart"
  
  ask roads [
    set-current-plot-pen (word "road_" road-id)
    plot num-drv
  ]
end

to update-roads-visual
  ask roads [
    ;set label (precision travel-time 2)
    
    let occ avg-num-drv / capacity
    
    if inst-road-view?
    [set occ num-drv / capacity]
    
    set color 68

    if occ > 0.80 [set color green]
    if occ > 1.00 [set color yellow]
    if occ > 1.20 [set color orange]
    if occ > 1.40 [set color red]
  ]
end

;reports the num-drv / capacity ratio for a road
to-report overload-ratio [rid]
  let ratio 0
  ask roads with [road-id = rid] [
    set ratio num-drv / capacity 
  ]
  report ratio
end

;reports the number of congested roads (num-drv > capacity)
to-report num-congested-roads 
    report count roads with [num-drv > capacity]
end

;reports the aed metric, given by the average of the difference between actual and expected travel times on a given OD pair
to-report aed [orig dest]
  report mean [actual-tt - expected-tt] of drivers with [origin = orig and destination = dest]
end

;reports the standard deviation of the AED metric
to-report std-dev-of-aed [orig dest]
  report standard-deviation [actual-tt - expected-tt] of drivers with [origin = orig and destination = dest]
end

  

;adjust roads thickness according to the capacity
to setup-roads-thickness 
  let min-cap min [capacity] of roads
  ask roads [
    set thickness capacity * road-thickness / min-cap
  ]
end

;0 = travel-time
;1 = occupation
;2 = both
to-report encode-reward-type [string]
  if string = "travel-time" [report 0]
  if string = "occupation" [report 1]
  if string = "both" [report 2]
  show (word "Bad string for reward: " string)
end

to-report decode-reward-type [integer]
  if integer = 0 [report "travel-time"]
  if integer = 1 [report "occupation"]
  if integer = 2 [report "both"]
  show (word "Bad integer for reward: " integer)
end

;saves all experiment parameters to a file
to save-setup 
  file-open user-new-file
  
  file-print (num-drivers) ;word "num-drivers " 
  file-print (q-learning-alpha) ;word "q-learning-alpha " 
  file-print (q-learning-gamma) ;word "q-learning-gamma " 
  file-print (epsilon) ;word "epsilon " 
  file-print (travel-time-alpha) ;word "travel-time-alpha " 
  file-print (travel-time-beta) ;word "travel-time-beta " 
  file-print encode-reward-type reward-by ;word "reward-by " 
  
  file-print "\n"
  file-print (length od-pairs) ;word "\nODPairs " 

  foreach od-pairs [
    ;show ?
    let drv-count count drivers with [origin = (item 0 ?) and destination = (item 1 ?)]
    file-print(word (item 0 ?) " " (item 1 ?) " " drv-count)
  ]
  
  ;file-print("\nRoadsCapacity")
  file-print "\n"
  file-print(count roads);word "numRoads " 
  ask roads [
    file-print (word road-id " " capacity)
  ]
  
  
  file-close
end

to load-setup
  
  let inputfile user-file
  if inputfile = false [stop]
  load-setup-from-file inputfile
  
end

to load-setup-from-file [thefile]
  
  reset-experiment
  clear-all-plots
  
  
  file-open thefile
  
  ;let foo "nothing-important"

  ;set foo file-read ;"num-drivers"
  set num-drivers file-read
  
 ; set foo file-read ;"q-learning-alpha"
 ; set q-learning-alpha file-read
  
  ;set foo file-read ;"q-learning-gamma"
;  set q-learning-gamma file-read
  
  ;set foo file-read ;"epsilon"
;  set epsilon file-read
  
 ; set foo file-read ;"travel-time-alpha"
  set travel-time-alpha file-read
  
  ;set foo file-read ;"travel-time-beta"
  set travel-time-beta file-read
    ;show travel-time-beta
  
  ;set foo file-read ;"reward-by"
  set reward-by decode-reward-type file-read
  
  ;TODO loads od-pairs
  ;set foo file-read ;"ODPairs"
  let num-odpairs file-read
  
  ;deletes drivers if there are some
  ask drivers [die]

  repeat num-odpairs [
    let the-origin file-read
    let the-destination file-read
    let num-commuters file-read
    
    create-drivers num-commuters [
      setup-driver the-origin the-destination
    ]
  ]
  show count drivers with [origin = 1 and destination = 8]
  calculate-ett
  
  
  ;TODO loads road capacity  
  ;set foo file-read ;"Roads Capacity"
  ;set foo file-read ;"num-roads"
  let number-roads file-read
  
  repeat num-roads [
    let r-id file-read ;road-id
    let cap file-read ;capacity
    
    ask roads with [road-id = r-id] [
      set capacity cap
    ]
  ]
  file-close
  
  setup-roads-thickness
end

@#$#@#$#@
GRAPHICS-WINDOW
280
14
638
393
16
16
10.55
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks

BUTTON
21
52
94
85
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
103
52
166
85
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
19
268
192
301
travel-time-alpha
travel-time-alpha
.1
2
1
.1
1
NIL
HORIZONTAL

SLIDER
19
309
191
342
travel-time-beta
travel-time-beta
.1
3
2
.1
1
NIL
HORIZONTAL

BUTTON
102
12
165
45
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
18
158
190
191
q-learning-alpha
q-learning-alpha
0
1
0.5
.1
1
NIL
HORIZONTAL

PLOT
651
144
930
294
avg-travel-time
iteration
travel-time
0.0
10.0
0.0
10.0
true
true
PENS
"actual" 1.0 0 -16777216 true
"expected" 1.0 0 -2674135 true

PLOT
655
308
928
479
drivers-per-road
id-road
#drivers
1.0
25.0
0.0
10.0
true
false
PENS
"proportional" 1.0 1 -2674135 true
"actual" 1.0 1 -16777216 true
"capacity" 1.0 2 -10899396 true

SWITCH
923
431
1033
464
plot-capacity?
plot-capacity?
0
1
-1000

BUTTON
22
10
86
43
NIL
step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

PLOT
677
10
859
130
inst-drv-per-road
road-id
#drivers
1.0
25.0
0.0
10.0
true
false
PENS
"actual" 1.0 1 -16777216 true
"proportional" 1.0 1 -2674135 true

SWITCH
469
40
632
73
inst-road-view?
inst-road-view?
1
1
-1000

PLOT
651
10
930
136
travel-time-per-OD
od-pair
travel-time
1.0
10.0
0.0
10.0
true
false
PENS
"actual" 1.0 1 -16777216 true
"expected" 1.0 1 -2674135 true

TEXTBOX
308
322
458
340
Up to 100%
13
55.0
1

TEXTBOX
307
337
457
355
Up to 120%
13
45.0
1

TEXTBOX
307
352
457
370
Up to 140%
13
25.0
1

TEXTBOX
307
368
457
386
More than 140%
13
15.0
1

TEXTBOX
289
291
439
309
Occupation:
13
9.9
1

BUTTON
180
10
277
43
go [num]
repeat num-episodes [\ngo\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
20
193
192
226
q-learning-gamma
q-learning-gamma
0
1
0.9
.1
1
NIL
HORIZONTAL

SLIDER
20
227
193
260
epsilon
epsilon
0
1
0.1000000004521861
.1
1
NIL
HORIZONTAL

PLOT
441
402
641
552
drivers-per-road-linechart
timestep
#drivers
0.0
10.0
0.0
10.0
true
false
PENS
"road_1" 1.0 0 -16777216 true
"road_2" 1.0 0 -16777216 true
"road_3" 1.0 0 -16777216 true
"road_4" 1.0 0 -16777216 true
"road_5" 1.0 0 -16777216 true
"road_6" 1.0 0 -16777216 true
"road_7" 1.0 0 -16777216 true
"road_8" 1.0 0 -16777216 true
"road_9" 1.0 0 -16777216 true
"road_10" 1.0 0 -16777216 true
"road_11" 1.0 0 -16777216 true
"road_12" 1.0 0 -16777216 true
"road_13" 1.0 0 -16777216 true
"road_14" 1.0 0 -16777216 true
"road_15" 1.0 0 -16777216 true
"road_16" 1.0 0 -16777216 true
"road_17" 1.0 0 -16777216 true
"road_18" 1.0 0 -16777216 true
"road_19" 1.0 0 -16777216 true
"road_20" 1.0 0 -16777216 true
"road_21" 1.0 0 -16777216 true
"road_22" 1.0 0 -16777216 true
"road_23" 1.0 0 -16777216 true
"road_24" 1.0 0 -16777216 true

CHOOSER
8
363
146
408
reward-by
reward-by
"travel-time" "occupation" "both"
2

BUTTON
662
477
759
511
NIL
save-cap
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
772
477
867
511
NIL
load-cap
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

MONITOR
856
212
929
257
avg-tt
avg-travel-time
5
1
11

TEXTBOX
308
308
458
326
Up to 80%
12
68.0
1

BUTTON
671
519
782
552
NIL
save-setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
794
519
903
552
NIL
load-setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
735
429
838
462
avg?
avg?
0
1
-1000

BUTTON
876
478
1004
511
NIL
clear-all-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
150
349
246
382
tt-weight
tt-weight
0
1
1
.1
1
NIL
HORIZONTAL

MONITOR
155
385
237
430
occ-weight
1 - tt-weight
5
1
11

SWITCH
744
86
867
119
average?
average?
0
1
-1000

BUTTON
909
518
1064
551
NIL
reset-experiment
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

MONITOR
938
311
1058
356
NIL
avg-dev-episode
17
1
11

MONITOR
940
364
1030
409
NIL
avg-occ-dev
17
1
11

INPUTBOX
204
42
277
102
num-episodes
100
1
0
Number

INPUTBOX
208
205
266
265
decay
0.977237221
1
0
Number

BUTTON
963
88
1069
122
Load Trips
load-drivers-from-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

INPUTBOX
207
281
277
341
od-pairs-list
[[1 10]]
1
0
String

SLIDER
460
349
633
382
road-thickness
road-thickness
0.1
3
0.2
0.1
1
NIL
HORIZONTAL

INPUTBOX
21
88
96
148
num-drivers
1001
1
0
Number

INPUTBOX
99
89
173
149
roads-capacity
15
1
0
Number

INPUTBOX
178
90
252
150
cap-randomness
0
1
0
Number

@#$#@#$#@
WHAT IS IT?
-----------
This model is an implementation of the modified Minority Game algorithm for traffic assignment proposed in the paper: Road Traffic Optimisation Using an Evolutionary Game written by Syed Md. Galib and Irene Moser.


HOW IT WORKS
------------
Agents predict occupancy on the roads they can use on their trip. They have a set of predictors for each link and "believe" the prediction made by the best scored predictor. Then they choose the next link of their trip as the one with the least predicted occupancy. This is repeated until they reach their destinations. Then travel times are calculated and the predictors' score are updated.


HOW TO USE IT
-------------
Click 'setup' to load the road network and place the drivers on their origins (uniformely distributed on the nodes 1, 2 and 3). Click 'go' to simulate one iteration. The 'go 50' button simulates 50 iterations, and the looped 'go' repeats iterations forever.

If one of the parameters is changed, the setup button must be pressed again to create the simulation environment with the updated values. Parameters are:

- num-drivers: the number of drivers on the network
- history-size: drivers store road occupation from 'history-size' iterations from the past
- alpha and beta: these are parameters of the travel time calculation formula: 
fftt * (1 + alpha * (num-drv / capacity) ^ beta)
- learning-factor: it weights the score to be updated on the predictors (lower makes previous scores more important than the current)


THINGS TO NOTICE
----------------
The networks tends to self-organise. Drivers distribute themselves almost proportionally to the capacity of the roads. On average, actual travel times are lower than the expected.


THINGS TO TRY
-------------
It is interesting to see how the parameters affect the simulation.


EXTENDING THE MODEL
-------------------
It is possible to force drivers to have only one origin-destination pair by changing the code. 



CREDITS AND REFERENCES
----------------------
This implementation was built to the Advanced Artificial Intelligence course at UFRGS.
Author: Anderson Rocha Tavares
Advisor: Ana L. C. Bazzan

Paper: Road Traffic Optimisation Using an Evolutionary Game
Authors: Syed Md. Galib and Irene Moser
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 4.1.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="standard" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-alpha">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;travel-time&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="alpha-varying" repetitions="1" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>avg-occ-dev</metric>
    <metric>mean [average-tt] of drivers with [origin = 1 and destination = 8]</metric>
    <metric>mean [average-tt] of drivers with [origin = 1 and destination = 9]</metric>
    <metric>mean [average-tt] of drivers with [origin = 1 and destination = 10]</metric>
    <metric>mean [average-tt] of drivers with [origin = 2 and destination = 8]</metric>
    <metric>mean [average-tt] of drivers with [origin = 2 and destination = 9]</metric>
    <metric>mean [average-tt] of drivers with [origin = 2 and destination = 10]</metric>
    <metric>mean [average-tt] of drivers with [origin = 3 and destination = 8]</metric>
    <metric>mean [average-tt] of drivers with [origin = 3 and destination = 9]</metric>
    <metric>mean [average-tt] of drivers with [origin = 3 and destination = 10]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 1]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 2]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 3]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 4]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 5]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 6]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 7]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 8]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 9]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 10]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 11]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 12]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 13]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 14]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 15]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 16]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 17]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 18]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 19]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 20]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 21]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 22]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 23]</metric>
    <metric>item 0  [avg-num-drv] of roads with [road-id = 24]</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="0.95499259"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="q-learning-alpha" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="alpha-varying-light" repetitions="1" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>avg-occ-dev</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="0.95499259"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="q-learning-alpha" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="alpha-varying-light-10times" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>avg-occ-dev</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="0.95499259"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="q-learning-alpha" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gamma-varying-light-10times" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>avg-occ-dev</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="0.95499259"/>
    </enumeratedValueSet>
    <steppedValueSet variable="q-learning-gamma" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-alpha">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>num-congested-roads</metric>
    <metric>aed 1 8</metric>
    <metric>aed 1 9</metric>
    <metric>aed 1 10</metric>
    <metric>aed 2 8</metric>
    <metric>aed 2 9</metric>
    <metric>aed 2 10</metric>
    <metric>aed 3 8</metric>
    <metric>aed 3 9</metric>
    <metric>aed 3 10</metric>
    <metric>std-dev-of-aed 1 8</metric>
    <metric>std-dev-of-aed 1 9</metric>
    <metric>std-dev-of-aed 1 10</metric>
    <metric>std-dev-of-aed 2 8</metric>
    <metric>std-dev-of-aed 2 9</metric>
    <metric>std-dev-of-aed 2 10</metric>
    <metric>std-dev-of-aed 3 8</metric>
    <metric>std-dev-of-aed 3 9</metric>
    <metric>std-dev-of-aed 3 10</metric>
    <metric>overload-ratio 1</metric>
    <metric>overload-ratio 2</metric>
    <metric>overload-ratio 3</metric>
    <metric>overload-ratio 4</metric>
    <metric>overload-ratio 5</metric>
    <metric>overload-ratio 6</metric>
    <metric>overload-ratio 7</metric>
    <metric>overload-ratio 8</metric>
    <metric>overload-ratio 9</metric>
    <metric>overload-ratio 10</metric>
    <metric>overload-ratio 11</metric>
    <metric>overload-ratio 12</metric>
    <metric>overload-ratio 13</metric>
    <metric>overload-ratio 14</metric>
    <metric>overload-ratio 15</metric>
    <metric>overload-ratio 16</metric>
    <metric>overload-ratio 17</metric>
    <metric>overload-ratio 18</metric>
    <metric>overload-ratio 19</metric>
    <metric>overload-ratio 20</metric>
    <metric>overload-ratio 21</metric>
    <metric>overload-ratio 22</metric>
    <metric>overload-ratio 23</metric>
    <metric>overload-ratio 24</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="0.95499259"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-alpha">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-random" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-std-scenario</setup>
    <go>go</go>
    <final>setup-std-scenario</final>
    <timeLimit steps="100"/>
    <metric>avg-travel-time</metric>
    <metric>num-congested-roads</metric>
    <metric>aed 1 8</metric>
    <metric>aed 1 9</metric>
    <metric>aed 1 10</metric>
    <metric>aed 2 8</metric>
    <metric>aed 2 9</metric>
    <metric>aed 2 10</metric>
    <metric>aed 3 8</metric>
    <metric>aed 3 9</metric>
    <metric>aed 3 10</metric>
    <metric>std-dev-of-aed 1 8</metric>
    <metric>std-dev-of-aed 1 9</metric>
    <metric>std-dev-of-aed 1 10</metric>
    <metric>std-dev-of-aed 2 8</metric>
    <metric>std-dev-of-aed 2 9</metric>
    <metric>std-dev-of-aed 2 10</metric>
    <metric>std-dev-of-aed 3 8</metric>
    <metric>std-dev-of-aed 3 9</metric>
    <metric>std-dev-of-aed 3 10</metric>
    <metric>overload-ratio 1</metric>
    <metric>overload-ratio 2</metric>
    <metric>overload-ratio 3</metric>
    <metric>overload-ratio 4</metric>
    <metric>overload-ratio 5</metric>
    <metric>overload-ratio 6</metric>
    <metric>overload-ratio 7</metric>
    <metric>overload-ratio 8</metric>
    <metric>overload-ratio 9</metric>
    <metric>overload-ratio 10</metric>
    <metric>overload-ratio 11</metric>
    <metric>overload-ratio 12</metric>
    <metric>overload-ratio 13</metric>
    <metric>overload-ratio 14</metric>
    <metric>overload-ratio 15</metric>
    <metric>overload-ratio 16</metric>
    <metric>overload-ratio 17</metric>
    <metric>overload-ratio 18</metric>
    <metric>overload-ratio 19</metric>
    <metric>overload-ratio 20</metric>
    <metric>overload-ratio 21</metric>
    <metric>overload-ratio 22</metric>
    <metric>overload-ratio 23</metric>
    <metric>overload-ratio 24</metric>
    <enumeratedValueSet variable="two-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-gamma">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-drivers">
      <value value="1001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="q-learning-alpha">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="roads-capacity">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-eight">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="three-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="route-weight-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tt-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-ten">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inst-road-view?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-episodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="travel-time-beta">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cap-randomness">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-nine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-by">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
