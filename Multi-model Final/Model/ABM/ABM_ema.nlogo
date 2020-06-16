extensions [csv stats profiler]
breed [ neighborhoods neighborhood ]
breed [ new-neighborhoods new-neighborhood ]
breed [ municipalities municipality ]
breed [ MT-sources MT-source ]
breed [ LT-sources LT-source ]
breed [ contracts contract ]

globals
[
  ;data
  characteristics
  gamma_dist_data
  gamma_hp
  length-data
  kwb
  mun-keys
  mun-list
  e_label_data
  MT_source_data
  to-create-municipalities
  ABM-constants

  ;EMA TEST
  glob_cityheating

  ;SD-input
  SD-data
  SD-data_index
  time-index
  SD-municipality-index
  gas-price
  electricity-price
  heat-price
 ; insulation-improvement
  SD_construction
  SD_demolition
  SD_e-use_new-construction
  SD_systemcost-reduction
  SD_insulationcost-reduction
  SD_system-maturity
  SD_insulation-maturity
  SD_willingness
  relocation-mobility

  ;SD-output
  energy-improvement-investment
  insulation-investment-global
  current-systems-global
  percentage-AHP
  percentage-HCS+TESW


  ; General constants
  caloric-value
  relative_xy_netherlands
  NetLogo_RD_distance

  ; updated constants
  cost_substation
  delivery-system-cost
  booster_cost
  combi-hp_cost
  internal-piping-cost_noblock
  internal-piping-cost_block
  cost_management
  production-subsidy
  investment-subsidy
  largest-contract
  max-diversity

  ; initial constants
  hpAefficiency
  hpGefficiency
  GTF
  LT_efficiency
  MT_efficiency
  HT_efficiency
  c_pipe
  HCS_fixed_cost
  HCS_var_cost
  TESW_fixed_cost
  TESW_var_cost
  residual_heat_couple_cost
  detour_factor
  p_cap
  cost_substation_initial
  delivery-system-cost_initial
  booster_cost_initial
  combi-hp_cost_initial
  internal-piping-cost_noblock_initial
  internal-piping-cost_block_initial
  cost_management_initial
  cost_compensation
  residual_heat_production-cost
  pump-energy
  relative-maintenance-cost
  relative-overhead-cost
  efficiency_WKO
  efficiency_TEO

  ;plotting stuff
  plot-list

  ;tracking initials
  TESW-factor-initial
  initial-neighborhoods
  ;bugfix
  end?
]

municipalities-own
[
  name
  my-neighborhoods
  my-new-neighborhoods
  current-systems-aggregate
  energy-savings
  primary-heat-capacity
  available-heat-capacity
  construction-capacity
  current-construction
  current-demolition
  available-construction-capacity
  total_houses
  my-color
  energy-change-per-age-group
  insulation-investment
  current-systems
  current-new-construction
  current-willingness-factor

  traditional-contracts
  integrated-contracts

]

neighborhoods-own
[
  ; general characteristics
  name
  total_houses
  new_houses
  demolished-houses
  area
  property-distribution
  average-property-value
  average-income
  household-low-40
  household-high-20
  cityheating
  my-municipality
  municipality-agent
  my-grid
  my-contracts
  new?


  ; KPI's
  temp-suitability
  init-elabels
  reference-elabels
  current-elabels
  current-average-connection-value
  energy-savings
  savings-per-age-group
  energy-change-per-age-group
  investment-insulation
  insulation-potential
  investment-insulation-total
  investment-insulation-total_t-1
  last-year-insulation
  investment-HP
  investment-hp-total
  investment-hp-total_t-1
  current-average-spending-owner
  remaining-hp-budget
  HG-investment-per-connection
  net-capacity
  net-temp


  ; scenario variables
  ROI

  ; behavioural thresholds
  economic-threshold
  capital-threshold
  hp-threshold

  ;contracts
  individual-insulation-contract
  corp-insulation-contract
  HG-contract

  ; Housing characteristics
  housing-type
  housing-size
  housing-age
  share-monuments

  ; system characteristics
  HG-business-case
  net_connections
  HG-characteristics
  HG-investment-cost-total
  best_primary_source_option
  my_potential_MT-source
  my_potential_grid_connection
  pipe_distances_MT-source
  p_sec
  nearby-sources
  nearby-neighborhoods
  potential_grid-connections
  new-hg
  HCS-allowed?
  HCS-potential
  GHP-potential
  TESW-potential
  used-HCS/GHP
  source-options
  BEP-HG-options
  investment-cost-per-option

  ; exceedance probabilities
  BEP-exceedance
  cost-undershoot

  BEP-exceedance-HP
  cost-undershoot-HP

  ; Gamma-distributions

  BEP-distribution
  cost-distribution
  hp-cost-dist
  hp-threshold-dist


  ;for reinitializing without loading all agents

  initial-BEP-distribution
  initial-cost-distribution
  initial-hp-threshold-dist
  initial-hp-cost-dist
  potential
  binary-labels
  initial-average-connection-value
  initial-cityheating


]

new-neighborhoods-own
[
  ; general characteristics
  name
  total_houses
  cityheating
  my-municipality
  municipality-agent

]
MT-sources-own
[
  name
  type-source
  status
  souce-temp
  couple_cost_min
  couple_cost_max
  production-cost
  total-capacity
  current-capacity
  used-capacity
  my-grid

]

LT-sources-own
[
  type-source
  total-capacity
  my-grid
  new?
]

links-own
[
  initial?
]

contracts-own
[
  city
  my-neighborhood-agent
  my-neighborhood
  task ; insulation, HP or HG
  size-contract ; size
  stakeholder-diversity
  task-complexity
  life-cycle-score
  integrated-score
  building-team-score
  traditional-score
  most-suitable-contract

]

to run-profiler [x y]
  profiler:start
  repeat x [run y]
  profiler:stop
  print profiler:report
  profiler:reset

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;GO PROCEDURE;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
  if ticks = 2020 [reset-timer]

  check-for-errors

  if ema-controls = false
  [
  update-sd-vars
  ]
  update-cost-figures
  update-globs
  update-construction
  update-system-colors

  ask initial-neighborhoods
  [
    keep-track-developments
    update-insulation-costs
    update-HP-costs
    calculate-ROI
    calc-ex-probs
    invest-insulation
    calculate-current-elabels
    calculate-temp-suitability
    update-cost-stats
    invest-hp
    ; Speeding up the model
    if HG-investment-per-connection < dont-bother-slider [
      calculate-HG-investment-cost
      calculate-hg-business-case
      invest-hg
    ]
    calc-business-case-existing-grid-connection
    update-contract

  ]

  ask municipalities
  [
    update-municipal-contracts
    build-new-neighborhoods
    demolish-neighborhoods
    aggregate-neighborhood-data

  ]

  generate-SD-output


  save-system-output
  save-energy-change
  save-current-elabels
  save-neighbourhood-output

  ; Tick procedures
  if ticks = end-period
  [
    set end? True
    type "Simulation run took " type timer print " seconds"
    stop
  ]
  (ifelse
    time-step = "Quarter" [tick-advance 0.25]
    time-step = "Year" [tick-advance 1])
  update-plots
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;GLOBAL PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to update-globs
  set glob_cityheating sum [cityheating * total_houses] of neighborhoods / sum [total_houses] of neighborhoods
  set largest-contract max [size-contract] of contracts + 0.0000001
  set max-diversity max [stakeholder-diversity] of contracts
end

to update-sd-vars
  let time position "Time" SD-data_index
  let index_gas-p position "Average Gas Price" SD-data_index
  let index_elec-p position "Average Electricity Price" SD-data_index
  let index_reloc-f position "Relocation mobility factor" SD-data_index

  ; Will result in inssues with quarter, but probably not going to use that anyway
  let SD-this-tick item position ticks time-index SD-data

  if EMA-controls = False
  [
    set gas-price item index_gas-p SD-this-tick
    set heat-price gas-price /  (caloric-value / 100); factor for m3 to GJ
    set electricity-price item index_elec-p SD-this-tick
    set relocation-mobility item index_reloc-f SD-this-tick
  ]

  let construction-this-tick item position ticks time-index SD_construction
  let demolition-this-tick item position ticks time-index SD_demolition
  let willingness-this-tick item position ticks time-index SD_willingness

  ask municipalities [
    set current-new-construction round item position name SD-municipality-index construction-this-tick
    set current-demolition round item position name SD-municipality-index demolition-this-tick
    set current-willingness-factor item position name SD-municipality-index willingness-this-tick
  ]

end

to update-cost-figures
  let index-system first SD_systemcost-reduction

  let system-cost-reduction-this-tick item position ticks time-index SD_systemcost-reduction

  let LT-cost-reduction item position "LT Heating Grid" index-system system-cost-reduction-this-tick
  let MT-cost-reduction item position "MT Heating Grid" index-system system-cost-reduction-this-tick
  let HT-cost-reduction item position "HT Heating Grid" index-system system-cost-reduction-this-tick

  let general-HG-cost-reduction mean (list LT-cost-reduction MT-cost-reduction HT-cost-reduction)

  set cost_substation general-HG-cost-reduction * cost_substation_initial
  set delivery-system-cost LT-cost-reduction * delivery-system-cost_initial
  set booster_cost LT-cost-reduction * booster_cost_initial
  set combi-hp_cost LT-cost-reduction *  combi-hp_cost_initial
  set internal-piping-cost_noblock general-HG-cost-reduction * internal-piping-cost_noblock_initial
  set internal-piping-cost_block general-HG-cost-reduction * internal-piping-cost_block_initial
  set cost_management general-HG-cost-reduction * cost_management_initial

  set production-subsidy (list LT-production-subsidy MT-production-subsidy)
  set investment-subsidy (list LT-investment-subsidy MT-investment-subsidy)

end

to update-system-colors
  if system-colors = True
  [

    ask neighborhoods
    [
      let a item 0 reverse current-elabels;
      let b item 1 reverse current-elabels;

      let systems (list cityheating sum (map * investment-hp-total (list a b a b)) (1 - cityheating - sum (map * investment-hp-total (list a b a b)) ))
      set color (ifelse-value
        max systems = cityheating and net-temp = "HT" [red]
        max systems = cityheating and net-temp = "MT" [orange]
        max systems = cityheating and net-temp = "LT" [cyan]
        max systems = sum investment-hp-total [blue]
        [grey] )
    ]
  ]
end

to generate-SD-output

  set energy-improvement-investment [sentence (list name) energy-change-per-age-group] of municipalities
  set insulation-investment-global [map [ x -> x * (total_houses / sum [total_houses] of municipalities) ] insulation-investment] of municipalities

  let calc-list (list 0 0 0 0 0 0)
  foreach insulation-investment-global [ x -> set calc-list (map + calc-list x)]
  set insulation-investment-global calc-list

 ; set current-systems-global [sentence (list  word "'" name "'") current-systems] of municipalities

end

to save-energy-change
  if ema-controls = True
  [
    file-open "energy-change.csv"
    file-print csv:to-row [csv:to-row (sentence (list ticks name) energy-change-per-age-group)] of municipalities
    file-close
  ]
end

to save-current-elabels
  if ema-controls = True
  [
    file-open "insulation-investment.csv"
    file-print csv:to-row sentence ticks insulation-investment-global
    file-close
  ]
end


to save-system-output
  if ema-controls = True
  [
    file-open "systems.csv"
    file-print csv:to-row [csv:to-row (sentence (list ticks name) current-systems-aggregate)] of municipalities
    file-close
  ]
end

to save-neighbourhood-output
  if True = True
  ;if ema-controls = True
  [
    file-open "neighbourhood-model-outcome.csv"
    file-print csv:to-row [csv:to-row (sentence (list ticks my-municipality name) current-elabels (list cityheating net-temp sum investment-hp-total current-average-spending-owner))] of initial-neighborhoods
    file-close
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;AGENT PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to calc-ex-probs
  ; update-cost-stats

  ; create empty lists to fill with exceedance probabilities from cdf-gamma
  set BEP-exceedance (list) set cost-undershoot (list)
  foreach (list 0 1 2) [ y ->
    let BEP-exceedance-calc (list)
    foreach BEP-distribution [ x -> set BEP-exceedance-calc lput (1 - cdf-gamma item y economic-threshold item 1 x item 2 x) BEP-exceedance-calc]
    set BEP-exceedance-calc (map * binary-labels BEP-exceedance-calc)
    set BEP-exceedance lput BEP-exceedance-calc BEP-exceedance
  ]
  foreach cost-distribution [ x -> set cost-undershoot lput (cdf-gamma capital-threshold item 1 x item 2 x) cost-undershoot]
  ; set cost-undershoot (map * binary-labels cost-undershoot )


  ; set threshold values for heating pumps based on ROI, E-price, G-price and efficiencies. ALL in kWh
  let hpA-threshold map [ x -> x / max (list 0.001 ((gas-price / caloric-value * 3.6) - (electricity-price / hpAefficiency)))] ROI
  let hpG-threshold  map [ x -> x / max (list 0.001 ((gas-price / caloric-value * 3.6) - (electricity-price / hpGefficiency))) ] ROI


  set hp-threshold (list hpA-threshold hpG-threshold) set cost-undershoot-hp (list)
  ; create empty lists to fill with exceedance probabilities from cdf-gamma
  set BEP-exceedance-HP (list)
  let hp-techs (list hpA-threshold hpG-threshold hpA-threshold hpG-threshold)

  foreach (list 0 1 2)
  [ z ->
    let BEP-exceedance-HP-calc (list)
    (foreach hp-threshold-dist hp-techs [ [x y] -> set BEP-exceedance-HP-calc lput (1 - cdf-gamma item z y item 1 x item 2 x) BEP-exceedance-HP-calc])
    set BEP-exceedance-HP lput BEP-exceedance-HP-calc BEP-exceedance-HP
  ]


  foreach hp-cost-dist [ x -> set cost-undershoot-hp lput (cdf-gamma remaining-hp-budget item 1 x item 2 x) cost-undershoot-HP]

end

to update-HP-costs
  let index-system first SD_systemcost-reduction
  let system-cost-reduction-this-tick item position ticks time-index SD_systemcost-reduction

  let AHP-cost-reduction item position "Air Heat Pump" index-system system-cost-reduction-this-tick
  let GHP-cost-reduction item position "Ground Heat Pump" index-system system-cost-reduction-this-tick
  let HHP-cost-reduction item position "Hybrid Heat Pump" index-system system-cost-reduction-this-tick

  let factors (list AHP-cost-reduction AHP-cost-reduction GHP-cost-reduction GHP-cost-reduction)

  set hp-cost-dist (list)
  set hp-threshold-dist (list)
  foreach [0 1 2 3] [ x ->
    set hp-threshold-dist lput replace-item 1 item x initial-hp-threshold-dist (item x factors * item 1 item x initial-hp-threshold-dist) hp-threshold-dist
    set hp-cost-dist lput replace-item 1 item x initial-hp-cost-dist (item x factors * item 1 item x initial-hp-cost-dist) hp-cost-dist
  ]


end

to update-insulation-costs
  ;; Monstercode: BEP-distribution consists of [mean alpha theta] for each label. Alpha is multiplied with insulation improvement to shift mean due to cost improvements from technology.
  ;; Mean is shifted in opposite direction for step-wise investments. The cumulative percentage of investments made multiplied by the step-wise factor determines the effect of step-wise investment on cost.
  let index-insulation butfirst (first SD_insulationcost-reduction)

  let year ifelse-value end? != false [2020][ticks]

  let insulation-cost-reduction-this-tick butfirst item position year time-index SD_insulationcost-reduction

  foreach BEP-distribution [ x ->
    let position-x position x BEP-distribution
    ; insulation cost is from A to F, BEP from F to A
    let insulation-improvement item position-x reverse insulation-cost-reduction-this-tick

    ; implementation for technology-effect and subsidies
    set BEP-distribution replace-item position-x BEP-distribution replace-item 1 item position-x BEP-distribution
    ((item 1 item position-x initial-BEP-distribution) * 1 / (insulation-improvement * (1 - insulation-subsidy)))
  ]

  foreach cost-distribution [ x ->
    let position-x position x cost-distribution
    let insulation-improvement item position-x reverse insulation-cost-reduction-this-tick
    ; implementation for technology-effect and subsidies
    set cost-distribution replace-item position-x cost-distribution replace-item 1 item position-x cost-distribution
    ((item 1 item position-x initial-cost-distribution) * insulation-improvement * (1 - insulation-subsidy))
  ]

end

to invest-insulation
  ; lets assume that only home-owners consider income. Corporations always find investors if ROI is ok.
  set insulation-potential (list (map * item 0 BEP-exceedance cost-undershoot) item 1 BEP-exceedance item 2 BEP-exceedance)

  ; compensate for monuments that are difficult to renovate.
  let m (1 - (4 * share-monuments))
  let monumental-options (list 1 1 m m m m)
  foreach insulation-potential [ x ->
    let index (position x insulation-potential)
    ; Potential needs to differ for 2 investment-methods based on homo-economicus assumption
    if sum x > 1 and best-value-investment = True [set insulation-potential replace-item index insulation-potential (map [ y -> y / ( sum x)] x ) ]
    set insulation-potential replace-item index insulation-potential (map * item index insulation-potential monumental-options)
  ]

  ;; Two options: invest on best-value or invest on best-insulation
  ifelse best-value-investment = False
  [
    ; This option assumes that people will always invest to the best insulation option affordable, even though not necessary the best economic alternative
    foreach (list 0 1 2)
    [y ->

      let new-investment (map - item y insulation-potential item y investment-insulation)
      let construction-requirement sum  map [x -> x * (item y property-distribution)] (new-investment) * total_houses

      ; Check if construction requirement is high enough to construct, else reduce construction
      if construction-requirement != 0 and [available-construction-capacity] of municipality-agent < construction-requirement
      [
        let reductionfactor [available-construction-capacity] of municipality-agent /  construction-requirement
        set new-investment map [x -> reductionfactor * x] new-investment
        set insulation-potential replace-item y insulation-potential map [x -> reductionfactor * x] item y insulation-potential
      ]

      let a reverse item y insulation-potential
      let b reverse item y investment-insulation
      let new-investment-insulation (list max( list item 0 a item 0 b)) ;reverse investment-insulation)

      ; create new-investment list
      foreach (list 1 2 3 4 5 )
      [ x ->
        set new-investment-insulation fput max(list max(list 0(item x a - sum new-investment-insulation) ) (min(list (item x b) (1 - sum new-investment-insulation))) )  new-investment-insulation
      ]
      set new-investment (map - new-investment-insulation item y investment-insulation)
      set construction-requirement sum map [x -> x * (item y property-distribution)] new-investment * total_houses

      ; if needed calculate new new-investment scaled to construction-capacity
      if construction-requirement > [available-construction-capacity] of municipality-agent and construction-requirement != 0
      [
        let reductionfactor [available-construction-capacity] of municipality-agent / construction-requirement
        set new-investment-insulation map [x -> reductionfactor * x] new-investment-insulation
        set new-investment (map - new-investment-insulation item y investment-insulation)
        set construction-requirement sum map  [x -> x * (item y property-distribution)] new-investment * total_houses
      ]

      set investment-insulation replace-item y investment-insulation new-investment-insulation

      ask municipality-agent [
        set current-construction current-construction + construction-requirement
        set available-construction-capacity construction-capacity - current-construction
      ]

    ]
  ]
  [
    ; This option assumes a probability that people will choose a less economic option, based on the relative economic perfomance of these options.


    foreach (list 0 1 2)
    [ z ->

      ; prevent negative new-investment
      let new-investment (map - item z insulation-potential item z investment-insulation)
      set new-investment (map [ [x y] -> max (list x y)] new-investment [ 0 0 0 0 0 0 ])

      if sum new-investment > 0 [
        set new-investment map [ x -> x / (sum new-investment + 1)] new-investment
      ]

      let construction-requirement sum (map * potential map [x -> x * (item z property-distribution)] (new-investment)) * total_houses

      ; Adapt construction if requirement is too high.
      if construction-requirement != 0 and [available-construction-capacity] of municipality-agent < construction-requirement
      [
        let reductionfactor [available-construction-capacity] of municipality-agent / (1.01 * construction-requirement)
        set new-investment map [x -> reductionfactor * x] new-investment
        set construction-requirement sum (map * potential map [x -> x * (item z property-distribution)] (new-investment)) * total_houses
      ]

      ; Prevent that new investment is bigger than that what is remaining.
      ; Rounding error may cause this to be negative
      let total-remainder max(list 0 (1 - sum item z investment-insulation))

      if sum new-investment > total-remainder
      [
        set new-investment map [x -> x * (total-remainder / (6 * sum new-investment))] new-investment
      ]

      set construction-requirement sum (map * potential map [x -> x * (item z property-distribution)] (new-investment)) * total_houses
      ask municipality-agent
      [
        set current-construction current-construction + construction-requirement
        set available-construction-capacity construction-capacity - current-construction
      ]

      ;; UPDATE ACTUAL investment
      set investment-insulation replace-item z investment-insulation (map + item z investment-insulation new-investment)



    ]
  ]

  ; Take mean for all property types
  calculate-investment-total

end

to calculate-investment-total

  let investment-owner map [ x -> x * item 0 property-distribution] item 0 investment-insulation
  let investment-corp map [ x -> x * item 1 property-distribution] item 1 investment-insulation
  let investment-rent map [ x -> x * item 2 property-distribution] item 2 investment-insulation

  set investment-insulation-total (map + (map + investment-owner investment-corp) investment-rent)

end

to update-contract

  let time position "Time" SD-data_index
  let system-maturity-this-tick item position 2020 time-index SD_system-maturity
  let insulation-maturity-this-tick item position 2020 time-index SD_insulation-maturity
  let new-insulation [ 0 0 0 0 0 0 ]
  let new-hp [0 0 0 0]
  ifelse end? = false
  [
    set system-maturity-this-tick item position ticks time-index SD_system-maturity
    set insulation-maturity-this-tick item position ticks time-index SD_insulation-maturity
    set new-insulation (map[[x y] -> max(list x y)] (map - investment-insulation-total investment-insulation-total_t-1) (list 0 0 0 0 0 0))
    set new-hp (map - investment-hp-total investment-hp-total_t-1)

  ]
  [
    set largest-contract 1
    set max-diversity 1
  ]

  ask my-contracts with [task = "Insulation"]
  [

    let total-investment sum new-insulation
    let share-investments [ 0 0 0 0 0 0 ]
   ;let share-investments [ 0.17 0.17 0.17 0.17 0.17 0.17 ]
    if total-investment > 0
    [ set share-investments map [x -> x / total-investment] new-insulation]


    set task-complexity sum (map * share-investments map [x -> 1 - x ] butfirst insulation-maturity-this-tick)
    if task-complexity < 0 [show total-investment show insulation-maturity-this-tick show new-insulation]
    set size-contract total-investment * [total_houses] of my-neighborhood-agent

  ]

  ask my-contracts with [task = "HP"]
  [

    let total-investment sum new-hp
    let share-investments [ 0.25 0.25 0.25 0.25]
    if total-investment > 0
    [ set share-investments map [x -> x / total-investment] new-hp]
    let hp-maturities map [x -> 1 - x ] sublist system-maturity-this-tick 6 9

    ; BEUN omdat hybride niet is meegenomen
    let hp-list (list item 0 hp-maturities item 0 hp-maturities item 1 hp-maturities item 1 hp-maturities)
    set task-complexity sum (map * share-investments hp-list )
    set size-contract 0.5 * total-investment * [total_houses] of my-neighborhood-agent

  ]

  ask my-contracts with [task = "HG"]
  [
    let hg-temp [net-temp] of my-neighborhood-agent
    set task-complexity (ifelse-value
      hg-temp = "LT" [1 - item 3 system-maturity-this-tick]
      hg-temp = "MT" [1 - item 4 system-maturity-this-tick]
      hg-temp = "HT" [1 - item 5 system-maturity-this-tick]
      [0]
    )

    set size-contract [cityheating * total_houses] of my-neighborhood-agent
  ]

;  set largest-contract max [size-contract] of contracts + 0.0000001
;  set max-diversity max [stakeholder-diversity] of contracts

  ask my-contracts [
      set life-cycle-score ((2 - task-complexity) + (1 + size-contract / largest-contract) + (2 - stakeholder-diversity / max-diversity)) / 6
      set integrated-score ((2 - 0.5 * task-complexity) + (2 - size-contract / largest-contract) + (2 - 1.5 * (stakeholder-diversity / max-diversity))) / 6
      set building-team-score ((1 + 0.5 * task-complexity) + (2 - size-contract / largest-contract) + (2 - 0.5 * (stakeholder-diversity / max-diversity))) / 6
      set traditional-score ((1 + task-complexity) + (1 + size-contract / largest-contract) + (1 + stakeholder-diversity / max-diversity)) / 6

      let contract-list (list life-cycle-score integrated-score building-team-score traditional-score)
      set most-suitable-contract item position max contract-list contract-list ["Life-cycle" "Integrated" "Building Team" "Traditional"]

;      set life-cycle-score mean (list (1 - task-complexity) (size-contract / largest-contract) (1 - stakeholder-diversity))
;      set integrated-score mean (list (1 - 0.75 * task-complexity) (1 - size-contract / largest-contract) (1 - 0.75 * stakeholder-diversity))
;      set building-team-score mean (list (0.75 * task-complexity) (1 - size-contract / largest-contract) (0.75 * stakeholder-diversity))
;      set traditional-score mean (list (task-complexity) (size-contract / largest-contract) (stakeholder-diversity))
  ]
  ;  my-neighborhood
  ;  owner ;Individuals or corp
  ;  task ; insulation, HP or HG
  ;  number_houses ; size
  ;  diversity-stakeholders
  ;  diversity-technology

end

to invest-hp

  let a item 0 reverse current-elabels;
  let b item 1 reverse current-elabels;


  ; potential is product of BEP-exceedance, cost-undershoot and percentage of label A and B for howme-owner, only BEP and Label for investors. Compensate for current initial-cityheating

  let investment-hp-potential-homeowners  map [x -> x * (1 - cityheating)] (map * (map * item 0 BEP-exceedance-HP cost-undershoot-HP) (list a b a b))
  let investment-hp-potential-corp map [x -> x * (1 - cityheating)] (map * item 1 BEP-exceedance-HP (list a b a b))
  let investment-hp-potential-rent map [x -> x * (1 - cityheating)] (map * item 2 BEP-exceedance-HP (list a b a b))

  let investment-hp-potential (list investment-hp-potential-homeowners investment-hp-potential-corp investment-hp-potential-rent)

  foreach (list 0 1 2) [ z ->

    ; normalize potential on potential of other factors, split on energy label
    let norm-A item 0 item z investment-hp-potential + item 2 item z investment-hp-potential + 0.0001
    let norm-B item 1 item z investment-hp-potential + item 3 item z investment-hp-potential + 0.0001
    let norm-list (list norm-A norm-B norm-A norm-B)
    let relative-investment-hp-potential (map / item z investment-hp-potential norm-list)

    ;calculate actual potential based on real and normalized potential
    set investment-hp-potential replace-item z investment-hp-potential (map * item z investment-hp-potential relative-investment-hp-potential )

    ; set up a list for investment-boundary
    let A-HP 1 - (item 0 item z investment-hp + item 2 item z investment-hp)
    let B-HP 1 - (item 1 item z investment-hp + item 3 item z investment-hp)
    let remaining-list (list a-hp b-hp a-hp b-hp)

    ; New-investment is increase: increase may not be negative, and not higher than the remainder available.
    let new-investment-hp (map - item z investment-hp-potential item z investment-hp)

    set new-investment-hp (map [ [x y] -> min (list x y)] new-investment-hp remaining-list)
    set new-investment-hp (map [ [x y] -> max (list x y)] new-investment-hp (list 0 0 0 0))

    ; add increase to current
    set investment-hp replace-item z investment-hp (map + item z investment-hp new-investment-hp)
  ]

  let hp-owner map [ x -> x * item 0 property-distribution  ] item 0 investment-hp
  let hp-corp map [ x -> x * item 1 property-distribution ] item 1 investment-hp
  let hp-rent map [ x -> x * item 2 property-distribution  ] item 2 investment-hp

  set investment-hp-total (map + (map + hp-owner hp-corp ) hp-rent)

end

to invest-HG
  if best_primary_source_option != "No available options"
  [
    let temp-index (ifelse-value
      best_primary_source_option = "Residual Heat" [ 1 ]
      best_primary_source_option = "Heat Cold Storage" [ 0 ]
      best_primary_source_option = "Thermal Energy Surface Water" [ 0]
    )

    let construction-requirement item temp-index net_connections * Relative-capacity-use-grid-construction

    if construction-requirement < [available-construction-capacity] of municipality-agent or new? and HG-business-case = True and new-hg != True
    [
      if my_potential_MT-source != 0 and best_primary_source_option = "Residual Heat"

      [
        if not new? [
          ask municipality-agent
          [
            set current-construction current-construction + construction-requirement
            set available-construction-capacity construction-capacity - current-construction
          ]
        ]
        set new-HG true
        set cityheating cityheating + item temp-index net_connections / total_houses

        ifelse my_potential_grid_connection != nobody
        [
          create-link-with my_potential_grid_connection [set color red ]
          ask my_potential_grid_connection [set my-grid (turtle-set my-grid [my-grid] of myself)]
          set my-grid (turtle-set my-grid [my-grid] of my_potential_grid_connection)

          ask my_potential_grid_connection
          [
            let to_connect_source [my_potential_MT-source] of myself
            if member? to_connect_source my-grid = False
            [
              let connected? False
              let potential_with_connection potential_grid-connections with [member? to_connect_source my-grid]
              if any? potential_with_connection [
                ask min-one-of potential_with_connection [distance myself]
                [
                  create-link-with myself [set color yellow]
                  set connected? True
                  set my-grid (turtle-set my-grid [my-grid] of myself [my-grid] of to_connect_source)
                  ask to_connect_source [set my-grid (turtle-set my-grid [my-grid] of myself) ]
                  ask myself [set my-grid (turtle-set my-grid [my-grid] of myself)
                    ask my-grid [set my-grid (turtle-set my-grid [my-grid] of myself)]]
                ]
              ]
              if connected? = false
              [
                let closest-of-grid nobody
                ask to_connect_source
                [
                  set closest-of-grid min-one-of [my-grid] of myself [distance to_connect_source]
                  create-link-with closest-of-grid [set color yellow]
                  set my-grid (turtle-set my-grid [my-grid] of closest-of-grid )
                  ask closest-of-grid [ set my-grid (turtle-set my-grid [my-grid] of myself )
                    ask my-grid [set my-grid (turtle-set my-grid [my-grid] of myself)] ]
                ]
              ]
            ]
          ]

          ask my_potential_MT-source
          [
            set current-capacity current-capacity - [item 1 p_sec] of myself
            set my-grid (turtle-set my-grid [my-grid] of myself)
          ]
        ]

        [
          create-link-with my_potential_MT-source [set color white]
          ask my_potential_MT-source
          [
            set current-capacity current-capacity - [item 1 p_sec] of myself
            set my-grid (turtle-set my-grid [my-grid] of myself)
          ]
          set my-grid (turtle-set my-grid [my-grid] of my_potential_MT-source)
        ]

        ;; make sure everyone is part of the grid!
        ask my-grid [set my-grid (turtle-set my-grid [my-grid] of myself)]

        set net-capacity item 1 p_sec
        set net-temp "MT"
      ]

      ;; OTHER OPTIONS THAN RESIDUAL HEAT
      if new-hg != True and best_primary_source_option != "Residual Heat"
      [
        set new-HG true
        set cityheating cityheating + item temp-index net_connections / total_houses
        if not new? [
          ask municipality-agent
          [
            set current-construction current-construction + construction-requirement
            set available-construction-capacity construction-capacity - current-construction
          ]
        ]
        if best_primary_source_option = "Heat Cold Storage" [set used-HCS/GHP used-HCS/GHP + item 0 p_sec ]

        hatch-LT-sources 1
        [

          set size 0.5
          set color cyan - 2
          set xcor xcor + 0.05
          set type-source [best_primary_source_option] of myself
          set shape (ifelse-value
            type-source = "Heat Cold Storage" [ "dot" ]
            type-source = "Thermal Energy Surface Water" ["drop"]
          )
          set total-capacity [item 0 p_sec] of myself
          set my-grid [my-grid] of myself
          set new? True

        ]
        set my-grid (turtle-set my-grid one-of LT-sources with [member? myself my-grid])
        set net-capacity item 0 p_sec
        set net-temp "LT"
      ]
    ]
  ]
end


to calculate-ROI
  ;; ROI is used as measure to determine willingness-to-invest. Higher willingness-to-invest, lower ROI
  ;property-distribution -> (list owner corp rental)

  let adjusted-private-ROI base-private-ROI
  let neighbor-effect 1 - group-behaviour * sum investment-insulation-total
  set adjusted-private-ROI base-private-ROI * neighbor-effect / [current-willingness-factor] of municipality-agent

  ; sum the weighted-ROI's for all categories
  ;set average-ROI weighted-ROI-private + weighted-ROI-corp + weighted-ROI-rental
  set ROI (list adjusted-private-ROI base-corp-ROI base-private-rent-ROI)
  ;set ROI map [x -> x / relocation-mobility] ROI
end

to calculate-temp-suitability
  ; investment-insulation items: 6 = G 5 = F 4 = E 3 = D 2 = C , 1 = B, 0 = A
  let low-temp sum sublist reverse current-elabels 0 2
  let med-temp sum sublist reverse current-elabels 0 5
  let high-temp 1
  set temp-suitability (list low-temp med-temp high-temp)
end

to calculate-current-elabels
  ; calculate current labels based on initial labels and investment behaviour
  set current-elabels (list)
  set energy-savings (list)
  foreach (list 0 1 2 3 4 5 ) [ x ->
    set current-elabels fput ((sum sublist reverse reference-elabels (x + 1) 7 ) * (item x reverse investment-insulation-total) + item x reverse reference-elabels - item x reverse reference-elabels * sum sublist reverse investment-insulation-total 0 x) current-elabels

    ;;; RELOCATE SAVING FACTORS
    set energy-savings fput (sum sublist reverse (map * [1 0.83 0.7 0.6 0.38 0.37 0] reference-elabels) (x + 1) 7  * (item x reverse (map * [0.17 0.24 0.34 0.39 0.34 0.45 ] investment-insulation-total))) energy-savings
  ]
  set current-elabels fput (1 - sum current-elabels) current-elabels

  set energy-savings sum energy-savings
  set current-average-connection-value initial-average-connection-value * (1 - energy-savings)

  ; Calculate age group specific savings
  let current-savings-per-group map [x -> x * energy-savings] housing-age
  set energy-change-per-age-group (map - current-savings-per-group savings-per-age-group)
  set savings-per-age-group current-savings-per-group

end

to update-cost-stats
  ; update thresholds
  set economic-threshold map [x -> x / gas-price] ROI
  ; people invest with
  set capital-threshold min((list (average-property-value * max-capital-inv-share) (average-income * max-income-inv-share  / item 0 ROI)))

  ; what is left for HP's
  let average-cost-insulation (list)
  foreach cost-distribution [ x -> set average-cost-insulation lput (item 0 x) average-cost-insulation ]
  ;; use difference between current and last year as increment
  let owner-investment item 0 investment-insulation
  let last-year-owner item 0 last-year-insulation

  set current-average-spending-owner current-average-spending-owner + (sum map [ x -> max(list 0 x) ](map - owner-investment last-year-owner)) * capital-threshold
  ; set remaining budget -> fix for rounding errors
  ifelse capital-threshold - current-average-spending-owner >= 0
  [set remaining-hp-budget capital-threshold - current-average-spending-owner]
  [set remaining-hp-budget 0 ]

end

to calculate-HG-investment-cost
  ; All cost formulas have been derived from VESTA 4.0 functioneel ontwerp by PBL and CE Delft (2019)
   let a item 0 reverse current-elabels;
  let b item 1 reverse current-elabels;
  ;; NEW HEATING GRIDS
  let total_net_connections total_houses * max(list 0 (1 - cityheating - sum(map * investment-hp-total (list a b a b))))
  set net_connections map [x -> x * total_net_connections] temp-suitability

  if min net_connections < 0 [show min net_connections]
  let percentage_connections map [x -> x / total_houses ] net_connections

  let efficiencies (list LT_efficiency MT_efficiency HT_efficiency)
  set p_sec map [x -> x * current-average-connection-value] (map * efficiencies net_connections)
  set p_sec (map [[x y] -> max (list x y)] p_sec (list 0 0 0))
  ; HEEL ERG VEEL BEUN
  if new-HG != True [

    ifelse total_net_connections = 0 [set best_primary_source_option "No available options"]
    [
      ; GENERAL FACTORS
      ;Based on new net_connections and p_sec select best available source
      select-best-MT-source

      ; BOOSTER cost depending on labels and primary source
      let boosters item 0 net_connections * booster_cost
      let combi-hp (item 1 net_connections - item 0 net_connections) * combi-hp_cost
      let individual-hp boosters + combi-hp

      ; Create list with viable options for primary source
      set source-options (list)
      if my_potential_MT-source != nobody [ if item 1 p_sec < [current-capacity] of my_potential_MT-source and item 1 net_connections > 0 [ set source-options lput "Residual Heat" source-options ]]
      if item 0 p_sec < HCS-potential and item 0 net_connections > 0 [set source-options lput "Heat Cold Storage" source-options]
      if item 0 p_sec < TESW-potential and item 0 net_connections > 0 [set source-options lput "Thermal Energy Surface Water" source-options]
      if length source-options = 0 [ set source-options (list "No available options") ]

      ; Cost of piping is dependent on p_sec, which depends on temperature
      let p_sec_store p_sec
      set p_sec item 1 p_sec_store
      ; HIER ERROR
      ;let cost_pipes (list (400 + 210 * (p_sec * 0.001)^ 0.5))
      let cost_pipes (list runresult c_pipe)
      set p_sec item 0 p_sec_store
      ;set cost_pipes fput (400 + 210 * (p_sec * 0.001)^ 0.5) cost_pipes
      set cost_pipes fput runresult c_pipe cost_pipes
      set p_sec p_sec_store

      ; PRIMARY SOURCE
      let primary_source_cost (list)

      ; COSTS FOR COUPLING RESIDUAL HEAT
      let residual_heat_cost 0
      if my_potential_MT-source != nobody
      [
        let transport-pipe_cost min pipe_distances_MT-source * NetLogo_RD_distance * detour_factor * item 1 cost_pipes
        let residual_heat_coupling_cost
        ( ifelse-value
          couple-cost-residual-heat = "High" [[couple_cost_max] of my_potential_MT-source]
          couple-cost-residual-heat = "Low"  [[couple_cost_min] of my_potential_MT-source]
        )

        set residual_heat_cost residual_heat_coupling_cost * item 1 p_sec + transport-pipe_cost
        set primary_source_cost lput residual_heat_cost primary_source_cost
      ]

      ;COSTS FOR HCS
      if item 0 p_sec < HCS-potential
      [
        let HCS_cost (HCS_fixed_cost + HCS_var_cost * item 0 p_sec + individual-hp)
        set primary_source_cost lput HCS_cost primary_source_cost
      ]
      ;COSTS FOR TESW
      if item 0 p_sec < TESW-potential
      [
        let TESW_cost (TESW_fixed_cost + TESW_var_cost * item 0 p_sec + individual-hp)
        set primary_source_cost lput TESW_cost primary_source_cost
      ]
      ;add huge number for if there are no options at all.
      set primary_source_cost lput 1e17 primary_source_cost

      if force-collective-solution != "No" [ set best_primary_source_option force-collective-solution ]

      set investment-cost-per-option (list)
      let y 0
      foreach source-options [ x ->
        let temp-index (ifelse-value
          x = "Residual Heat" [ 1 ]
          x = "Heat Cold Storage" [ 0 ]
          x = "Thermal Energy Surface Water" [ 0]
          ; BEUN, BETER FIXEN
          x = "No available options" [0]
        )

        let lt-only? (ifelse-value
          x = "Residual Heat" [ 1 ]
          x = "Heat Cold Storage" [ 0 ]
          x = "Thermal Energy Surface Water" [ 0]
          ; BEUN, BETER FIXEN
          x = "No available options" [0]
        )

        ; DISTRIBUTION COST
        let cost_connection_pipes_m item temp-index cost_pipes ; "K_aansl_m"
        let cost_main_pipes_m item temp-index cost_pipes ;"k_dist_m"

        let cost_distribution_substations cost_substation * item temp-index p_sec  ;GTF
        let cost_connection_pipes cost_connection_pipes_m * current-average-connection-value * item temp-index percentage_connections * item 1 HG-characteristics * (item 0 HG-characteristics / total_houses)
        let cost_main_pipes cost_main_pipes_m * item 2 HG-characteristics

        ;TOTAL DISTRIBUTION COST
        let hg-distribution-cost cost_distribution_substations + cost_connection_pipes + cost_main_pipes

        ;INVESTMENTS INTERNAL SYSTEMS
        let delivery-system-total item temp-index net_connections * delivery-system-cost * lt-only?
        let perc_block 0.5
        let stacked item temp-index net_connections * item 1 housing-type
        let internal-piping-cost-total stacked * (internal-piping-cost_noblock * (1 - perc_block) + internal-piping-cost_block)

        ;TOTAL INTERNAL SYSTEMS
        let hg-internal-systems internal-piping-cost-total + delivery-system-total

        ; PROJECT MANAGEMENT
        let project-management (cost_management + cost_compensation) * ((item temp-index net_connections - stacked) + stacked * perc_block)

        ;SUM All cost parts
        set HG-investment-cost-total hg-distribution-cost + hg-internal-systems + item y primary_source_cost + project-management

        ifelse item temp-index net_connections != 0
        [ set HG-investment-per-connection HG-investment-cost-total / item temp-index net_connections]
        [ set HG-investment-per-connection 1e17 ]

        set investment-cost-per-option lput HG-investment-per-connection investment-cost-per-option
        set y y + 1
      ]
    ]
  ]

end

to calculate-hg-business-case
  if best_primary_source_option != "No available options" [

    set BEP-HG-options (list)
    foreach source-options [x ->
      let temp-index (ifelse-value
        x = "Residual Heat" [1 ]
        x = "Heat Cold Storage" [0]
        x = "Thermal Energy Surface Water" [0]
        x = "No available options" [1]
      )

      ; if HG-investment-per-connection != "No Connections" [
      let estimated-heat-use current-average-connection-value * 1650 / 277.78 ;; 1650 -> full load hours. 277.78 kWh per GJ
      let yearly-income estimated-heat-use * heat-price + max-fixed-tariff
      let investment-cost-after-tariff max(list 0.1 (item position x source-options investment-cost-per-option - max-connection-tariff ))

      let primary-production-cost (ifelse-value
        x = "Residual Heat" [[production-cost] of my_potential_MT-source ]
        x = "Heat Cold Storage" [efficiency_WKO * electricity-price]
        x = "Thermal Energy Surface Water" [efficiency_TEO * electricity-price]
        x = "No available options" [1e17]
      )

      let primary-source-cost estimated-heat-use * item temp-index net_connections * primary-production-cost
      let pumping-energy-cost electricity-price * pump-energy * estimated-heat-use * item temp-index net_connections
      let maintenance-cost HG-investment-cost-total * relative-maintenance-cost
      let overhead-cost HG-investment-cost-total * relative-overhead-cost


      let total-investment (investment-cost-after-tariff * item temp-index net_connections) * (1 - item temp-index investment-subsidy)
      let total-yearly-income yearly-income * item temp-index net_connections
      let total-yearly-cost (sum (list primary-source-cost pumping-energy-cost maintenance-cost overhead-cost)) * (1 - item temp-index production-subsidy)
      let BEP-HG (total-yearly-income - total-yearly-cost) / (total-investment + 0.01 )
      set BEP-HG-options lput BEP-HG BEP-HG-options
    ]

    set best_primary_source_option item position max BEP-HG-options BEP-HG-options source-options
    if max BEP-HG-options < 0 [set best_primary_source_option "No available options"]
    ifelse max BEP-HG-options > Heat-company-ROI [set HG-business-case True][set HG-business-case False]
  ]
end

to calc-business-case-existing-grid-connection
  if new-hg = True [
    let temp-index (ifelse-value
      net-temp = "LT" [0]
      net-temp = "MT" [1]
      net-temp = "HT" [2]
    )

    let a item 0 reverse current-elabels;
    let b item 1 reverse current-elabels;
    let total_net_connections total_houses * max(list 0 (1 - cityheating - sum(map * investment-hp-total (list a b a b))))
    set net_connections map [x -> x * total_net_connections] temp-suitability
    let to-connect item temp-index net_connections
   ; type cityheating print (to-connect / total_houses)
    set cityheating cityheating + (to-connect / total_houses); - cityheating * total_houses
  ]

end


to keep-track-developments
  ifelse end? = True
  [ ];set last-year-insulation 0 ]
  [ set last-year-insulation investment-insulation
    set investment-hp-total_t-1 investment-hp-total
    set investment-insulation-total_t-1 investment-insulation-total]
end


to select-best-MT-source

  let find-new-source? True
  if find-new-source? = True [

    ;; 1
    let available-options (ifelse-value
      MT-source-choice = "Option A only"  [(list "Optie A" "" "") ]
      MT-source-choice = "Option A and B" [(list "Optie A" "Optie B" "")  ]
      MT-source-choice = "All-options"    [(list "Optie A" "Optie B" "Optie C")  ]
    )

    let available_sources nearby-sources with [current-capacity > [item 1 p_sec] of myself and member? status available-options]
    set potential_grid-connections nearby-neighborhoods with [cityheating > 0]

    let available_indirect nobody


    let direct_source nobody
    let direct_distance 1E17
    let indirect_source nobody
    let indirect_distances 1E17

    set my_potential_MT-source nobody
    set my_potential_grid_connection nobody

    if any? available_sources [
      set direct_source min-one-of available_sources [distance myself]
      set direct_distance [distance myself] of direct_source
    ]


    ; SELECTION LOGIC CAN BE IMPROVED
    if any? potential_grid-connections
    [

      set available_indirect (turtle-set [nearby-sources] of potential_grid-connections) with [current-capacity > [item 1 p_sec] of myself and member? status available-options]
      let closest_source min-one-of available_indirect [distance min-one-of [potential_grid-connections] of myself [distance myself] ]

      ;  let closest_source min-one-of available_indirect [distance myself]
      if  closest_source != nobody [
        ifelse any? potential_grid-connections with [member? closest_source my-grid]
        [
          set my_potential_grid_connection min-one-of potential_grid-connections with [member? closest_source my-grid] [distance myself]
        ]
        [
          set my_potential_grid_connection min-one-of potential_grid-connections [distance myself + distance closest_source]
        ]
      ]
      set indirect_source closest_source
      ;  set indirect_source min-one-of available_indirect [distance [my_potential_grid_connection] of myself]

      if my_potential_grid_connection != nobody
      [
        ifelse [member? indirect_source my-grid] of my_potential_grid_connection
        [set indirect_distances distance my_potential_grid_connection]
        [set indirect_distances [distance [my_potential_grid_connection] of myself] of indirect_source + distance my_potential_grid_connection]
      ]
    ]

    set pipe_distances_MT-source (list direct_distance indirect_distances)


    ifelse any? available_sources
    [
      set my_potential_MT-source item position min(pipe_distances_MT-source) pipe_distances_MT-source (list direct_source indirect_source)
      if my_potential_MT-source != indirect_source [set my_potential_grid_connection nobody ]
    ]
    [
      set my_potential_MT-source nobody
      set my_potential_grid_connection nobody
    ]
  ]


end

to setup-new-neighborhood
  let neighbor-hood nobody
  ; ifelse random 2 = 0 [set neighbor-hood one-of max-n-of (round count [my-neighborhoods] of myself / 10) [my-neighborhoods] of myself [distance myself]] [set neighbor-hood one-of max-n-of (round count [my-neighborhoods] of myself / 10) [my-neighborhoods] of myself [distance myself]]
  set neighbor-hood one-of [my-neighborhoods] of myself
  set xcor [xcor] of neighbor-hood + random-float 0.5 - 0.25
  set ycor [ycor] of neighbor-hood + random-float 0.5 - 0.25

  set-nearby-agents
  ask nearby-neighborhoods [set nearby-neighborhoods (turtle-set nearby-neighborhoods myself)]

  set shape "house efficiency"
  set size 1.25
  set my-municipality [name] of myself
  set municipality-agent myself
  set current-elabels [ 0 0 0 0 0 0 1 ]
  set investment-hp-total [ 0 0 0 0 ]
  set temp-suitability [ 1 1 1 ]
  set net_connections (list total_houses total_houses total_houses)

  let estimated-heat-use item position my-municipality SD-municipality-index item position ticks time-index SD_e-use_new-construction; * 1650 / 277.78
  set current-average-connection-value estimated-heat-use * 277.78 / 1650
  ask myself [
    set my-new-neighborhoods (turtle-set my-neighborhoods myself)
  ]
  set source-options [source-options] of neighbor-hood
  set p_sec total_houses * current-average-connection-value
  set p_sec map [x -> x * p_sec] (list LT_efficiency MT_efficiency HT_efficiency)
  let system-probability (list)
  let system-options (list new-neighborhoods-gas new-neighborhoods-HG new-neighborhoods-HP)
  let total 0
  foreach system-options [x ->
    set total total + x
    set system-probability lput total system-probability
  ]
  let chance random-float 1
  let choice false
  foreach system-probability [x -> if choice = false and x - chance >= 0 [set choice x ]]
  let system item position choice system-probability ["Gas" "Heating Grid" "Heat pump"]
  if (empty? source-options or source-options = "No available options") and my_potential_MT-source != nobody [set system one-of ["Gas" "Heat pump"]]
  if best_primary_source_option = "Not yet determined" [set source-options ["Residual Heat" "Heat Cold Storage" "Thermal Energy Surface Water"]]
  (ifelse system = "Gas" [set investment-hp-total [ 0 0 0 0 ] ]
    system = "Heating Grid" [set investment-hp-total [ 0 0 0 0 ]  calculate-best-new-HG ]
    ; CHECK for AHP GHP A B label
    system = "Heat pump" [set investment-hp-total [ 0.5 0 0.5 0 ] ] )



end

to calculate-best-new-HG

  ;GRID is more expensive with MT

  select-best-MT-source

  let p_sec_store p_sec
  set p_sec item 1 p_sec_store
  let cost_pipes (list runresult c_pipe)
  set p_sec item 0 p_sec_store
  set cost_pipes fput runresult c_pipe cost_pipes
  set p_sec p_sec_store

  ; PRIMARY SOURCE
  let primary_source_cost (list)

  ; COSTS FOR COUPLING RESIDUAL HEAT
  let residual_heat_cost 1e17

  if my_potential_MT-source != nobody and member? "Residual Heat" source-options
  [
    let transport-pipe_cost min pipe_distances_MT-source * NetLogo_RD_distance * detour_factor * item 1 cost_pipes
    let residual_heat_coupling_cost
    (
      ifelse-value
      couple-cost-residual-heat = "High" [[couple_cost_max] of my_potential_MT-source]
      couple-cost-residual-heat = "Low"  [[couple_cost_min] of my_potential_MT-source]
    )

    set residual_heat_cost residual_heat_coupling_cost * item 1 p_sec + transport-pipe_cost
    set primary_source_cost lput residual_heat_cost primary_source_cost
  ]

  let HCS_cost 1e17 let TESW_cost 1e17
  let boosters total_houses * booster_cost
  ;HCS and TESW potential is ignored --> choice is cost only. Depends on unkown location anyway
  if member? "Heat Cold Storage" source-options [
    set HCS_cost (HCS_fixed_cost + HCS_var_cost * item 0 p_sec + boosters)
    set primary_source_cost lput HCS_cost primary_source_cost
  ]
  if member? "Thermal Energy Surface Water" source-options [
    set TESW_cost (TESW_fixed_cost + TESW_var_cost * item 0 p_sec + boosters)
    set primary_source_cost lput TESW_cost primary_source_cost
  ]

  (ifelse new-HG-temp-preference = "Economic" [ set best_primary_source_option item position min primary_source_cost primary_source_cost source-options ifelse best_primary_source_option = "Residual Heat" [set net-temp "MT" ][set net-temp "LT"]]
    new-HG-temp-preference = "MT" [ set best_primary_source_option "Residual Heat" set net-temp "MT" ]
    new-HG-temp-preference = "LT" [ set best_primary_source_option item position min (list HCS_cost TESW_cost) (list HCS_cost TESW_cost) (list "Heat Cold Storage" "Thermal Energy Surface Water") set net-temp "LT" ] )


  set HG-business-case True

  invest-HG

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;MUNICIPAL PROCEDURES;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to update-construction
  ask municipalities [

    set current-construction 0
    set available-construction-capacity construction-capacity
  ]


end


to build-new-neighborhoods

  let size-neighborhoods mean [total_houses] of my-neighborhoods

  let to-build-houses current-new-construction
  ifelse to-build-houses >  min-new-neighborhood-size [
    while [to-build-houses > 0]
    ; some random function for determining neighborhood size
    [ let new-houses-build (round random-poisson (size-neighborhoods / 100) ) * 100
      set new-houses-build min (list new-houses-build to-build-houses)
      set to-build-houses to-build-houses - new-houses-build

      if new-houses-build > 0
      [
        hatch-neighborhoods 1 [
          set name "new-neighborhood"
          set new? True
          set total_houses new-houses-build
          set color [my-color] of myself
          initialize-variable-types
          setup-new-neighborhood
        ]
      ]
    ]
  ]
  [
    ask one-of my-neighborhoods with [not new?]
    [

      let growth ( total_houses + to-build-houses ) / total_houses
      set total_houses total_houses + to-build-houses
      set new_houses new_houses + to-build-houses
      set reference-elabels map [x -> x / growth] reference-elabels
      let increase to-build-houses / total_houses


      set investment-insulation-total map [x -> x / growth] investment-insulation-total
      set investment-insulation-total replace-item 5 investment-insulation-total (item 5 investment-insulation-total + increase)
      set reference-elabels replace-item 6 reference-elabels (item 6 reference-elabels + increase )

    ]
  ]
end

to demolish-neighborhoods
  ;; Add choice for bad houses to demolish?
  if current-demolition > 0 [
    ask min-one-of (my-neighborhoods with [total_houses > [current-demolition] of myself ]) [total_houses]
      [
        set total_houses total_houses - [current-demolition] of myself
        set demolished-houses demolished-houses + [current-demolition] of myself
      ]
  ]
end


to aggregate-neighborhood-data

  ; Calculate change of energy-use due to insulation
  let calc_list (list 0 0 0 0 0 0 0 0 0 0 0 )
  set energy-change-per-age-group  [map [x -> x * total_houses / [total_houses] of municipality-agent] energy-change-per-age-group] of my-neighborhoods
  foreach energy-change-per-age-group [ x ->
    set calc_list (map + x calc_list)
  ]
  set energy-change-per-age-group calc_list
  set energy-savings energy-savings + sum energy-change-per-age-group

  ; Calculate aggregate insulation-investment
  set calc_list (list 0 0 0 0 0 0)
  set insulation-investment [map [x -> x * total_houses / [total_houses] of municipality-agent] investment-insulation-total] of my-neighborhoods
  foreach insulation-investment [ x ->
    set calc_list (map + x calc_list)
  ]
  set insulation-investment calc_list

  ; Calculate aggregate system-investment
  ; [Gas LT-heating MT-heating HT-heating AHP GHP]

  let LT-hg sum [total_houses * cityheating * ifelse-value net-temp = "LT" [1][0]]  of my-neighborhoods / total_houses
  let MT-hg sum [total_houses  * cityheating * ifelse-value net-temp = "MT" [1][0]]  of my-neighborhoods / total_houses
  let HT-hg sum [total_houses  * cityheating * ifelse-value net-temp = "HT" [1][0]] of my-neighborhoods / total_houses
  let AHP sum [total_houses * (item 0 investment-hp-total + item 2 investment-hp-total) ]  of my-neighborhoods / total_houses
  let GHP sum [total_houses * (item 1 investment-hp-total + item 3 investment-hp-total)]  of my-neighborhoods / total_houses
  let gas sum [total_houses *  (1 - cityheating - sum investment-hp-total)] of my-neighborhoods / total_houses

  let systems (list LT-hg MT-hg HT-hg AHP GHP gas)
  set current-systems-aggregate systems

  set total_houses sum [total_houses] of my-neighborhoods

end

to update-municipal-contracts
  let diversity-threshold 0.3
  set traditional-contracts (turtle-set [my-contracts with [stakeholder-diversity > diversity-threshold]] of my-neighborhoods)
  set integrated-contracts (turtle-set [my-contracts with [stakeholder-diversity < diversity-threshold]] of my-neighborhoods)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;INIT PROCEDURE;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to init-agents
  ;setup variables
  clear-turtles
  clear-all-plots
  clear-ticks
  reset-timer
  stop-inspecting-dead-agents
  set end? True
  reset-save-files

  init-globals

  let n-sources (length MT_source_data) - 1
  let s 1

  while [s < n-sources] [
    create-MT-sources  1
    [
      set shape "Factory"
      set size 0.5
      set color 63
      set_coordinates s
      set_source_characteristics s
      set s s + 1
      set my-grid (turtle-set self)
    ]
  ]

  ; set up neighborhoods
  let k 1 ; start at firt data point [kwb]
  let c 1 ; start at first data point [characteristics]
  let g 3 ; start at first data point [gamma_dist_data]
  let e_ 1 ; start at first data point [e_label_data

  set to-create-municipalities (list)
  (ifelse
    view-municipality = "All" [set to-create-municipalities mun-list ]
    view-municipality = "Random-10" [set to-create-municipalities (sentence n-of 10 mun-list "end")]
    view-municipality = "Randstad" [set to-create-municipalities (list "Amsterdam" "Rotterdam" "'s-Gravenhage" "Utrecht" "Leiden" "Delft" "Zoetermeer" "Gouda" "Haarlem" "Almere" "Zaanstad" "Haarlemmermeer" "Dordrecht" "Alphen aan den Rijn" "Amersfoort" "Westland" "Alkmaar" "end")]
    [set to-create-municipalities (list view-municipality "end" )]
  )
  let m 0
  foreach butlast to-create-municipalities [ x ->
    create-municipalities 1 [

      ; Base characteristics
      set name x
      set shape "building institution"
      set size 1
      set color 5 + m
      set my-color color
      set-municipality-initials
      ; Initialisation of neighborhoods
      let n 0
      let n-neighborhoods position (item (position x mun-list + 1) mun-list) mun-keys - position x mun-keys
      while [n < n-neighborhoods ] [

        hatch-neighborhoods 1
        [
          initialize-variable-types

          let current-neighborhood (n + position x mun-keys)
          set new? false
          set shape "house"
          set size 0.75
          set my-municipality x
          set municipality-agent myself
          set name item (position "buurt_naam" item 0 kwb) item (n + position x mun-keys + 1) kwb
          set total_houses item (position "total" item 0 kwb) item (n + position x mun-keys + 1) kwb

          set_coordinates k + current-neighborhood
          set_gamma_BEP g + current-neighborhood
          set_gamma_cost g + current-neighborhood
          set_gamma_HP g + current-neighborhood

          set_characteristics c + current-neighborhood

          set_elabels e_ + current-neighborhood
          set TESW-potential TESW-potential * relative-TESW-potential
          set potential (list)


          foreach (list 1 2 3 4 5 6 7 )
          [ z ->
            set potential (lput (sum sublist reference-elabels 0 z) potential)
          ]

          set binary-labels butlast map [z -> min(list 1 ceiling z)] potential
          set potential butlast potential
          calculate-investment-total

          repeat init-calibration-factor  ;; for calibrating mutually dependent variables
          [
            calibrate-mutual-dependent-variables
          ]
          update-cost-stats
          calculate-current-elabels
          calculate-temp-suitability
          invest-hp
          ; This should be moved to initialisation part

          set my-contracts (turtle-set)

          let tasks ["Insulation" "HP" "HG" ]
          foreach tasks [t ->
            hatch-contracts 1
            [ set hidden? True
              set task t
              set city [my-municipality] of myself
              set my-neighborhood-agent myself
              set my-neighborhood [name] of myself
              ask myself [set my-contracts (turtle-set my-contracts myself)]
              set stakeholder-diversity [sum(list item 0 property-distribution item 2 property-distribution)] of myself
            ]

          ]
          update-contract

        ]

        set n n + 1
      ]
      ;; Neighborhood based characteristics municipality

      set my-neighborhoods neighborhoods with [municipality-agent = myself]
      set my-new-neighborhoods nobody
      set total_houses sum [total_houses] of my-neighborhoods
      set xcor mean [xcor] of my-neighborhoods
      set ycor mean [ycor] of my-neighborhoods
      set color white
      set m m + 10

      set-municipality-initials



    ]
  ]
  ask neighborhoods [ set-nearby-agents ]
  ask neighborhoods with [cityheating > 0] [ setup-initial-grids ]
  update-construction
  set TESW-factor-initial relative-TESW-potential
  set initial-neighborhoods neighborhoods with [not new?]
  type ("Neighborhoods and Municipalities initialised! Initialsation took ") type timer print " seconds"


  set end? false
  reset-ticks
  tick-advance 2020
  update-plots
end

to init-globals
  ; initials
  init-SD-vars

  import_constants

  ;set gas-price 0.604486
 ; set electricity-price 0.190872
  set hpAefficiency 3.7
  set hpGefficiency 4.4
  set caloric-value 31.65
  set relative_xy_netherlands 0.8672
  set NetLogo_RD_distance 303183.95700 / 100 ; (ymax - ymin) --> set to 1, multiplied by 100



end

to init-SD-vars
  let time position "Time" SD-data_index
  let index_gas-p position "Average Gas Price" SD-data_index
  let index_elec-p position "Average Electricity Price" SD-data_index
  let index_reloc_f position "Relocation mobility factor" SD-data_index


  let SD-this-tick item position 2020 time-index SD-data
  set gas-price item index_gas-p SD-this-tick
  set electricity-price item index_elec-p SD-this-tick
  set relocation-mobility item index_reloc_f SD-this-tick

end

to reset-save-files
  if ema-controls = True
  [
    csv:to-file "energy-change.csv" []
    csv:to-file "systems.csv" []
    csv:to-file "insulation-investment.csv" []
    csv:to-file "neighbourhood-model-outcome.csv" []
  ]
end

to reset-initials
  clear-all-plots
  clear-ticks
  reset-timer
  set end? true
  reset-save-files
  csv:to-file "energy-change.csv" []

  ask links [die]
  ask lt-sources with [new? = true] [die]
  ask neighborhoods with [new?] [die]

  ask municipalities
  [
    set-municipality-initials
  ]

  init-globals
  ask neighborhoods
  [
    initialize-variable-types
    set cost-distribution initial-cost-distribution
    set BEP-distribution initial-BEP-distribution
    set current-average-spending-owner 0
    set TESW-potential TESW-potential / TESW-factor-initial * relative-TESW-potential
    calculate-ROI ;; needed to reset ROI before calculating investment etc.

    repeat init-calibration-factor ;; for calibrating mutually dependent variables
    [
      calibrate-mutual-dependent-variables

    ]
    update-contract
    ;    calculate-current-elabels

    ; update-cost-stats
    ; keep-track-developments
  ]

  ask municipalities
  [
    set-municipality-initials
  ]

  ask mt-sources [ set-source-intitals]

  ask neighborhoods with [cityheating > 0]  [setup-initial-grids]
  update-construction
  update-system-colors
  set TESW-factor-initial relative-TESW-potential

  set end? false
  reset-ticks
  tick-advance 2020
  update-plots
  type "Initialisation took " type timer print " seconds"
end

to setup
  import-data
  init-globals
  init-agents
end

to initialize-variable-types
  set investment-hp (list (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0))
  set investment-hp-total (list 0 0 0 0)
  set investment-insulation (list (list 0 0 0 0 0 0 ) (list 0 0 0 0 0 0 ) (list 0 0 0 0 0 0 ))
  set last-year-insulation (list (list 0 0 0 0 0 0 ) (list 0 0 0 0 0 0 ) (list 0 0 0 0 0 0 ))
  set ROI (list base-private-ROI base-corp-ROI base-private-rent-ROI)
  set reference-elabels init-elabels
  set current-elabels init-elabels
  set HG-business-case false
  set new-HG false
  set cityheating initial-cityheating
  set my-grid (turtle-set self)
  set potential_grid-connections (turtle-set)
  set my_potential_grid_connection nobody
  set my_potential_MT-source nobody
  set best_primary_source_option "Not yet determined"
  set HG-investment-cost-total 0
  set HG-investment-per-connection 0
  set source-options (list)
  set BEP-HG-options (list)
  set savings-per-age-group (list 0 0 0 0 0 0 0 0 0 0 0)
  set net-temp 0
  set total_houses total_houses - new_houses + demolished-houses
  set new_houses 0
  set demolished-houses 0




end

to set-municipality-initials
  set construction-capacity total_houses * relative-construction-capacity
  set available-construction-capacity construction-capacity
  set energy-savings 0
  set my-neighborhoods neighborhoods with [municipality-agent = myself]
  set current-willingness-factor item position name item 0 SD_willingness item 1 SD_willingness
end

to set-nearby-agents
  let maximum-distance-sources 20
  let maximum-distance-neighborhoods 1
  set nearby-sources MT-sources in-radius maximum-distance-sources
  set nearby-neighborhoods other neighborhoods in-radius maximum-distance-neighborhoods
end

to set-source-intitals
  set current-capacity total-capacity
  set my-grid (turtle-set self)
end

to calibrate-mutual-dependent-variables
  calculate-current-elabels
  update-cost-stats
  keep-track-developments
  calc-ex-probs
  invest-insulation
  update-insulation-costs
  calculate-ROI
  calculate-temp-suitability
  invest-hp

end

to setup-initial-grids
  let closest_grid min-one-of nearby-neighborhoods with [cityheating > 0 and not member? myself my-grid ] [distance myself]
  if closest_grid != nobody [

    ask closest_grid [
      create-link-with myself [set color magenta set initial? True]
      set my-grid (turtle-set my-grid [my-grid] of myself)
    ]
    set my-grid (turtle-set my-grid [my-grid] of closest_grid)
    ask my-grid [set my-grid (turtle-set my-grid [my-grid] of myself)]
  ]
  set net-temp "HT"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;DATA READING;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to import-data
  clear-all
  reset-timer
  stop-inspecting-dead-agents
  ; Load datasets
  set characteristics csv:from-file "input/total_characteristics.csv"
  set length-data length characteristics ;; number of rows: neighborhoods
  set gamma_dist_data csv:from-file "input/total_gamma.csv"
  set gamma_hp csv:from-file "input/Gamma_HP.csv"
  set kwb csv:from-file "input/total_kwb.csv"
  set e_label_data csv:from-file "input/buurten_elabels.csv"
  set MT_source_data csv:from-file "input/MT_sources_relativexycor.csv"


  update-SD-data-import
   ; UTF-8 gemekker
  set SD-municipality-index item 0 SD_construction


  set mun-keys (list) let x 1 while [length kwb > x]
  [
    set mun-keys lput (item (position "gemeente" item 0 kwb) item x kwb) mun-keys
    set x x + 1
  ]
  set mun-keys lput "end" mun-keys
  set mun-list remove-duplicates mun-keys
  print "Data import ready!"
  type "Data import took " type timer print " seconds"
end

to update-SD-data-import
  set SD_construction csv:from-file "input/SD_output_construction.csv"
  set SD_demolition csv:from-file "input/SD_output_demolition.csv"
  set SD_e-use_new-construction  csv:from-file "input/SD_output_e-use_new-construction.csv"
  set SD_systemcost-reduction  csv:from-file "input/SD_output_systemcost-reduction.csv"
  set SD_insulationcost-reduction  csv:from-file "input/SD_output_insulationcost-reduction.csv"
  set SD_system-maturity csv:from-file "input/SD_output_system_maturity.csv"
  set SD_insulation-maturity csv:from-file "input/SD_output_insulation_maturity.csv"
  set SD_willingness csv:from-file "input/SD_output_willingness.csv"

  set SD-data csv:from-file  "input/SD_output_globals.csv"
  set SD-data_index item 0 SD-data
  let time-col position "Time" SD-data_index

  set time-index (list)
  foreach SD-data [ x ->
    set time-index lput (item time-col x) time-index
  ]
end

to set_gamma_BEP [y]
  ; This section defines the turtle variable 'BEP-distribution'. It contains a list of threshold distributions containing
  ; lists with 3 items for gamma distributions [mean alpha theta]

  set initial-BEP-distribution (list)
  let ratio (list "Savings / cost F" "Savings / cost E" "Savings / cost D" "Savings / cost C" "Savings / cost B" "Savings / cost A")
  foreach ratio
  [x ->
    let a (position x item 0 gamma_dist_data)
    set initial-BEP-distribution lput sublist (item y gamma_dist_data) a (a + 3) initial-BEP-distribution
  ]
  ;; NEEDED FOR REINITIALIZING WITHOUT DELETING AGENTS
  set BEP-distribution initial-BEP-distribution

end

to set_gamma_cost [y]
  ; Same functionality as set_gamma_BEP [y]
  set initial-cost-distribution (list)
  let cost (list "Total cost F" "Total cost E" "Total cost D" "Total cost C" "Total cost B" "Total cost A")
  foreach cost
  [x ->
    let a (position x item 0 gamma_dist_data)
    set initial-cost-distribution lput sublist (item y gamma_dist_data) a (a + 3) initial-cost-distribution

  ]

  ;; NEEDED FOR REINITIALIZING WITHOUT DELETING AGENTS
  set cost-distribution initial-cost-distribution
end

to set_gamma_HP [y]
  set initial-hp-cost-dist (list)
  let cost (list "investment cost A-HP at A" "investment cost A-HP at B" "investment cost G-HP at A" "investment cost G-HP at B")
  foreach cost
  [x ->
    let a (position x item 0 gamma_hp)
    set initial-hp-cost-dist lput sublist (item y gamma_hp) a (a + 3) initial-hp-cost-dist
  ]

  set hp-cost-dist initial-hp-cost-dist

  set initial-hp-threshold-dist (list)
  let threshold (list "threshold A-HP at A" "threshold A-HP at B" "threshold G-HP at A" "threshold G-HP at B")
  foreach threshold
  [x ->
    let a (position x item 0 gamma_hp)
    set initial-hp-threshold-dist lput sublist (item y gamma_hp) a (a + 3) initial-hp-threshold-dist
  ]

  set hp-threshold-dist initial-hp-threshold-dist
end

to set_characteristics [y]
  ; general characteristics from kwb
  set area item (position "a_lan_ha" item 0 kwb) item y kwb
  set average-property-value item (position "g_woz" item 0 kwb) item y kwb * 1000
  set average-income item (position "g_ink_pi" item 0 kwb) item y kwb * 1000
  set household-low-40 item (position "p_hh_li" item 0 kwb) item y kwb / 100
  set household-high-20 item (position "p_hh_hi" item 0 kwb) item y kwb / 100
  set initial-cityheating item (position "p_stadsv" item 0 kwb) item y kwb / 100
  set HCS-allowed? item (position "Centroid in WKO-contour?" item 0 kwb) item y kwb
  set HCS-potential item (position "P-WKO" item 0 kwb) item y kwb * HCS-allowed?
  set GHP-potential item (position "P-GHP" item 0 kwb) item y kwb
  set TESW-potential item (position "P-TESW" item 0 kwb) item y kwb

  set cityheating initial-cityheating

  ; list for distribution ownership
  let owner item (position "p_koopw" item 0 kwb) item y kwb / 100
  let corp item (position "p_wcorpw" item 0 kwb) item y kwb / 100
  let rental item (position "p_ov_hw" item 0 kwb) item y kwb / 100
  set property-distribution (list owner corp rental)

  ; lists for distribution type, size and age
  set housing-type (list) set housing-size (list) set housing-age (list)
  let a (list 0 1 2 3 4)
  foreach a [x ->

    set housing-type lput item ((position "2-onder-1-kapwoning" item 0 characteristics) + x ) item y characteristics housing-type ]
  set a lput 5 a
  foreach a [x ->
    set housing-size lput item ((position "15 tot 50 m2" item 0 characteristics) + x ) item y characteristics housing-size ]
  set a (sentence a (list 6 7 8 9 10))
  foreach a [x ->
    set housing-age lput item ((position "Age0to9" item 0 characteristics) + x ) item y characteristics housing-age ]

  ; Heating grid characteristics from PBL
  set HG-characteristics
  (list
    (item (position "Aansluitingen" item 0 kwb) item y kwb )
    (item (position "Aansluit_lengte_m" item 0 kwb) item y kwb )
    (item (position "Hoofdleiding_lengte_m" item 0 kwb) item y kwb )
  )

end

to set_source_characteristics [y]
  set name (item (position "bron_naam" item 0 MT_source_data) item y MT_source_data)
  set type-source (item (position "type_bron" item 0 MT_source_data) item y MT_source_data)
  set status (item (position "status" item 0 MT_source_data) item y MT_source_data)
  set souce-temp (item (position "Brontemperatuur" item 0 MT_source_data) item y MT_source_data)
  set total-capacity (item (position "MWcapaciteit" item 0 MT_source_data) item y MT_source_data) * 1000
  set couple_cost_min (item (position "Uitkoppel_min" item 0 MT_source_data) item y MT_source_data)
  set couple_cost_max (item (position "Uitkoppel_max" item 0 MT_source_data) item y MT_source_data)
  set production-cost (item (position "Productiekosten" item 0 MT_source_data) item y MT_source_data)
  set current-capacity total-capacity

end


to set_elabels [y]
  ; list for elabels
  let labels (list "G" "F" "E" "D" "C" "B" "A")
  set init-elabels (list)
  foreach labels [x ->
    set init-elabels lput item (position x item 0 e_label_data) item y e_label_data init-elabels
  ]

  set initial-average-connection-value item (position "Mean connection value" item 0 e_label_data) item y e_label_data
  set share-monuments item (position "Share Monuments" item 0 e_label_data) item y e_label_data
  set reference-elabels init-elabels

end
to set_coordinates [y]
  if breed = neighborhoods [
    setxy (item (position "x" item 0 kwb) item y kwb) * 100 * relative_xy_netherlands (item (position "y" item 0 kwb) item y kwb) * 100
  ]
  if breed = MT-sources [
    setxy (item (position "xcor" item 0 MT_source_data) item y MT_source_data) * 100 * relative_xy_netherlands (item (position "ycor" item 0 MT_source_data) item y MT_source_data) * 100
  ]

end

to import_constants

  set ABM-constants csv:from-file  "input/ABM Constants.csv"
  let var-index position "Var" item 0 ABM-constants
  let value-index position "Value" item 0 ABM-constants

  ; Loop through variables and set values
  foreach butfirst ABM-constants [x ->
    run (word "set " item var-index x " " item value-index x )
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;REPORTERS;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report cdf-gamma [x alpha theta ]
  ;; calculates exceedance probability given [x alpha theta]
  let result stats:incompleteGamma alpha (x / theta)
  report result
end

; the following has been used from: https://stackoverflow.com/questions/51111131/netlogo-plot-histogram-percentage
to-report freq [ i_ list_ ]
  report length filter [ ind -> ind = i_ ] list_
end

to-report freq_map [ list_ ]
  ; get length of input list
  let len length list_

  ;round list items
  let new-list (list)
  foreach list_ [ x -> set new-list fput precision x 1 new-list ]
  set list_ new-list

  ; get unique values for the input list
  let uniques remove-duplicates list_

  ; get counts of each unique value
  let counts map [ i -> freq i list_ ] uniques

  ; report an xy pair for each unique value / proportion
  report ( map [ [ x y ] -> list  x  ( y / len )  ] uniques counts )
end

to check-for-errors
  if sum (list new-neighborhoods-gas new-neighborhoods-HG new-neighborhoods-HP) < 1 [ error "New neighborhood systems don't add up to 100%" ]
end

; till here: https://stackoverflow.com/questions/51111131/netlogo-plot-histogram-percentage
;to-report report-insulation
;  ifelse sum investment-insulation-total = 0 [report 0.01][report sum investment-insulation-total]
;end
@#$#@#$#@
GRAPHICS-WINDOW
213
14
902
804
-1
-1
7.74
1
10
1
1
1
0
0
0
1
0
87
0
100
0
0
1
Year
30.0

BUTTON
1024
16
1087
49
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
1

BUTTON
18
249
102
283
Initialize agents
init-agents\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
115
192
148
end-period
end-period
2020
2060
2060.0
1
1
NIL
HORIZONTAL

PLOT
1713
19
1913
169
Gas-price
Year
€ / m3
2020.0
2060.0
0.0
8.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks gas-price"

PLOT
1093
16
1293
166
BEP-exceedance owner
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -12087248 true "" "plotxy ticks sum [item 5 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"B" 1.0 0 -10899396 true "" "plotxy ticks sum [item 4 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"C" 1.0 0 -6565750 true "" "plotxy ticks sum [item 3 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"D" 1.0 0 -723837 true "" "plotxy ticks sum [item 2 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"E" 1.0 0 -612749 true "" "plotxy ticks sum [item 1 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"F" 1.0 0 -955883 true "" "plotxy ticks sum [item 0 item 0 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

SLIDER
910
91
1084
124
max-capital-inv-share
max-capital-inv-share
0
0.2
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
910
129
1085
162
max-income-inv-share
max-income-inv-share
0
0.10
0.05
0.01
1
NIL
HORIZONTAL

PLOT
1091
176
1291
326
Affordability
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -10899396 true "" "plotxy ticks sum [item 5 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"B" 1.0 0 -8732573 true "" "plotxy ticks sum [item 4 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"C" 1.0 0 -6565750 true "" "plotxy ticks sum [item 3 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"D" 1.0 0 -723837 true "" "plotxy ticks sum [item 2 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"E" 1.0 0 -612749 true "" "plotxy ticks sum [item 1 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"F" 1.0 0 -955883 true "" "plotxy ticks sum [item 0 cost-undershoot * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

CHOOSER
17
11
155
56
Time-step
Time-step
"Quarter" "Year"
1

BUTTON
18
208
116
241
Import Data
import-data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
20
63
155
108
View-Municipality
View-Municipality
"All" "Random-10" "Randstad" "'s-Gravenhage" "'s-Hertogenbosch" "Aa en Hunze" "Aalsmeer" "Aalten" "Achtkarspelen" "Alblasserdam" "Albrandswaard" "Alkmaar" "Almelo" "Almere" "Alphen aan den Rijn" "Alphen-Chaam" "Altena" "Ameland" "Amersfoort" "Amstelveen" "Amsterdam" "Apeldoorn" "Appingedam" "Arnhem" "Assen" "Asten" "Baarle-Nassau" "Baarn" "Barendrecht" "Barneveld" "Beek" "Beekdaelen" "Beemster" "Beesel" "Berg en Dal" "Bergeijk" "Bergen (L.)" "Bergen (NH.)" "Bergen op Zoom" "Berkelland" "Bernheze" "Best" "Beuningen" "Beverwijk" "Bladel" "Blaricum" "Bloemendaal" "Bodegraven-Reeuwijk" "Boekel" "Borger-Odoorn" "Borne" "Borsele" "Boxmeer" "Boxtel" "Breda" "Brielle" "Bronckhorst" "Brummen" "Brunssum" "Bunnik" "Bunschoten" "Buren" "Capelle aan den IJssel" "Castricum" "Coevorden" "Cranendonck" "Cuijk" "Culemborg" "Dalfsen" "Dantumadiel" "De Bilt" "De Fryske Marren" "De Ronde Venen" "De Wolden" "Delft" "Delfzijl" "Den Helder" "Deurne" "Deventer" "Diemen" "Dinkelland" "Doesburg" "Doetinchem" "Dongen" "Dordrecht" "Drechterland" "Drimmelen" "Dronten" "Druten" "Duiven" "Echt-Susteren" "Edam-Volendam" "Ede" "Eemnes" "Eersel" "Eijsden-Margraten" "Eindhoven" "Elburg" "Emmen" "Enkhuizen" "Enschede" "Epe" "Ermelo" "Etten-Leur" "Geertruidenberg" "Geldrop-Mierlo" "Gemert-Bakel" "Gennep" "Gilze en Rijen" "Goeree-Overflakkee" "Goes" "Goirle" "Gooise Meren" "Gorinchem" "Gouda" "Grave" "Groningen" "Gulpen-Wittem" "Haaksbergen" "Haaren" "Haarlem" "Haarlemmermeer" "Halderberge" "Hardenberg" "Harderwijk" "Hardinxveld-Giessendam" "Harlingen" "Hattem" "Heemskerk" "Heemstede" "Heerde" "Heerenveen" "Heerhugowaard" "Heerlen" "Heeze-Leende" "Heiloo" "Hellendoorn" "Hellevoetsluis" "Helmond" "Hendrik-Ido-Ambacht" "Hengelo" "Het Hogeland" "Heumen" "Heusden" "Hillegom" "Hilvarenbeek" "Hilversum" "Hoeksche Waard" "Hof van Twente" "Hollands Kroon" "Hoogeveen" "Hoorn" "Horst aan de Maas" "Houten" "Huizen" "Hulst" "IJsselstein" "Kaag en Braassem" "Kampen" "Kapelle" "Katwijk" "Kerkrade" "Koggenland" "Krimpen aan den IJssel" "Krimpenerwaard" "Laarbeek" "Landerd" "Landgraaf" "Landsmeer" "Langedijk" "Lansingerland" "Laren" "Leeuwarden" "Leiden" "Leiderdorp" "Leidschendam-Voorburg" "Lelystad" "Leudal" "Leusden" "Lingewaard" "Lisse" "Lochem" "Loon op Zand" "Lopik" "Loppersum" "Losser" "Maasdriel" "Maasgouw" "Maassluis" "Maastricht" "Medemblik" "Meerssen" "Meierijstad" "Meppel" "Middelburg" "Midden-Delfland" "Midden-Drenthe" "Midden-Groningen" "Mill en Sint Hubert" "Moerdijk" "Molenlanden" "Montferland" "Montfoort" "Mook en Middelaar" "Neder-Betuwe" "Nederweert" "Nieuwegein" "Nieuwkoop" "Nijkerk" "Nijmegen" "Nissewaard" "Noardeast-Fryslan" "Noord-Beveland" "Noordenveld" "Noordoostpolder" "Noordwijk" "Nuenen Gerwen en Nederwetten" "Nunspeet" "Oegstgeest" "Oirschot" "Oisterwijk" "Oldambt" "Oldebroek" "Oldenzaal" "Olst-Wijhe" "Ommen" "Oost Gelre" "Oosterhout" "Ooststellingwerf" "Oostzaan" "Opmeer" "Opsterland" "Oss" "Oude IJsselstreek" "Ouder-Amstel" "Oudewater" "Overbetuwe" "Papendrecht" "Peel en Maas" "Pekela" "Pijnacker-Nootdorp" "Purmerend" "Putten" "Raalte" "Reimerswaal" "Renkum" "Renswoude" "Reusel-De Mierden" "Rheden" "Rhenen" "Ridderkerk" "Rijssen-Holten" "Rijswijk" "Roerdalen" "Roermond" "Roosendaal" "Rotterdam" "Rozendaal" "Rucphen" "Schagen" "Scherpenzeel" "Schiedam" "Schiermonnikoog" "Schouwen-Duiveland" "Simpelveld" "Sint Anthonis" "Sint-Michielsgestel" "Sittard-Geleen" "Sliedrecht" "Sluis" "Smallingerland" "Soest" "Someren" "Son en Breugel" "Stadskanaal" "Staphorst" "Stede Broec" "Steenbergen" "Steenwijkerland" "Stein" "Stichtse Vecht" "Sudwest-Fryslan" "Terneuzen" "Terschelling" "Texel" "Teylingen" "Tholen" "Tiel" "Tilburg" "Tubbergen" "Twenterand" "Tynaarlo" "Tytsjerksteradiel" "Uden" "Uitgeest" "Uithoorn" "Urk" "Utrecht" "Utrechtse Heuvelrug" "Vaals" "Valkenburg aan de Geul" "Valkenswaard" "Veendam" "Veenendaal" "Veere" "Veldhoven" "Velsen" "Venlo" "Venray" "Vijfheerenlanden" "Vlaardingen" "Vlieland" "Vlissingen" "Voerendaal" "Voorschoten" "Voorst" "Vught" "Waadhoeke" "Waalre" "Waalwijk" "Waddinxveen" "Wageningen" "Wassenaar" "Waterland" "Weert" "Weesp" "West Betuwe" "West Maas en Waal" "Westerkwartier" "Westerveld" "Westervoort" "Westerwolde" "Westland" "Weststellingwerf" "Westvoorne" "Wierden" "Wijchen" "Wijdemeren" "Wijk bij Duurstede" "Winterswijk" "Woensdrecht" "Woerden" "Wormerland" "Woudenberg" "Zaanstad" "Zaltbommel" "Zandvoort" "Zeewolde" "Zeist" "Zevenaar" "Zoetermeer" "Zoeterwoude" "Zuidplas" "Zundert" "Zutphen" "Zwartewaterland" "Zwijndrecht" "Zwolle"
3

BUTTON
911
13
1017
47
Reset Initials
Reset-initials
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
17
168
82
202
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1093
335
1293
485
Cumulative Investment insulation
Year 
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -12087248 true "" "plotxy ticks sum [item 5 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"B" 1.0 0 -10899396 true "" "plotxy ticks sum [item 4 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"C" 1.0 0 -6565750 true "" "plotxy ticks sum [item 3 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"D" 1.0 0 -723837 true "" "plotxy ticks sum [item 2 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"E" 1.0 0 -612749 true "" "plotxy ticks sum [item 1 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"F" 1.0 0 -955883 true "" "plotxy ticks sum [item 0 investment-insulation-total * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"Total" 1.0 2 -7500403 true "" "plotxy ticks sum [sum investment-insulation-total * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

SLIDER
910
167
1082
200
base-private-ROI
base-private-ROI
0.01
0.20
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
17
290
189
323
init-calibration-factor
init-calibration-factor
1
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
909
202
1081
235
base-corp-ROI
base-corp-ROI
0.01
0.2
0.15
0.01
1
NIL
HORIZONTAL

SLIDER
909
242
1082
275
base-private-rent-ROI
base-private-rent-ROI
0.01
0.2
0.12
0.01
1
NIL
HORIZONTAL

SLIDER
911
282
1083
315
group-behaviour
group-behaviour
0
1
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
912
320
1084
353
insulation-subsidy
insulation-subsidy
0
0.50
0.2
0.05
1
NIL
HORIZONTAL

MONITOR
1203
749
1290
794
ROI owners
sum [item 0 ROI * total_houses] of neighborhoods / sum [total_houses] of neighborhoods
3
1
11

BUTTON
909
52
986
85
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1296
333
1496
483
System temperature suitability
NIL
NIL
0.0
1.1
0.0
1.0
false
true
"" "clear-plot"
PENS
"LT " 0.1 1 -10899396 true "" "foreach (freq_map ([item 0 temp-suitability] of neighborhoods) ) [ x -> plotxy first x last x]"
"MT" 0.1 1 -955883 true "" "foreach (freq_map ([item 1 temp-suitability] of neighborhoods) ) [ x -> plotxy first x last x ]"

PLOT
1295
175
1495
325
Share E-labels
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -12087248 true "" "plotxy ticks (sum [sum sublist current-elabels 0 7 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"B" 1.0 0 -10899396 true "" "plotxy ticks (sum [sum sublist current-elabels 0 6 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"C" 1.0 0 -6565750 true "" "plotxy ticks (sum [sum sublist current-elabels 0 5 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"D" 1.0 0 -723837 true "" "plotxy ticks (sum [sum sublist current-elabels 0 4 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"E" 1.0 0 -612749 true "" "plotxy ticks (sum [sum sublist current-elabels 0 3 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"F" 1.0 0 -955883 true "" "plotxy ticks (sum [sum sublist current-elabels 0 2 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"
"G" 1.0 0 -2674135 true "" "plotxy ticks (sum [sum sublist current-elabels 0 1 * total_houses] of neighborhoods) / sum [total_houses] of neighborhoods"

PLOT
1715
176
1915
326
Electricity-price
Year
€ / kWh
2020.0
2060.0
0.0
0.5
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks electricity-price "

PLOT
1091
489
1291
639
hp-exceedance
NIL
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"Owner" 1.0 0 -13791810 true "" "plotxy ticks mean [mean item 0 BEP-exceedance-HP] of neighborhoods  with [not new?]"
"Corp" 1.0 0 -2674135 true "" "plotxy ticks mean [mean item 1 BEP-exceedance-HP] of neighborhoods with [not new?]"
"Private" 1.0 0 -10899396 true "" "plotxy ticks mean [mean item 2 BEP-exceedance-HP] of neighborhoods with [not new?]"

PLOT
1715
334
1915
484
Average insulation spending home-owners
NIL
NIL
2020.0
2060.0
0.0
0.0
true
false
"" "set-plot-x-range 2020 2060\n\n"
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks sum [current-average-spending-owner * total_houses ] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]\n\n;plotxy ticks sum [current-average-spending-owner * total_houses / report-insulation ] of neighborhoods / sum [total_houses] of neighborhoods"

PLOT
1513
334
1713
484
HP-investment
NIL
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A-HP" 1.0 0 -13791810 true "" ";plotxy ticks sum [(item 0 investment-hp-total * item 0 reverse current-elabels + item 1 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods\nplotxy ticks sum [(item 0 investment-hp-total  + item 1 investment-hp-total) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods"
"G-HP" 1.0 0 -10899396 true "" ";plotxy ticks sum [(item 2 investment-hp-total * item 0 reverse current-elabels + item 3 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods\nplotxy ticks sum [(item 2 investment-hp-total + item 3 investment-hp-total ) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods"
"Total" 1.0 0 -16777216 true "" "plotxy ticks sum [(item 0 investment-hp-total + item 1 investment-hp-total) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods + sum [(item 2 investment-hp-total + item 3 investment-hp-total ) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods\n;plotxy ticks sum [(item 0 investment-hp-total * item 0 reverse current-elabels + item 1 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods + sum [(item 2 investment-hp-total * item 0 reverse current-elabels + item 3 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods"

MONITOR
1168
695
1250
740
LT-suitability
sum [item 0 temp-suitability * total_houses] of neighborhoods / sum [total_houses] of neighborhoods
2
1
11

MONITOR
1100
694
1159
739
Total HP
;sum [(item 0 investment-hp-total * item 0 reverse current-elabels + item 1 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods + sum [(item 2 investment-hp-total * item 0 reverse current-elabels + item 3 investment-hp-total * item 1 reverse current-elabels) * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods\nsum [sum investment-hp-total * total_houses ] of neighborhoods / sum [total_houses] of neighborhoods
3
1
11

PLOT
1508
175
1708
325
Average-hp-cost-undershoot
NIL
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A-HP-A" 1.0 0 -13791810 true "" "plotxy ticks sum [item 0 cost-undershoot-hp * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods  with [not new?]"
"A-HP-B" 1.0 0 -13345367 true "" "plotxy ticks sum [item 1 cost-undershoot-hp * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods  with [not new?]"
"G-HP-A" 1.0 0 -12087248 true "" "plotxy ticks sum [item 2 cost-undershoot-hp * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"G-HP-B" 1.0 0 -14439633 true "" "plotxy ticks sum [item 3 cost-undershoot-hp * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

SWITCH
9
340
189
373
Best-value-investment
Best-value-investment
0
1
-1000

MONITOR
1196
643
1260
688
Spending
sum [current-average-spending-owner * total_houses ] of neighborhoods  with [not new?] / sum [total_houses] of neighborhoods  with [not new?]
0
1
11

PLOT
1297
14
1497
164
BEP-exceedance corp
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -12087248 true "" "plotxy ticks sum [item 5 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"B" 1.0 0 -10899396 true "" "plotxy ticks sum [item 4 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"C" 1.0 0 -6565750 true "" "plotxy ticks sum [item 3 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"D" 1.0 0 -723837 true "" "plotxy ticks sum [item 2 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"E" 1.0 0 -612749 true "" "plotxy ticks sum [item 1 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"F" 1.0 0 -955883 true "" "plotxy ticks sum [item 0 item 1 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

PLOT
1504
16
1704
166
BEP-exceedance rent
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"A" 1.0 0 -13210332 true "" "plotxy ticks sum [item 5 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"B" 1.0 0 -12087248 true "" "plotxy ticks sum [item 4 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"C" 1.0 0 -6565750 true "" "plotxy ticks sum [item 3 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"D" 1.0 0 -723837 true "" "plotxy ticks sum [item 2 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"E" 1.0 0 -612749 true "" "plotxy ticks sum [item 1 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"
"F" 1.0 0 -955883 true "" "plotxy ticks sum [item 0 item 2 bep-exceedance * total_houses] of neighborhoods with [not new?] / sum [total_houses] of neighborhoods with [not new?]"

SLIDER
11
386
189
419
Relative-construction-capacity
Relative-construction-capacity
0
0.15
0.05
0.01
1
NIL
HORIZONTAL

PLOT
1293
487
1704
642
Available construction capacity
Year
NIL
2020.0
2060.0
0.0
1.0
false
true
"let n min(list 10 count municipalities)\nset plot-list sort [name] of n-of n municipalities\nforeach plot-list [ x -> \ncreate-temporary-plot-pen x \nset-plot-pen-color [my-color] of one-of municipalities with [name = x]; [[color] of one-of my-neighborhoods ] of one-of municipalities with [name = x] ;color-n \n]" "foreach plot-list [x ->\nset-current-plot-pen x\nplotxy ticks [available-construction-capacity / construction-capacity] of one-of municipalities with [name = x]\n]"
PENS

PLOT
1715
489
1915
639
Energy savings
Year
% reduction
2020.0
2060.0
0.0
0.5
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks sum [energy-savings * total_houses] of neighborhoods / sum [total_houses] of neighborhoods"

MONITOR
1095
644
1191
689
%  Cityheating
sum [cityheating * total_houses] of neighborhoods / sum [total_houses] of neighborhoods
3
1
11

BUTTON
89
167
190
200
Hide all
ask turtles  [set hidden? True]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
122
207
192
240
Unhide
ask neighborhoods [set hidden? False]\nask mt-sources [set hidden? False]\nask lt-sources [set hidden? False]\nask municipalities [set hidden? False]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
10
466
148
511
MT-source-choice
MT-source-choice
"Option A only" "Option A and B" "All-options"
1

SWITCH
985
53
1088
86
System-colors
System-colors
0
1
-1000

SLIDER
7
426
187
459
Relative-capacity-use-grid-construction
Relative-capacity-use-grid-construction
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
4
617
191
650
max-connection-tariff
max-connection-tariff
0
5000
3722.0
1
1
€
HORIZONTAL

SLIDER
1
653
193
686
max-fixed-tariff
max-fixed-tariff
0
2000
1267.77
1
1
€ / Year
HORIZONTAL

SLIDER
912
359
1084
392
Heat-company-ROI
Heat-company-ROI
0
0.15
0.05
0.01
1
NIL
HORIZONTAL

MONITOR
1074
750
1199
795
% business case HG
count neighborhoods with [hg-business-case = True] / count neighborhoods
17
1
11

CHOOSER
16
516
181
561
couple-cost-residual-heat
couple-cost-residual-heat
"High" "Low"
1

SLIDER
1
692
182
725
relative-TESW-potential
relative-TESW-potential
0
10
10.0
1
1
NIL
HORIZONTAL

CHOOSER
10
567
175
612
force-collective-solution
force-collective-solution
"No" "Residual Heat" "Heat Cold Storage" "Thermal Energy Surface Water"
0

SLIDER
6
732
178
765
dont-bother-slider
dont-bother-slider
15000
100000
70000.0
1000
1
NIL
HORIZONTAL

PLOT
1716
642
1916
792
Heating grid best sources
NIL
NIL
0.0
4.0
0.0
1.0
true
true
"" "clear-plot"
PENS
"RH" 1.0 1 -8431303 true "" "plotxy 0 count neighborhoods with [best_primary_source_option = \"Residual Heat\"] / count neighborhoods"
"HCS" 1.0 1 -14439633 true "" "plotxy 1 count neighborhoods with [best_primary_source_option = \"Heat Cold Storage\"] / count neighborhoods"
"TESW" 1.0 1 -13345367 true "" "plotxy 2 count neighborhoods with [best_primary_source_option = \"Thermal Energy Surface Water\"] / count neighborhoods"
"N/A" 1.0 1 -16777216 true "" "plotxy 3 count neighborhoods with [best_primary_source_option = \"No available options\"] / count neighborhoods"

SLIDER
912
398
1089
431
LT-production-subsidy
LT-production-subsidy
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
909
435
1089
468
MT-production-subsidy
MT-production-subsidy
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
910
469
1089
502
LT-investment-subsidy
LT-investment-subsidy
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
910
506
1088
539
MT-investment-subsidy
MT-investment-subsidy
0
1
0.32
0.01
1
NIL
HORIZONTAL

MONITOR
921
721
988
766
% any hg
count neighborhoods with [cityheating > 0] / count neighborhoods
2
1
11

SLIDER
5
769
207
802
min-new-neighborhood-size
min-new-neighborhood-size
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
911
552
1070
585
new-neighborhoods-gas
new-neighborhoods-gas
0
1 - new-neighborhoods-HG - new-neighborhoods-HP
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
913
591
1070
624
new-neighborhoods-HG
new-neighborhoods-HG
0
1 - new-neighborhoods-gas - new-neighborhoods-HP
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
911
630
1071
663
new-neighborhoods-HP
new-neighborhoods-HP
0
1 - new-neighborhoods-gas - new-neighborhoods-HG
0.6
0.05
1
NIL
HORIZONTAL

CHOOSER
911
669
1071
714
new-HG-temp-preference
new-HG-temp-preference
"Economic" "MT" "LT"
0

MONITOR
990
722
1068
767
new ConVal
mean [current-average-connection-value] of neighborhoods with [new?]
2
1
11

SWITCH
102
249
192
282
EMA-controls
EMA-controls
1
1
-1000

MONITOR
731
489
803
534
task-compl
mean [task-complexity] of contracts with [task = \"HP\"]
2
1
11

PLOT
1294
645
1494
795
Contract scores
NIL
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"LC" 1.0 0 -14439633 true "" "plotxy ticks mean [life-cycle-score] of contracts"
"I" 1.0 0 -2674135 true "" "plotxy ticks mean [integrated-score] of contracts"
"BT" 1.0 0 -1184463 true "" "plotxy ticks mean [building-team-score] of contracts"
"T" 1.0 0 -13791810 true "" "plotxy ticks mean [traditional-score] of contracts"

PLOT
1499
645
1699
795
Most suitable contract
NIL
NIL
2020.0
2060.0
0.0
1.0
false
true
"" ""
PENS
"LC" 1.0 0 -14439633 true "" "plotxy ticks count contracts with [most-suitable-contract = \"Life-cycle\"] / count contracts"
"I" 1.0 0 -2674135 true "" "plotxy ticks count contracts with [most-suitable-contract = \"Integrated\"] / count contracts"
"BT" 1.0 0 -1184463 true "" "plotxy ticks count contracts with [most-suitable-contract = \"Building Team\"] / count contracts"
"T" 1.0 0 -13791810 true "" "plotxy ticks count contracts with [most-suitable-contract = \"Traditional\"] / count contracts"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

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

drop
false
0
Circle -7500403 true true 73 133 152
Polygon -7500403 true true 219 181 205 152 185 120 174 95 163 64 156 37 149 7 147 166
Polygon -7500403 true true 79 182 95 152 115 120 126 95 137 64 144 37 150 6 154 165

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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

house efficiency
false
0
Rectangle -7500403 true true 180 90 195 195
Rectangle -7500403 true true 90 165 210 255
Rectangle -16777216 true false 165 195 195 255
Rectangle -16777216 true false 105 202 135 240
Polygon -7500403 true true 225 165 75 165 150 90
Line -16777216 false 75 165 225 165

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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>current-systems-global</metric>
    <enumeratedValueSet variable="step-wise-investment-factor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Relative-capacity-use-grid-construction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-income-inv-share">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-fixed-tariff">
      <value value="1267.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-neighborhoods-HG">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View-Municipality">
      <value value="&quot;Random-10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-private-rent-ROI">
      <value value="0.12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-capital-inv-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MT-source-choice">
      <value value="&quot;Option A and B&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Relative-construction-capacity">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-HG-temp-preference">
      <value value="&quot;Economic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Best-value-investment">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-corp-ROI">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="force-collective-solution">
      <value value="&quot;No&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MT-investment-subsidy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MT-production-subsidy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-step">
      <value value="&quot;Year&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dont-bother-slider">
      <value value="70000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Heat-company-ROI">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-behaviour">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-neighborhoods-HP">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-calibration-factor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LT-investment-subsidy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LT-production-subsidy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="couple-cost-residual-heat">
      <value value="&quot;Low&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-private-ROI">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="System-colors">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="insulation-subsidy">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-neighborhoods-gas">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-period">
      <value value="2060"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-new-neighborhood-size">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-connection-tariff">
      <value value="3725"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative-TESW-potential">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
