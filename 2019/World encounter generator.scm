;;World encounter generator.scm
;;by Nathan Pelletier
;;2019

;;This rats nest is based off a random encounter table at the end of 
;; Tomb of annihilation. It also similates biome weather.

;; just finished salamander

(define (basic_day)
    (rain (random 101))
    (todays_temp))

(define (port_nyan)
    (basic_day)
    (display "Morning encounter: \n")
    (port_nyan2 (random 20))
    (display "Afternoon encounter: \n")
    (port_nyan2 (random 20))
    (display "Night encounter: \n")
    (port_nyan2 (random 20)))

(define (beach)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_beach (random 100))
    (display "Afternoon encounter: \n")
    (encounters_beach (random 100))
    (display "Night encounter: \n")
    (encounters_beach (random 100)))

(define (jungle)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_jungle (random 100))
    (display "Afternoon encounter: \n")
    (encounters_jungle (random 100))
    (display "Night encounter: \n")
    (encounters_jungle (random 100)))

(define (lesser_undead)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_lesser_undead (random 100))
    (display "Afternoon encounter: \n")
    (encounters_lesser_undead (random 100))
    (display "Night encounter: \n")
    (encounters_lesser_undead (random 100)))

(define (greater_undead)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_greater_undead (random 100))
    (display "Afternoon encounter: \n")
    (encounters_greater_undead (random 100))
    (display "Night encounter: \n")
    (encounters_greater_undead (random 100)))

(define (mountains)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_mountains (random 100))
    (display "Afternoon encounter: \n")
    (encounters_mountains (random 100))
    (display "Night encounter: \n")
    (encounters_mountains (random 100)))

(define (rivers)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_rivers (random 100))
    (display "Afternoon encounter: \n")
    (encounters_rivers (random 100))
    (display "Night encounter: \n")
    (encounters_rivers (random 100)))

(define (ruins)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_ruins (random 100))
    (display "Afternoon encounter: \n")
    (encounters_ruins (random 100))
    (display "Night encounter: \n")
    (encounters_ruins (random 100)))

(define (swamp)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_swamp (random 100))
    (display "Afternoon encounter: \n")
    (encounters_swamp (random 100))
    (display "Night encounter: \n")
    (encounters_swamp (random 100)))

(define (wasteland)
    (basic_day)
    (display "Morning encounter: \n")
    (encounters_wasteland (random 100))
    (display "Afternoon encounter: \n")
    (encounters_wasteland (random 100))
    (display "Night encounter: \n")
    (encounters_wasteland (random 100)))

(define (rain is_rain)
    (cond
        ((< is_rain 40) 
            (display "The day starts with a refreshing mist.\n\n"))
        ((> is_rain 90) 
            (display "The party is greated by clear skies.\n\n"))
        (else 
            (display "Today the party is greeted with heavy downpour.
    With heavy downpour expect 50ft visibility and rough terrain. \n")
            (hurricane (random 4) (+ 1 (random 24))))))

(define (hurricane is_hurricane hour)
    (cond
        ((equal? 1 is_hurricane) 
            (display "The party is hurrased by a hurricane starting at ")
            (write hour)
            (display ":00 and lasting 12 hours. 
    Boats sink in 15 minutes if not covered
    1 level of exaustion is automatically gained if characters venture into the storm
    DC 10 constitution check or the characters take another each hour of travel
    Disadvantage on survival checks for the day\n\n"))
        (else (display "\n"))))

(define (todays_temp)
    (temp (+ 28 (-(random 12) 6))))

(define (temp what_temp)
    (display "Today it is ")
    (write what_temp)
    (display " degrees celcius.\n\n"))

(define (port_nyan2 encounter)
    (case encounter
        ((1) (display "A parrot poops on a players head.\n\n"))
        ((2) 
            (display "A berserk ankylosaurus crosses the parties path.\n\n")
            (ankylosaurus 1))
        ((3) (display "A NPC shouts STOP! THIEF! \n\n"))
        ((4) (display "A drunk NPC is challenging strangers to fight. \n\n"))
        ((5) (display "The players are assaulted by 1 or more beggers \n\n"))
        ((6) 
            (display "The alarm horns sound and the city gates begin to close as people rush to the nearest temple. \n\n")
            (encounter))
        ((7) (display "An NPC shouts WATCH OUT before something big falls on the parties heads.\n")
            (display "  DC 12 dex saving throw else take 4d6 bludgeoning.\n")
            (display "  DC 15 perception to spot the assailant (a disgused yuan-ti)\n\n"))
        ((8) (display "A begger asks the party for 1 gp. If the party returns in a week the begger will have 10 gp.\n\n"))
        ((9) (display "A pickpocket makes an attempt on one of the players.\n\n"))
        ((10) (display "Volothamp Volo Geddarm crosses the parties path.\n\n"))
        (else (side_quests_nyan))))

(define (side_quests_nyan)
    (display "The players are given a side quest by one of the NPCs on page 17.\n")
    (display "NOTE: use the first one in the list and go chronologically.\n\n"))


(define (encounters_beach encounter)
    (cond 
        ((< encounter 7) 
            (aarakocra (+ 2 (random 4)) (+ 2 (random 4))))
        ((equal? encounter 8) 
            (artus_cimber))
        ((or (equal? encounter 9) (equal? encounter 10)) 
            (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? encounter 11) (equal? encounter 12))
            (chwinga))
        ((or (equal? encounter 13) (equal? encounter 14))
            (allosaurus))
        ((or (equal? encounter 15) (equal? encounter 16))
            (dimetrodon (+ 2 (random 6) (random 6))))
        ((or (> encounter 16) (< encounter 22))
            (plesiosaurus))
        ((or (> encounter 21) (< encounter 29))
            (pteranodon (+ 2 (random 6) (random 6))))
        ((or (> encounter 28) (< encounter 32))
            (quetzalcoatlus (+ 2 (random 4))))
        ((or (> encounter 31) (< encounter 38))
            (velociraptor (+ 3 (random 6) (random 6) (random 6))))
        ((or (> encounter 37) (< encounter 41))
            (dragon))
        ((or (equal? encounter 41) (equal? encounter 42)) 
            (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (> encounter 42) (< encounter 47))
            (explorers))
        ((or (> encounter 46) (< encounter 50))
            (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (> encounter 49) (< encounter 53))
            (flying_monkeys))
        ((or (> encounter 52) (< encounter 56))
            (flying_snakes))
        ((or (equal? encounter 56) (equal? encounter 57))
            (frost_giant (random 5)))
        ((or (> encounter 57) (< encounter 64))
            (giant_lizard (+ 1 (random 6))))
        ((or (> encounter 63) (< encounter 68))
            (giant_snapping_turtle))
        ((or (> encounter 67) (< encounter 72))
            (lizard_folk))
        ((or (> encounter 71) (< encounter 75))
            (red_wizard (+ 1 (random 6))))
        ((or (> encounter 74) (< encounter 85))
            (sea_hag))
        ((or (> encounter 84) (< encounter 88))
            (stirges))
        ((or (> encounter 87) (< encounter 90))
            (swarm_of_bats (+ 1 (random 4))))
        ((or (> encounter 89) (< encounter 95))
            (tabaxi_hunter))   
        (else (tri-flower_frond (+ 1 (random 4))))))


(define (encounters_jungle logic)
  (case logic
    ((0) (albino_dwarves (+ 4 (random 4))))
    ((1) (almiraj (+ 1 (random 6))))
    ((4) (artus_cimber))
    ((7) (axe_beaks (+ 4 (random 6))))
    ((8) (baboons (+ 3 (random 6))))
    ((15) (cyclops))
    ((16) (allosaurs (+ 1 (random 3))))
    ((17) (ankylosaurus))
    ((18) (brontosaurus))
    ((23) (pteranodon (+ 2 (random 6) (random 6))))
    ((35) (faerie_dragon))
    ((36) (eblis (+ 2 (random 4))))
    ((44) (explorer (+ 1 (random 6)) (random 6)))
    ((45) (flail_snail))
    ((50) (flying_monkey (+ 3 (random 6) (random 6) (random 6))))
    ((55) (giant_boar (+ 2 (random 4))))
    ((56) (giant_frog (+ 2 (random 6) (random 6))))
    ((57) (giant_lizard (+ 1 (random 6))))
    ((58) (giant_scorpion (+ 1 (random 3))))
    ((59) (giant_wasp))
    ((66) (jaculis (+ 1 (random 6))))
    ((67) (kamadan (+ 1 (random 3))))
    ((72) (traps (+ 2 (random 4))))
    ((73) (night_hag))
    ((74) (pterafolk))
    ((75) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((76) (red_wizard (+ 1 (random 6))))
    ((79) (giant_constrictor_snake))
    ((80) (giant_snake))
    ((81) (spider (+ 1 (random 6))))
    ((85) (stirge (+ 2 (random 6))))
    ((86) (su-monster (+ 2 (random 4))))
    ((87) (swarm_of_bats (+ 1 (random 4))))
    ((88) (swarm_of_insects (+ 1 (random 4))))
    ((89) (tabaxi_hunter))
    ((90) (tiger))
    ((91) (tri-flower_frond (+ 1 (random 4))))
    ((92) (vegepygmie (+ 1 (random 4))))
    ((93) (wereboar))
    ((94) (weretiger))
    ((95) (winterscape))
    ((96) (yellow_musk_creeper_and_zombies (+ 3 (random 6) (random 6) (random 6))))
    ((97) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))
    ((98) (zhentarim (+ 1 (random 6)) (+ 2 (random 6) (random 6))))
    ((99) (zorbos (+ 2 (random 6) (random 6))))
    (else
      (cond
        ((or (equal? logic 2) (equal? logic? 3)) (apes (+ 2 (random 4) (random 4))))
        ((or (equal? logic 5) (equal? logic? 6)) (assassin_vines (+ 1 (random 3))))
        ((or (equal? logic 9) (equal? logic? 10)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? logic 11) (equal? logic? 12)) (cannibals (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 13) (equal? logic? 14)) (chwinga))
        ((or (equal? logic 19) (equal? logic? 20)) (deinonychus (+ 3 (random 4))))
        ((or (equal? logic 21) (equal? logic? 22)) (hadrosaurus (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6))))
        ((or (equal? logic 24) (equal? logic? 25)) (stegosaurus))
        ((or (equal? logic 26) (equal? logic? 27)) (triceratops))
        ((or (equal? logic 28) (equal? logic? 29)) (tyrannosaurus (random 2) (random 5)))
        ((or (> logic 29) (< logic 35)) (velociraptor (+ 3 (random 6) (random 6) (random 6))))
        ((or (> logic 36) (< logic 42)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (equal? logic 42) (equal? logic? 43)) (dead_explorer (random 20)))
        ((or (> logic 45) (< logic 50)) (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 51) (equal? logic? 52)) (flying_snake (+ 2 (random 6) (random 6))))
        ((or (equal? logic 53) (equal? logic? 54)) (frost_giant (random 5)))
        ((or (equal? logic 60) (equal? logic? 61)) (girallon))
        ((or (equal? logic 62) (equal? logic? 63)) (goblin (+ 5 (random 6) (random 6))))
        ((or (equal? logic 64) (equal? logic? 65)) (grung (+ 2 (random 6) (random 6))))
        ((or (equal? logic 68) (equal? logic? 69)) (lizardfolk (+ 2 (random 4) (random 4))))
        ((or (equal? logic 70) (equal? logic? 71)) (mad_monkey_mist))
        ((or (equal? logic 77) (equal? logic? 78)) (snake_constrictor))
        ((or (> logic 81) (< logic 85)) (statue_ubtao (random 4)))))))


(define (encounters_lesser_undead logic)
  (case logic
    ((0) (albino_dwarves (+ 4 (random 4))))
    ((1) (artus_cimber))
    ((5) (axe_beaks (+ 4 (random 6))))
    ((10) (allosaurs (+ 1 (random 3))))
    ((11) (ankylosaurus))
    ((12) (deinonychus (+ 3 (random 4))))
    ((13) (hadrosaurus (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6))))
    ((14) (pteranodon (+ 2 (random 6) (random 6))))
    ((15) (stegosaurus))
    ((16) (triceratops))
    ((17) (tyrannosaurus (random 2) (random 5)))
    ((22) (explorer (+ 1 (random 6)) (random 6)))
    ((26) (flying_snake (+ 2 (random 6) (random 6))))
    ((27) (giant_lizard (+ 1 (random 6))))
    ((28) (giant_wasp))
    ((41) (night_hag))
    ((44) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((45) (red_wizard (+ 1 (random 6))))
    ((48) (giant_constrictor_snake))
    ((49) (giant_snake))
    ((65) (tri-flower_frond (+ 1 (random 4))))
    ((66) (troll))
    ((79) (wight))
    ((91) (wereboar))
    ((92) (weretiger))
    ((93) (winterscape))
    ((98) (zhentarim (+ 1 (random 6)) (+ 2 (random 6) (random 6))))
    ((99) (zorbos (+ 2 (random 6) (random 6))))
    (else
      (cond
        ((or (> logic 1) (< logic 5)) (assassin_vines (+ 1 (random 3))))
        ((or (equal? logic 6) (equal? logic? 7)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? logic 8) (equal? logic? 9)) (cannibals (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 18) (equal? logic? 19)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (equal? logic 20) (equal? logic? 21)) (dead_explorer (random 20)))
        ((or (> logic 22) (< logic 26)) (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 29) (equal? logic? 30)) (girallon))
        ((or (equal? logic 31) (equal? logic? 32)) (goblin (+ 5 (random 6) (random 6))))
        ((or (equal? logic 33) (equal? logic? 34)) (grung (+ 2 (random 6) (random 6))))
        ((or (> logic 34) (< logic 39)) (mad_monkey_mist))
        ((or (equal? logic 39) (equal? logic? 40)) (traps (+ 2 (random 4))))
        ((or (equal? logic 42) (equal? logic? 43)) (pterafolk))
        ((or (equal? logic 46) (equal? logic? 47)) (snake_constrictor))
        ((or (equal? logic 50) (equal? logic? 51)) (spider (+ 1 (random 6))))
        ((or (> logic 51) (< logic 55)) (statue_ubtao (random 4)))
        ((or (equal? logic 55) (equal? logic? 56)) (stirge (+ 2 (random 6))))
        ((or (equal? logic 57) (equal? logic? 58)) (su-monster (+ 2 (random 4))))
        ((or (> logic 58) (< logic 62)) (swarm_of_bats (+ 1 (random 4))))
        ((or (> logic 61) (< logic 65)) (swarm_of_insects (+ 1 (random 4))))
        ((or (> logic 66) (< logic 72)) (ghoul (+ 2 (random 6) (random 6))))
        ((or (> logic 71) (< logic 77)) (skeleton (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 77) (equal? logic? 78)) (specter))
        ((or (> logic 79) (< logic 89)) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 89) (equal? logic? 90)) (vegepygmie (+ 1 (random 4))))
        ((or (equal? logic 94) (equal? logic? 95)) (yellow_musk_creeper_and_zombies (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 96) (equal? logic? 97)) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))))))


(define (encounters_greater_undead logic)
  (case logic
    ((0) (artus_cimber))
    ((1) (assassin_vines (+ 1 (random 3))))
    ((5) (allosaurs (+ 1 (random 3))))
    ((6) (ankylosaurus))
    ((7) (hadrosaurus (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6))))
    ((8) (pteranodon (+ 2 (random 6) (random 6))))
    ((9) (stegosaurus))
    ((20) (explorer (+ 1 (random 6)) (random 6)))
    ((23) (giant_wasp))
    ((24) (traps (+ 2 (random 4))))
    ((25) (pterafolk))
    ((26) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((27) (red_wizard (+ 1 (random 6))))
    ((31) (giant_constrictor_snake))
    ((32) (giant_snake))
    ((44) (su-monster (+ 2 (random 4))))
    ((45) (swarm_of_bats (+ 1 (random 4))))
    ((49) (tri-flower_frond (+ 1 (random 4))))
    ((50) (troll))
    ((91) (winterscape))
    (else
      (cond
        ((or (> logic 1) (< logic 5)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? logic 10) (equal? logic? 11)) (tyrannosaurus (random 2) (random 5)))
        ((or (equal? logic 12) (equal? logic? 13)) (velociraptor (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 14) (equal? logic? 15)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (> logic 15) (< logic 20)) (dead_explorer (random 20)))
        ((or (equal? logic 21) (equal? logic? 22)) (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (> logic 27) (< logic 31)) (snake_constrictor))
        ((or (> logic 32) (< logic 36)) (spider (+ 1 (random 6))))
        ((or (> logic 35) (< logic 40)) (statue_ubtao (random 4)))
        ((or (> logic 39) (< logic 44)) (stirge (+ 2 (random 6))))
        ((or (> logic 45) (< logic 49)) (swarm_of_insects (+ 1 (random 4))))
        ((or (> logic 50) (< logic 63)) (ghoul (+ 2 (random 6) (random 6))))
        ((or (> logic 62) (< logic 67)) (skeleton (+ 3 (random 6) (random 6) (random 6))))
        ((or (> logic 66) (< logic 70)) (specter))
        ((or (> logic 69) (< logic 73)) (wight))
        ((or (> logic 72) (< logic 85)) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 85) (equal? logic? 86)) (vegepygmie (+ 1 (random 4))))
        ((or (equal? logic 87) (equal? logic? 88)) (wereboar))
        ((or (equal? logic 89) (equal? logic? 90)) (weretiger))
        ((or (> logic 91) (< logic 96)) (yellow_musk_creeper_and_zombies (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 96) (equal? logic? 97)) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 98) (equal? logic? 99)) (zorbos (+ 2 (random 6) (random 6))))))))


(define (encounters_mountains logic)
  (case logic
    ((61) (giant_boar (+ 2 (random 4))))
    ((62) (giant_lizard (+ 1 (random 6))))
    ((80) (red_wizard (+ 1 (random 6))))
    (else
      (cond
        ((or (> logic -1) (< logic 11)) (aarakocra (+ 2 (random 4))))
        ((or (> logic 10) (< logic 17)) (albino_dwarves (+ 4 (random 4))))
        ((or (> logic 16) (< logic 20)) (apes (+ 2 (random 4) (random 4))))
        ((or (equal? logic 20) (equal? logic? 21)) (baboons (+ 3 (random 6))))
        ((or (> logic 21) (< logic 25)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? logic 25) (equal? logic? 26)) (chwinga))
        ((or (equal? logic 27) (equal? logic? 28)) (cyclops))
        ((or (> logic 28) (< logic 38)) (pteranodon (+ 2 (random 6) (random 6))))
        ((or (> logic 37) (< logic 42)) (quetzalcoatlus (+ 2 (random 4))))
        ((or (> logic 41) (< logic 45)) (dragon))
        ((or (equal? logic 45) (equal? logic? 46)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (> logic 46) (< logic 50)) (dead_explorer (random 20)))
        ((or (> logic 49) (< logic 53)) (explorer (+ 1 (random 6)) (random 6)))
        ((or (> logic 52) (< logic 59)) (flying_monkey (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 59) (equal? logic? 60)) (flying_snake (+ 2 (random 6) (random 6))))
        ((or (equal? logic 63) (equal? logic? 64)) (giant_wasp))
        ((or (> logic 64) (< logic 70)) (girallon))
        ((or (> logic 69) (< logic 73)) (night_hag))
        ((or (> logic 72) (< logic 80)) (pterafolk))
        ((or (> logic 80) (< logic 84)) (giant_snake))
        ((or (> logic 83) (< logic 87)) (stirge (+ 2 (random 6))))
        ((or (> logic 86) (< logic 90)) (swarm_of_bats (+ 1 (random 4))))
        ((or (equal? logic 90) (equal? logic? 91)) (tabaxi_hunter))
        ((or (> logic 91) (< logic 97)) (troll))
        ((or (> logic 96) (< logic 100)) (wereboar))))))


(define (encounters_rivers logic)
  (case logic
    ((9) (assassin_vines (+ 1 (random 3))))
    ((23) (brontosaurus))
    ((36) (faerie_dragon))
    ((62) (giant_wasp))
    ((66) (jaculis (+ 1 (random 6))))
    ((67) (lizardfolk (+ 2 (random 4) (random 4))))
    ((72) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((73) (red_wizard (+ 1 (random 6))))
    ((79) (giant_constrictor_snake))
    ((80) (statue_ubtao (random 4)))
    ((93) (ghoul (+ 2 (random 6) (random 6))))
    ((94) (skeleton (+ 3 (random 6) (random 6) (random 6))))
    ((95) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
    (else
      (cond
        ((or (> logic -1) (< logic 3)) (aarakocra (+ 2 (random 4))))
        ((or (> logic 2) (< logic 7)) (aldani (+ 1 (random 4))))
        ((or (equal? logic 7) (equal? logic? 8)) (artus_cimber))
        ((or (equal? logic 10) (equal? logic? 11)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (> logic 11) (< logic 15)) (cannibals (+ 3 (random 6) (random 6) (random 6))))
        ((or (> logic 14) (< logic 18)) (chwinga))
        ((or (> logic 17) (< logic 23)) (crocodiles (+ 2 (random 4))))
        ((or (equal? logic 24) (equal? logic? 25)) (dimetrodon (+ 2 (random 6) (random 6))))
        ((or (equal? logic 26) (equal? logic? 27)) (hadrosaurus (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6))))
        ((or (> logic 27) (< logic 31)) (plesiosaurus))
        ((or (> logic 30) (< logic 34)) (pteranodon (+ 2 (random 6) (random 6))))
        ((or (equal? logic 34) (equal? logic? 35)) (quetzalcoatlus (+ 2 (random 4))))
        ((or (> logic 36) (< logic 40)) (eblis (+ 2 (random 4))))
        ((or (> logic 39) (< logic 43)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (equal? logic 43) (equal? logic? 44)) (dead_explorer (random 20)))
        ((or (> logic 44) (< logic 49)) (explorer (+ 1 (random 6)) (random 6)))
        ((or (equal? logic 49) (equal? logic? 50)) (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 51) (equal? logic? 52)) (flying_monkey (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 53) (equal? logic? 54)) (flying_snake (+ 2 (random 6) (random 6))))
        ((or (> logic 54) (< logic 58)) (giant_crocodile))
        ((or (equal? logic 58) (equal? logic? 59)) (giant_frog (+ 2 (random 6) (random 6))))
        ((or (equal? logic 60) (equal? logic? 61)) (giant_snapping_turtle))
        ((or (> logic 62) (< logic 66)) (grung (+ 2 (random 6) (random 6))))
        ((or (equal? logic 68) (equal? logic? 69)) (mad_monkey_mist))
        ((or (equal? logic 70) (equal? logic? 71)) (pterafolk))
        ((or (equal? logic 74) (equal? logic? 75)) (sea_hag))
        ((or (> logic 75) (< logic 79)) (snake_constrictor))
        ((or (equal? logic 81) (equal? logic? 82)) (stirge (+ 2 (random 6))))
        ((or (equal? logic 83) (equal? logic? 84)) (swarm_of_insects (+ 1 (random 4))))
        ((or (> logic 84) (< logic 91)) (swarm_of_quippers (+ 1 (random 4))))
        ((or (equal? logic 91) (equal? logic? 92)) (tabaxi_hunter))
        ((or (equal? logic 96) (equal? logic? 97)) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 98) (equal? logic? 99)) (zhentarim (+ 1 (random 6)) (+ 2 (random 6) (random 6))))))))


(define (encounters_ruins logic)
  (case logic
    ((2) (almiraj (+ 1 (random 6))))
    ((18) (chwinga))
    ((21) (deinonychus (+ 3 (random 4))))
    ((22) (velociraptor (+ 3 (random 6) (random 6) (random 6))))
    ((38) (flying_snake (+ 2 (random 6) (random 6))))
    ((41) (giant_lizard (+ 1 (random 6))))
    ((57) (lizardfolk (+ 2 (random 4) (random 4))))
    ((60) (night_hag))
    ((61) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((62) (red_wizard (+ 1 (random 6))))
    ((77) (tabaxi_hunter))
    ((80) (troll))
    ((93) (weretiger))
    ((94) (winterscape))
    ((95) (yellow_musk_creeper_and_zombies (+ 3 (random 6) (random 6) (random 6))))
    (else
      (cond
        ((or (equal? logic 0) (equal? logic? 1)) (albino_dwarves (+ 4 (random 4))))
        ((or (> logic 2) (< logic 6)) (apes (+ 2 (random 4) (random 4))))
        ((or (equal? logic 6) (equal? logic? 7)) (artus_cimber))
        ((or (> logic 7) (< logic 12)) (assassin_vines (+ 1 (random 3))))
        ((or (equal? logic 12) (equal? logic? 13)) (baboons (+ 3 (random 6))))
        ((or (> logic 13) (< logic 18)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (equal? logic 19) (equal? logic? 20)) (cyclops))
        ((or (> logic 22) (< logic 26)) (emerald_enclave (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))))
        ((or (equal? logic 26) (equal? logic? 27)) (dead_explorer (random 20)))
        ((or (> logic 27) (< logic 31)) (explorer (+ 1 (random 6)) (random 6)))
        ((or (equal? logic 31) (equal? logic? 32)) (flail_snail))
        ((or (> logic 32) (< logic 36)) (flaming_fist (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 36) (equal? logic? 37)) (flying_monkey (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 39) (equal? logic? 40)) (frost_giant (random 5)))
        ((or (> logic 41) (< logic 45)) (giant_scorpion (+ 1 (random 3))))
        ((or (> logic 44) (< logic 48)) (giant_wasp))
        ((or (equal? logic 48) (equal? logic? 49)) (girallon))
        ((or (equal? logic 50) (equal? logic? 51)) (goblin (+ 5 (random 6) (random 6))))
        ((or (equal? logic 52) (equal? logic? 53)) (jaculis (+ 1 (random 6))))
        ((or (> logic 53) (< logic 57)) (kamadan (+ 1 (random 3))))
        ((or (equal? logic 58) (equal? logic? 59)) (mad_monkey_mist))
        ((or (> logic 62) (< logic 66)) (giant_snake))
        ((or (equal? logic 66) (equal? logic? 67)) (spider (+ 1 (random 6))))
        ((or (> logic 67) (< logic 73)) (statue_ubtao (random 4)))
        ((or (equal? logic 73) (equal? logic? 74)) (stirge (+ 2 (random 6))))
        ((or (equal? logic 75) (equal? logic? 76)) (swarm_of_bats (+ 1 (random 4))))
        ((or (equal? logic 78) (equal? logic? 79)) (tri-flower_frond (+ 1 (random 4))))
        ((or (> logic 80) (< logic 84)) (ghoul (+ 2 (random 6) (random 6))))
        ((or (> logic 83) (< logic 87)) (skeleton (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 87) (equal? logic? 88)) (specter))
        ((or (equal? logic 89) (equal? logic? 90)) (wight))
        ((or (equal? logic 91) (equal? logic? 92)) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 96) (equal? logic? 97)) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))
        ((or (equal? logic 98) (equal? logic? 99)) (zhentarim (+ 1 (random 6)) (+ 2 (random 6) (random 6))))))))


(define (encounters_swamp logic)
  (case logic
    ((10) (artus_cimber))
    ((21) (allosaurs (+ 1 (random 3))))
    ((22) (ankylosaurus))
    ((69) (mephit (+ 2 (random 6) (random 6))))
    ((70) (night_hag))
    ((71) (rare_plants (random 8) (+ 1 (random 4)) (+ 1 (random 6))))
    ((94) (ghoul (+ 2 (random 6) (random 6))))
    ((97) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
    ((98) (yellow_musk_creeper_and_zombies (+ 3 (random 6) (random 6) (random 6))))
    ((99) (yuan-ti (+ 2 (random 6)) (+ 1 (random 4))))
    (else
      (cond
        ((or (> logic -1) (< logic 10)) (aldani (+ 1 (random 4))))
        ((or (> logic 10) (< logic 14)) (assassin_vines (+ 1 (random 3))))
        ((or (equal? logic 14) (equal? logic? 15)) (chwinga))
        ((or (> logic 15) (< logic 21)) (crocodiles (+ 2 (random 4))))
        ((or (equal? logic 23) (equal? logic? 24)) (brontosaurus))
        ((or (> logic 24) (< logic 30)) (dimetrodon (+ 2 (random 6) (random 6))))
        ((or (> logic 29) (< logic 33)) (hadrosaurus (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6))))
        ((or (equal? logic 33) (equal? logic? 34)) (pteranodon (+ 2 (random 6) (random 6))))
        ((or (> logic 34) (< logic 39)) (eblis (+ 2 (random 4))))
        ((or (equal? logic 39) (equal? logic? 40)) (dead_explorer (random 20)))
        ((or (> logic 40) (< logic 45)) (explorer (+ 1 (random 6)) (random 6)))
        ((or (equal? logic 45) (equal? logic? 46)) (flail_snail))
        ((or (> logic 46) (< logic 50)) (flying_snake (+ 2 (random 6) (random 6))))
        ((or (> logic 49) (< logic 53)) (giant_crocodile))
        ((or (> logic 52) (< logic 56)) (giant_frog (+ 2 (random 6) (random 6))))
        ((or (equal? logic 56) (equal? logic? 57)) (giant_lizard (+ 1 (random 6))))
        ((or (equal? logic 58) (equal? logic? 59)) (giant_snapping_turtle))
        ((or (equal? logic 60) (equal? logic? 61)) (giant_wasp))
        ((or (equal? logic 62) (equal? logic? 63)) (grung (+ 2 (random 6) (random 6))))
        ((or (equal? logic 64) (equal? logic? 65)) (lizardfolk (+ 2 (random 4) (random 4))))
        ((or (> logic 65) (< logic 69)) (mad_monkey_mist))
        ((or (> logic 71) (< logic 76)) (shambling_mound))
        ((or (> logic 75) (< logic 80)) (snake_constrictor))
        ((or (equal? logic 80) (equal? logic? 81)) (giant_constrictor_snake))
        ((or (> logic 81) (< logic 85)) (statue_ubtao (random 4)))
        ((or (equal? logic 85) (equal? logic? 86)) (stirge (+ 2 (random 6))))
        ((or (equal? logic 87) (equal? logic? 88)) (swarm_of_bats (+ 1 (random 4))))
        ((or (> logic 88) (< logic 94)) (swarm_of_insects (+ 1 (random 4))))
        ((or (equal? logic 95) (equal? logic? 96)) (skeleton (+ 3 (random 6) (random 6) (random 6))))))))


(define (encounters_wasteland logic)
  (case logic
    ((0) (artus_cimber))
    ((18) (explorer (+ 1 (random 6)) (random 6)))
    ((78) (statue_ubtao (random 4)))
    ((97) (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))))
    (else
      (cond
        ((or (> logic 0) (< logic 5)) (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))
        ((or (> logic 4) (< logic 9)) (dragon))
        ((or (> logic 8) (< logic 18)) (dead_explorer (random 20)))
        ((or (> logic 18) (< logic 37)) (firenewt (+ 1 (random 4))))
        ((or (> logic 36) (< logic 45)) (giant_scorpion (+ 1 (random 3))))
        ((or (> logic 44) (< logic 54)) (magmin (+ 2 (random 6) (random 6))))
        ((or (> logic 53) (< logic 71)) (mephit (+ 2 (random 6) (random 6))))
        ((or (> logic 70) (< logic 78)) (night_hag))
        ((or (> logic 78) (< logic 83)) (troll))
        ((or (equal? logic 83) (equal? logic? 84)) (ghoul (+ 2 (random 6) (random 6))))
        ((or (> logic 84) (< logic 95)) (skeleton (+ 3 (random 6) (random 6) (random 6))))
        ((or (equal? logic 95) (equal? logic? 96)) (wight))
        ((or (equal? logic 98) (equal? logic? 99)) (zhentarim (+ 1 (random 6)) (+ 2 (random 6) (random 6))))))))
  

;; (+ 2 (random 4))
(define (aarakocra how_many)
    (display "Passive Perception score of 15 spots ")
    (write how_many)
    (display " aarakocra flying overhead. 
They will follow the party at a safe distance.
These creatures are scouts from Kir Sabal. They
observe the party from a safe distance but don’t approach
unless the characters demonstrate peaceful intentions. The
bird folk are friendly and can point characters in the direction
of nearby landmarks.\n\n"))

;; number of creatures = (+ 4 (random 4))
(define (albino_dwarves number_of_creatures)
    (write number_of_creatures)
    (display " Albino Dwarfs emerge from the ground
    AC: 13 (hide armor)
    HP: 4d8 + 12
    Speed: 25ft
    CR: 1/4(50 XP)
STR: +1     DEX: +1     CON: +3     INT: +1     WIS: +2     CHA: 0
Skills: perception +4 stealth +3 survival +4
resist: poison (advantage against as well)
senses: 60ft darkvision
language: common, dwarvish

    3|1 1d6 slash, throw 20/60

Any character with a passive Wisdom (Perception) score of 13
or higher spots the dwarves, but all others are surprised. The
dwarves knock characters out rather than killing them,
stealing food, water, and gear from those rendered
unconscious. They break off their attack if any character
speaks Dwarvish to them or demonstrates peaceful intentions.\n\n"))


;; (+ 1 (random 4))
(define (aldani number_of_creatures) 
    (display "DC 12 perception
    The party is tailed by ")
    (write number_of_creatures)
    (display " aldani (lobsterfolk) 
    They attempt to bribe the party to leave if talked to.
    If bribe not taken they will attack.
    pg 210 tomb of annihilation\n\n"))


;; (+ 1 (random 6))
(define (almiraj number_of_creatures)
    (display "DC 12 perception
    The players spot ")
    (write number_of_creatures)
    (display " almiraj 60ft away.
    If traped DC 14 animal handling to calm it down.
    Once calmed the creature it will stay with the party.
    pg 217 tomb of annihilation\n\n"))

;; (+ 2 (random 4) (random 4))
(define (apes number_of_creatures)
    (display "The party stumbles across ")
    (write number_of_creatures)
    (display " apes as they eat fruit.
    The apes begin to make a racket at the parties presence, guarding their food.
    If the party makes sudden movement or advances the apes attack.
    pg 317 monster manual\n\n"))


(define (artus_cimber)
    (display "The party crosses path with NPC Artus Cimber
    pg 212 in tomb of annihilation\n\n"))

;; (+ 1 (random 3))
(define (assassin_vines number_of_creatures)
    (write number_of_creatures)
    (display " assassin vines are present close to the characters.
    Describe a rotten smell in the air. Please note assasin vines can move.
    pg 213 tomb of annihilation\n\n"))

;; (+ 4 (random 6))
(define (axe_beaks number_of_creatures)
    (display "A flock of ")
    (write number_of_creatures)
    (display " axe beaks burst from the under growth through the party
    The party has one round of prep as the birds are loud.
    pg 317 monster manual\n\n"))

;; (+ 3 (random 6))
(define (baboons number_of_creatures)
    (write number_of_creatures)
    (display " baboons begin to make a ruckus as the players pass
    Describe them as hungry and give the players one turn to act.
    pg 318 monster manual\n\n"))

;; (random 20) (+ 1 (random 4)) (+ 1 (random 10))
(define (cache type random1 random2)
    (display "The players find a cache containing ")
    (case type
    ((1) (display "a rain catcher (pg 32) and a mess kit."))
    ((2) (display "10 days of rations."))
    ((3) (display "20 days of rations."))
    ((4) (display "50 days of rations."))
    ((5) (write random1) (display " casks of water. (5 gallons each)"))
    ((6) (write random1) (display " casks of tej. (pg 32)"))
    ((7) (write random1) (display " climber kits."))
    ((8) (write (+ random1 random1)) (display " vials of antitoxin."))
    ((9) (display "20 days of insect repellent. (pg 32)"))
    ((10) (write random1) (display " full quivers."))
    ((11) (display "a canoe with six paddles."))
    ((12) (display "2 hooded lanterns and 10 flasks of lanturn oil."))
    ((13) (display "a 2 man tent and ") (write random1) (display " explorer pack(s)."))
    ((14) (display "a wooden box containing ") (write (+ random2 random2)) (display " weapons."))
    ((15) (display "navigators tools."))
    ((16) (write random2) (display " changes of clothing."))
    ((17) (display "cartographer's tools."))
    ((18) (display "2 man tent and ") (write random1) (display " healer kits."))
    ((19) (display "2 two man tents, folding camp table, four folding stools."))
    ((0) (display "Wooden box containing ") (write (+ random1 random1)) (display " potions of healing (strength will vary)")))
    (display "\n\n"))


;; (+ 3 (random 6) (random 6) (random 6))
(define (cannibals number_of_creatures)
    (display "If it is night then this is an ambush encounter.
    If it is day then the canibals are feeding on corpses.
    The party encounters ")
    (write number_of_creatures)
    (display "  tribal warriors.
    They paint a
    blue triangle (Ras Nsi’s symbol) on their foreheads as proof of
    their devotion
    pg 350 monsters manual\n\n"))


(define (chwinga)
    (display "A chwinga (tiny shadow elemental) stalks the party to steal something.
    It will follow the party until a unattended item is left. 
    DC 17 perception check when it strikes.
    It will leave something shiny or food related in the items place.
    If it hasn't found anything yet it will aid the party with:
    Druidcraft, guidance, pass without a trace, resistance and
    1/day magical gift: pick a charm from pg 228 of DM guide.
    pg 216 tomb of annihilation\n\n"))


;; (+ 2 (random 4))
(define (crocodiles number_of_creatures)
    (display "DC 12 perception to notice ")
    (write number_of_creatures)
    (display " crocodiles in wait for the party. Else combat ensuse.
    pg 320 monster manual\n\n"))


(define (cyclops number_of_creatures)
    (display "A cyclops is heading home to snapping turtle bay.
    It isn't looking for a fight  and will ignore the party unless
    the party shows hostile intent. It speaks Giant and is quite friendly to adventures.
    pg 45 of monsters manual.\n\n"))

;; (+ 1 (random 3)) 
(define (allosaurs number_of_creatures)
    (display "This encounter depends on scent. If the party is wearing repelent and has 
    bathed recently then this encounter is avoided.
    The party sees ")
    (write number_of_creatures)
    (display " allosaurus 100 ft out and looking to eat.
    pg 79 monster manual\n\n"))


(define (ankylosaurus)
    (display "A grumpy ankylosaurus is eating plants. If disturbed consult
    pg 79 monsters manual\n\n"))


(define (brontosaurus)
    (display "A lone brontosaurus lumbers past the party without noticing them.
    DC 15 acrobatics to avoid its steps else take 5d8 + 5 bludgening
    A DC 25 strength save can also prevent the damage.
    The creature is non hostile but can be found on 
    pg 215 tomb of annihilation\n\n"))


;; (+ 3 (random 4))
(define (deinonychus number_of_creatures)
    (display "A wild beast (small game) runs across the parties path.
    This diversion is followed by ")
    (write number_of_creatures)
    (display " deinonychus (popularly mistaken for velocoraptors) on the hunt
    pg 217 tomb of annihilation.\n\n"))
    

;; (+ 2 (random 6) (random 6))
(define (dimetrodon number_of_creatures)
    (write number_of_creatures)
    (display " dimetrodons are spoted by the party flip a coin
    If heads they are hungry and are looking at the party.
    If tails they don't care about the party.
    Either way they are highly territorial.
    pg 217 tomb of annihilation\n\n"))


;; (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 6)) 
(define (hadrosaurus number_of_creatures number_of_children)
    (display "The party spots ")
    (write number_of_creatures)
    (display " adult hadrosaurus and ")
    (write number_of_children)
    (display " young hadrosaurus. The children can be tamed with grass and 
    a DC 15 animal handling check. The adults will attack anyone close to their kids.
    The children can be used as pack mules, horses or sold to the market in Nyanzaru for 50gp
    pg 224 tomb of annihilation\n\n"))


(define (plesiosaurus)
    (display "2 plesiosaurus are fighting over something dead.
    If the party is on the water then at 300ft out they attack the party. 
    If the party is walking then they don't.
    pg 80 manster manual\n\n"))


;; (+ 2 (random 6) (random 6))
(define (pteranodon number_of_creatures)
    (display "A flock of ")
    (write number_of_creatures)
    (display " pteradons fly low over head.
    pg 80 monster manual\n\n"))


;; (+ 2 (random 4))
(define (quetzalcoatlus number_of_creatures)
    (write number_of_creatures)
    (display " quetzalcoatluses fly high overhead. 
    Unless in mountains or cliff where they nest.
    pg 230 tomb of annihilation\n\n"))


(define (stegosaurus)
    (display "A lone stegosaurus wanders towards the party out of curiousity
    If the players touch it it will attack the offender once as a warning.
    pg 231 Tomb of annihilation\n\n"))


(define (triceratops)
   (display "Flip a coin, Heads: Male, territorial and alone
   Tails: Female with hidden 2 pups and 4 eggs, angered in self defence and if someone gets between her and her eggs
   pg 80 monster manual\n\n"))

;; (random 2) (random 5)
(define (tyrannosaurus coin_flip which_fighter)
    (display "The party runs into a tyrannosaurus
    pg 80 monster manual")
    (cond 
        ((equal? coin_flip 0) (display ". It is alone\n\n"))
        (else 
            (display "it is fighting with another creature.\n")
            (case which_fighter 
                ((0) (stegosaurus))
                ((1) (triceratops))
                ((2) (giant_constrictor_snake 2))
                ((3) (king_kong))
                ((4) 
                    (ghoul (+ 2 (random 6) (random 6)) (+ 2 (random 6) (random 6))) 
                    (zombie (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4)) 1))))))


;; (+ 3 (random 6) (random 6) (random 6))
(define (velociraptor number_of_creatures)
    (display "Make a stealth check for velocoraptor
    If the groups passive perception beats it then a party member notices a rustling bush.
    ")
    (write number_of_creatures)
    (display " velociraptors desend on the party.
    pg 235 Tomb of annihilation\n\n"))


(define (faerie_dragon)
    (display "A green faerie dragon takes intrest in a party member.
    Gauge the parties mood for the hour:
	If the party is in good spirits then the dragon plays a prank at the parties next rest
	The dragon then gauges their mood one more time. If angry, sour or sad the dragon runs away
	else the dragon provides the answers 3 questions.
	It know the location of omu.
    pg 133 monster manual\n\n"))


(define (dragon)
    (display "A dragon flies overhead. 
    It is either headed to the sea, the Wyrmheart Mine or it is circling the mine.
    For the most part the dragon ignores the players unless they do something stupid.
    pg 83-118 monster manual, have fun\n\n"))


;; (+ 2 (random 4))
(define (eblis number_of_creatures)
    (write number_of_creatures)
    (display " eblis are either stumbled upon or come across.
    They typically live in reed huts in marshes.
    They attack the weak or injured but trade with healthy adventures for rare gems.
    50 gp to gain them as a guide.
    If it is night then they attempt to drag off one party member in their sleep.
    pg 219 Tomb of Annihilation\n\n"))


;; (+ 2 (random 4)) (* 5 (+ 2 (random 6) (random 6)))
(define (emerald_enclave scouts outpost)
    (display "The party encouter either:
    ")
    (write scouts)
    (display " scouts of the emerald enclave who are willing to trade or
    An outpost in the trees that is 
    ") 
    (write outpost) 
    (display " feet from the ground.
    Scouts: pg 349 monster manual\n\n"))


;; (random 20)
(define (dead_explorer type)
    (display "The party finds \n\n")
    (case type
	((0) (display "The bloated corpse of a dead halfling,
riddled with tiny arrows and dangling from
a tree vine. (The halfling trespassed on
grung sacred ground, and the corpse was
hung here as a warning.) \n\n"))
	((1) (display "The bones of an unarmored humanoid,
lashed to a tree by vines. (This explorer
was captured by Batiri goblins, doused in
honey, and left to be devoured by hungry
insects.)\n\n"))
	((2) (display "The crushed remains of an unarmored
dwarf, showing signs that she was
stomped to death by a rampaging
dinosaur.\n\n"))
	((3) (display "The gnawed and charred bones of a
humanoid. (This unfortunate was
murdered and cannibalized by his starving,
fever-crazed companions.)\n\n"))
	((4) (display "The mangled body of a half-elf,
seemingly bludgeoned to death. (She was
dropped from high altitude by pterafolk.)\n\n"))
	((5) (display "The scattered bones of a dwarf, torn to
pieces before being devoured. (A hunting
pack of velociraptors did the dwarf in.)\n\n"))
	((6) (display "The swollen, purple corpse of an elf,
dead only a few days ago from the bite of a
poisonous snake.\n\n"))
	((7) (display "A fresh human corpse stuffed into a
hollow tree. (Girallons plan to return and
devour it later.)\n\n"))
	((8) (display "The desiccated husk of a gnome,
cocooned in giant spider webs.\n\n"))
	((9) (display "The body of a human — from the waist
up. Signs show that the explorer crawled a
considerable distance after being bitten in
half by a tyrannosaurus. (A Flaming Fist
charter found on the corpse identifies it as
Lord Onovan IV, of the Dales.)\n\n"))
	((10)(display "A charred elf’s skeleton inside a
charred constrictor snake’s skeleton. (Both
were killed by a lightning bolt spell.)\n\n"))
	((11)(display "The rotting body of a giant frog with the
blade of a shortsword poking out its back.
(If the frog is cut open, the partially
digested body of a halfling is found inside.)\n\n"))
	((12)(display "A tabaxi spread-eagled on the ground,
but with its limbs and head severed from
its torso and crudely stitched back on in
the wrong arrangement.\n\n"))
	((13)(display "A half-orc spiked to an enormous tree
by the broken-off horn of a triceratops.\n\n"))
	((14)(display "A gnome, spitted over a burned-out fire
pit and thoroughly overcooked, but not
eaten. (Goblin weapons and tools are
scattered around amid velociraptor tracks.)\n\n"))
	((15)(display "A headless humanoid, hung upside
down from a tree and with six Batiri goblin
spears thrust symmetrically through the
body. (It was a Red Wizard, judging by the
robes. The head is nowhere to be found.)\n\n"))
	((16)(display "An elf, balanced on a tree branch 40
feet above the ground, arms and legs
dangling downward. (A note pressed
between the body and the branch explains
that the elf climbed the tree to get away
from a prowling allosaurus and was too
terrified to come down.)\n\n"))
	((17)(display "The smoldering remains of a human
wearing a Flaming Fist-style helmet, his
legs broken. (He succumbed to a faerie
dragon’s euphoria breath weapon and
stepped off a cliff. A companion tried to
carry him back to Fort Beluarian, but the
warrior died en route.)\n\n"))
	((18)(display "A dwarf with six large holes piercing her
armor and chest. (A stegosaurus caught
her squarely with a swipe of its tail.)\n\n"))
	((19)(display "The skeleton of a humanoid seated on
a folding camp stool, clutching a knife and
fork in its bony hands.\n\n"))))

;; (+ 1 (random 6)) (random 6)
(define (explorer number_of_explorers type )
    (display "The party runs into another band of explorers, consisting of a
mage, a priest, a scout, and ")
    (write number_of_explorers)
    (display " tribal warriors.
    ")
    (cond 
        ((or (equal? type 1) (equal? type 2))
            (display "They are lost and hungry."))
        ((or (equal? type 3) (equal? type 4))
            (display "They are in good shape and are hunted by something."))
        ((equal? type 5) (display "They are heading to the nearest landmark."))
        (else (display "They are headed to port Nyanzaru.")))
    (display "
    Mage: pg 347 monster manual
    Priest: pg 348 monster manual
    
    Scout:  
        HP: 28  AC: 14  Speed: 30ft
        Str     Dex     Con     Int     Wis     Cha
        +1      +3      +1      -1      0      -1
        Wields: leather, shield, longsword, light crossbow, 40 bolts
    -longsword: 3|1 1d8 slash (verstile 1d10)
    -light crossbow: 5|3 1d8 piercing
    -dash, disengage, hide as bonus action
    -1d6 on sneak attack
        CR: 1/2 (100 XP)
        
    Tribal Warriors: pg 350 monster manual\n\n"))


;; (+ 1 (random 4)) 
(define (firenewt number_of_creatures)
    (display "A light firenewt patrol consists of ")
    (write number_of_creatures)
    (display " firenewt warriors mounted on giant striders. 
A heavy patrol consists of ")
    (write (+ number_of_creatures number_of_creatures))
    (display " firenewt warriors and a firenewt warlock of Imix, all
mounted on giant striders. The firenewts always strike their final
blows with the intention of knocking out enemies, who are
then taken back to the creatures’ cave lair to be tortured and
eaten.
    pg 219 Tomb of Annihilation\n\n"))

    
(define (flail_snail)
    (display "Characters spot the slimy trail of a flail snail.
If they wish to follow it, a successful DC 10 Wisdom (Survival)
check correctly deduces which direction the snail was traveling.
    pg 220 Tomb of annihilation\n\n"))


;; (+ 2 (random 6)) (+ 1 (random 4))
(define (flaming_fist number_of_guards number_of_deinonychus)
    (display "The soldiers of the Flaming Fist know the dangers of Chult
better than most, and they don’t take the wilderness lightly. A
typical patrol is made up of a knight or veteran leading an
acolyte, a scout, and 2d6 guards — and is sometimes
accompanied by 1d4 deinonychuses (see appendix D) trained
to fight and hunt alongside their handlers. The Flaming Fist is
friendly and helpful toward adventurers possessing a charter
of exploration issued by Commander Liara Portyr of Fort
Beluarian. If the party has no such document, the patrol tries
to confiscate the adventurers’ critical gear and advises them to
replace it at Fort Beluarian — and to obtain a proper charter
while they’re at it.

    Knight: 
        HP: 88  AC: 18  Speed: 30ft
        Str     Dex     Con     Int     Wis     Cha
        +4      +3      +3      +1      +1      +2
        Perception: +6     Athletics: +9   Intimidate: +7
        Wields: Plate mail, shield, bastard sword, 14 javelins
    -Bastard sword: 9|4 1d10 slash (versitile 1d12)
    -javilin: 9|4 1d6 piercing (30/120)
    -Powerful strike: 9|4 1d12 slash, Athletics competition to avoid being knocked prone
    -Multi Attack: Knight attacks 3 times
        CR: 6.5 (2,500 XP)

    Veteran:
        HP: 126 AC: 20 (half cover behind shield)   Speed: 30ft
        Str     Dex     Con     Int     Wis     Cha
        +6      +2      +4      +3      +3      +3
        Athletics: +11  Intimidate: +8
        Wields: Plate mail, Heavy sheild, castlevania flail
    -Castlevania Flail: 11|6 1d8 bludgeoning 1d6 radiant, Finesse, reach
    -Tripping Flail: Athletics vs Athletics/Acrobatics compitition, Vetran has advantage.
    -Giving orders: if his attack lands One ally is compelled to strike as a reaction
    -Multi Attack: Veteran attacks 3 times
        CR: 7.5 (3,750 XP)

    Acolyte: 
        HP: 7    AC: 12     Speed: 30 ft
        Wields: Holy robes, holy symbol, 10 days rations, soap, 100 gp, a holy text
        Cantrips: Guidence, Light, Spare the Dying
        1st Spells: (2/2) Bless, Sanctuary, Cure Wounds (1d8 + 3), Shield of Faith
        -Fist: 4|2 1 bludgeoning
            CR: 1/4 (50 XP)

    Scout:  
        HP: 28  AC: 14  Speed: 30ft
        Str     Dex     Con     Int     Wis     Cha
        +1      +3      +1      -1      0      -1
        Wields: leather, shield, longsword, light crossbow, 40 bolts
    -longsword: 3|1 1d8 slash (verstile 1d10)
    -light crossbow: 5|3 1d8 piercing
    -dash, disengage, hide as bonus action
    -1d6 on sneak attack
        CR: 1/2 (100 XP)

    Guard:
        HP: 47   AC: 18  Speed: 30ft
        Str     Dex     Con     Int     Wis     Cha
        +4      +2      +3      +1      +1      +2
        Wields: Scale mail, shield, longsword, light crossbow, 40 bolts
    -longsword: 7|4 1d8 slash (verstile 1d10)
    -light crossbow: 5|2 1d8 piercing
    -push: Athletics competition to push opponent 5ft back
    -Multi Attack: Guards attack twice
        CR: 2 (450 XP)

    Deinonychus: pg 217 tomb of annihilation.\n\n"))


;; (+ 3 (random 6) (random 6) (random 6))
(define (flying_monkey number_of_creatures)
    (display "The sound of wings heralds the arrival of ")
    (write number_of_creatures)
    (display " flying monkeys
(see appendix D), which swoop through the adventurers in a
wave of grasping hands and feet. Each character must succeed
on a DC 12 Dexterity saving throw or lose a useful piece of
gear to the furry thieves.
These sociable creatures are curious about humanoids and
have little fear of them. A flying monkey that is successfully
grappled, caught in a net, or otherwise prevented from
escaping can be trained.
    pg 220 Tomb of annihilation\n\n"))


;; (+ 2 (random 6) (random 6))
(define (flying_snake number_of_creatures)
    (display "Have a player flip a coin, heads there is one flying snake.
Tails the characters encounter ")
    (write number_of_creatures)
    (display " flying snakes. 
These snakes attack only when threatened. A
flying snake that is successfully grappled can be stuffed in a
sack or other soft container. After 1 hour of confinement, the
snake settles down. A character who succeeds on a DC 13
Wisdom (Animal Handling) check can remove a calm snake
from the container without causing it to attack or fly away.
Characters can sell captured flying snakes to Ifan Talro’a in
Port Nyanzaru. He offers 25 gp for each snake
    pg 322 monster manual\n\n"))

;; (random 5)
(define (frost_giant chance_of_search)
    (display "The characters hear the sound of huge creatures stomping
through the wilderness. If they follow the noise, they
encounter a search party of three frost giants accompanied by
2 winter wolves.
    
    Frost Giant: 
        HP: 140 AC: 15  Speed: 40 ft    Huge
        Str     Dex     Con     Int     Wis     Cha
        +6      -1      +5      -1      +0      +1
        Saves: Con, Wis, Cha
        Proficincy: +3  Athletics: +9   Perception: +3
        Immune: cold
        Language: Giant
    -Multiattack: 2 attacks with greataxe
    -Greataxe: 9|6 3d12 slash, reach
    -Rock: 9|6 4d10 bludgeoning, range(60/240)
        CR: 8 (3,900 XP)

    Winter Wolf: pg 340 monster manual\n")
    (cond 
        ((equal? chance_of_search 1) 
            (display "This is Drufi's search party pg 64 Tomb of annihilation
They are looking for the ring of winter.\n\n"))
        (else (display "\n"))))


;; (+ 2 (random 4))
(define (giant_boar number_of_creatures)
    (display "The characters see and hear ")
    (write number_of_creatures)
    (display " giant boars foraging
ahead of them. Skirting around the boars to prevent them from
charging requires a successful DC 12 group Dexterity (Stealth)
check.
Pg 323 Monster manual\n\n"))


(define (giant_crocodile)
    (display "As the characters refill water or ride in a boat make one character make a 
    Dex saving throw DC 18.
    A Giant crocodile jumps out of the water and drags said player into the water.
    pg 324 monster manual\n\n"))


;; (+ 2 (random 6) (random 6)) 
(define (giant_frog number_of_creatures)
    (display "These ")
    (write number_of_creatures)
    (display " giant frogs have come together in hunger and try
    to eat everything that crosses their path. The characters have
    plenty of warning as the amphibians hop noisily toward them.
    pg 325 monster manual\n\n"))


;; (+ 1 (random 6))
(define (giant_lizard number_of_creatures)
    (display "The characters encounter ")
    (write number_of_creatures)
    (display " giant lizards sunning
    themselves on warm rocks. The lizards pose no threat unless
    they’re attacked, and they’re too set in their ways to be trained
    as pack animals.
    pg 326 monster manual\n\n"))


;; (+ 1 (random 3))
(define (giant_scorpion number_of_creatures)
    (display "Any character with a passive Wisdom (Perception) score of 11
    or higher spots ")
    (write number_of_creatures)
    (display " giant scorpions moments before they
    emerge from hiding and attack. At the end of the encounter,
    any character damaged by a giant scorpion must succeed on a
    DC 11 Constitution saving throw or become infected with
    shivering sickness (see “Diseases,�? page 40).
        pg 327 monster manual"))


(define (giant_snapping_turtle)
    (display "The characters spot a giant snapping turtle
    sunning itself on the shore. The turtle attacks any character it
    can see within 30 feet of it.
    pg 222 Tomb of annihilation\n\n"))


(define (giant_wasp number_of_creatures)
    (display "A droning sound announces the presence of ")
    (write number_of_creatures)
    (display " giant wasps
    before the characters see them. The wasps attack at once,
    ignoring heavily armored targets in favor of those with little or
    no defensive protection. At the end of the encounter, any
    character damaged by a giant wasp must succeed on a DC 11
    Constitution saving throw or become infected with shivering
    sickness (see “Diseases,�? page 40).
    Giant wasps are found on pg 329 monster manual\n\n"))


(define (girallon)
    (display "Two girallons hang in the trees, perch atop
    rocks, or lumber between crumbling, vine-covered ruins. The
    characters spot them automatically and can avoid a hostile
    encounter if they withdraw and succeed on a DC 13 group
    Dexterity (Stealth) check.
    If the characters defeat the girallons or frighten them away,
    a search of the area reveals a hidden cache. The girallons might also have some
    treasure hidden near the cache.
    Girallon is found pg 222 Tomb of annihilation\n")
    (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10)))
    (treasure (random 100)))


;; (+ 5 (random 6) (random 6))
(define (goblin number_of_creatures)
    (display "A typical Batiri (goblin) patrol or hunting party consists of a goblin
    boss leading ")
    (write number_of_creatures)
    (display " goblins, all wearing painted wooden
    masks. They move quietly through familiar areas and seldom
    range outside their home territory. Batiri prefer to hunt at
    night and lay low in ambush positions during the day.
    A night encounter with Batiri goblins involves an attack on
    the characters’ camp. Each party member standing watch must
    attempt a DC 16 Wisdom (Perception) check, made with
    disadvantage because of the noise of the jungle at night. On a
    success, a character detects the goblins moving into attack
    positions and can rouse the rest of the party. If no one on
    watch succeeds on the check, all the characters are surprised.
    If this encounter occurs while the characters are traveling
    during the day, have each party member make a DC 16
    Wisdom (Perception or Survival) check to spot the telltale
    signs of an ambush: disadvantageous terrain coupled with an
    eerie silence not normal for the jungle.
    Goblins bargain for their lives if captured. If the characters
    can force or coerce a vow of cooperation from goblin prisoners,
    the Batiri will serve as guides. They have the following
    additional skill: Survival +1.
        pg 166 monster manual\n\n"))


;; (+ 2 (random 6) (random 6))
(define (grung number_of_creatures)
    (display "A grung hunting party consists of ")
    (write number_of_creatures)
    (display " grungs led by a grung
    elite warrior (see appendix D for both). If this encounter
    occurs while the characters are traveling, the grungs have set
    up an ambush in the trees. Any character with a passive
    Wisdom (Perception) score of 14 or higher spots them just in
    time. All other characters are surprised.
    If this encounter occurs while the party is camped, the
    grungs spotted the characters earlier in the day and have
    shadowed them unseen. Each character standing watch must
    succeed on a DC 14 Wisdom (Perception) check, made with
    disadvantage because of the noise of the jungle at night. On a
    success, a character detects the encroaching grungs and can
    rouse the rest of the party. If no one on watch succeeds on the
    check, all the characters are surprised.
    If the characters capture one or more grungs, the frogfolk
    offer to lead the characters to treasure in exchange for a
    promise of freedom. The “treasure” is a half-mile away from
    the party’s present location and consists of a cache.
    grung: pg 223 Tomb of annihilation\n")
    (cache (random 20) (+ 1 (random 4)) (+ 1 (random 10))))


;; (+ 1 (random 6))
(define (jaculis number_of_creatures)
    (display "Without warning, ")
    (write number_of_creatures)
    (display " jaculis launch
    themselves at the party from the trees. Any character with a
    passive Wisdom (Perception) score of 14 or higher is able to
    react, but all others are surprised.
    jaculis: pg 225 Tomb of annihilation\n\n"))


;; (+ 1 (random 3))
(define (kamadan number_of_children)
    (display "The party is ambushed by 2 kamadan (+ 1 (random 3))s.
    Any character with a passive Wisdom (Perception) score of 16
    or higher gets a warning of the attack, but all others are
    surprised. Characters who prevail against the kamadans can
    search the area for their lair, finding it with a successful DC 15
    Wisdom (Survival) check.
    roll a d4 if 1 then
    the kamadan lair contains ")
    (write number_of_children)
    (display " noncombatant young the size of house cats. With their snakes
    not yet grown out, they look like leopard cubs. Ifan Talro’a in
    Port Nyanzaru will pay 150 gp for a live kamadan cub.
    kamadan: pg 225 Tomb of annihilation\n\n"))


(define (king_kong)
    (display "King Kong:
    Huge beast
    HP: 214     AC: 13      Speed: 40 ft, climb 40 ft
        Str     Dex     Con     Int     Wis     Cha
        +8      +3      +5      -1      +2      +1
    athletics: +12  acrobatics: +7  Perception: +6  
    -multiattack: King kong makes three attacks with its fists
    -Fist 12|8 3d10 bludgeoning, reach
    -Rock 12|8 7d6 bludgeoning, thrown(50/100)
    -Tree 12|8 3d12 bludgeoning, reach 20 ft, two handed
        CR: 9 5000"))


;; (+ 2 (random 4) (random 4))
(define (lizardfolk number_of_creatures)
    (display "The characters encounter ")
    (write number_of_creatures)
    (display " lizardfolk and a lizardfolk shaman. 
    These lizardfolk belong to a tribe or kingdom in the
    Valley of Dread and can be appeased with food (one day’s
    supply per lizardfolk in the group). Otherwise they attack.
    Lizardfolk: pg 204 monster manual
    Lizardfolk Shaman: pg 205 monster manual\n\n"))


(define (mad_monkey_mist)
    (display "A bank of blue mist drifts toward the party, covering an area of
     20-120 foot squares. Any character with a passive Wisdom
    (Perception) score of 13 or higher notices the mist and can
    warn others of its approach. If the encounter occurs while the
    party is camped, the mist drifts through the camp at a speed of
    5 feet per round. Characters who come into contact with the
    mist are exposed to mad monkey fever (see “Diseases,” page 40).\n\n"))


;; (+ 2 (random 6) (random 6))
(define (magmin number_of_creatures)
    (display "The characters are attacked by ")
    (write number_of_creatures)
    (display " magmins, which flee if
    reduced to fewer than half their starting number.
    magmin: pg 212 monster manual\n\n"))


;; (+ 2 (random 4))
(define (traps number_of_traps)
    (display "The characters blunder into a patch of ")
    (write number_of_traps)
    (display " mantraps, 
    which are undetectable until they attack. The
    plants are 10 feet apart, so that only one plant attacks on the
    first round. The others must wait until characters maneuver
    within 5 feet of them.
    mantrap: pg 227 Tomb of annihilation\n\n"))


;; (+ 2 (random 6) (random 6))
(define (mephit number_of_creatures)
    (display "The characters might encounter magma mephits, mud
    mephits, smoke mephits, or steam mephits (each appearing
    as a group of ")
    (write number_of_creatures)
    (display "). The mephits don’t attack unless they
    outnumber the characters two to one, but they’re reluctant to
    be helpful unless they themselves are outnumbered.
    mephit: monster manual pg 216-217\n\n"))


(define (night_hag)
    (display "One of the Sewn Sisters (see pg 179 ToA) shadows the party
    while staying in the Border Ethereal. During the party’s next
    long rest, the night hag materializes and snatches some blood
    or hair from a random character before returning to the
    Ethereal Plane.
    Night Hag: pg 178 monster manual\n\n"))


;; (+ 3 (random 4))
(define (pterafolk number_of_creatures)
    (display "Looming in the sky, ")
    (write number_of_creatures)
    (display " pterafolk watch
    the characters’ every move and wait until they blunder into
    danger. The next time a random encounter occurs, the
    pterafolk take advantage of the distraction and attack from the
    air, launching javelin attacks at wounded characters while
    staying out of melee. If they meet firm resistance, the pterafolk
    fly away, but they might regroup for a follow-up attack at your
    discretion.
    pterafolk: pg 229 Tomb of annihilation\n\n"))


;; (random 8) (+ 1 (random 4)) (+ 1 (random 6))
(define (rare_plants type d4 d6)
    (display "The players find one or more unusual plants found on
    pg 205 Tomb of annihilation
    ")
    (case type
        ((0) (write (+ d6 d6)) (display " dancing monkey fruit growing on a tree."))
        ((1) (display "A Menga bush with ") (write (+ d6 d6)) (display " ounces of leaves."))
        ((2) (write d4) (display " ryath roots growing in the ground."))
        ((3) (write (+ d6 d6)) (display " blue wild roots growing in a patch of sun."))
        ((4) (write (+ d6 d6 d6 d6)) (display " sinda berries growing on a bush."))
        ((5) (display "A Wukka tree with ") (write (+ d6 d6)) (display " wukka nuts."))
        ((6) (display "A pair of blue Yahcha beetles slowly moving around."))
        ((7) (write d6) (display " zabou growing on a dead tree.")))
    (display "\n\n"))


;; (+ 1 (random 6)) 
(define (red_wizard number_of_creatures)
    (display "This group consists of a Red Wizard (LE male or female
    Thayan human mage), ")
    (write number_of_creatures)
    (display " guards, and ")
    (write number_of_creatures)
    (display " skeletons, all
    answerable to Valindra Shadowmantle (see “Heart of Ubtao,”
    page 58). If this encounter occurs outside of Omu, these
    reinforcements are on their way to the city. If the encounter
    takes place in Omu, the Red Wizard is searching the city for
    shrines (pg 97, 101-109). The Thayans aren’t spoiling for a fight;
    if defeat seems inevitable, the Red Wizard surrenders and
    offers a crude map of Chult marking the regions occupied by
    undead (see map 2.1). The characters can use the map to steer
    clear of these regions.
        mage: pg 347 monster manual
        guard: pg 347 monster manual
        skeleton: pg 272 monster manual\n\n"))


(define (salamander)
    (salamander2 (+ 1 (random 6))))

;; (+ 1 (random 6))
(define (salamander2 number_of_creatures)
    (display "The characters see a salamander tending a nest of ")
    (write number_of_creatures)
    (display " fire
    snakes. The salamander has no interest in fighting and attacks
    only to protect itself and the snakes.
    salamander: pg 266 monster manual
    fire snakes: pg 265 monster manual\n\n"))


(define (sea_hag number_of_creatures)
    (display "The characters encounter three sea hags that comprise a
    coven. Their favorite trick is to pull a damaged or abandoned
    canoe onto a riverbank and pretend to be stranded or
    wounded explorers in need of rescue.
    If the characters defeat the sea hags and search the area then 
roll three times on the Treasure Drops table to determine
what, if anything, they find. whatever treasure the hags have is
stowed inside a rotted wooden chest.
    sea hag: pg 179 monster manual\n")
    (treasure (random 100))
    (treasure (random 100))
    (treasure (random 100)))


(define (shambling_mound)
    (display "Roll any die when a shambling mound encounter occurs. On
    an even result, the characters hear the creature trudging
    through the muck before it attacks them. On an odd result, the
    shambling mound lurks within a mass of vegetation, where it
    can be noticed by any character with a passive Wisdom
    (Perception) score of 15 or higher.
    shambling mound: pg 270 monster manual\n\n"))


(define (snake_constrictor)
    (display "A constrictor snake attacks a random party member from
    hiding. The character targeted by the snake is surprised unless
    he or she has a passive Wisdom (Perception) score of 12 or
    higher.
    boa constrictor: pg 320 monster manual\n\n"))


(define (giant_constrictor_snake)
    (display "A giant constrictor snake attacks a random party member from
    hiding. The character targeted by the snake is surprised unless
    he or she has a passive Wisdom (Perception) score of 12 or
    higher.
    giant boa constrictor: pg 324 monster manual\n\n"))


(define (giant_snake)
    (display "A giant poisonous snake shoots out from the undergrowth to
    attack a random character. The character targeted by the
    snake is surprised unless he or she has a passive Wisdom
    (Perception) score of 14 or higher.
    giant snake: pg 327 monster manual\n\n"))


;; (+ 1 (random 6))
(define (spider number_of_creatures)
    (display "Giant spider webs are easily concealed in Chult’s dense jungles
    and swamps. Any character with a passive Wisdom
    (Perception) score of 13 or higher spots the webs in time to
    alert the other characters to an encounter with ")
    (write number_of_creatures)
    (display " giant
    spiders. Otherwise, the spiders attack with surprise when the
    lead party member blunders into a sticky web and becomes
    grappled by it (escape DC 12). Hundreds of baby giant spiders
    crawl through the webs, but they are harmless. 
    giant spider: pg 328 monster manual\n\n"))


;; (random 4)
(define (statue_ubtao type)
    (display "Any character with a passive Wisdom (Perception) score of 12
    or higher spots a 10-foot-tall statue overgrown with vines. The
    statue depicts a stylized Chultan king — a representation of
    Ubtao.")
    (case type
        ((0) (display "Treasure lies at the foot of the statue, left there as
    tribute by some jungle creature.\n") (treasure (random 100)))
        ((1) (display "Goblin, grung, and su-monster (+ 2 (random 4)) skulls are piled around
    the statue’s base.\n"))
        ((2) (display "A glyph of warding is inscribed on the statue. To spot the
    glyph, a character searching the statue must succeed on a DC
    15 Intelligence (Investigation) check. If any creature touches
    the statue, the glyph erupts with magical energy in a 20-footradius
    sphere centered on the statue. Each creature in the area
    must succeed on a DC 14 Dexterity saving throw, taking 22
    (5d8) thunder damage on a failed save, or half as much
    damage on a successful one. The thunderous noise has a 75
    percent chance of attracting stirges or a troll lurking nearby.\n") (stirge (+ 2 (random 6))) (troll))
        ((3) (display "The statue has grooves carved into its stomach that form
    a maze. Any character who studies the grooves and succeeds
    on a DC 10 Intelligence check sees a clear pathway through the
    labyrinth. That character is then bestowed with the power to
    cast the find the path spell as an action, no components
    required, by tracing the same path with his or her finger. Once
    used, this ability goes away. Once the statue has granted this
    benefit, it can’t do so again until the next dawn.\n")))
    (display "nalfeshnee: pg 69 monster manual\n\n"))


;; (+ 2 (random 6))
(define (stirge number_of_creatures)
    (display "Chult is rich in caves, ruins, and hollow logs in which stirges
    can hide. By day, the characters disturb ")
    (write number_of_creatures)
    (display " stirges as they
    move through the jungle. At night, the same number of stirges
    descend on the party’s camp.
    stirge: pg 284 monster manual\n\n"))


;; (+ 2 (random 4))
(define (su-monster number_of_creatures)    
    (display "The party comes across ")
    (write number_of_creatures)
    (display " su-monsters. 
    The su-monsters approach cautiously, feigning curiosity. If
    allowed to get close, each su-monster uses its Psychic Crush in
    the hope of stunning an adventurer before attacking with its
    bite and claws. The su-monsters flee to their treetop lairs if the
    fight goes against them.
    The su-monsters might have treasure stashed in one of
    their trees.
    su-monster: pg 232 Tomb of annihilation\n")
    (treasure (random 100)))


;; (+ 1 (random 4))
(define (swarm_of_bats number_of_creatures)
    (display "Ruins, hollow trees, and hidden caverns can all be homes to
    bats. This encounter sees the characters disturb ")
    (write number_of_creatures)
    (display " swarms
    of bats that have become unnaturally aggressive from feeding
    on undead flesh.
    swarm of bats: pg 337 monster manual\n\n"))


;; (+ 1 (random 4))    
(define (swarm_of_insects number_of_creatures)
    (display "The characters are beset by ")
    (write number_of_creatures)
    (display " swarms of insects. 
    At the end of the encounter, any character
    damaged by a swarm must succeed on a DC 11 Constitution
    saving throw or become infected with shivering sickness (see
    “Diseases,” page 40).
    swarm of insects: pg 338 monster manual\n\n"))

;; (+ 1 (random 4))
(define (swarm_of_quippers number_of_creatures)
    (display "This encounter indicates that ")
    (write number_of_creatures)
    (display " swarms of quippers catch
    sight of the party, but these creatures are dangerous only if the
    characters are in the water with them. Creatures on the shore
    or in canoes are safe, but in both cases, the swarms follow the
    characters until they’re out of sight and away from the water.
    swarm of quippers: pg 338 monster manual\n\n"))


(define (tabaxi_hunter)
    (display "Any character with a passive Wisdom (Perception) score of 15
    or higher spots a tabaxi hunter watching
    the party from a vantage point 300 feet away. If the tabaxi
    goes unseen, it might shadow the characters for a while, then
    suddenly appear to help them fight off a tough encounter or
    warn them of danger in the vicinity.
    If this encounter takes place in Omu, see chapter 3 for
    more information on the tabaxi hunters found there.
    tabaxi hunter: pg 232 tomb of annihiation\n\n"))


(define (tiger)
    (display "A tiger lies in wait for the party but is noticed by any character
    whose passive Wisdom (Perception) score is 16 or higher. If
    not detected, the tiger pounces at a character who comes
    within 40 feet of it. The tiger retreats if it loses more than half
    its hit points.
    tiger: pg 339 monster manual\n\n"))

;; (random 100)
(define (treasure kind_found)
    (display "The party finds ")
    (case kind_found
        ((0) (display "a wand of secrets
        Wand, uncommon
        
        3 charges, 1 action to use, range: caster, effect: 30ft, duration: until trap/door found
        Automatically detect traps and secret doors
        It pulses at the nearest one to the weilder. 
        Regains 1d4 charges at dawn.\n\n"))
        ((51) (display "a partial map. 
        Reveal a location and a route to port Nyanzaru from said location.\n\n"))
        ((52) (display "a partial map. 
        Reveal a location and a route to port Nyanzaru from said location.\n\n"))
        ((53) (display "a pouch with ")
        (write (+ 1 (random 4))) 
        (display " gemstones inside. Each 10gp\n\n"))
        ((54) (display "a pouch with ")
        (write (+ 1 (random 4))) 
        (display " gemstones inside. Each 10gp\n\n"))
        ((55) (display "a malachite ring inlaid with electrum worth 50gp\n\n"))
        ((56) (display "a malachite ring inlaid with electrum worth 50gp\n\n"))
        ((57) (display "a clean vial with an engraving (5gp)\n\n"))
        ((58) (display "a clean vial with an engraving (5gp)\n\n"))
        ((59) (display "a quiver containing 10 silver arrows\n\n"))
        ((60) (display "a quiver containing 10 silver arrows\n\n"))
        ((61) (display "a gold nugget (50gp)\n\n"))
        ((62) (display "a gold nugget (50gp)\n\n"))
        ((63) (display "a vial of serpent venom
        Type: injury, price: 200gp per dose
        DC 11 Con save else take 3d6 poison dmg
        half dmg on sucess.\n\n"))
        ((64) (display "a vial of serpent venom
        Type: injury, price: 200gp per dose
        DC 11 Con save else take 3d6 poison dmg
        half dmg on sucess.\n\n"))
        ((65) (display "an ivory smoke pipe (25 gp)\n\n"))
        ((66) (display "an ivory smoke pipe (25 gp)\n\n"))
        ((67) (write (+ 2 (random 12) (random 12)))
        (display " blocks of insect repellent incense.\n\n"))
        ((68) (write (+ 2 (random 12) (random 12)))
        (display " blocks of insect repellent incense.\n\n"))
        ((69) (display "a iron triceratops head (show the party the ytepka society symbol)\n\n"))
        ((70) (display "a iron triceratops head (show the party the ytepka society symbol)\n\n"))
        ((71) (display "a potion of animal friendship.\n\n"))
        ((72) (display "a potion of animal friendship.\n\n"))
        ((73) (display "a potion of vitality\n\n"))
        ((74) (display "a potion of vitality\n\n"))
        ((75) (display "a scroll spell\n\n"))
        ((76) (display "a scroll spell\n\n"))
        ((77) (display "a vial of wyvern poison
	type: injury price: 1200 gp 
	DC 15 con save else take 7d6 poison dmg
	half dmg on sucess\n\n"))
        ((78) (display "a vial of wyvern poison
	type: injury price: 1200 gp 
	DC 15 con save else take 7d6 poison dmg
	half dmg on sucess\n\n"))
        ((79) (display "an obsidian knife (25gp)\n\n"))
        ((80) (display "an obsidian knife (25gp)\n\n"))
        ((81) (write (+ 1 (random 4)))
	(display " gemstones worth 50gp a peice.\n\n"))
        ((82) (write (+ 1 (random 4)))
	(display " gemstones worth 50gp a peice.\n\n"))
        ((83) (display "a small onyx orb with maze-like
 	patterns carved into its surface (75gp), 
	usable as an arcane or druidic focus.\n\n"))
        ((84) (display "a small onyx orb with maze-like
 	patterns carved into its surface (75gp), 
	usable as an arcane or druidic focus.\n\n"))
        ((85) (display "a batiri goblin (+ 5 (random 6) (random 6)) mask made of painted
	wood, set with nine gemstones (10 gp each)\n\n"))
        ((86) (display "a batiri goblin mask made of painted
	wood, set with nine gemstones (10 gp each)\n\n"))
        ((87) (display "a spyglass (1000 gp)\n\n"))
        ((88) (display "a spyglass (1000 gp)\n\n"))
        ((89) (display "a lute (35 gp)\n\n"))
        ((90) (display "a lute (35 gp)\n\n"))
        ((91) (display "a set of thieves' tools (25 gp)\n\n"))
        ((92) (display "a set of thieves' tools (25 gp)\n\n"))
        ((93) (display "an exellent padlock and key. DC 20 sleight of hand to pick\n\n"))
        ((94) (display "an exellent padlock and key. DC 20 sleight of hand to pick\n\n"))
        ((95) (display "a jar of Keoghtom's ointment\n\n"))
        ((96) (display "a jar of Keoghtom's ointment\n\n"))
        ((97) (display "a horn of blasting\n\n"))
        ((98) (display "a horn of blasting\n\n"))
        ((99)(display "The party finds a wand of secrets
        Wand, uncommon
        
        3 charges, 1 action to use, range: caster, effect: 30ft, duration: until trap/ door found
        Automatically detect traps and secret doors
        It pulses at the nearest one to the weilder. 
        Regains 1d4 charges at dawn.\n\n"))
        (else (display "nothing\n\n"))))


;; (+ 1 (random 4))
(define (tri-flower_frond number_of_creatures)
    (display "If this encounter occurs while the characters are traveling,
    they wander into a patch of ")
    (write number_of_creatures)
    (display " tri-flower fronds 
    which seem like ordinary plants until they strike.
    If the encounter occurs while the party is camped, the plants
    try to infiltrate the camp, anesthetize characters with their
    orange blossoms, then slay them with their yellow and red
    blossoms.
    tri-flower frond: pg 234 tomb of annihilation\n\n"))


(define (troll)
    (display "A hungry troll comes crashing out of the jungle, intent on
    eating the characters.
    troll: pg 291 monster manual\n\n"))


;; (+ 2 (random 6) (random 6))
(define (ghoul number_of_creatures)
    (display "Any character with a passive Wisdom (Perception)
    score of 12 or higher hears and smells a ghoul pack
    approaching, consisting of ")
    (write number_of_creatures)
    (display " ghouls led by a ghast. The
    ghast has a blue triangle tattooed on its forehead — an
    indicator that it once served Ras Nsi.
    ghoul: pg 148 monster manual
    ghast: pg 148 monster manual\n\n"))


;; (+ 3 (random 6) (random 6) (random 6))
(define (skeleton number_of_creatures)
    (display "The characters come across ")
    (write number_of_creatures)
    (display " skeletons. If
    the encounter occurs while the party is traveling, the skeletons
    are either lying on the ground or buried under it, ready to
    spring up when wayward explorers pass by. If the party is
    camped, the skeletons wander into the camp and attack.
    skeleton: pg 179 monster manual\n\n"))


(define (specter)
    (display "The evil remnant of a dead explorer has become a
    specter that attacks the party. The explorer’s body can be
    found with a successful DC 13 Wisdom (Survival) check. If the
    characters locate the body, see “Explorer, Dead” earlier in this
    section to determine what they find.
    specter: pg 279 monster manual\n\n")
    (dead_explorer (random 20)))


(define (wight)
    (display "The characters encounter a wight that has lurked
    in the Chultan jungle since before the Spellplague. It harbors
    an eternal hatred for Chultans and everything related to Ubtao.
    If the party includes any Chultans or any character wearing
    the holy symbol of Ubtao, the wight attacks those characters in
    preference to other targets.
    wight: pg 300 monster manual\n\n"))


;; (+ 1 (random 10)) (+ 3 (random 6) (random 6) (random 6)) (+ 1 (random 4))
(define (zombie type number_of_zombies d4)
    (display "Characters catch the scent of death on the air and
    hear the undead lumbering through the jungle.\n")
    (cond 
        ((< type 4) (write number_of_zombies) (display " zombies attack
    zombie: pg 316 monster manual"))
        ((and (> type 3) (< type 6)) (display "an ankylosaurus zombie aproaches
    ankylosaurus zombie: pg 240 Tomb of Annihilation"))
        ((and (> type 5) (< type 8)) (write d4) (display " girallon zombies swing in.
        girallon zombie: pg 240 Tomb of Annihilation"))
        ((equal? type 10) (display "A tyrannosaurus zombie shakes the undergrowth as it aproaches
        tyrannosaurus zombie: pg 241 Tomb of Annihilation"))
        (else (write d4)(display " ogre zombies lumber forward.
        ogre zombie: pg 316 monster manual")))
    (display "\n\n"))


;; (+ 1 (random 4))
(define (vegepygmie number_of_creatures)
    (display "The characters cross paths with ")
    (write number_of_creatures)
    (display " vegepygmies, each one
    mounted on a thorny. These
    vegepygmy hunters have wandered far from their tribe. They
    flee if outnumbered; otherwise, they attack.
    vegepygmie: pg 234 tomb of annihilaion\n\n"))


(define (wereboar)
    (display "A wereboar masquerading as a Chultan priest takes a dim
    view of explorers encroaching on its territory and demands
    that the characters turn back. Around its neck, it wears a
    wooden holy symbol of Ubtao (a labyrinthine pattern carved
    into a circular disk). The wereboar might be guarding a shrine
    to Ubtao, a grove of wukka trees (see appendix C), or a cave it
    uses as a lair. The creature might also have treasure in its lair;
    roll three times on the Treasure Drops table to determine
    what, if anything, a search of the wereboar’s lair yields.
    wereboar: pg 209 monster manual\n\n")
    (treasure (random 100))
    (treasure (random 100))
    (treasure (random 100)))


(define (weretiger)
    (display "A weretiger in human form offers to escort the party through
    a particularly dangerous stretch of wilderness. It has no
    ulterior motive and doesn’t ask for payment. If the characters
    accept its assistance, they have no hostile random encounters
    while the weretiger is with them. It leaves after accompanying
    the party for 24 hours or when it reaches a location it does not
    wish to explore, including Omu, Nangalore, or Orolunga.
    weretiger: pg 210 monster manual\n\n"))


(define (winterscape)
    (display "The characters stumble into a wondrous sight: a 120-footradius
    sphere of winter weather. To drive off some monsters,
    Artus Cimber (see appendix D) created the sphere using the
    Ring of Winter. All plants and surfaces within the sphere are
    covered with glittering ice and frost, and the temperature
    within the sphere is a biting −30 degrees Fahrenheit. The
    effect was created by an artifact and can’t be dispelled"))


;; (+ 3 (random 6) (random 6) (random 6)) 
(define (yellow_musk_creeper_and_zombies number_of_creatures)
    (display "The characters pass close to a ruin inhabited by ")
    (write number_of_creatures)
    (display " yellow
    musk zombies. The zombies might be
    spread across the area or bunched together, depending on the
    terrain. In the heart of the ruin, a yellow musk creeper
    clings to a crumbling archway, statue, or polluted well.

    If this encounter occurs while the party is camped, the
    zombies emerge from a nearby ruin to attack the camp and
    attempt to knock characters unconscious. They then drag
    those characters back to the yellow musk creeper.
    yellow musk zombie: pg 237 Tomb of annihilation
    yellow musk creeper: pg 237 Tomb of annihilation\n\n"))


;; (+ 2 (random 6)) (+ 1 (random 4))
(define (yuan-ti number_of_creatures number_of_malisons)
    (display "Yuan-ti patrols consist of ")
    (write number_of_creatures)
    (display " yuan-ti purebloods, which
    keep their distance as they try to gather information to take
    back to Ras Nsi. The yuan-ti are camouflaged, but any
    character who succeeds on a DC 13 Wisdom (Perception)
    check made with disadvantage catches sight of the serpent folk
    as they withdraw.
    If this encounter occurs within 25 miles of Omu, the patrol
    is instead made up of ")
    (write number_of_malisons)
    (display " yuan-ti malisons (type 1), and it
    takes a DC 14 Wisdom (Perception) check to spot them. If the
    characters chase after the malisons, they transform into
    snakes and vanish into the jungle.
    Yuan-ti pureblood: pg 310 monster manual
    Yuan-ti malisons: pg 309 monster manual\n\n"))


;; (+ 1 (random 6)) (+ 2 (random 6) (random 6))
(define (zhentarim number_of_tribals number_of_thugs)
    (display "A Zhent assassin with a flying snake pet leads a priest, ")
    (write number_of_thugs)
    (display " 
    thugs, and ")
    (write number_of_tribals)
    (display " tribal warriors through the wilderness in
    search of Artus Cimber and the Ring of Winter. If Artus is with
    the characters, the Zhents demand the ring and attack if they
    don’t receive it quickly. Otherwise, they show little interest in
    the characters.
    assassin: pg 343 monster manual
    flying snake: pg 322 monster manual
    priest: pg 348 monster manual
    thug: pg 350 monster manual
    tribal warrior: pg 350 monster manual\n\n")
    (treasure (random 100)))


;; (+ 2 (random 6) (random 6))
(define (zorbos number_of_creatures)
    (display "If this encounter occurs while the characters are traveling,
    they spot ")
    (write number_of_creatures)
    (display " zorbos in wukka trees (see
    appendix C). The creatures growl and bare their teeth if any
    characters approach them. If the characters act in a
    threatening manner, the zorbos attack.
    If this encounter occurs while the party is camped, the
    hungry zorbos drop from the surrounding trees and attack.
    zorbo: pg 241 Tomb of annihilation\n\n"))