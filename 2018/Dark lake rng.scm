;; Out of the abyss rng.scm
;;
;; by Nathan Pelletier
;;
;; places a random encounter on each tile in D&D out of the abyss map

(define (fungi fungi_found)
    (case fungi_found
        ((0) (nightlight (+ 1 (random 4))))
        ((10) (nilhoggs_nose (+ 1 (random 4))))
        ((20) (ormu (+ 1 (random 4))))
        ((30) (timmask (+ 1 (random 4))))
        ((40) (tongue_of_madness (+ 1 (random 4))))
        ((50) (torchstalk (+ 1 (random 4))))
        ((60) (grove_of_mushrooms))
        (else 
            (cond 
                ((< fungi_found 10) (barrel_stalk (+ 1 (random 10))))
                ((and
                    (> fungi_found 10)
                    (< fungi_found 20)) 
                        (blue_cap (+ 1 (random 10))))
                ((and
                    (> fungi_found 20)
                    (< fungi_found 30)) 
                        (fire_lichen (+ 1 (random 10))))
                ((and
                    (> fungi_found 30)
                    (< fungi_found 40)) 
                        (ripplebark (+ 1 (random 10))))
                ((and
                    (> fungi_found 40)
                    (< fungi_found 50)) 
                        (trillimac (+ 1 (random 10))))
                ((and
                    (> fungi_found 50)
                    (< fungi_found 60)) 
                        (waterorb (+ 1 (random 10))))
                ((and
                    (> fungi_found 60)
                    (< fungi_found 70) 
                        (zurkhwood (+ 1 (random 10)))))))))

;;////////////////////////////////////////////////////
;; dark_lake
;; do once a adventure day, don't do on special days
(define (dark_lake)
    (dark_lake2 (random 20)))

(define (dark_lake2 encounter_type)
    (cond
        ((< encounter_type 13) 
            (display "Nothing of note today.\n\n"));none
        ((or (equal? 13 encounter_type) (equal? 14 encounter_type))
            (dark_lake_locations (random 10)));terrain
        ((or (equal? 15 encounter_type) (equal? 16 encounter_type))
            (dark_lake_creatures (random 12)));creature
        (else 
            (dark_lake_locations (random 10)) 
            (dark_lake_creatures (random 12)))));both


;;////////////////////////////////////////////////////////////////
;; dark_lake_locations(int)
;;
;; takes a number between 0 - 9 and prints out a location for an event

(define (dark_lake_locations generated_number)
    (case generated_number 
        ((1) (collision))
        ((2) (falls_or_locks (random 2)))
        ((3) (island))
        ((4) (low_celing))
        ((5) (rockfall (+ 1 (random 6))))
        ((6) (rough_current (+(random 7) (random 7))))
        ((7) (run_aground (random 2)))
        ((8) (stone_teeth 
            (+ (random 7) (random 7) (random 7) (random 7) (random 7) (random 7))))
        ((9) (tight_passage))
        (else (whirlpool))))


;;/////////
;; terain//
(define (collision)
    (rough_current (+ (random 7) (random 7))) 
    (display "At the last moment your group notices a large object.
    DC 13 group Dexterity check 
    On fail ") 
    (write (+ (random 10) (random 10))) 
    (display " blugeoning to vessel and 
    DC 10 strength or dex save\n\n"))

(define (falls_or_locks coin_flip)
    (cond 
        ((equal? coin_flip 1) (falls))
        (else (lock))))

(define (falls)
    (display "Passive wisdom 12 to hear waterfall
    Group DC 14 strength(athletics) to prevent going over
    On fail fall ") 
    (write (* 10 (+ 1 (random 6))))
    (display " feet into a pool of water ") 
    (write (* 5 (+ 1 (random 6)))) 
    (display " feet deep \n DC 11 dex save or take 1d6 bludg per 10 feet\n\n"))

(define (lock)
    (display "You find a duergar boat lock, it goes up and down waterfalls.
    DC 11 Intellegence(investigation) check
    once active takes an hour to raise and lower.\n\n"))

(define (island)
    (display "The party discovers a safe place for a long rest")
    (is_fungi (random 2)))

(define (is_fungi coin_flip)
    (cond 
        ((equal? coin_flip 0) (display " containing various fungi\n\n")
	(fungi (random 100))
	(fungi (random 100)))
        (else (display " it is baren.\n\n"))))

(define (low_celing)
    (display "The party meets a low celing. Pending on speed either
        a) get off boat and swim until clear
        b) DC 10 (+5 per mph) dex save or take 1d4 bludge * mph\n\n"))

(define (rockfall number_of_rocks)
    (write number_of_rocks)
    (display " rocks fall from the celing
        DC 12 Dex save
        else take 2d6 bludge per rock\n\n"))

(define (rough_current damage_to_boat)
    (display "You hit a rough current,
    DC 13 Dex group check
    else boat takes ")
    (write damage_to_boat)
    (display " and all passangers must pass DC 10 Str or Dex saves or be bucked off\n\n"))

(define (run_aground do_monsters_attack)
    (display "Either:
    a) Your boat is beached DC 10 group str(athletics)
    b) A party memeber catches their foot in the shallows and begins to drown
        DC 14 dex(sleight of hand)")
    (cond
        ((equal? 0 do_monsters_attack) 
            (display "\n while you fight to get free your presence is noticed.\n\n")
            (dark_lake_creatures (random 12)))
        (else (display "\n\n"))))

(define (stone_teeth tanking_stone)
    (rough_current (+ (random 7) (random 7)))
    (display "Stalagmites are below the surface
    scout makes DC 13 Wis(perception) check
    if success then DC 12 group Dex check to navigate
    else take ")
    (write tanking_stone)
    (display " damage to boat or split amoung party\n\n"))

(define (tight_passage)
    (display "The party works through a single file passage
    which takes time to navigate and push through. Their efforts 
    attract the attention of others.\n\n")
    (dark_lake_creatures (random 12)))

(define (whirlpool)
    (rough_current (+ (random 7) (random 7)))
    (display "The party encounters a vortex\n\n"))


#|////////////////////////////////////////////////////////////
# dark_lake_creatures(int)
# 
# takes numbers 0 - 11 and prints out a corrisponding monster
|#
(define (dark_lake_creatures encounter_type)
    (case encounter_type
        ((1) (merfolk 1))
        ((2) (aquatic_troll 1))
        ((3) (darkmantles (+ 2 (random 4) (random 4))))
        ((4) (duergar (+ 3 (random 4))))
        ((5) (green_hag 1))
        ((6) (grell 1))
        ((7) (ixitxachitl (+ 3 (random 6))))
        ((8) (kuo_toa (+ 1 (random 4))))
        ((9) (merrow (+ 1 (random 4))))
        ((10) (quipper_swarm 1))
        ((11) (water_weird 1))))


;;/////////////
;;//CREATURES//
(define (merfolk how_many)
    (write how_many)
    (display " Merfolk \nSpeed: 10ft, swim 40ft\nhp: ")
    (monster_hp (+ 4 (random 8) (random 8)))
    (display " AC: 11 \nStr: +0 Dex: +1 Con: +1 Int: +0 Wis: +0 Cha: +1")
    (display "\nperception: +2 Languages: Aquan, Common
      spear: 2|0 d8 piercing (thrown 20/60 ft)\nChallenge: 1/8(25XP)\n\n"))

(define (aquatic_troll how_many)
    (write how_many)
    (display " Aquatic troll \nSpeed: 30ft, swim 30ft\nhp: ")
    (monster_hp 
        (+ 48 
            (random 10) (random 10) (random 10) (random 10) 
            (random 10) (random 10) (random 10) (random 10)))
    (display " AC: 15 \nStr: +4 Dex: +1 Con: +5 Int: -2 Wis: -1 Cha: -2")
    (display "\nperception: +2, Darkvision 60ft Languages: Giant 
    Keen smell: advantage to smell \nRegeneration: +10 hp unless fire or acid
      multiattack: 3 attacks each turn \nBite: 7|4 1d6 piercing
      claw: 7|4 2d6 slash \nChallenge: 5(1800XP)\n\n"))

(define (darkmantles how_many)
    (write how_many)
    (display " Darkmantle \nSpeed: 10ft, fly 30ft\nhp: ")
    (monster_hp (+ 10 (random 6) (random 6) (random 6) (random 6) (random 6)))
    (display " AC: 11 \nStr: +3 Dex: +1 Con: +1 Int: -4 Wis: +0 Cha: -3")
    (display "\nstealth: +3 (looks like roof) Echolocation 60ft
      crush: 5|3 d6 bludgeoning, adv on medium or smaller creatures, attaches to head causing blind and suffocation
      DC 13 strength to disloge 
      Darkness Aura(1/day) 15ft radius darkness that follows, 10 min concentration
    Challenge: 1/2(100XP)\n\n"))

(define (duergar how_many)
    (write how_many)
    (display " Duergar \nSpeed: 25ft\nhp: ")
    (monster_hp (+ 8 (random 8) (random 8) (random 8) (random 8)))
    (display " AC: 16 (Scale Mail, Shield) 
    Str: +2 Dex: +0 Con: +2 Int: +0 Wis: +0 Cha: -1
    resist poison, charm, paralyzed
    Darkvision 120ft
    Languages: Dwarvish, Undercommon
    advantage against poison, spell and illusion saving throws
    problems seeing in bright light
      Enlarge:(recharge on rest) double damage dice on str weapons, adv on str check and save
      War Pick: 4|2 d8 piercing
      Javelin: 4|2 d6 piercing, thrown(30/120ft)
      Invisibility(recharge on rest):cons 1 hour
    Challenge: 1 (200 XP) \n\n"))

(define (green_hag how_many)
    (display "Consult green hag on pg 43\n")
    (write how_many)
    (display " Green Hag \nSpeed: 30ft\nhp: ")
    (monster_hp (+ 44 
        (random 8) (random 8) (random 8) (random 8) (random 8) (random 8)
        (random 8) (random 8) (random 8) (random 8) (random 8)))
    (display " AC: 17 (Natural Armor) DC: 13 
    Str: +4 Dex: +1 Con: +3 Int: +1 Wis: +2 Cha: +2
    Arcana +3, Deception +4, Perception +4, Stealth +3
    Darkvision 60ft
    Languages: Common, Draconic, Sylvan
    Breaths air and water
    Mimics sounds: DC 14 wis(insight)
      Cantrips: dancing lights, minor illusion, vicious mockery 2|0 1d4 60ft
      Claws: 6|4 2d8 slash
      looks like another huminoid of size, unless touched DC 20 int(investigation)
      Invisibility
      ONLY WHEN 3 ARE PRESENT AND SHARED 5|3
        1st 4 slots: identify, ray of sickness
        2nd 3 slots: hold person, locate object
        3rd 3 slots: bestow curse, counterspell, lightning bolt
        4th 3 slots: phantasmal killer, polymorph
        5th 2 slots: contact other plane, scrying
        6th 1 slot: eye bite
        Hag eye: hag can see through it, AC 10, HP: 1, darkvision 60ft
          if ever destroyed then hag takes 3d10 psychic and blind for 24h
    Challenge: 3 (700 XP) \n\n"))

(define (grell how_many)
    (write how_many)
    (display " Grell \nSpeed: 10ft, hover 30ft\nhp: ")
    (monster_hp (+ 20 
        (random 8) (random 8) (random 8) (random 8) (random 8)
        (random 8) (random 8) (random 8) (random 8) (random 8)))
    (display " AC: 12 
    Str: +2 Dex: +4 Con: +1 Int: +1 Wis: +0 Cha: -1
    Perception +4, Stealth +6
    immune to lightning, blind, prone
    blindsight 60ft
    Languages: Grell
      multiattack: Two attacks
      tentacles: 4|2 d10 piercing, reach, grapple DC 15, DC 11 con save or paralyzed
      beak: 4|2 2d4 piercing
    Challenge: 3 (700 XP) \n\n"))

(define (ixitxachitl how_many)
    (write how_many)
    (display " Ixitxachitl \nSpeed: swim 30ft\nhp: ")
    (monster_hp (+ 8 (random 6) (random 6) (random 6) (random 6)))
    (display " AC: 15 (natural armour) 
    Str: +1 Dex: +3 Con: +1 Int: +1 Wis: +1 Cha: -2
    perception +3, stealth +4
    Darkvision 60ft
    Languages: Abyssal, Ixitxachitl
      Bite: 3|1 d6 piercing
      Sting: 5|3 d8 piercing, poison
    Challenge: 1 (200 XP) \n\n"))

(define (kuo_toa how_many)
    (display "Boat pg 43\n")
    (write how_many)
    (display " Kuo Toa \nSpeed: 30ft swim 30ft\nhp: ")
    (monster_hp (+ 4 (random 8) (random 8) (random 8) (random 8)))
    (display " AC: 13 (natural armour, shield) 
    Str: +1 Dex: +0 Con: +0 Int: +0 Wis: +0 Cha: -1
    perception +4
    Darkvision 120ft
    Languages: Undercommon
    Amphibious: air and water
    Can spot invisible or ethereal plane beings 30ft away
    Slippery: advantage to break grapple
    Problems seeing in sun
      Bite: 3|1 d4 piercing
      Spear: 3|1 d8 piercing, thrown(20/60ft)
      Net: 3|0 (5/15ft) DC 10 str check to free
      Sticky Shield: missing the Kuo Toa with melee grants DC 11 Str save
        else weapon is taken by sheild
    Challenge: 1/4 (50 XP) \n\n"))

(define (merrow how_many)
    (display "In Abyssal \"Blood and salt for the Prince of Demons!\" \n")
    (write how_many)
    (display " Merrow \nSpeed: 10ft swim 40ft\nhp: ")
    (monster_hp (+ 18 
        (random 10) (random 10) (random 10) 
        (random 10) (random 10) (random 10)))
    (display " AC: 13 (natural armour) 
    Str: +4 Dex: +0 Con: +2 Int: -1 Wis: +0 Cha: -1
    Darkvision 60ft
    Languages: Abyssal, Aquan
    Amphibious: air and water
      Bite: 6|4 d8 piercing
      Claws: 6|4 2d4 slash
      Harpoon: 6|4 2d6 piercing, thrown(20/60ft)
    Challenge: 2 (450 XP) \n\n"))

(define (quipper_swarm how_many)
    (display "A swarm approaches, if everyone is at perfect hp and in boat 
    they follow for an hour before losing intrest\n")
    (write how_many)
    (display " quipper swarm
    Speed: swim 40ft\nhp: ")
    (monster_hp (+ 
        (random 8) (random 8) (random 8) (random 8)
        (random 8) (random 8) (random 8) (random 8)))
    (display " AC: 13  
    Str: +1 Dex: +3 Con: -1 Int: -5 Wis: -2 Cha: -4
    Resist physical
    Immune to: charm, fright, grapple, paralyzed, prone, restrain, stun
    Darkvision 60ft
    Blood frenzy: advantage on hurt oponents
    Only breaths water
      Bite(above half hp): 5|0 4d6 piercing
      Bite(below half hp): 5|0 2d6 piercing
      Bite(3 or less hp): 5|0 d1 piercing
    Challenge: 1 (200 XP) \n\n"))

(define (water_weird how_many)
    (display "After fight consult page 43")
    (write how_many)
    (display " Water Weird \nSpeed: swim 60ft\nhp: ")
    (monster_hp (+ 18 
        (random 10) (random 10) (random 10) 
        (random 10) (random 10) (random 10)
        (random 10) (random 10) (random 10)))
    (display " AC: 13  
    Str: +3 Dex: +3 Con: +1 Int: +0 Wis: +0 Cha: +0
    Resist: physical, fire
    Immune: poison, exhaustion, grapple, paralyze, restrain, prone, unconscious
    Blindsight 30ft
    Languages: Aquan(doesn't speak)
    Invisible in water
    Dies if leaves water
      Constrict: 5|3 3d6, reach, DC 13, if grappled then pulled 5ft and restrained
    Challenge: 3 (700 XP) \n\n"))


;;/////////
;;//Fungi//
(define (barrel_stalk number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " barrel stalks are in the enviroment. \n") 
            (display "A house sized mushroom that can: be tapped for 1d4 * 16 cups\n")
            (display "of water be used for 1d6 + 4 rations\n\n"))))

(define (blue_cap number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " blue caps are in the enviroment.
    A grasslike mushroom that:
    grows spores on organic substances 
    can be kept alive to harvest spores for flour
    are valuable to towns, 5 sp a dusting of spores once a day\n\n"))))

(define (fire_lichen number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " fire lichen are in the enviroment.
    A pale orange-white mushroom of normal size that: 
    is present near geothermal vents or rot
    can be sold for 15 gp
    can be ground into a spicy spice
    can be fermented and transformed into fireball whiskey\n\n"))))


(define (ripplebark number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " ripplebark are in the enviroment.
    A shelf like mushroom that:
    looks and smells like rotting flesh
    tastes amazing and heals 1d4 + 6 hp\n\n"))))

(define (trillimac number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " trillimac are in the enviroment.
    A broad green-grey cap and light grey stalk mushroom that:
    grows 4 to 5 feet tall
    leathery surface can be used for paper
    can be soaked in water for an hour and dried
    once preped it can be eaten restoring 1d4 spell slots of all levels
    \n\n"))))

(define (waterorb number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " waterorb are in the enviroment.
    A spongelike fungi that:
    grows in shallow water
    can be wrung for 16 cups of water
    is poisionous to eat"))))

(define (zurkhwood number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " zurkhwood are in the enviroment.
    A 30ft-40ft tall mushroom that:
    emits large spores that grow 1d20 inches on clothing per day
    is a source of timber\n\n"))))

(define (nightlight number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " nightlight are in the enviroment.
    A 1d6 + 4ft tall and tube shaped bioluminecent mushroom that:
    emits 15 ft of bright light and 15ft of dim
    can be touched to turn on or off
    must be potted to continue to produce light\n\n"))))

(define (nilhoggs_nose number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " nilhoggs nose are in the enviroment.
    A small simple mushroom that:
    after eating grants enhanced smell
    grants advantage on perception checks, smell wise for 1d4 hours
    requires constutution saves for all strong smells\n\n"))))

(define (ormu number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " ormu are in the enviroment.
    A bioluminecent green moss that:
    grows in damp areas
    emits 5ft of dim light
    can be dried and applied to clothing or tools\n\n"))))

(define (timmask number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " timmask are in the enviroment.
    A orange and red striped two foot tall toadstool with beige cap that:
    is know to locals as devils mushroom
    uproot or destroy causes 15ft of poisonous spores
    DC 14 Con save else poisoned and under confusion spell for 1 min\n\n"))))

(define (tongue_of_madness number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " tongue of madness are in the enviroment.
    A fungus that:
    looks like a human tounge
    provides 1d12 + 4 rations
    DC 12 Con save on eat else must speak mind for hour\n\n"))))

(define (torchstalk number_found)
    (cond 
        ((equal? 0 number_found))
        (else 
            (write number_found)
            (display " torchstalk are in the enviroment. \n") 
            (display "A one to two foot tall mushroom with:    
    a cumbustable cap that:
        burns for 24 hours once lit
        1d6 (if roll 1) it exploads causing 10ft radius of 5d6 fire dmg \n\n"))))


(define (monster_hp input)
    (write input))