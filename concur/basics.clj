;; in a more interesting example we could make this a 
;; multiplayer game -- the world would be a 2D matrix of locations,
;; player agents would contain their own score, and the game state
;; might consist of a global scoreboard ... the atom example might
;; be a "flag holder"?

(def current-weapon (atom {:name "RPG" :damage 25}))

(deref current-weapon) ; returns the current value

@current-weapon ; => {:name "RPG", :damage 25}

;; reset! changes the current value of the atom
(reset! current-weapon {:name "Shotgun" :damage 5})

;; swap! applies a function to the current atom value to get its new value
(swap! current-weapon assoc :damage 30)

(def game-state (ref {:points 0}))

@game-state ; => {:points 0}

(defn add-points [n]
  (dosync
    (when (not (:won @game-state))
      (let [newpts (+ n (:points @game-state))]
        (alter game-state assoc :points newpts)
        (if (>= newpts 100)
          (alter game-state assoc :won true))))))

(add-points 10)

@game-state ; => {:points 10}

(add-points 90) ; => {:points 100, :won true}

(defn add-achievement 
  ([k v] (dosync (commute game-state merge {k v})))
  ([a] (dosync (commute game-state 
                        (partial merge-with concat) 
                        {:achievements [a]}))))

(add-achievement :killed-boss "You killed the boss!")

@game-state ; => {:killed-boss "You killed the boss!", :points 100, etc.}

(add-achievement "Level 1 beat!")
(add-achievement "Level 2 beat!")

@game-state ; => {:achievements ("Level 1 beat!" "Level 2 beat!"), etc.}

(def player (agent {:pos [0 0] :health 100}))

@player

(defn do-battle [[x y]] 5) ;; placeholder for battle logic

(defn move-player [player [dx dy]]
  (let [[nx ny] (map + [dx dy] (:pos player))
        damage (do-battle [nx ny])
        nhealth (- (:health player) damage)]
    (if (<= nhealth 0)
      (dosync (alter game-state assoc :game-over true)))
    (assoc player :pos [nx ny] :health nhealth)))

(send player move-player [1 1]) ; => nil (immediate return)

@player ; (sometime later ...) => {:pos [1 1], :health 95}

(dotimes [n 19] (send player move-player [1 1])) ; => nil (immediate return)

@player ; (sometime later ...) => {:pos [20, 20], :health 0}
@game-state ; => {:game-over true, etc.}
