(def running true)

;; # baboons in simulation
(def num-baboons 30)

;; amount of time baboons sleep (+random)
(def baboon-sleep  1000)

;; maximum # of baboons that rope can support (in one direction)
(def rope-capacity 5)

;; rope, east-side, and west-side are all sets of baboons.
;; a baboon can only be in one place at a time!
(def rope (ref #{})) ; can also have "draining" metadata
(def east-side (ref #{}))
(def west-side (ref #{}))

;; baboons have :id identifier,
;; :loc is :west, :east, or :rope
(defstruct baboon :id :loc)

(defn create-baboons
  "Create a sequence of baboon agents with random headings.
   Agent state is an instance of the baboon struct."
  [n]
  (reduce (fn [s x]
	    (let [side     ([:west :east] (rand-int 2))
		  side-ref (if (= side :west) west-side east-side)
		  agt      (agent (struct baboon x side))]
	      (dosync (alter side-ref conj @agt))
	      (conj s agt)))
          []
          (range n)))

;; our baboons!
(def baboons (create-baboons num-baboons))

(defn side-ref [name]
  (if (= :west name) west-side east-side))

(defn reverse-heading
  "Returns reverse direction heading -- :east for :west, and v.v."
  [heading]
  (if (= heading :west) :east :west))

(defn behave 
  "Defines baboon behavior.
   Baboons will:
    - sleep for a while
    - if currently on the rope, jump off
    - else, try to get on the rope"
  [baboon]
  (Thread/sleep (+ baboon-sleep (rand-int baboon-sleep)))
  (dosync
    (when running
      (send-off *agent* behave))
    (let [from (:loc baboon)
	  to   (reverse-heading from)
	  n-crossing (count @rope)]
      (cond
       ;; if I'm on the rope, get off & switch locations
       (contains? @rope baboon)
       (do (alter rope disj baboon)
	   (alter (side-ref to) conj (assoc baboon :loc to))
	   (assoc baboon :loc to))
       ;; if the rope is empty, or everyone on the rope is from my side
       ;; and the rope isn't at capacity, get on
       (and (< n-crossing rope-capacity)
	    (every? #(= (:loc %) from) @rope))
       (do (alter rope conj baboon)
	   (alter (side-ref from) disj baboon)
	   baboon)
       ;; otherwise, nothing changes
       :else
       baboon))))

(defn behave-with-drain [baboon]
  "Defines baboon behavior.
   Improves the default by checking to see if the rope is saturated with
   other baboons from my side. If so, tell the rope to enter a draining mode,
   where nobody is allowed on until everyone's off."
  (Thread/sleep (+ baboon-sleep (rand-int baboon-sleep)))
  (dosync
    (when running
      (send-off *agent* behave-with-drain))
    (let [from (:loc baboon)
	  to   (reverse-heading from)
	  n-crossing (count @rope)]
      ;; if there's nobody on the rope, no need to drain it
      (if (= 0 n-crossing)
	(alter-meta! rope assoc :draining false))
      (cond
       ;; if I'm on the rope, get off & switch locations
       (contains? @rope baboon)
       (do (alter rope disj baboon)
	   (alter (side-ref to) conj (assoc baboon :loc to))
	   (assoc baboon :loc to))
       ;; if the rope is not draining, and I can get on it, get on
       (and (not (:draining (meta rope)))
	    (< n-crossing rope-capacity)
	    (every? #(= (:loc %) from) @rope))
       (do (alter rope conj baboon)
	   (if (= (count @rope) rope-capacity)
	     (alter-meta! rope assoc :draining true))
	   (alter (side-ref from) disj baboon)
	   baboon)
       ;; otherwise, wait
       :else
       baboon))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI stuff follows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

; width of baboon representation on screen
(def scale 20)

;; height and width of frame
(def height (* num-baboons scale))
(def width 400)

;; animation refresh rate
(def animation-sleep-ms 100)

(defn draw-centered-string [s ^Graphics g x y w h]
  (let [fm (.getFontMetrics g)
        b (.getStringBounds fm s g)
        dx (int (/ (- w (.getWidth b)) 2))
        dy (int (+ (.getHeight b) (/ (- h (.getHeight b) 2))))]
    (.drawString g s (+ x dx) (+ y dy))))

;; draw a single baboon -- color based on heading
(defn render-baboon [baboon ^Graphics g x y]
  (doto g
    (.setColor (if (= :west (:loc baboon)) 
                 (new Color 0x404040)
                 (new Color 0x930c08)))
    (.fill3DRect x y scale scale true)
    (.setColor Color/white))
  (draw-centered-string (str (:id baboon)) g x y scale scale))

;; draw an arrow pointing in the given direction
(defn render-direction [dir ^Graphics g x y]
  (let [x-left x
        x-right (+ x (* rope-capacity scale))
        y (+ y 5)]
    (.setColor g Color/black)
    (.drawLine g x-left y x-right y)
    (if (= dir :east)
      (doto g
        (.drawLine x-right y (- x-right 7) (- y 4))
        (.drawLine x-right y (- x-right 7) (+ y 4)))
      (doto g
        (.drawLine x-left y (+ x-left 7) (- y 4))
        (.drawLine x-left y (+ x-left 7) (+ y 4))))))

;; draw the "rope", including the baboons on it, their common heading,
;; and whether the rope is being drained
(defn render-rope [rope ^Graphics g x y]
  (when (:draining (meta rope))
    (.setColor g Color/black)
    (draw-centered-string "(draining...)" 
                          g x (+ y 30) 
                          (* scale rope-capacity) scale))
  (when-let [heading (reverse-heading (:loc (first rope)))]
    (render-direction heading g x (- y 18))
    (reduce (fn [baboons offset] 
              (render-baboon (first baboons) g (+ x offset) y)
              (rest baboons))
            rope
            (if (= heading :west)
              (range 0 (* scale (count rope)) scale)
              (range (* (- rope-capacity 1) scale) 
                     (* (- rope-capacity (count rope) 1) scale) 
                     (- scale))))))

;; render the entire scene, consisting of all baboons (on and off the rope)
(defn render [g]
  (let [img (new BufferedImage width height BufferedImage/TYPE_INT_ARGB)
        bg (.getGraphics img)]
    (doto bg
      (.setColor (new Color 0xdddddd))
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    ; take a snapshot of the world before running the let body, to ensure
    ; we're drawing a consistent state, and we're not stopping everyone
    ; while painting ...
    (let [[west-side east-side rope rope-meta]
          (dosync
	   [@west-side @east-side @rope (meta rope)])]
      (dorun
       (map #(render-baboon % bg 0 (* (:id %) scale)) west-side))
      (dorun
       (map #(render-baboon % bg (- width scale) (* (:id %) scale)) east-side))
      (when-not (empty? rope)
	(render-rope (with-meta rope rope-meta)
		     bg 
		     (int (/ (- width (* rope-capacity scale)) 2))
		     (int (/ (- height scale) 2)))))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel (doto 
             (proxy [JPanel] []
               (paint [g] (render g)))
             (.setPreferredSize (new Dimension width height))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(def animator (agent nil))

(defn animation [x]
  (when running
    (send-off *agent* animation))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  nil)

;(send-off animator animation)

;(dorun (map #(send-off % behave) baboons))

;(dorun (map #(send-off % behave-with-drain) baboons))