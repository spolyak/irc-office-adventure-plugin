(ns sexpbot.plugins.adventure
  (:use [sexpbot registry]))

;;; office adventure!

(def objects '(laptop bug-report bozo beer tps-report))

(defn spel-print [list] (map (fn [x] (symbol (name x))) list))

(def game-map (hash-map
               'office-room '(("you are in a large office room with poor lighting. there are a few developers sitting here, typing away. " )
                              (west door room2)
                              (east door room6))
               'room1  '(("you are standing in front of your bosses office door to the east. it is closed. you hear a muffled conversation and decide to not disturb him." )
                         (north hallway room2))
               'room2  '(("you are in a narrow hallway of cubicles. you can see sunlight over the top. there is a poster on the wall of your boss. " )
                         (south hallway room1)
                         (east door office-room)
                         (north hallway room3))
               'room3  '(("you begin to see more sunlight amongst the cubes and hear the whirring of a printer at work." )
                         (south hallway room1)
                         (east hallway room4))
               'room4  '(("you are standing in front of a large printer that is vigorously spitting out copies of TPS reports." )
                         (west hallway room3)
                         (east hallway room5))
               'room5  '(("you move along the cube maze, the sounds of printing are off to the west hallway." )
                         (west hallway room4)
                         (south hallway room6)
                         (east hallway room7))
               'room6  '(("you are in a hallway. you hear someone talking loudly about software testing. the door frame to the west has and odd carboard cover over the door lock. A cleaning cart blocks your way south." )
                         (north hallway room5)
                         (west door office-room))
               'room7  '(("you are in a hallway. you smell popcorn and coffee to the northeast. there is a cubicle full of tests nearby. " )
                         (north hallway room8)
                         (west hallway room5))
               'room8  '(("you are next to the break room. the windows invite you outside and the smell of microwaved lunches is in the air. " )
                         (south hallway room7)
                         (east hallway break-room))
               'break-room  '(("you are in a large breakroom. there is some guy there reading a paper. there is a small group of people holding a meeting. there is no coffee, as usual. the cleaning people have blocked the east exit." )
                         (west hallway room8))))

;; Object locations in a hash map
(def object-locations (hash-map
                       'laptop 'office-room
                       'bug-report 'room4
                       'bozo 'office-room
                       'beer 'break-room
                       'tps-report 'room4))
(def location 'office-room)
(defn describe-location [location game-map]
  (first (location game-map)))

(defn describe-path [path]
  `("there is a "  ~(second path) " going "  ~(first path)  " from here. "))

(defn describe-paths [location game-map]
  (apply concat (map describe-path (rest (get game-map location)))))

(defn is-at? [obj loc obj-loc] (= (obj obj-loc) loc))

(defn describe-floor [loc objs obj-loc]
  (apply concat (map (fn [x]
                         `("you see a "  ~x " on the floor. " ))
                     (filter (fn [x] (is-at? x loc obj-loc)) objs))))

(defn look []
  (spel-print  (concat (describe-location location game-map)
          (describe-paths location game-map)
          (describe-floor location objects object-locations))))

(defn walk-direction [direction]
  (let [next (first (filter (fn [x] (= direction (first x)))
                            (rest (location game-map))))]
    (cond next (do (def location (nth next 2)) (look))
          :else '("you cannot go that way." ))))

(defmacro defspel [& rest] `(defmacro ~@rest))
(defspel walk [direction] `(walk-direction '~direction))
(defn pickup-object [object]
  (cond (is-at? object location object-locations)
        (do
          (def object-locations (assoc object-locations object 'body))
          `("you are now carrying the " ~object))
        :else '("you cannot get that.")))

(defn drop-object [object]
  (cond (have? object)
        (do
          (def object-locations (dissoc object-locations object 'body))
          (def object-locations (assoc object-locations object location))
          `("you are no longer carrying the " ~object))
        :else '("you don't have that.")))

(defspel pickup [object] `(spel-print (pickup-object '~object)))

(defn inventory []
  (filter (fn [x] (is-at? x 'body object-locations)) objects))

(defn have? [object]
  (some #{object} (inventory)))

(def bug-fixed false)
(def fix-committed false)

(defspel game-action [command subj  hve place & args]
  `(defspel ~command [subject#]
     `(spel-print (cond (and (= location '~'~place)
                             (= ~subject# ~~subj)
                             (have? '~'~hve))
                        ~@'~args
                        :else '("i cannot "  ~'~command  " like that.")))))

(game-action fix "bug" laptop  office-room
             (cond (and  (have? 'bug-report) (def bug-fixed true))
                   '("you brilliantly code the perfect fix on the laptop. don't forget to commit." )
                   :else '("you do not have a  bug-report.")))

(game-action commit "fix" laptop office-room
             (cond bug-fixed 
                   (do (def fix-committed  true)
                       '("the brilliant fix has been committed to your SCM. update the bug." ))
                   :else '("unable to commit a fix at this time." )))

(game-action update "bug" laptop office-room
             (cond (not fix-committed) '("there are no fixes committed for this bug." )
                   (have? 'beer) '("your boss has discovered that you were drinking while working. you lose! the end. " )
                   :else '("you have saved the day with your timely fix.  you win! the end." )))

(defplugin
  (:cmd
    "look"
    #{"look" "l"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (look)))))
  (:cmd
    "pickup"
    #{"pickup" "get" "take" "t"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (pickup-object (symbol (first args)))))))
  (:cmd
    "drop"
    #{"drop"  "put"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (drop-object (symbol (first args)))))))
  (:cmd
    "inventory"
    #{"inventory" "i" "inv"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (print-str  (inventory)))))
  (:cmd
    "east"
    #{"east" "e"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (walk east)))))
  (:cmd
    "east"
    #{"north" "n"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (walk north)))))
  (:cmd
    "south"
    #{"south" "s"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (walk south)))))
  (:cmd
    "west"
    #{"west" "w"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (walk west)))))
  (:cmd
    "fix"
    #{"fix" "code"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (fix (first args) )))))
  (:cmd
    "commit"
    #{"commit"}
    (fn [{:keys [com bot channel args] :as com-m}]
        (send-message com-m (apply str (commit (first args) )))))
  (:cmd
    "update"
    #{"update"}
    (fn [{:keys [com bot channel args] :as com-m}]
      (send-message com-m (apply str (update (first args))))))
  (:cmd
    "go"
    #{"go" "move"}
    (fn [{:keys [com bot channel args] :as com-m}]  
      (send-message com-m (apply str (walk-direction (symbol (first args))))))))
