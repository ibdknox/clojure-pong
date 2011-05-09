(ns clojure-pong.core
  (:gen-class)
  (:import [javax.swing JFrame]
           [java.awt Color Font Dimension GraphicsEnvironment Toolkit]
           [java.awt.event KeyListener]))


(def pressed-keys (atom {}))

(defn on-keypress [ch]
  (swap! pressed-keys assoc (keyword (str ch)) true))

(defn on-keyrelease [ch]
  (swap! pressed-keys assoc (keyword (str ch)) nil))

(defn pressed? [k]
  (get @pressed-keys k))

(defn vel-to-dxy [speed direction] 
  [(* speed (Math/cos direction))
   (* speed (Math/sin direction))])

(defn rad->deg [rad]
  (* rad (/ 180 Math/PI)))

(defn deg->rad [deg]
  (* deg (/ Math/PI 180)))

(defn move [{x :x y :y [speed direction] :vel :as obj}]
  (let [[dx dy] (vel-to-dxy speed (deg->rad direction))]
    {:x (+ dx x) :y (+ y dy)}))

(defn hit-bounds? [{x :x y :y obj-width :width obj-height :height} {width :width height :height}]
  (let [x-hit (or (>= 0 x)
                   (>= x (- width obj-width)))
        y-hit (or (>= 0 y)
                   (>= y (- height obj-height)))]
    (cond
      (and x-hit y-hit) :xy
      x-hit :x
      y-hit :y)))

(defn intersects? [{x1 :x y1 :y w1 :width h1 :height} {x2 :x y2 :y w2 :width h2 :height}]
  (let [top-lefts [y1 y2 x1 x2]
        bottom-rights [(+ y2 h2) (+ y1 h1) (+ x2 w2) (+ x1 w1)]]
    (every? true? (map < top-lefts bottom-rights))))

(defn hit-obj? [{x :x y :y obj-width :width obj-height :height} {width :width height :height}] (let [x-hit (or (>= 0 x)
                   (>= x (- width obj-width)))
        y-hit (or (>= 0 y)
                   (>= y (- height obj-height)))]
    (cond
      (and x-hit y-hit) :xy
      x-hit :x
      y-hit :y)))

(defn signed-rand [upper]
  (let [sign (rand-int 2)
        cur (rand-int upper)]
    (if (zero? sign)
      (* cur -1)
      cur)))

(defn reflect-with-wobble [cur axis] 
  (let [wobble (signed-rand 11)
        new-angle (case axis
                    :x (- 180 cur)
                    :y (- cur)
                    :xy (- cur 180))]
    (+ new-angle wobble)))

(defn collide [{[speed direction] :vel :as obj} axis]
  (let [reflected (assoc obj :vel [speed (reflect-with-wobble direction axis)])]
    (merge reflected (move reflected))))

(declare end-game)

(defprotocol updateable
  (update [this state] "updates the current game object based on the state passed in"))

(defprotocol drawable
  (draw [this graphics] "draws the current game object on the given graphics"))

(defrecord ball [x y width height vel]
  updateable
  (update [this {{board :board left :left-paddle right :right-paddle} :objs}]
          (let [moved (merge this (move this))]
            (if-let [axis (hit-bounds? moved board)]
              (collide this axis)
              (if (or (intersects? moved left) (intersects? moved right))
                (collide this :x)
                 moved))))
  drawable
  (draw [{x :x y :y w :width h :height} graphics]
        (.setColor graphics Color/WHITE)
        (.fillOval graphics x y w h)))

(defrecord paddle [x y width height ks]
  updateable
  (update [{x :x y :y [up down] :ks :as this} {{board :board} :objs}] 
          (let [direction (cond 
                          (pressed? up) -90
                          (pressed? down) 90)
                moved (when direction
                        (merge this (move {:x x :y y :vel [5 direction]})))]
            (or (when (and moved 
                           (not (hit-bounds? moved board)))
                  moved) 
                this)))
  drawable
  (draw [{x :x y :y w :width h :height} graphics]
        (doto graphics
          (.setColor Color/WHITE)
          (.fillRect x y width height))))

(defrecord board [width height]
  updateable
  (update [this state] 
          (when (pressed? :q)
            (end-game))
          this)
  drawable
  (draw [{width :width height :height} graphics]
        (.setColor graphics Color/BLACK)
        (.fillRect graphics 0 0 width height)))


(defn setup-frame []
  (let [frame (new JFrame "Clojure Pong")
        ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        gd (. ge getDefaultScreenDevice)]
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setUndecorated true)
      (.setResizable false))

    (.setFullScreenWindow gd frame)

    (doto frame
      (.setVisible true)
      (.createBufferStrategy 2)

      (.addKeyListener
        (proxy [KeyListener] []
          (keyPressed [e]
                      (on-keypress (.getKeyChar e)))
          (keyReleased [e]
                       (on-keyrelease (.getKeyChar e)))
          (keyTyped [e])))

      ; Makes sure everything inside the frame fits
      (.validate)
      (.show))
    frame))  

(def game-state (atom {:loop-state false
                       :objs {}}))

(def pong-obj {:board (board. 400 400)
               :left-paddle (paddle. 20 150 10 100 [:a :s])
               :right-paddle (paddle. 370 150 10 100 [:j :k])
               :ball (ball. 200 200 19 19 [5 50])})

(defn change-state [k v]
  (swap! game-state #(assoc % k v)))

(defn init-game [objs]
  (change-state :loop-state true)
  (reset! pressed-keys {})
  (change-state :objs objs))

;;set the frame rate to be 60fps
(def fps 60)
(def frame-time (/ 1000 fps))

(defn game-loop [frame]
  ;;update the world
  (if-not (:loop-state @game-state) 
    (doto frame
      (.hide)
      (.dispose))
    (let [start-time (System/currentTimeMillis)]
      (doseq [[k v] (:objs @game-state)]
        (when (satisfies? updateable v)
          (change-state :objs (assoc (:objs @game-state) k (update v @game-state)))))

      ;;draw the world
      (let [buffer (.getBufferStrategy frame)
            graphics (.getDrawGraphics buffer)]
        (doseq [[_ obj] (:objs @game-state)]
          (when (satisfies? drawable obj)
            (draw obj graphics)))
        (.dispose graphics)
        (.show buffer))

      ;;wait until the next frame
      (let [fps-rest (- frame-time (- (System/currentTimeMillis) start-time))]
        (when (> fps-rest 0)
          (Thread/sleep fps-rest)))

      (recur frame))))

(defn end-game []
  (change-state :loop-state false))

(defn run [objs]
  (init-game objs)
  (let [frame (setup-frame)]
    (game-loop frame)))

(defn pong []
  (run pong-obj))
    
(defn- main []
  (pong))
