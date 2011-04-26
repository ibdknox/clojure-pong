(ns clojure-pong.core
  (:gen-class)
  (:import [javax.swing JFrame]
           [java.awt Color Font Dimension GraphicsEnvironment Toolkit]
           [java.awt.event KeyListener]))

(defn vel-to-dxy [speed direction] 
  [(* speed (Math/cos direction))
   (* speed (Math/sin direction))])

(defn move [{x :x y :y [speed direction] :vel :as obj}]
  (let [[dx dy] (vel-to-dxy speed direction)]
    {:x (+ dx x) :y (+ y dy)}))

(defn in-bounds? [{x :x y :y obj-width :width obj-height :height} {width :width height :height}]
  (and (<= 0 x)
       (<= x (- width obj-width))
       (<= 0 y)
       (<= y (- height obj-height))))

(defn signed-rand [upper]
  (let [sign (rand-int 2)
        cur (rand upper)]
    (if (zero? sign)
      (* cur -1)
      cur)))

(defn reflect-with-wobble [cur] 
  (let [wobble (signed-rand (/ Math/PI 16))]
    (+ (/ (Math/PI) 2) cur wobble)))

(defn collide [{[speed direction] :vel :as obj}]
  (let [reflected (assoc obj :vel [speed (reflect-with-wobble direction)])]
    (merge reflected (move reflected))))

(defprotocol updateable
  (update [this state] "updates the current game object based on the state passed in"))

(defprotocol drawable
  (draw [this graphics] "draws the current game object on the given graphics"))

(defrecord ball [x y width height vel]
  updateable
  (update [this {{board :board} :objs}]
          (let [moved (merge this (move this))]
            (if (in-bounds? moved board)
              moved
              (collide this))))
  drawable
  (draw [{x :x y :y w :width h :height} graphics]
        (.setColor graphics Color/WHITE)
        (.fillOval graphics x y w h)))

(defrecord board [width height]
  updateable
  (update [this state] this)
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
      
;;     (.addKeyListener
;;       (proxy [KeyListener] []
;;         (keyPressed [e]
;;                     (handle-keypress (.getKeyChar e)))
;;         (keyReleased [e]
;;                      (handle-keyrelease (.getKeyChar e)))
;;         (keyTyped [e])))

      ; Makes sure everything inside the frame fits
      (.validate)
      (.show))
    frame))  

(def game-state (atom {:objs {:board (board. 400 400)
                              :ball (ball. 200 200 19 19 [10 (* Math/PI 3/4)])}}))

(defn change-state [k v]
  (swap! game-state #(assoc % k v)))

(defn game-loop [frame]
  ;;update the world
  (doseq [[k v] (:objs @game-state)]
    (change-state :objs (assoc (:objs @game-state) k (update v @game-state))))

  ;;draw the world
  (let [buffer (.getBufferStrategy frame)
        graphics (.getDrawGraphics buffer)]
    (doseq [[_ obj] (:objs @game-state)]
      (draw obj graphics))

    (.dispose graphics)
    (.show buffer))

  ;;wait until the next frame
  (Thread/sleep 20)
  (recur frame))

(defn run []
  (let [frame (setup-frame)
       ]
    (Thread/sleep 1000)
    (game-loop frame)))
    
(defn- main []
  (run))
