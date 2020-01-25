;;Oliver McHale
;;Kyle Spence
;;Stephen Cartmail


;;-------------------------------------------------------------------------------------
;;WORLD GENERATION 

;; we can define a world as a map.
;; the map has a key for each position in the world.
;; each key has a map which contains key value pairs
;; of the next available state and the cost
;; of moving to that state...
(def start-map {})


(def world-map {})

(defn up-left [max-x max-y cur-x cur-y]
  (cond
    (<= (dec cur-x) 0 ) false
    (<= (inc cur-y) 0 ) false
    (> (dec cur-x) max-x ) false
    (> (inc cur-y) max-y ) false
    :else true
  )
)

(defn up [max-y cur-y]
  (cond
    (> (inc cur-y) max-y) false
    :else true))

(defn up-right [max-x max-y cur-x cur-y]
  (cond
    (<= (inc cur-x) 0) false
    (<= (inc cur-y) 0) false
    (> (inc cur-x) max-x ) false
    (> (inc cur-y) max-y ) false
    :else true
  )  
)

(defn left [max-x cur-x]
  (cond
    (<= (dec cur-x ) 0 ) false
    :else true))

(defn right [max-x cur-x]
  (cond 
    (> (inc cur-x) max-x) false
    :else true))

(defn down-left [max-x max-y cur-x cur-y]
  (cond
    (<= (dec cur-x) 0) false
    (<= (dec cur-y) 0) false
    (> (dec cur-x) max-x ) false
    (> (dec cur-y) max-y ) false
    :else true
  )
)

(defn down [cur-y]
  (cond
    (<= (dec cur-y) 0) false
    :else true))

(defn down-right [max-x max-y cur-x cur-y]
  (cond
    (<= (inc cur-x) 0) false
    (<= (dec cur-y) 0) false
    (> (inc cur-x) max-x ) false
    (> (dec cur-y) max-y ) false
    :else true
  )
)
;; to do this programatically we need to know the size of the generated map
;; only 8 possible moves can exist for any node of the world
;; 
(defn gen-map [max-x max-y cur-x cur-y]
  ;; if current x or y is greater than max, return the map
  (if(or (> cur-x max-x) (> cur-y max-y)) world-map nil)
  ;; otherwise, we can check the cur-x and cur-y surroundings
  ;; to begin generating the map.
  (def map {})
  (def x "x")
  ;; make a new empty list which will contain the current x-y's possible moves.
  ;;lets see if up-left exists
    (if (up-left max-x max-y cur-x cur-y) 
      ;;has evaluated to true, so we can now add this to the list for our current node
      ;;(def ls (conj ls (concat (list (dec cur-x) (inc cur-y)))))
      (def map (assoc map (str (dec cur-x) x (inc cur-y)) 1.4))
      ;;has evaluated to false, so we ignore this and instead try another neighbour node.
      false
    )
    (if (up max-y cur-y)
      (def map (assoc map (str cur-x x (inc cur-y)) 1))
      false
    )
    (if (up-right max-x max-y cur-x cur-y)
      (def map (assoc map (str (inc cur-x) x (inc cur-y)) 1.4))
      false  
    )
    (if (left max-x cur-x)
      (def map (assoc map (str (dec cur-x) x cur-y) 1))
      false
    )
    (if (right max-x cur-x)
      (def map (assoc map (str (inc cur-x) x cur-y) 1))
      false
    )
    (if (down-left max-x max-y cur-x cur-y)
      (def map (assoc map (str (dec cur-x) x (dec cur-y)) 1.4))
      false  
    )
    (if (down cur-y)
      (def map (assoc map (str cur-x x (dec cur-y)) 1))
      false
    )
    (if (down-right max-x max-y cur-x cur-y)
      (def map (assoc map (str (inc cur-x) x (dec cur-y)) 1.4))
      false
    )
    map
)

(defn add-to-map [map key values]
  (assoc map key values)
)

;;For-loop helper -https://stackoverflow.com/questions/9981943/how-to-implement-a-for-loop-in-clojure
(defmacro for-loop [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#)))


(defn map-gen [max-x max-y]
  (def ls {})
  (for-loop [i 1 (< i (inc max-x)) (inc i)]
    (for-loop [j 1 (< j (inc max-y)) (inc j)]
      (def ls (assoc ls (str i "x" j) (gen-map max-x max-y i j)))
    )
  )
  ls
)

;; takes a costed map and removes the cost from it. used in a* search.
(defn costless-map [cost-map max-x max-y]
  (def cm {})
  (for-loop [i 1 (< i (inc max-x)) (inc i)]
    (for-loop [j 1 (< j (inc max-y)) (inc j)]
      (def cm (assoc cm (str i "x" j) (into #{} (keys (get cost-map (str i "x" j))))))
    )
  )
  cm
)

(defn rem-map [my-map value]
  (def new-map {})
  (doseq [[k x] my-map]
    ;;(println k ":" x)
    ;;(println "dissacoationg" value "from" x)
    (def new-map (merge new-map (assoc new-map k (dissoc x value))))
    ;;(println  "new map is now " new-map)
    )
    ;;(clojure.pprint/pprint new-map)
    new-map)


;;takes a map and a position on that map, and removes all references in the nested map
(defn add-obstacle [world-map value]
  (def obs-map (dissoc world-map value))
  (rem-map obs-map value)
)

;;----------------------------------------------------------------------------------------



;;A STAR SEARCH ALGORITHM
;;https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html
(declare a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a* [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
    (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
      (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (get graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
    (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))


;; costs used in cost function
(def costs (map-gen 8 8))

;; cost function for a* algorithm
 (defn cost [node node']
   (get-in costs [node node']))

;; example map used in a* without costs. consits of a map where the values are sets.

;; calculates the absolute number
(defn abs [n] (max n (- n)))

(defn diag-dis [nx ny gx gy]
  (def dx (abs (- nx gx)))
  (def dy (abs (- ny gy)))
  ;; D = 1, D2 = 1.4 - octile distance
  (def dxdy (+ dx dy))
  (def lhs (* (- 1.4 2) 1))
  (def rhs (min dx dy))
  (def lhsrhs (* lhs rhs))
  (def result (+ (* 1 dxdy) lhsrhs))
  result
)

;; heuristic function used to caclulate values a* search, using the octile distance
;;http://theory.stanford.edu/~amitp/GameProgramming/Heuristics.html#S7
(defn heuris [node node']
   (def nx (Character/digit (first node) 10))
   (def ny (Character/digit (last node) 10))
   (def gx (Character/digit (first node') 10))
   (def gy (Character/digit (last node') 10))
   (diag-dis nx ny gx gy)
)

;;----------------------ADD TESTS HERE 
;;7X7 with obsticles 
(def left-size 7)
(def right-size 7)
(def start "1x1")
(def goal "6x6")
(def world (map-gen left-size right-size))

(def obst-map (add-obstacle world "2x1"))
(def obst-map2 (add-obstacle obst-map "2x2"))
(def obst-map3 (add-obstacle obst-map2 "2x3"))
(def obst-map4 (add-obstacle obst-map3 "3x3"))
(def obst-map5 (add-obstacle obst-map4 "3x4"))
(def obst-map6 (add-obstacle obst-map5 "5x6"))

(def costless-world (costless-map  obst-map6 left-size right-size))
(time (first (a* costless-world start goal :distance cost :heuristic heuris)))
;;-----------------------------------------------------------------------------------







;;------------------------------------------------------------------------------------
;;DIJKSTRA ALGORITHM 
(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  ;;"Returns costs updated with any shorter paths found to curr's unvisisted
  ;;neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  ;;"Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  ;;Graph is a map of nodes to map of neighboring nodes and associated cost.
  ;;Optionally, specify destination node to return once cost is known"
  ([g src]
    (dijkstra g src nil))
  ([g src dst]
    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
           curr src
           unvisited (disj (apply hash-set (keys g)) src)]
      (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))


;;ADD TESTS HERE 
;;Without Obsticles
(def whole-map (map-gen 5 5))
(time (dijkstra whole-map "1x1" "5x5"))

;;Without obsticles 
(def obst-map (add-obstacle whole-map "4x4"))
(time (dijkstra obst-map "1x1" "5x5"))




;;-----------------------------------------------------------------------------------------

;;CUSTOM SEARCH ALGORITHM 
(defn custom-search [world-map start goal ls]
  ;;(println "START" start)
  ;;(println "GOAL" goal)
  (if (= start goal) 
    (
        (println (distinct (cons ls goal)))
        (println "FOUND" start "is equal to" goal)
        goal
    )
    (
      (doseq [[map-key map-value] world-map]
        (doseq [set-value map-value]
          (def removed-map (assoc world-map map-key (disj map-value set-value)))
          (custom-search removed-map set-value goal (cons ls map-value))
        )
      )
   )
  )
)

(defn run-search [world start end initial]
  (try 
    (custom-search world start end (list initial))
  (catch Exception e (str "done"))
))

;;(def custom-map (costless-map (map-gen 5 5) 5 5))
;;(time(run-search custom-map "1x1" "5x5" "1x1"))
;;(def custom-map2 (costless-map (map-gen 10 10) 10 10))
;;(time(run-search custom-map2 "1x1" "10x10" "1x1"))
;;------------------------------------------------------------------------------------------
