(ns com.test.graph.graph-algos)

;;; Sample tree of the following form
;;;     1
;;;    / \
;;;   2   3
;;;  /     \
;;; 4       5
(def tree
  {
   :1 {:l :2 :r :3 :d 1}
   :2 {:l :4 :d 2}
   :3 {:r :5 :d 3}
   :4 {:d 4}
   :5 {:d 5}
   })

;;; Prints the data element for a particular node
(defn print-data
  [tree cur]
  (-> tree
      cur
      :d))

;;; Pre order traversal of binary tree
(defn pre-order
  [tree root]        
  (println (print-data tree root))
  (if-let [left (-> tree
                    root
                    :l)]
    (pre-order tree left))
  (if-let [right (-> tree
                     root
                     :r)]
    (pre-order tree right)))

;;; In order traversal of binary tree
(defn in-order
  [tree root]                
  (if-let [left (-> tree
                    root
                    :l)]
    (in-order tree left))
  
  (println (print-data tree root))
  
  (if-let [right (-> tree
                     root
                     :r)]
    (in-order tree right)))

;;; Post order traversal of binary tree
(defn post-order
  [tree root]                
  (if-let [left (-> tree
                    root
                    :l)]
    (post-order tree left))
  
  (if-let [right (-> tree
                     root
                     :r)]
    (post-order tree right))
  
  (println (print-data tree root)))

;;; Sample Graph represented by and adj-list
(def graph
  {
   :1 [[:2 10] [:3 3] [:4 14]]
   :2 []
   :3 [[:5 5] [:6 6]]
   :4 []
   :5 []
   :6 [[:7 7]]
   :7 []
   })

;;; Breadth First Search of a Graph
(defn bfs
  [graph root search]
  (let [done (atom {})
        next (atom [root])]
    
    (loop []

      (if (empty? @next)
        (not (nil? (search @done)))
        (let [cur (first @next)]

          ;; Update done map for current node          
          (swap! done assoc cur 'true)

          ;; dequeue the first node from the list of unprocessed nodes
          ;; Remember that filter returns a lazy sequence.
          ;; So use a function that takes a lazy sequence
          (swap! next (partial drop 1))

          ;; Add all elements unprocessed elements in node's adjacency
          ;; list to the list of unprocessed elements
          (let [unprocessed-adj-nodes (map #(first %) (filter #(not ((first %) done)) (cur graph)))]
            (swap! next concat unprocessed-adj-nodes))          
          (recur))))))


;; For a given node (cur) in a (graph), uses (done)  map to return the next
;; unprocessed adjacent node.
(defn get-next-unprocessed-node  
  [graph done cur]
  (first (first (filter #(not ((first %) @done)) (cur graph)))))

;;; Depth first search
(defn dfs
  [graph root search]
  (let [done (atom {})
        next (atom [root])]
    (loop []
      (if (empty? @next)
        (not (nil? (search @done)))
        (let [cur (first @next)
              next-node (get-next-unprocessed-node graph done cur)]
          (if (nil? next-node)
            ;; If next unprocessed node is nil then we are done processing
            ;; this node. Remove from the list of unprocessed nodes.
            ;; Update done status of current node
            (do
              (swap! next (partial drop 1))
              (swap! done assoc cur 'true))
            ;; Else add the new node to the list of unprocessed nodes
            (swap! next (partial concat [next-node])))
          (recur))))))

;;; Sample graph with more than one path between two vertices
(def graph2
  {
   :1 [[:2 10] [:3 3] [:4 14]]
   :2 []
   :3 [[:2 3] [:4 4] [:5 15] [:6 6]]
   :4 []
   :5 []
   :6 [[:5 2] [:7 7]]
   :7 []
   })

;;; Given a node (graph-node) in a undirected/directed graph (graph)
;;; represented as an adjacency list, returns a sequence of edges 
(defn make-edge-list-for-node
  [graph graph-node]
  (let [adj-vertices (graph-node graph)]    
    (map #(vector graph-node (first %) (second %)) adj-vertices)
    ))

;;; Given an adjacency list for a graph,
;;; returns the list of edges sorted by edge weight
(defn make-sorted-edge-list
  [adj-list]
  (let [nodes (keys adj-list)
        make-edges (partial make-edge-list-for-node adj-list)]
    (->> nodes
         (map make-edges)
         (reduce into '())
         (filter #(not (empty? %)))            
         (sort-by #(nth % 2))
         (vec))))

;;; Given an edge-list and  a set of processed nodes returns an edge
;;; with minimum weight such that one vertex is processed and the
;;; other is not
(defn get-partition-edge
  [edge-list processed-nodes]
  (let [filter-fn
        (fn [edge]
          (and
           (some #{(first edge)} processed-nodes)
           (not (some #{(second edge)} processed-nodes))))]

    (first (filter filter-fn edge-list))))

;;; Minimum spanning tree of a graph using an arbitrary node as start
;;; Prim's Algorithm based implementation
;;; Assumes graphis connected
(defn mst
  [graph]
  (let [
        processed-nodes (atom #{(first (keys graph))})
        edge-list (make-sorted-edge-list graph)
        new-edge-list (atom [])]    
    (loop []
      (if (= (into #{} @processed-nodes) (into #{} (keys graph)))
        (do 
            (println @new-edge-list))
        (let [next-edge (get-partition-edge edge-list @processed-nodes)]
          (swap! processed-nodes conj (second next-edge))
          (swap! new-edge-list conj next-edge)
          (recur)
          )))))


;;; Tree traversal calls
#_(pre-order tree :1)
#_(in-order tree :1)
#_(post-order tree :1)

;;; Returns true
#_(bfs graph :1 :1)
;;; Returns false
#_(bfs graph :1 :9)

;;; Returns true
#_(dfs graph :1 :1)
;;; Returns false
#_(dfs graph :1 :9)
