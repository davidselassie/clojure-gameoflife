(ns gameoflife.core)

(defn empty-board
	"Creates a rectangular empty board of the specified width and height."
	[w h]
	; repeat repeats the 2nd argument 1st argument number of times.
	(vec (repeat w (vec (repeat h nil)))))

(defn populate
	"Turns :on each of the cells specified as [y, x] coordinates."
	[board living-cells]
	(reduce (fn [board coordinates]
		; coordinates is a vector. Use that vector as a nested lookup in
		; board, and set that to :on. This is sort of like
		; yelp_lib.set_deep_by_key.
		(assoc-in board coordinates :on))
	board
	living-cells))

(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})

(defn neighbours
	"Returns neighbors on a 2D square grid."
	[[x y]]
	; for is like a Python list comprehention.
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn step
	"Yields the next state of the 2D grid world."
	[cells] ; the set of :on cells.
	; for is a list comprehention.
	(set (for
		; We're unpacking a map of {obj count, ...} into this 2-vector.
		[[loc n]
		; frequencies returns a map from value to number of counts in the
		; input list.
		(frequencies
			; mapcat first "maps" neighbours onto every live cell; this means
			; the resulting lists will collectively contain each cell once for
			; the number of living neighbors it has.
			; mapcat then "concats" all those lists together so frequencies
			; can produce neighbor counts. THIS BLOWS JR'S MIND.
			(mapcat neighbours cells))
		; this is like the ... if .. at the end of a Python list
		; comprehention. Once we have unpacked neighbor counts, we can decide
		; if this cell should be :on (and returned in the next step's list of
		; living cells) or nil and missing from that set.
		:when (or (= n 3) (and (= n 2) (cells loc)))]
		; Now actually comprehend the location that satisfies :when.
		loc)))

(defn pprint
	"Prints a vector of vectors very neatly."
	[matrix]
	; doseq traverses a seq just for its side effects.
	(doseq [row matrix]
		(doseq [item row]
			(print item " "))
		(print "\n"))
	(print "\n"))

(defn print-live
	"Prints the world given."
	[w h cells]
	(pprint (populate (empty-board w h) cells)))

(defn stepper
	"'step' function generator given a neighbour returning function and a birth and death predicate."
	[neighbours birth? survive?]
	; This is exactly like above, but uses the factory's predicates.
	(fn [cells]
		(set (for [[loc n] (frequencies (mapcat neighbours cells))
			:when (if (cells loc) (survive? n) (birth? n))]
			loc))))

(defn sphere-neighbours
	"Returns neighbors on a 2D square grid that wraps to 0 at x = width and y = height."
	[w h]
	(fn [[x y]]
		(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
			; mod to get wrapping.
			[(mod (+ dx x) w) (mod (+ dy y) h)])))

; make an 8x8 stepper with usual rules except in a spherical space.
(def sphere-stepper (stepper (sphere-neighbours 8 8) #(= % 2) #(= % 3)))

(defn -main []
	(pprint (populate (empty-board 8 8) (step glider))))
