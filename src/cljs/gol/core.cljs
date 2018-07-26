(ns gol.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def shifts
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not= [x y] [0 0])]
    [x y]))

(defn bound [max v]
  (if (< v 0)
    (+ max v)
    (if (>= v max)
      (- v max)
      v)))

(defn neibs [w h [x y] state]
  (->>
    shifts
    (map (fn [[dx dy]] [(bound w (+ x dx))
                        (bound h (+ y dy))]))
    (filter (fn [p] (not= p [x y])))
    set
    (clojure.set/intersection state)
    count))


(defn populate [w h state]
  (let [all  (for [x (range w)
                   y (range h)
                   :let [p [x y]]]
               [p (neibs w h p state)])
        only (fn [pred]
               (->> all
                    (filter (comp pred second))
                    (map first)
                    set))
        dead (only #(or (< % 2) (> % 3)))
        born (only #(= % 3))]
    (-> state
        (clojure.set/difference dead)
        (clojure.set/union born))))

(def settings
  {:rows 20
   :cols 20})

(defonce state (atom {:life     #{[1 0] [2 1] [0 2] [1 2] [2 2]
                                  ;[8 0] [8 1] [8 2]
                                  [12 0] [12 1] [12 2] [11 1]}
                      :settings settings}))

(defn app []
  (let [population (:life @state)
        {:keys [rows cols]} (:settings @state)]
    [:div
     [:div.board {:style {:grid-template-rows    (str "repeat(" rows ", 20px)")
                          :grid-template-columns (str "repeat(" cols ", 20px)")}}
      (for [row (range 0 rows)
            col (range 0 cols)
            :let [alive (contains? population [row col])]]
        ^{:key [row col]}
        [:div.cell {:class (when alive "cell_alive")}])]

     [:input {:type          "range"
              :default-value rows
              :min           10
              :max           40
              :on-change     (fn [event]
                               (swap! state assoc :settings
                                      {:rows (js/parseInt (-> event .-target .-value))
                                       :cols (js/parseInt (-> event .-target .-value))}))}]]))

(defonce interval (atom nil))

(defn create-interval []
  (reset! interval (js/setInterval
                     (fn []
                       (let [rows (-> @state :settings :rows)
                             cols (-> @state :settings :cols)]
                         (swap! state assoc :life (populate rows cols (:life @state)))))
                     100)))

(defn mount-root []
  (println "init" @interval)
  (js/clearInterval @interval)
  (println "after clear" @interval)
  (reagent/render [app] (.getElementById js/document "app"))
  (create-interval)
  (println "after create" @interval))

(defn init! []
  (mount-root))
