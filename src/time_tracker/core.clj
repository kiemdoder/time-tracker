(ns time-tracker.core
  (:require
   [clojure.edn :as edn]))

(defonce cfg (atom {:times-db-file "times-db.edn"}))
(defonce times-db (atom nil))

(defn load-cfg! []
  (let [cfg-str (slurp "time-tracker-cfg.edn")
        cfg_ (edn/read-string cfg-str)]
    (reset! cfg cfg_ )))

(defn load-times! []
  (let [db-file (:times-db-file @cfg)
        times-db-str (slurp db-file)
        times-db_ (edn/read-string times-db-str)]
    (reset! times-db times-db_)))

(defn load-state! []
  (remove-watch times-db :times-db)
  (load-cfg!)
  (load-times!)
  (add-watch times-db :times-db (fn [_ _ _ new-state]
                                  (spit (:times-db-file @cfg) new-state))))

(defn resume! [job]
  (let [job-periods (get @times-db job)
        last-period (last job-periods)]
    (if (or (empty? job-periods) (:stop last-period))
      (swap! times-db update job conj {:start (System/currentTimeMillis) :stop nil})
      (println "Job already in progress"))))

(defn stop! [job]
  (let [job-periods (get @times-db job)
        last-period-index (dec (count job-periods))]
    (if-not (get-in job-periods [last-period-index :stop])
      (swap! times-db assoc-in [job last-period-index :stop] (System/currentTimeMillis))
      (println "Job already in progress"))))

(defn in-progress? [job]
  (let [job-periods (get @times-db job)]
    (and (seq job-periods)
         (->> job-periods
              last
              :stop
              not))))

(defn sum-times [times-seq]
  (reduce
   (fn [sum {:keys [start stop]}]
     (if stop
       (+ sum (- stop start))
       sum))
   0
   times-seq))

(defn total-minutes [job]
  (let [millis (sum-times (get @times-db job))]
    (quot millis (* 1000 60))))

(defn -main [& args]
  (println "toets 123"))

(comment
  (load-state!)
  (resume! :wso2)
  (stop! :wso2)
  (in-progress? :wso2)
  (/ (total-minutes :wso2) 60.0)
  
  )
