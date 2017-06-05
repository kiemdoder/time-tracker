(ns time-tracker.core
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io])
  (:import java.text.SimpleDateFormat))

(defonce cfg (atom {:times-db-file "times-db.edn"}))
(defonce times-db (atom nil))

(def month-formatter (SimpleDateFormat. "yyyy-MM"))
(defn month [d]
  (.format month-formatter d))

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
                                  (let [file-path (:times-db-file @cfg)]
                                    (.renameTo (io/file file-path) (io/file (str file-path ".0")))
                                    (spit file-path new-state)))))

(defn resume! [job]
  (let [job-periods (get @times-db job)
        last-period (last job-periods)]
    (if (or (empty? job-periods) (:stop last-period))
      (do
        (swap! times-db update job conj {:start (System/currentTimeMillis) :stop nil})
        (format "resumed %s" job))
      "Job already in progress")))

(defn stop! [job]
  (let [job-periods (get @times-db job)
        last-period-index (dec (count job-periods))]
    (if-not (get-in job-periods [last-period-index :stop])
      (do
        (swap! times-db assoc-in [job last-period-index :stop] (System/currentTimeMillis))
        (format "stopped %s" job))
      "Job already stopped")))

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

(defn total-hours-per-month [job]
  (->> @times-db
       job
       (group-by #(month (:start %)))
       (map (fn [[k v]] [k (/ (sum-times v) (* 1000 60 60.0))]))))

(defn addHour [job]
  (let [print-hours (fn [] (println (/ (total-minutes job) 60.0)))]
    (print-hours)
    (swap! times-db update job #(conj % {:start 0 :stop (* 1 60 60 1000)}))
    (print-hours)))

(defn -main [& args]
  (println "toets 123"))

(comment
  (load-state!)
  (resume! :wso2)
  (stop! :wso2)
  (in-progress? :wso2)
  (/ (total-minutes :wso2) 60.0)
  (doseq [m (total-hours-per-month :wso2)]
    (println (apply format "%s %.2fh" m)))
  
  )
