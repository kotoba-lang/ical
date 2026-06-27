(ns ical.ical-test
  (:require [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.java.io :as io])
            [ical.model    :as model]
            [ical.validate :as v]
            [ical.ical     :as ical]
            [ical.execute  :as ex]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- slurp-sample []
  #?(:clj  (slurp (io/resource "ical/sample.ics"))
     :cljs nil))                       ; CLJS: skip resource tests

(defn- mk-event
  "Build a minimal VEVENT map directly."
  [uid dtstart rrule]
  (cond-> {:ical/uid uid :ical/dtstart dtstart}
    rrule (assoc :ical/rrule rrule)))

(defn- wide-range
  "A [from, to) window large enough to see any finite recurrence."
  []
  [{:y 2000 :m 1 :d 1 :hh 0 :mm 0}
   {:y 2100 :m 1 :d 1 :hh 0 :mm 0}])

;; ---------------------------------------------------------------------------
;; parse-str: sample.ics
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest parse-sample-ics
     (let [cal  (ical/parse-str (slurp-sample))
           evts (:ical/events cal)]
       (testing "calendar-level fields"
         (is (= "-//com-junkawasaki//ical-clj//EN" (:ical/prodid cal)))
         (is (= "2.0" (:ical/version cal))))
       (testing "vevent fields"
         (is (= 1 (count evts)) "one event in sample")
         (let [evt (first evts)]
           (is (= "sample-weekly-standup-001@ical-clj" (:ical/uid evt)))
           (is (= "Weekly Standup" (:ical/summary evt)))
           (is (= {:y 2026 :m 7 :d 1 :hh 10 :mm 0} (:ical/dtstart evt)))
           (is (= {:y 2026 :m 7 :d 1 :hh 11 :mm 0} (:ical/dtend evt)))
           (is (= "Conference Room A" (:ical/location evt)))))
       (testing "rrule parsed"
         (let [rrule (get-in evts [0 :ical/rrule])]
           (is (= :weekly (:ical/freq rrule)))
           (is (= 1 (:ical/interval rrule)))
           (is (= 5 (:ical/count rrule)))
           (is (= [:mo :we] (:ical/byday rrule)))))
       (testing "vtodo parsed"
         (let [td (first (:ical/todos cal))]
           (is (= "sample-todo-001@ical-clj" (:ical/uid td)))
           (is (= :needs-action (:ical/status td))))))))

;; ---------------------------------------------------------------------------
;; round-trip: emit-str then parse-str
;; ---------------------------------------------------------------------------

(deftest round-trip-calendar
  (let [cal (-> (model/calendar {:prodid "-//test//en" :version "2.0"})
                (model/add-event
                 (model/event "uid-rt-001@test"
                              {:summary  "Semicolon;test"
                               :dtstart  {:y 2026 :m 8 :d 1 :hh 9 :mm 0}
                               :dtend    {:y 2026 :m 8 :d 1 :hh 10 :mm 0}
                               :rrule    (model/rrule {:freq :daily :interval 3 :count 4})})))
        emitted (ical/emit-str cal)
        parsed  (ical/parse-str emitted)]
    (is (= "-//test//en" (:ical/prodid parsed)) "prodid round-trips")
    (is (= 1 (count (:ical/events parsed))) "one event survives")
    (let [evt (first (:ical/events parsed))]
      (is (= "uid-rt-001@test" (:ical/uid evt)) "uid round-trips")
      (is (= {:y 2026 :m 8 :d 1 :hh 9 :mm 0} (:ical/dtstart evt)) "dtstart round-trips")
      (is (= :daily (get-in evt [:ical/rrule :ical/freq])) "rrule freq round-trips")
      (is (= 4 (get-in evt [:ical/rrule :ical/count])) "rrule count round-trips"))))

;; ---------------------------------------------------------------------------
;; validate
;; ---------------------------------------------------------------------------

(deftest validate-valid-calendar
  (let [cal (-> (model/calendar {:prodid "-//x//en"})
                (model/add-event
                 (model/event "v-uid-001"
                              {:dtstart {:y 2026 :m 1 :d 1 :hh 0 :mm 0}
                               :summary "OK"})))]
    (is (v/valid? cal))
    (is (empty? (v/errors cal)))))

(deftest validate-missing-uid
  (let [cal {:ical/events [{:ical/dtstart {:y 2026 :m 1 :d 1 :hh 0 :mm 0}}]
             :ical/todos  []}]
    (is (not (v/valid? cal)))
    (is (some #(= :vevent/missing-uid (:ical/code %)) (v/problems cal)))))

(deftest validate-rrule-both-count-and-until
  (let [cal {:ical/events [{:ical/uid "x"
                             :ical/dtstart {:y 2026 :m 1 :d 1 :hh 0 :mm 0}
                             :ical/rrule {:ical/freq :weekly
                                          :ical/count 3
                                          :ical/until {:y 2026 :m 2 :d 1 :hh 0 :mm 0}}}]
             :ical/todos []}]
    (is (not (v/valid? cal)))
    (is (some #(= :rrule/count-and-until (:ical/code %)) (v/problems cal)))))

;; ---------------------------------------------------------------------------
;; days_from_civil weekday sanity checks
;; ---------------------------------------------------------------------------

(deftest weekday-known-dates
  (testing "1970-01-01 is Thursday (wd=4)"
    (is (= 4 (ex/weekday-from-days 0))))
  (testing "2024-01-01 is Monday (wd=1)"
    ;; Jan 1, 2024: days-from-civil(2024,1,1)
    ;; y=2023 (m<=2), era=5, yoe=23, m+9=10, doy=(153*10+2)/5+0=306, doe=23*365+5+306=8706
    ;; result=730485+8706-719468=19723
    (is (= 1 (ex/weekday-from-days 19723))))
  (testing "2026-06-27 is Saturday (wd=6)"
    ;; computed above: days-from-civil(2026,6,27)=20631
    (is (= 6 (ex/weekday-from-days 20631)))))

;; ---------------------------------------------------------------------------
;; execute: weekly RRULE COUNT=N expands to exactly N dates, 7-day spacing
;; ---------------------------------------------------------------------------

(deftest weekly-simple-count
  ;; FREQ=WEEKLY;INTERVAL=1;COUNT=4, no BYDAY — just steps the same weekday
  ;; dtstart = 2026-07-01 (Wednesday)
  (let [evt (mk-event "ew-1" {:y 2026 :m 7 :d 1 :hh 10 :mm 0}
                      {:ical/freq :weekly :ical/interval 1 :ical/count 4})
        [from to] (wide-range)
        occ (ex/occurrences evt from to)]
    (is (= 4 (count occ)) "exactly COUNT occurrences")
    (is (= {:y 2026 :m 7 :d 1  :hh 10 :mm 0} (first occ))  "first = dtstart")
    (is (= {:y 2026 :m 7 :d 8  :hh 10 :mm 0} (second occ)) "second = +7 days")
    (is (= {:y 2026 :m 7 :d 15 :hh 10 :mm 0} (nth occ 2))  "third = +14 days")
    (is (= {:y 2026 :m 7 :d 22 :hh 10 :mm 0} (nth occ 3))  "fourth = +21 days")))

;; ---------------------------------------------------------------------------
;; execute: weekly BYDAY=MO,WE → both weekdays per interval week
;; ---------------------------------------------------------------------------

(deftest weekly-byday-mo-we
  ;; dtstart = 2026-07-01 (Wed); BYDAY=MO,WE; COUNT=5
  ;; Week 0 base-Mon = 2026-06-29:  Mon(skip<dtstart), Wed=2026-07-01
  ;; Week 1 base-Mon = 2026-07-06:  Mon=2026-07-06, Wed=2026-07-08
  ;; Week 2 base-Mon = 2026-07-13:  Mon=2026-07-13, Wed=2026-07-15 ← 5th
  (let [evt (mk-event "ew-2" {:y 2026 :m 7 :d 1 :hh 10 :mm 0}
                      {:ical/freq :weekly :ical/interval 1
                       :ical/byday [:mo :we] :ical/count 5})
        [from to] (wide-range)
        occ (ex/occurrences evt from to)]
    (is (= 5 (count occ)) "exactly COUNT occurrences")
    (is (= {:y 2026 :m 7 :d  1 :hh 10 :mm 0} (first occ))  "first = Wed dtstart")
    (is (= {:y 2026 :m 7 :d  6 :hh 10 :mm 0} (second occ)) "second = Mon")
    (is (= {:y 2026 :m 7 :d  8 :hh 10 :mm 0} (nth occ 2))  "third = Wed")
    (is (= {:y 2026 :m 7 :d 13 :hh 10 :mm 0} (nth occ 3))  "fourth = Mon")
    (is (= {:y 2026 :m 7 :d 15 :hh 10 :mm 0} (nth occ 4))  "fifth = Wed")))

;; ---------------------------------------------------------------------------
;; execute: daily INTERVAL=2
;; ---------------------------------------------------------------------------

(deftest daily-interval-2
  (let [evt (mk-event "ed-1" {:y 2026 :m 7 :d 1 :hh 8 :mm 30}
                      {:ical/freq :daily :ical/interval 2 :ical/count 3})
        [from to] (wide-range)
        occ (ex/occurrences evt from to)]
    (is (= 3 (count occ)) "COUNT=3")
    (is (= {:y 2026 :m 7 :d 1 :hh 8 :mm 30} (first occ))  "day 1")
    (is (= {:y 2026 :m 7 :d 3 :hh 8 :mm 30} (second occ)) "day 3 (+2)")
    (is (= {:y 2026 :m 7 :d 5 :hh 8 :mm 30} (nth occ 2))  "day 5 (+4)")))

;; ---------------------------------------------------------------------------
;; execute: monthly RRULE
;; ---------------------------------------------------------------------------

(deftest monthly-rrule
  (let [evt (mk-event "em-1" {:y 2026 :m 7 :d 31 :hh 9 :mm 0}
                      {:ical/freq :monthly :ical/interval 1 :ical/count 3})
        [from to] (wide-range)
        occ (ex/occurrences evt from to)]
    (is (= 3 (count occ)) "COUNT=3")
    (is (= {:y 2026 :m 7 :d 31 :hh 9 :mm 0} (first occ))  "July 31")
    ;; Aug has 31 days: Aug 31
    (is (= {:y 2026 :m 8 :d 31 :hh 9 :mm 0} (second occ)) "Aug 31")
    ;; Sep has 30 days: clamped to Sep 30
    (is (= {:y 2026 :m 9 :d 30 :hh 9 :mm 0} (nth occ 2))  "Sep 30 (clamped)")))

;; ---------------------------------------------------------------------------
;; execute: UNTIL bound stops correctly
;; ---------------------------------------------------------------------------

(deftest weekly-until-bound
  ;; dtstart=2026-07-01 (Wed), FREQ=WEEKLY, UNTIL=2026-07-15 (Wed)
  ;; → occurrences: 2026-07-01, 2026-07-08, 2026-07-15 (3 total; 2026-07-22 excluded)
  (let [evt (mk-event "eu-1" {:y 2026 :m 7 :d 1 :hh 10 :mm 0}
                      {:ical/freq :weekly :ical/interval 1
                       :ical/until {:y 2026 :m 7 :d 15 :hh 0 :mm 0}})
        [from to] (wide-range)
        occ (ex/occurrences evt from to)]
    (is (= 3 (count occ)) "stops at UNTIL")
    (is (= {:y 2026 :m 7 :d  1 :hh 10 :mm 0} (first occ))  "first occurrence")
    (is (= {:y 2026 :m 7 :d 15 :hh 10 :mm 0} (last occ))   "last = UNTIL date")))

;; ---------------------------------------------------------------------------
;; execute: non-recurring event
;; ---------------------------------------------------------------------------

(deftest non-recurring-in-range
  (let [dt  {:y 2026 :m 9 :d 1 :hh 14 :mm 0}
        evt {:ical/uid "nr-1" :ical/dtstart dt}
        in  (ex/occurrences evt {:y 2026 :m 1 :d 1 :hh 0 :mm 0}
                                {:y 2027 :m 1 :d 1 :hh 0 :mm 0})
        out (ex/occurrences evt {:y 2027 :m 1 :d 1 :hh 0 :mm 0}
                                {:y 2028 :m 1 :d 1 :hh 0 :mm 0})]
    (is (= [dt] in)  "dtstart in range → returned")
    (is (= []   out) "dtstart out of range → empty")))
