(ns ical.ical
  "iCalendar (RFC 5545) text ⇄ EDN model, with zero third-party deps and portable .cljc.

  `parse-str` / `emit-str` cover the well-formed iCalendar subset commonly produced
  by calendaring clients:
  - BEGIN/END block nesting for VCALENDAR, VEVENT, VTODO
  - RFC 5545 §3.1 line *unfolding* (a line starting with SPACE or HT continues the
    previous content line)
  - Property syntax `PROPNAME[;PARAM=V]:value` with value unescaping (\\\\, \\;, \\,, \\n)
  - DTSTART/DTEND/DUE as `YYYYMMDD[Thhmmss[Z]]` → {:y :m :d :hh :mm} maps
  - RRULE as `FREQ=…;INTERVAL=…;COUNT=…;UNTIL=…;BYDAY=MO,WE` → rrule map

  `emit-str` round-trips: parse-str ∘ emit-str is identity on the EDN model."
  (:require [clojure.string :as str]))

;; --- portable integer parser (no java.lang.Long/parseLong in CLJS) ---

(defn- parse-int
  "Parse a decimal string to integer. Portable across JVM and CLJS."
  [s]
  (reduce (fn [acc c] (+ (* acc 10) (- (int c) 48))) 0 s))

;; --- date-time parsing ---

(defn- parse-dt
  "Parse iCalendar date/date-time string `YYYYMMDD[Thhmmss[Z]]` to a {:y :m :d :hh :mm} map."
  [s]
  (when (and s (>= (count s) 8))
    (let [y  (parse-int (subs s 0 4))
          m  (parse-int (subs s 4 6))
          d  (parse-int (subs s 6 8))
          has-time? (and (>= (count s) 15) (= \T (nth s 8)))
          hh (if has-time? (parse-int (subs s 9 11)) 0)
          mm (if has-time? (parse-int (subs s 11 13)) 0)]
      {:y y :m m :d d :hh hh :mm mm})))

;; --- RRULE parsing ---

(def ^:private str->byday
  {"MO" :mo "TU" :tu "WE" :we "TH" :th "FR" :fr "SA" :sa "SU" :su})

(defn- parse-rrule
  "Parse an RRULE value string `FREQ=WEEKLY;INTERVAL=1;…` to a rule map."
  [s]
  (let [pairs (map #(let [eq (str/index-of % "=")]
                      [(subs % 0 eq) (subs % (inc eq))])
                   (str/split s #";"))
        m     (into {} pairs)]
    (cond-> {}
      (get m "FREQ")
      (assoc :ical/freq (keyword (str/lower-case (get m "FREQ"))))

      (get m "INTERVAL")
      (assoc :ical/interval (parse-int (get m "INTERVAL")))

      (get m "COUNT")
      (assoc :ical/count (parse-int (get m "COUNT")))

      (get m "UNTIL")
      (assoc :ical/until (parse-dt (get m "UNTIL")))

      (get m "BYDAY")
      (assoc :ical/byday
             (mapv #(get str->byday (str/upper-case (str/trim %)))
                   (str/split (get m "BYDAY") #","))))))

;; --- value unescaping (RFC 5545 §3.3.11) ---

(defn- unescape
  "Unescape iCalendar value text sequences."
  [s]
  (-> s
      (str/replace "\\;" ";")
      (str/replace "\\," ",")
      (str/replace "\\n" "\n")
      (str/replace "\\N" "\n")
      (str/replace "\\\\" "\\")))

;; --- value escaping (for emit) ---

(defn- escape
  "Escape special characters in a text value."
  [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace ";" "\\;")
      (str/replace "," "\\,")
      (str/replace "\n" "\\n")))

;; --- content line parser ---

(defn- parse-prop-line
  "Split a single unfolded content line into [PROPNAME params-map value-str].
  Returns nil for lines with no colon (e.g., empty lines)."
  [line]
  (let [colon (str/index-of line ":")]
    (when colon
      (let [name-params (subs line 0 colon)
            value       (unescape (subs line (inc colon)))
            semi        (str/index-of name-params ";")
            prop-name   (str/upper-case (if semi (subs name-params 0 semi) name-params))
            params      (when semi
                          (into {} (map (fn [p]
                                         (let [eq (str/index-of p "=")]
                                           (if eq
                                             [(subs p 0 eq) (subs p (inc eq))]
                                             [p ""])))
                                       (str/split (subs name-params (inc semi)) #";"))))]
        [prop-name (or params {}) value]))))

;; --- main parser ---

(defn parse-str
  "iCalendar text → EDN model {:ical/prodid :ical/version :ical/events :ical/todos}.
  Handles RFC 5545 line unfolding (continuation lines starting with SPACE or HT)."
  [ics-text]
  (let [;; RFC 5545 §3.1 unfolding: CRLF/LF followed by whitespace → remove
        unfolded (->> (str/replace ics-text #"\r?\n[ \t]" "")
                      str/split-lines
                      (remove #(str/blank? %)))
        state    (volatile! {:ctx [] :cur nil :cal {:ical/events [] :ical/todos []}})]
    (doseq [line unfolded]
      (when-let [[prop-name _params value] (parse-prop-line line)]
        (let [{:keys [ctx cur cal]} @state]
          (cond
            ;; --- BEGIN ---
            (= prop-name "BEGIN")
            (cond
              (= value "VCALENDAR")
              (vswap! state assoc :ctx [:vcal])

              (= value "VEVENT")
              (vswap! state assoc :ctx [:vcal :vevent] :cur {})

              (= value "VTODO")
              (vswap! state assoc :ctx [:vcal :vtodo] :cur {}))

            ;; --- END ---
            (= prop-name "END")
            (cond
              (= value "VEVENT")
              (do (vswap! state update-in [:cal :ical/events] conj cur)
                  (vswap! state assoc :ctx [:vcal] :cur nil))

              (= value "VTODO")
              (do (vswap! state update-in [:cal :ical/todos] conj cur)
                  (vswap! state assoc :ctx [:vcal] :cur nil))

              (= value "VCALENDAR")
              (vswap! state assoc :ctx []))

            ;; --- VCALENDAR-level properties ---
            (= (last ctx) :vcal)
            (case prop-name
              "PRODID"   (vswap! state assoc-in [:cal :ical/prodid]  value)
              "VERSION"  (vswap! state assoc-in [:cal :ical/version] value)
              nil)

            ;; --- VEVENT properties ---
            (= (last ctx) :vevent)
            (case prop-name
              "UID"         (vswap! state update :cur assoc :ical/uid         value)
              "SUMMARY"     (vswap! state update :cur assoc :ical/summary     value)
              "DESCRIPTION" (vswap! state update :cur assoc :ical/description value)
              "LOCATION"    (vswap! state update :cur assoc :ical/location    value)
              "DTSTART"     (vswap! state update :cur assoc :ical/dtstart     (parse-dt value))
              "DTEND"       (vswap! state update :cur assoc :ical/dtend       (parse-dt value))
              "RRULE"       (vswap! state update :cur assoc :ical/rrule       (parse-rrule value))
              nil)

            ;; --- VTODO properties ---
            (= (last ctx) :vtodo)
            (case prop-name
              "UID"         (vswap! state update :cur assoc :ical/uid         value)
              "SUMMARY"     (vswap! state update :cur assoc :ical/summary     value)
              "DESCRIPTION" (vswap! state update :cur assoc :ical/description value)
              "DTSTART"     (vswap! state update :cur assoc :ical/dtstart     (parse-dt value))
              "DUE"         (vswap! state update :cur assoc :ical/due         (parse-dt value))
              "STATUS"      (vswap! state update :cur assoc :ical/status
                                    (keyword (str/lower-case (str/replace value "_" "-"))))
              nil)))))
    (:cal @state)))

;; --- emit helpers ---

(defn- zero-pad
  "Left-pad integer `n` to `width` digits with zeros."
  [n width]
  (let [s (str n)
        pad (- width (count s))]
    (if (pos? pad)
      (str (apply str (repeat pad "0")) s)
      s)))

(defn- emit-dt
  "Emit a {:y :m :d :hh :mm} map as an iCalendar date-time string."
  [dt]
  (when dt
    (str (zero-pad (:y dt) 4) (zero-pad (:m dt) 2) (zero-pad (:d dt) 2)
         "T" (zero-pad (get dt :hh 0) 2) (zero-pad (get dt :mm 0) 2) "00")))

(def ^:private byday->str
  {:mo "MO" :tu "TU" :we "WE" :th "TH" :fr "FR" :sa "SA" :su "SU"})

(defn- emit-rrule
  "Emit a rule map as an iCalendar RRULE value string."
  [rrule]
  (when rrule
    (str "FREQ=" (str/upper-case (name (:ical/freq rrule)))
         (when-let [i (:ical/interval rrule)] (str ";INTERVAL=" i))
         (when-let [c (:ical/count rrule)]    (str ";COUNT=" c))
         (when-let [u (:ical/until rrule)]    (str ";UNTIL=" (emit-dt u)))
         (when-let [bd (:ical/byday rrule)]
           (str ";BYDAY=" (str/join "," (map byday->str bd)))))))

(defn- prop-line
  "Emit a single `PROPNAME:value` content line."
  [prop-name value]
  (when value (str prop-name ":" value)))

(defn- emit-event [evt]
  (filter some?
          ["BEGIN:VEVENT"
           (prop-line "UID"         (escape (:ical/uid evt)))
           (prop-line "SUMMARY"     (some-> (:ical/summary evt) escape))
           (prop-line "DESCRIPTION" (some-> (:ical/description evt) escape))
           (prop-line "LOCATION"    (some-> (:ical/location evt) escape))
           (prop-line "DTSTART"     (emit-dt (:ical/dtstart evt)))
           (prop-line "DTEND"       (emit-dt (:ical/dtend evt)))
           (prop-line "RRULE"       (emit-rrule (:ical/rrule evt)))
           "END:VEVENT"]))

(defn- emit-todo [td]
  (filter some?
          ["BEGIN:VTODO"
           (prop-line "UID"         (escape (:ical/uid td)))
           (prop-line "SUMMARY"     (some-> (:ical/summary td) escape))
           (prop-line "DESCRIPTION" (some-> (:ical/description td) escape))
           (prop-line "DTSTART"     (emit-dt (:ical/dtstart td)))
           (prop-line "DUE"         (emit-dt (:ical/due td)))
           (prop-line "STATUS"      (some-> (:ical/status td) name str/upper-case
                                            (str/replace "-" "_")))
           "END:VTODO"]))

(defn emit-str
  "iCalendar EDN model → iCalendar text string (RFC 5545 subset)."
  [cal]
  (let [lines (concat
               ["BEGIN:VCALENDAR"]
               (when-let [v (:ical/version cal)] [(str "VERSION:" v)])
               (when-let [p (:ical/prodid cal)]  [(str "PRODID:" p)])
               (mapcat emit-event (:ical/events cal))
               (mapcat emit-todo  (:ical/todos cal))
               ["END:VCALENDAR"])]
    (str (str/join "\r\n" lines) "\r\n")))
