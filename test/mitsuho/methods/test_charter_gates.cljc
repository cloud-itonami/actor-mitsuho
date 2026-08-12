(ns mitsuho.methods.test-charter-gates
  "mitsuho — constitutional-gate conformance tests. Substrate-native Clojure (ADR-2606160842); 1:1 port of pruned test_charter_gates.py."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def ^:private here (.getParentFile (java.io.File. ^String *file*)))
(def ^:private actor-dir (.getParentFile here))
(def ^:private root (.. actor-dir getParentFile getParentFile))
(def ^:private lexdir (java.io.File. root "wire/lexicons"))

(def ^:private non-chemical-preservation
  #{"dried" "canned" "lacto-fermented" "cold-stored" "vacuum-sealed" "freeze-dried"})

(defn- manifest [] (:actor/manifest (clojure.edn/read-string (slurp (java.io.File. root "manifest.edn")))))
(defn- lex [name]
  (let [wire (json/parse-string (slurp (java.io.File. lexdir (str name ".json"))))
        entity (when (and (sequential? wire) (= 1 (count wire))) (first wire))
        defs (when (map? entity)
               (some (fn [[attribute value]]
                       (when (str/ends-with? attribute "/defs") value))
                     entity))]
    ;; wire/lexicons is the JSON projection of canonical Datomic tx-data.
    ;; Nested lexicon defs therefore remain an EDN blob; normalize them back
    ;; to string-keyed data so the gate walkers test the actual contract.
    (if (string? defs)
      (json/parse-string (json/generate-string (edn/read-string defs)))
      wire)))

(defn- collect [doc attr]
  (let [acc (atom {})]
    (letfn [(walk [x parent]
              (cond (map? x) (do (when (and (string? parent) (contains? x attr)) (swap! acc assoc parent (get x attr)))
                                 (doseq [[k v] x] (walk v k)))
                    (sequential? x) (doseq [v x] (walk v parent))))]
      (walk doc nil)) @acc))

(defn- known [doc field] (some-> (get (collect doc "knownValues") field) set))

(defn- required-union [doc]
  (let [acc (atom #{})]
    (letfn [(walk [x] (cond (map? x) (do (when (sequential? (get x "required")) (swap! acc into (get x "required"))) (doseq [v (vals x)] (walk v)))
                            (sequential? x) (doseq [v x] (walk v))))]
      (walk doc)) @acc))

(defn- property-keys [doc]
  (let [acc (atom #{})]
    (letfn [(walk [x] (cond (map? x) (do (when (map? (get x "properties")) (swap! acc into (keys (get x "properties")))) (doseq [v (vals x)] (walk v)))
                            (sequential? x) (doseq [v x] (walk v))))]
      (walk doc)) @acc))

;; ── full gate set ──
(deftest test-all-14-gates-declared
  (let [gates (set (keys (get-in (manifest) ["constitutionalGates" "gates"])))]
    (is (= gates (set (map #(str "G" %) (range 1 15)))))))

;; ── G2 seed sovereignty ──
(deftest test-g2-seed-sovereignty-required
  (let [req (required-union (lex "cropPlanAttestation"))]
    (doseq [field ["seedSourceAttestation" "varietalManifest"]]
      (is (contains? req field)))))

;; ── G6/G7 — pesticide manifest + GMO attestation hooks exist ──
(deftest test-g6-g7-pesticide-and-gmo-hooks
  (let [keys (property-keys (lex "cropPlanAttestation"))]
    (is (contains? keys "pesticideManifest"))
    (is (contains? keys "gmoAttestationCid"))))

;; ── G4 soil regeneration ──
(deftest test-g4-soil-carbon-logged
  (let [req (required-union (lex "harvestAttestation"))]
    (doseq [field ["soilCarbonDeltaTonsCo2Eq" "yieldKgDryMatter" "photoCid" "cropPlanAttestationCid"]]
      (is (contains? req field)))))

;; ── witness quorum + agronomist signature ──
(deftest test-witness-and-agronomist
  (is (contains? (required-union (lex "harvestAttestation")) "attestingRobots"))
  (is (contains? (required-union (lex "cropPlanAttestation")) "attestingAgronomistDid")))

;; ── parcel biodiversity-no-harm + LANDS registry ──
(deftest test-parcel-biodiversity-and-lands
  (let [req (required-union (lex "parcelAttestation"))]
    (doseq [field ["biodiversityNoHarmAttestationCid" "landsRegistryCid"]]
      (is (contains? req field)))))

;; ── non-chemical preservation only ──
(deftest test-preservation-non-chemical
  (is (= non-chemical-preservation (known (lex "foodLotAttestation") "preservationMethod"))))

;; ── N1 — animal product is R4-gated (not R0–R3) ──
(deftest test-n1-animal-product-r4-gated
  (is (contains? (known (lex "silenAgricultureReview") "scope") "n1-animal-product-r4-gate")))
