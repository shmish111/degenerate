(ns degenerate.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [cheshire.core :as cheshire]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf])
  (:import (java.util Date)
           (clojure.lang APersistentMap)))

(def char-safe
  "Selects from all characters (in the unicode basic multilingual plane) excluding 'dodgy' control characters.
  This equates to the ranges 32-126 (ascii) and (160-0xFFFF) (extended Latin onwards)"
  (gen/fmap #(char (if (<= 65 % 159) (- % 33) %)) (gen/choose 65 0xFFFF))

(def host-name-char
  "Generate alphanumeric characters."
  (gen/fmap char
            (gen/one-of [(gen/choose 97 122)
                         (gen/choose 48 57)
                         (gen/return 45)])))

(defn host-name-string
  "Generate strings that have a-z, 0-9 and -"
  [minimum maximum]
  (gen/such-that
   (fn [v] (and
            (->> v first #{\. \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} not)
            (->> v last #{\-} not)))
   (gen/fmap str/join (gen/vector host-name-char minimum maximum))
   100))

(def host-name
  "Generate a valid DNS host name according to RFC 952"
  (gen/not-empty
   (gen/such-that (fn [v] (and (-> v (str/join) count (<= 253))))
                  (gen/fmap (partial str/join \.) (gen/vector (host-name-string 1 63) 1 4))
                  100)))

(def email-local-part
  "Generate an email address local part according to RFC 6531 with the exception of quoted strings
  I.e. david.\"smi th\" is not generated despite it being allowed in the RFC"
  (gen/fmap (partial clojure.string/join \.)
            (gen/vector (gen/fmap str/join
                                  (gen/vector (gen/fmap char
                                                        (gen/such-that #(not (#{34 40 41 44 46 58 59 60 62 64 91 92 93} %))
                                                                       (gen/choose 33 126)))
                                              1 10))
                        1 5)))

(def email
  "email address generator based on https://tools.ietf.org/html/rfc2822#section-3.4
  Characters that can be quoted have been excluded, I don't think it's necessary for most cases.
  The local part is also restricted to maximum 5 parts with maximum 10 characters, just for ease of use.
  This may not be suitable with some systems such as hotmail which doesn't allow many special characters."
  (gen/let [name-part email-local-part
            domain-part host-name]
    (str name-part "@" domain-part)))

(defn custom-email
  "Returns an email generator which based on degenerate.core/email but modified accordign to optional parameters.
  tlds - list of top level domains, one of which will be appended to the email"
  [& {:keys [tlds max-length]}]
  (gen/let [name-part email-local-part
            domain-part host-name
            tld (if (empty? tlds) (gen/return nil) (gen/fmap (partial str ".") (gen/elements tlds)))]
    (str name-part "@" domain-part tld)))

(def url
  (gen/let [protocol (gen/one-of [(gen/return "http") (gen/return "https")])
            host host-name]
    (str protocol "://" host)))

(def clojure-hash-map
  "random clojure map generator"
  (gen/such-that map? gen/any 20))

(def json
  "JSON generator, the root is either a map or an array"
  (gen/fmap cheshire/encode gen/any))                       ;; gen/any only generates maps, lists and vectors

(def finite-double
  (gen/such-that #(not (Double/isInfinite %)) gen/double))

(def currency
  "generates a string that is a double with 2 decimal places"
  (gen/fmap #(Double/parseDouble (format "%.2f" %))
            (gen/double* {:infinite false :NaN? false :min 1})))

(def phone-number
  "generates a string that starts 07 or 08 and has 9 digits"
  (gen/fmap #(str "0" %) (gen/large-integer* {:min 70000000 :max 80000000})))

(defn date
  "Create a generator that generates a string date formatted using clj-time
  since - the earliest timestamp ms that the date should be, defaults to 0 (unix epoch)
  until - the latest timestamp ms that the date should be, defaults to now
  format - clj-time format, defaults to :date-time-no-ms"
  [& {:keys [since until format]}]
  (gen/let [^Long n (gen/choose (or since 0) (or until (.getTime (Date.))))]
    (tf/unparse (tf/formatters (or format :date-time-no-ms)) (tc/from-date (Date. n)))))

(defn str
  "Returns a string generator based on various options
  (gen/generate (str)) => \"some crazy string based on gen/char\"
  (gen/generate (str :fixed-length 22)) => \"a string of lenghth 22\"
  (gen/generate (str :max-length 50)) => \"a string between 0 and 50 chars\"
  (gen/generate (str :min-lenght 5)) => \"a string between 5 and MAX_INTEGER chars\"
  (gen/generate (str :min-lenght 5 :max-length 50)) => \"a string between 5 and 50 chars\"
  (gen/generate (str :char-gen gen/char-alphanumeric)) => \"an alphanumeric string\""
  [& {:keys [fixed-length min-length max-length char-gen]}]
  (let [char-gen (or char-gen gen/char)]
    (cond
      fixed-length
      (gen/fmap clojure.string/join (gen/vector char-gen fixed-length))

      (and min-length max-length)
      (gen/fmap clojure.string/join (gen/vector char-gen min-length max-length))

      max-length
      (gen/fmap clojure.string/join (gen/vector char-gen 0 max-length))

      min-length
      (gen/fmap clojure.string/join (gen/vector char-gen min-length (Integer/MAX_VALUE)))

      :else
      (gen/fmap clojure.string/join (gen/vector char-gen)))))

(defrecord Optional [v])

(def optional-key ->Optional)

(defn gen-maybe-kv
  "Either generate a tuple of key and value or an empty tuple"
  [k gen-v]
  (gen/one-of [(gen/return [])
               (gen/tuple (gen/return k) gen-v)]))

(defn map->generator
  "a hash map generator, takes a map of keys and generators
  key can be optional by wrapping in `optional-key` e.g.
  (map->generator {:mandatory-key gen/string (optional-key :a-different-key) gen/int}"
  [m]
  (gen/fmap #(->> %
                  (mapcat identity)
                  (apply hash-map))
            (apply gen/tuple
                   (map (fn [[k v]]
                          (let [v (if (instance? APersistentMap v)
                                    (map->generator v)
                                    v)]
                            (if (instance? Optional k)
                              (gen-maybe-kv (:v k) v)
                              (gen/tuple (gen/return k) v))))
                        m))))

(defn vector
  "the same as clojure.test.check.generators/vector except that you can give it a map instead of a generator where the
  map is a map of keys and generators as used by map->generator"
  [v & args]
  (let [generator (if (instance? APersistentMap v)
                    (map->generator v)
                    v)]
    (apply gen/vector generator args)))

(def country-gen-2
  "Generates 2 character country iso codes"
  (gen/fmap first
            (gen/shuffle ["AF"
                          "AL"
                          "DZ"
                          "AD"
                          "AO"
                          "AG"
                          "AR"
                          "AM"
                          "AU"
                          "AT"
                          "AZ"
                          "BS"
                          "BH"
                          "BD"
                          "BB"
                          "BY"
                          "BE"
                          "BZ"
                          "BJ"
                          "BT"
                          "BO"
                          "BA"
                          "BW"
                          "BR"
                          "BN"
                          "BG"
                          "BF"
                          "BI"
                          "KH"
                          "CM"
                          "CA"
                          "CV"
                          "CF"
                          "TD"
                          "CL"
                          "CN"
                          "CO"
                          "KM"
                          "CD"
                          "CG"
                          "CR"
                          "CI"
                          "HR"
                          "CU"
                          "CY"
                          "CZ"
                          "DK"
                          "DJ"
                          "DM"
                          "DO"
                          "EC"
                          "EG"
                          "SV"
                          "GQ"
                          "ER"
                          "EE"
                          "ET"
                          "FJ"
                          "FI"
                          "FR"
                          "GA"
                          "GM"
                          "GE"
                          "DE"
                          "GH"
                          "GR"
                          "GD"
                          "GT"
                          "GN"
                          "GW"
                          "GY"
                          "HT"
                          "HN"
                          "HU"
                          "IS"
                          "IN"
                          "ID"
                          "IR"
                          "IQ"
                          "IE"
                          "IL"
                          "IT"
                          "JM"
                          "JP"
                          "JO"
                          "KZ"
                          "KE"
                          "KI"
                          "KP"
                          "KR"
                          "KW"
                          "KG"
                          "LA"
                          "LV"
                          "LB"
                          "LS"
                          "LR"
                          "LY"
                          "LI"
                          "LT"
                          "LU"
                          "MK"
                          "MG"
                          "MW"
                          "MY"
                          "MV"
                          "ML"
                          "MT"
                          "MH"
                          "MR"
                          "MU"
                          "MX"
                          "FM"
                          "MD"
                          "MC"
                          "MN"
                          "ME"
                          "MA"
                          "MZ"
                          "MM"
                          "NA"
                          "NR"
                          "NP"
                          "NL"
                          "NZ"
                          "NI"
                          "NE"
                          "NG"
                          "NO"
                          "OM"
                          "PK"
                          "PW"
                          "PA"
                          "PG"
                          "PY"
                          "PE"
                          "PH"
                          "PL"
                          "PT"
                          "QA"
                          "RO"
                          "RU"
                          "RW"
                          "KN"
                          "LC"
                          "VC"
                          "WS"
                          "SM"
                          "ST"
                          "SA"
                          "SN"
                          "RS"
                          "SC"
                          "SL"
                          "SG"
                          "SK"
                          "SI"
                          "SB"
                          "SO"
                          "ZA"
                          "ES"
                          "LK"
                          "SD"
                          "SR"
                          "SZ"
                          "SE"
                          "CH"
                          "SY"
                          "TJ"
                          "TZ"
                          "TH"
                          "TL"
                          "TG"
                          "TO"
                          "TT"
                          "TN"
                          "TR"
                          "TM"
                          "TV"
                          "UG"
                          "UA"
                          "AE"
                          "GB"
                          "US"
                          "UY"
                          "UZ"
                          "VU"
                          "VA"
                          "VE"
                          "VN"
                          "YE"
                          "ZM"
                          "ZW"
                          "GE"
                          "TW"
                          "AZ"
                          "CY"
                          "MD"
                          "SO"
                          "GE"
                          "AU"
                          "CX"
                          "CC"
                          "AU"
                          "HM"
                          "NF"
                          "NC"
                          "PF"
                          "YT"
                          "GP"
                          "GP"
                          "PM"
                          "WF"
                          "TF"
                          "PF"
                          "BV"
                          "CK"
                          "NU"
                          "TK"
                          "GG"
                          "IM"
                          "JE"
                          "AI"
                          "BM"
                          "IO"
                          "VG"
                          "KY"
                          "FK"
                          "GI"
                          "MS"
                          "PN"
                          "SH"
                          "GS"
                          "TC"
                          "MP"
                          "PR"
                          "AS"
                          "UM"
                          "GU"
                          "UM"
                          "UM"
                          "UM"
                          "UM"
                          "UM"
                          "UM"
                          "UM"
                          "VI"
                          "UM"
                          "HK"
                          "MO"
                          "FO"
                          "GL"
                          "GF"
                          "GP"
                          "MQ"
                          "RE"
                          "AX"
                          "AW"
                          "AN"
                          "SJ"
                          "AC"
                          "TA"
                          "AQ"
                          "AQ"
                          "AQ"
                          "AQ"
                          "AQ"])))

(def country-gen-3
  "Generates 3 character country iso codes"
  (gen/fmap first
            (gen/shuffle ["AFG"
                          "ALB"
                          "DZA"
                          "ASM"
                          "AND"
                          "AGO"
                          "AIA"
                          "ATA"
                          "ATG"
                          "ARG"
                          "ARM"
                          "ABW"
                          "AUS"
                          "AUT"
                          "AZE"
                          "BHS"
                          "BHR"
                          "BGD"
                          "BRB"
                          "BLR"
                          "BEL"
                          "BLZ"
                          "BEN"
                          "BMU"
                          "BTN"
                          "BOL"
                          "BES"
                          "BIH"
                          "BWA"
                          "BVT"
                          "BRA"
                          "IOT"
                          "BRN"
                          "BGR"
                          "BFA"
                          "BDI"
                          "KHM"
                          "CMR"
                          "CAN"
                          "CPV"
                          "CYM"
                          "CAF"
                          "TCD"
                          "CHL"
                          "CHN"
                          "CXR"
                          "CCK"
                          "COL"
                          "COM"
                          "COG"
                          "COD"
                          "COK"
                          "CRI"
                          "HRV"
                          "CUB"
                          "CUW"
                          "CYP"
                          "CZE"
                          "CIV"
                          "DNK"
                          "DJI"
                          "DMA"
                          "DOM"
                          "ECU"
                          "EGY"
                          "SLV"
                          "GNQ"
                          "ERI"
                          "EST"
                          "ETH"
                          "FLK"
                          "FRO"
                          "FJI"
                          "FIN"
                          "FRA"
                          "GUF"
                          "PYF"
                          "ATF"
                          "GAB"
                          "GMB"
                          "GEO"
                          "DEU"
                          "GHA"
                          "GIB"
                          "GRC"
                          "GRL"
                          "GRD"
                          "GLP"
                          "GUM"
                          "GTM"
                          "GGY"
                          "GIN"
                          "GNB"
                          "GUY"
                          "HTI"
                          "HMD"
                          "VAT"
                          "HND"
                          "HKG"
                          "HUN"
                          "ISL"
                          "IND"
                          "IDN"
                          "IRN"
                          "IRQ"
                          "IRL"
                          "IMN"
                          "ISR"
                          "ITA"
                          "JAM"
                          "JPN"
                          "JEY"
                          "JOR"
                          "KAZ"
                          "KEN"
                          "KIR"
                          "PRK"
                          "KOR"
                          "KWT"
                          "KGZ"
                          "LAO"
                          "LVA"
                          "LBN"
                          "LSO"
                          "LBR"
                          "LBY"
                          "LIE"
                          "LTU"
                          "LUX"
                          "MAC"
                          "MKD"
                          "MDG"
                          "MWI"
                          "MYS"
                          "MDV"
                          "MLI"
                          "MLT"
                          "MHL"
                          "MTQ"
                          "MRT"
                          "MUS"
                          "MYT"
                          "MEX"
                          "FSM"
                          "MDA"
                          "MCO"
                          "MNG"
                          "MNE"
                          "MSR"
                          "MAR"
                          "MOZ"
                          "MMR"
                          "NAM"
                          "NRU"
                          "NPL"
                          "NLD"
                          "NCL"
                          "NZL"
                          "NIC"
                          "NER"
                          "NGA"
                          "NIU"
                          "NFK"
                          "MNP"
                          "NOR"
                          "OMN"
                          "PAK"
                          "PLW"
                          "PSE"
                          "PAN"
                          "PNG"
                          "PRY"
                          "PER"
                          "PHL"
                          "PCN"
                          "POL"
                          "PRT"
                          "PRI"
                          "QAT"
                          "ROU"
                          "RUS"
                          "RWA"
                          "REU"
                          "BLM"
                          "SHN"
                          "KNA"
                          "LCA"
                          "MAF"
                          "SPM"
                          "VCT"
                          "WSM"
                          "SMR"
                          "STP"
                          "SAU"
                          "SEN"
                          "SRB"
                          "SYC"
                          "SLE"
                          "SGP"
                          "SXM"
                          "SVK"
                          "SVN"
                          "SLB"
                          "SOM"
                          "ZAF"
                          "SGS"
                          "SSD"
                          "ESP"
                          "LKA"
                          "SDN"
                          "SUR"
                          "SJM"
                          "SWZ"
                          "SWE"
                          "CHE"
                          "SYR"
                          "TWN"
                          "TJK"
                          "TZA"
                          "THA"
                          "TLS"
                          "TGO"
                          "TKL"
                          "TON"
                          "TTO"
                          "TUN"
                          "TUR"
                          "TKM"
                          "TCA"
                          "TUV"
                          "UGA"
                          "UKR"
                          "ARE"
                          "GBR"
                          "USA"
                          "UMI"
                          "URY"
                          "UZB"
                          "VUT"
                          "VEN"
                          "VNM"
                          "VGB"
                          "VIR"
                          "WLF"
                          "ESH"
                          "YEM"
                          "ZMB"
                          "ZWE"
                          "ALA"])))
