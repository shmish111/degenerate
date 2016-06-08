(ns degenerate.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [cheshire.core :as cheshire]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf])
  (:import (java.util Date)))

(def host-name-char
  "Generate alphanumeric characters."
  (gen/fmap char
            (gen/one-of [(gen/choose 97 122)
                         (gen/choose 48 57)
                         (gen/return 45)])))

(defn host-name-string
  "Generate strings that have a-z, 0-9 and -"
  [minimum maximum]
  (gen/fmap str/join (gen/vector host-name-char minimum maximum)))

(def host-name
  "Generate a valid DNS host name according to RFC 952"
  (gen/not-empty
   (gen/such-that (fn [v] (and (-> v (str/join) count (<= 253))
                               (->> v first #{\. \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} not)))
                  (gen/fmap (partial str/join ".") (gen/vector (host-name-string 1 63) 1 4))
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
                  (remove empty?)
                  flatten
                  (apply hash-map))
            (apply gen/tuple
                   (map (fn [[k v]]
                          (if (instance? Optional k)
                            (gen-maybe-kv (:v k) v)
                            (gen/tuple (gen/return k) v)))
                        m))))