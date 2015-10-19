(ns ^{:author "Daniel Leong"
      :doc "Aileron stub, for when you don't have the real thing"}
  xradar.stubs.aileron
  (:require [stubby.core :refer [defstub]]))

(defstub config-pilot!)
(defstub connect!)
(defstub connected?)
(defstub create-connection)
(defstub disconnect!)
(defstub field)
(defstub handoff-accept)
(defstub handoff-reject)
(defstub handoff!)
(defstub listen)
(defstub request-atis)
(defstub request-metar)
(defstub send!)
(defstub toggle-track!)
(defstub transmit!)
(defstub update!)
