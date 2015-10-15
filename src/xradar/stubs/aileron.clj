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
(defstub listen)
(defstub request-atis)
(defstub request-metar)
(defstub send!)
(defstub transmit!)
(defstub update!)
