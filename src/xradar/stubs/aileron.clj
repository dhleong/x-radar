(ns ^{:author "Daniel Leong"
      :doc "Aileron stub, for when you don't have the real thing"}
  xradar.stubs.aileron
  (:require [stubby.core :refer [defstub]]))

(defstub config-pilot!)
(defstub connect!)
(defstub create-connection)
(defstub disconnect!)
(defstub listen)
(defstub request-atis)
(defstub send!)
(defstub transmit!)
(defstub update!)
