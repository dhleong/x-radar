(ns ^{:author "Daniel Leong"
      :doc "Aileron stub, for when you don't have the real thing"}
  xradar.stubs.aileron
  (:require [xradar.stubs.util :refer [defstub]]))

(defstub create-connection)
(defstub connect!)
(defstub disconnect!)
(defstub listen)
(defstub send!)
(defstub transmit!)
(defstub update!)
