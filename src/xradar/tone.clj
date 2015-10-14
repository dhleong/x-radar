(ns ^{:author "Daniel Leong"
      :doc "Simple util for generating simple tone sequences"}
  xradar.tone
  (:import [javax.sound.sampled AudioFormat AudioSystem SourceDataLine]))

(def sample-rate 8000.)
(def sample-size-bits 8)
(def channels 1)
(def signed? true)
(def big-endian? false)
(def buffer-size 16384)

(def default-volume 1.0)

(defn tone
  "Start a new tone sequence, or append a new tone
  to an existing one. Expected usage:
  (-> (tone 440 100)
      (pause 100)
      (tone 880 100)
      play)"
  ([hz msecs]
   (tone hz msecs default-volume))
  ([hz msecs volume]
   (if (instance? SourceDataLine hz)
     ;; forward with default volume
     (tone hz msecs volume default-volume)
     ;; initialize a new sdl
     (let [fmt (AudioFormat.
                 sample-rate
                 sample-size-bits
                 channels
                 signed?
                 big-endian?)
           sdl (AudioSystem/getSourceDataLine fmt)]
       (doto sdl
         (.open fmt buffer-size)
         (.start))
       (tone sdl hz msecs volume))))
  ([^SourceDataLine tone-obj hz msecs volume]
   (let [buf (byte-array 1)]
     (doseq [i (range 0 (* sample-size-bits msecs))]
       (let [angle (* (/ i (/ sample-rate hz)) 2 Math/PI)
             v (* (Math/cos angle) 127.0 volume)]
         (aset-byte buf 0 (byte v))
         (.write tone-obj buf 0 1))))
   tone-obj))

(defn pause
  "Create a pause in a tone. It is currently
  recommended to NOT pause at the end of a
  sequence, as it will create a gross popping
  sound. It's unclear why."
  [^SourceDataLine tone-obj msecs]
  (tone tone-obj 1 msecs 0))

(defn play
  [tone-obj]
  (future
    (doto tone-obj
      (.drain)
      ;; sleeping a bit ameliorates the 
      ;;  terrible pop if we otherwise
      ;;  (.stop) (.close)
      (Thread/sleep 200)
      (.close))))
