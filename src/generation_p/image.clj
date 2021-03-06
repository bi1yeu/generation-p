(ns generation-p.image
  (:require [clojure.java.io :as io]
            [generation-p.model :as m])
  (:import
   (java.awt.image AffineTransformOp BufferedImage WritableRaster)
   (java.awt.geom AffineTransform)
   (javax.imageio ImageIO)))

(def ^:const display-scale-factor 20.0)
;; images are square
(def ^:const img-width 32)
(def ^:const num-channels 3)

;; https://lospec.com/palette-list/japanese-woodblock
(def ^:const ^:private palette--japanesewoodblock
  [
   [0x2b 0x28 0x21]
   ;; [0x62 0x4c 0x3c]
   ;; [0xd9 0xac 0x8b]
   [0xe3 0xcf 0xb4]
   ;; [0x24 0x3d 0x5c]
   ;; [0x5d 0x72 0x75]
   ;; [0x5c 0x8b 0x93]
   ;; [0xb1 0xa5 0x8d]
   ;; [0xb0 0x3a 0x48]
   ;; [0xd4 0x80 0x4d]
   ;; [0xe0 0xc8 0x72]
   ;; [0x3e 0x69 0x58]
   ])

(def ^:const palette palette--japanesewoodblock)

(defn- save-img [img-name ^BufferedImage bi]
  (let [filename (format "output/%s.png" img-name)]
    (ImageIO/write bi "png" (io/file filename))
    filename))

;; https://stackoverflow.com/a/4216635
(defn- scale-up [^BufferedImage bi]
  (let [width    (* display-scale-factor (.getWidth bi))
        height   (* display-scale-factor (.getHeight bi))
        after    (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        xform    (AffineTransform.)
        _        (.scale xform display-scale-factor display-scale-factor)
        scale-op (AffineTransformOp. xform AffineTransformOp/TYPE_NEAREST_NEIGHBOR)]
    (.filter scale-op bi after)))

(defn- vec->image
  ([pixel-vec]
   ;; assume square image
   (let [dim (-> pixel-vec
                 count
                 Math/sqrt
                 int)]
     (vec->image pixel-vec dim dim)))
  ([pixel-vec width height]
   ;; via https://stackoverflow.com/a/125013/1181141
   (let [image  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
         raster (.getRaster image)]
     (.setPixels ^WritableRaster raster 0 0 width height (int-array (flatten pixel-vec)))
     image)))

(defn save-individual-as-image
  ([individual]
   (save-individual-as-image individual (::m/id individual)))
  ([individual filename]
   (->> individual
        ::m/chromosome
        vec->image
        scale-up
        (save-img filename))))

(comment
  ;; some utility functions used for experimentation

  (defn- solid-gray-vec [width height val]
    (partition num-channels (repeat (* width height num-channels) val)))

  (defn- black-vec [width height]
    (solid-gray-vec width height 0))

  (defn- white-vec [width height]
    (solid-gray-vec width height 255))

  (save-img "black-img" (scale-up (vec->image (black-vec 32 32))))
  (save-img "white-img" (scale-up (vec->image (white-vec 32 32))))

  )
