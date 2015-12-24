;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.pprint)

(defn print-table
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows.
   To rename headings ks, use a sequence of [key column-name] as ks."
  {:added "1.3"}
  ([ks rows]
   (when (seq rows)
     (let [col-names-specified (sequential? (first ks))
           col-names (if col-names-specified (map second ks) ks)
           ks (if col-names-specified (map first ks) ks)
           widths (map
                   (fn [k]
                     (apply max
                            (count (str k))
                            (map #(count (str (get % k))) rows)))
                   ks)
           spacers (map #(clojure.string/join (repeat % "-")) widths)
           fmts (map #(str "%" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (clojure.string/join
                           (interpose divider
                                      (for [[col fmt]
                                            (map vector
                                                 (map #(get row %) ks)
                                                 fmts)]
                                        (format fmt (str col)))))
                          trailer))]
       (println)
       (println (fmt-row "| " " | " " |" (zipmap ks col-names)))
       (println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (doseq [row rows]
         (println (fmt-row "| " " | " " |" row))))))
  ([rows] (print-table (keys (first rows)) rows)))
