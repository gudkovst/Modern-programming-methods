(defn ext ([cur-word alfabet] (ext cur-word alfabet (list)))
          ([cur-word alfabet res-list]
            (cond
              (= (count alfabet) 0) res-list
              (.endsWith (str cur-word) (first alfabet)) (recur cur-word (rest alfabet) res-list)
              true (recur cur-word (rest alfabet) (concat res-list (list (.concat (str cur-word) (first alfabet))))))))


(defn extension ([cur-list alfabet] (extension cur-list alfabet (list)))
                ([cur-list alfabet new-list]
                  (if (= (count cur-list) 0)
                       new-list
                       (recur (rest cur-list) alfabet (concat new-list (ext (first cur-list) alfabet))))))


(defn construct-words-function ([alfabet n] (if (= n 0) (list) (construct-words-function alfabet n alfabet)))
                               ([alfabet n cur-list]
                                 (if (= n 1)
                                     cur-list
                                     (recur alfabet (dec n) (extension cur-list alfabet)))))


(construct-words-function '(a b c d) 3)
