(defn last-elem [lst]
      (if (= (count lst) 1)
          (first lst)
          (last-elem (rest lst))))


(defn shift-alf [alfabet]
      (concat (rest alfabet) (list (first alfabet))))


(defn construct-words [cur-word alf n end-litera]
      (cond 
        (= n 0) (list cur-word)
        (= end-litera (first alf)) (construct-words (.concat cur-word (first alf)) (shift-alf alf) (dec n) (last-elem alf))
        true (concat (construct-words (.concat cur-word (first alf)) (shift-alf alf) (dec n) (last-elem alf))
                     (construct-words cur-word (shift-alf alf) n end-litera))))


(defn construct-words-function [alfabet n]
      (construct-words "" alfabet n (last-elem alfabet)))


(construct-words-function '(a b c d) 3)
