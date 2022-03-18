(def layout (->> ($ setxkbmap -query) (idx 2) (words) (last)))

(if (= layout "dvorak")
    ($* (setxkbmap us)
        (notify-send -t 750 "レイアウトをusに変更しました"))
  ($* (setxkbmap dvorak)
      (notify-send -t 750 "レイアウトをdvorakに変更しました")))
