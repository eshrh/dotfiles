(defun extract (vid sub)
  (let [[bvid bsub] (map basename [vid sub])
        tmpsub (s+ "/tmp/" bvid ".srt")
        renamedsub (s+ bsub "_retimed.srt")]
    ($*
     (ffmpeg -i $vid -map "0:s:0" $tmpsub -y)
     (ffsubsync $tmpsub -i $sub -o $renamedsub)
     (memento $vid --sub-file= $<> $renamedsub)
     :sh)))

(cli (extract (fst args) (snd args)))
