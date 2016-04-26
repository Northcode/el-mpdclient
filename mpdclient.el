(require 'libmpdee)

(defun mpdclient-format-song (song longest)
  (format (concat "%-"
		  (number-to-string (nth 0 longest))
		  "s | %-"
		  (number-to-string (nth 1 longest))
		  "s | %-"
		  (number-to-string (nth 2 longest))
		  "s")
	  (plist-get song 'Title) (plist-get song 'Album)  (plist-get song 'Artist))
  )


(defun mpdclient-format-status (status)
  (concat "Volume: " (number-to-string (plist-get status 'volume)) "%" " Bitrate: " (number-to-string (plist-get status 'bitrate)) " kbps"))

(defun mpdclient-make-elapsed-line (status length)
  (let ((elapsed (plist-get status 'time-elapsed)) (total (plist-get status 'time-total)))
    (let ((percent (/ (* elapsed length) total)))
      (concat (make-string (- percent 1) ?-) ">")
      )
    )
  )

(defun mpdclient-get-playlist-column-lengths (playlist)
  (let ((longest '(0 0 0)))
    (dolist (song playlist)
      (if (< (nth 0 longest) (length (plist-get song 'Title)))
	  (setcar longest (length (plist-get song 'Title)))
	)
      (if (< (nth 1 longest) (length (plist-get song 'Album)))
	  (setcar (cdr longest) (length (plist-get song 'Album)))
	)
      (if (< (nth 2 longest) (length (plist-get song 'Artist)))
	  (setcar (cdr (cdr longest)) (length (plist-get song 'Artist)))
	)
      )
    longest
    ))


(defun mpdclient-display-playlist (playlist longest-columns current-song)
  (insert "Playlist: \n")
  (dolist (song playlist)
    (insert (format
	     "%-3s %s"
	     (if (= current-song (plist-get song 'Id))
		 ">"
	       " ")
	     (concat (mpdclient-format-song song longest-columns) "\n")))
    )
  )

(defun mpdclient-display-current (conn longest)
  (let ((linelength (+ (apply '+ longest) 4)))
    
    ;; (print (mpd-get-status conn))
    (insert (concat "\nNow playing: \n"
		    "    "
		    (mpdclient-format-song (mpd-get-current-song conn) longest) "\n"
		    "    "
		    (format (concat "[%-" (number-to-string linelength) "s]")
			    (mpdclient-make-elapsed-line (mpd-get-status conn) linelength)) "\n"))
    )
  )

(defun mpdclient-print-buffer (conn)
  (let ((column-lengths '(0 0 0)))
    (let ((playlist (mpd-get-playlist-entry conn)))
      (setq column-lengths (mpdclient-get-playlist-column-lengths playlist))
      (mpdclient-display-playlist playlist column-lengths (plist-get (mpd-get-current-song conn) 'Id))
      )
    (mpdclient-display-current conn column-lengths)
    )
  )


(let ((conn (mpd-conn-new "localhost" 6600)))
  (mpd-connect conn)
  (mpdclient-print-buffer conn)
  )




(provide 'mpdclient)
