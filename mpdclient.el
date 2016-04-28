;;; mpdclient -- Mpd client buildn't on libmpdee
;;; Commentary:
;;; Mpd client buildt on libmpdee

;;; Code:

(if (not (featurep 'libmpdee))
    (package-install 'libmpdee)
  )

(require 'libmpdee)

(defun mpdclient-format-song (song longest)
  "Format SONG for playlist printing.  Use LONGEST as array of longest columns."
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
  "Format mpdclient STATUS line."
  (concat "Volume: " (number-to-string (plist-get status 'volume)) "%" " Bitrate: " (number-to-string (plist-get status 'bitrate)) " kbps"))

(defun mpdclient-make-elapsed-line (status length)
  "Make elapsed line from STATUS with LENGTH as max-length line should be."
  (let ((elapsed (plist-get status 'time-elapsed)) (total (plist-get status 'time-total)))
    (let ((percent (/ (* elapsed length) total)))
      (concat (make-string (- percent 1) ?-) ">")
      )
    )
  )

(defun mpdclient-get-playlist-column-lengths (playlist)
  "Get array of longest columns for PLAYLIST."
  (let ((longest '(0 0 0)))
    ;; (print "YES")
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
  "Display PLAYLIST using LONGEST-COLUMNS and use CURRENT-SONG to check if song is current."
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
  "Get current song from CONN and display with LONGEST as column lengths."
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
  "Print out mpdclient buffer from connection CONN."
  (let ((column-lengths '(0 0 0)) (inhibit-read-only t))
    (let ((playlist (mpd-get-playlist-entry conn)))
      (setq mpdclient-playlist playlist)
      (setq column-lengths (mpdclient-get-playlist-column-lengths playlist))
      (mpdclient-display-playlist playlist column-lengths (plist-get (mpd-get-current-song conn) 'Id))
      )
    (mpdclient-display-current conn column-lengths)
    )
  )

;; Define mode

(define-derived-mode mpdclient-mode special-mode
  "mpdclient"
  :map 'mpdclient-mode-map
  )

(defgroup mpdclient nil
  "Settings for mpdclient"
  :group 'convenience)

(defcustom mpdclient-host "localhost" "Host to connect to with mpdclient."
  :group 'mpdclient
  :type 'string)

(defcustom mpdclient-port 6600 "Port to connect to with mpdclient."
  :group 'mpdclient
  :type 'number
  )

(defvar mpdclient-mode-map nil "MpdClient Keymap.")

(unless mpdclient-mode-map
  (setq mpdclient-mode-map
	(let ((map (make-sparse-keymap)))

	  (define-key map (kbd "j") 'mpdclient-playlist-next)
	  (define-key map (kbd "k") 'mpdclient-playlist-prev)
	  (define-key map (kbd "g") 'mpdclient-redisplay)
	  (define-key map (kbd "p") 'mpdclient-toggle-play)

	  map
	  ))

  )


(defun mpdclient-connect ()
  "Connect mpdclient to mpd and store in local var."
  (let ((mpdconn (mpd-conn-new mpdclient-host mpdclient-port)))
    (set (make-local-variable 'mpdclient-conn) mpdconn)
    ))

(defun mpdclient-init ()
  "Initialize variables for mpdclient."
  (set (make-local-variable 'mpdclient-playlist-pos) 0)
  (set (make-local-variable 'mpdclient-playlist) '())
  )

(defun mpdclient-redisplay ()
  "Redisplay mpdclient buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mpdclient-print-buffer mpdclient-conn)
      )
    )
  (move-to-window-line (+ mpdclient-playlist-pos 1))
  )

(defun mpdclient-playlist-next ()
  "Move cursor to next entry in the playlist."
  (interactive)
  (if (< mpdclient-playlist-pos (- (length mpdclient-playlist) 1))
      (progn
	(setq mpdclient-playlist-pos (+ 1 mpdclient-playlist-pos))
	(next-line))
    (progn
      (message "End of playlist"))
    ))

(defun mpdclient-playlist-prev ()
  "Move cursor to previous entry in the playlist."
  (interactive)
  (if (> mpdclient-playlist-pos 0)
      (progn
	(setq mpdclient-playlist-pos (- mpdclient-playlist-pos 1))
	(previous-line))
    (progn
      (message "Begining of playlist"))
    ))

(defun mpdclient-play ()
  "Start playing music!"
  (interactive)
  (mpd-play mpdclient-conn)
  )

(defun mpdclient-pause ()
  "Pause the music."
  (interactive)
  (mpd-pause mpdclient-conn)
  )

(defun mpdclient-toggle-play ()
  "Toggle between playing and not."
  (interactive)
  (if (eql (plist-get (mpd-get-status mpdclient-conn) 'state) 'play)
      (mpdclient-pause)
    (mpdclient-play))
  )

(defun mpdclient-open ()
  "Open mpdclient."
  (interactive)
  (switch-to-buffer "*mpdclient*")
  (mpdclient-mode)
  (mpdclient-init)
  (hl-line-mode t)
  (mpdclient-connect)
  (mpdclient-redisplay)
  )

(provide 'mpdclient)
;;; mpdclient.el ends here
