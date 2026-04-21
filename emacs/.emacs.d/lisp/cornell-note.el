;;; cornell-note.el --- Simple Cornell Note taking in Org-mode  -*- lexical-binding: t; -*-

(require 'org)
(require 'ert)
(require 'cl-lib)

;;; Code:

(defun cornell-note-append (&optional topic)
  "Append a Cornell note template to the end of the current buffer.
If TOPIC is provided, use it. Otherwise, prompt the user."
  (interactive "sTopic: ")
  (let ((topic (if (or (null topic) (string-empty-p topic)) "Untitled" topic)))
    (goto-char (point-max))
    (unless (bobp) (newline))
    ;; Main Header
    (insert "* " topic " ")
    (org-insert-time-stamp (current-time) t t) ;; Active timestamp with time
    (insert "\n")
    
    ;; Sections
    (insert "** Cues\n\n")
    (insert "** Notes\n")
    (let ((notes-pos (point)))
      (insert "\n** Summary\n")
      (goto-char notes-pos))))

(defun explain-cornell-note ()
  "Display a help buffer explaining the Cornell Note Taking system."
  (interactive)
  (with-help-window "*Cornell Note Help*"
    (princ "Cornell Notes Template\n")
    (princ "======================\n\n")
    
    (princ "1. Cues (Left Column)\n")
    (princ "   ------------------\n")
    (princ "   - Main Ideas/Vocab: Jot down 1-2 words representing the main idea of a passage.\n")
    (princ "     Leave space between each one.\n")
    (princ "   - Questions: As soon as possible after reading, formulate questions based on the\n")
    (princ "     notes in the right-hand column. Write one under each cue word.\n")
    (princ "     * Clarifies meanings and reveals relationships.\n")
    (princ "     * Establishes continuity and strengthens memory.\n")
    (princ "     * Sets up a perfect stage for exam-studying later.\n\n")

    (princ "2. Notetaking Column (Right Column)\n")
    (princ "   --------------------------------\n")
    (princ "   A. Record: While reading, use this column to record supporting facts, ideas,\n")
    (princ "      dates, etc., using telegraphic sentences.\n\n")
    (princ "   B. Recite: Cover this column with a sheet of paper. Looking at the Questions\n")
    (princ "      or Cue-words only, say aloud, in your own words, the answers/facts indicated.\n\n")
    (princ "   C. Reflect: Ask yourself:\n")
    (princ "      * What's the significance of these facts?\n")
    (princ "      * What principle are they based on?\n")
    (princ "      * How can I apply them?\n")
    (princ "      * How do they fit with what I already know? What's beyond them?\n\n")
    (princ "   D. Review: Spend at least 10 minutes every week reviewing all previous notes.\n")
    (princ "      This ensures retention for current use and exams.\n\n")

    (princ "3. Summary (Bottom Section)\n")
    (princ "   ------------------------\n")
    (princ "   Use this space to write a two or three sentence summary of what you just read.\n\n")
    
    (princ "Press 'q' to close this window.\n")))

;;; Tests (Property Based Testing)
;;
;; To run the tests from the command line:
;; emacs -Q --batch -l cornell-note.el -f ert-run-tests-batch-and-exit

(defun cornell-note--random-string ()
  "Generate a random alphanumeric string."
  (let ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string (cl-loop repeat (+ 5 (random 10))
                          collect (aref alnum (random (length alnum)))))))

(ert-deftest cornell-note-pbt ()
  "Property Based Test: Verify Cornell note structure integrity."
  (dotimes (_ 10) ;; Run 10 randomized trials
    (let ((test-topic (cornell-note--random-string)))
      (with-temp-buffer
        (org-mode)
        (cornell-note-append test-topic)
        
        ;; Move to beginning to start checks
        (goto-char (point-min))
        
        ;; Property 1: Main Topic exists
        (should (search-forward (concat "* " test-topic) nil t))
        
        ;; Property 2: Timestamp exists (checking for brackets)
        (should (re-search-forward "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t))
        
        ;; Property 3: Cues section exists
        (should (search-forward "** Cues" nil t))
        
        ;; Property 4: Notes section exists
        (should (search-forward "** Notes" nil t))
        
        ;; Property 5: Summary section exists
        (should (search-forward "** Summary" nil t))
        
        ;; Property 6: Order verification (Cues < Notes < Summary)
        (goto-char (point-min))
        (let ((pos-cues (search-forward "** Cues" nil t))
              (pos-notes (search-forward "** Notes" nil t))
              (pos-summary (search-forward "** Summary" nil t)))
          (should (< pos-cues pos-notes))
          (should (< pos-notes pos-summary)))))))

(ert-deftest cornell-note-explain-test ()
  "Verify explain-cornell-note creates a help buffer with correct content."
  (explain-cornell-note)
  (let ((buf (get-buffer "*Cornell Note Help*")))
    (should buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (should (search-forward "Cues" nil t))
      (should (search-forward "Notes" nil t))
      (should (search-forward "Summary" nil t)))))

(defun cornell-note-run-tests ()
  "Run the PBT suite for cornell-note."
  (interactive)
  (ert 'cornell-note-pbt))

(provide 'cornell-note)
;;; cornell-note.el ends here
