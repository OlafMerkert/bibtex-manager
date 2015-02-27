(defpackage :bibtex-manager/mathscinet
  (:nicknames :mathscinet)
  (:use :cl :ol
        :iterate
        :web-utils
        :css-selectors
        :bibtex-runtime)
  (:export
   #:publication-ids-on-page
   #:bibtex-for-publication-id
   #:string->bib-entry
   #:bib-entry-author
   #:bib-entry-title
   #:bib-entry-year
   #:search-bibtex-entries
   #:bibtex-search-uri
   #:search-bibtex-entries/fallbacks
   #:bib-entry-doi
   #:bib-entry-editor
   #:bib-entry-mrnumber))

(in-package :bibtex-manager/mathscinet)

;;; retrieve bibtex information by mathscinet ID
(defun bibtex-for-publication-id (publication-id)
  (let1 (doc (uri->html-document (format nil "http://www.ams.org/mathscinet/search/publications.html?fmt=bibtex&pg1=MR&s1=~A" publication-id)))
    (text-content (first (query "div.doc pre" doc)))))

(defun string->bib-entry (string &optional no-autokey)
  (let ((*bib-database* (make-hash-table :test 'equal)))
    (with-input-from-string (stream string)
      (read-bib-database stream))
    (aprog1 (first (table-values *bib-database*))
      (unless no-autokey
        (setf (bib-entry-cite-key it) (generate-autokey it))))))

(defun bibtex-entries-for-page (page)
  (mapcar (lambda (pre)
            (le1 (bibtex (text-content pre))
              (string->bib-entry bibtex)))
          (query "div.doc pre" page)))

(defmacros! define-bib-entry-accessor (field-name)
  `(defun ,(symb 'bib-entry- field-name) (,g!entry)
     (bib-entry-ref ,(mkstr field-name) ,g!entry)))

(define-bib-entry-accessor author title year doi editor)

(memodefun bib-entry-mrnumber (entry)
  (values (parse-integer (bib-entry-ref "mrnumber" entry)
                         :junk-allowed t)))

(defun subseq- (seq count)
  "Remove `count' elements from `seq'."
  (subseq seq 0 (- (length seq) count)))

;; extracting publication ids from results page (currently not used)
(defun remove-MR (string)
  (parse-integer string :start 2))

(defun publication-ids-on-page (page)
  (mapcar (compose/names remove-MR text-content)
          (query "div.headline .mrnum strong" page)))

;;; build urls for searching
(defpar search-term-codes
    '(("Author" . "AUCN")
      ("Author/Related" . "ICN")
      ("Title" . "TI")
      ("Review Text" . "RT")
      ("Journal" . "JOUR")
      ("Institution Code" . "IC")
      ("Series" . "SE")
      ("MSC Primary/Secondary" . "CC")
      ("MSC Primary" . "PC")
      ("MRNumber" . "MR")
      ("Reviewer" . "RVCN")
      ("Anywhere" . "ALLF")
      ("References" . "REFF")))

(defun search-term-code (term)
  (assoc1 (mkstr term) search-term-codes nil :test 'string-equal))

(defpar remove-characters ",;.-_") ; these are replaced by spaces

(defun sanitise-string (string)
  (map 'string (lambda (char) (if (position char remove-characters :test #'char=)
                             #\Space char)) string))

(defun searches->req-parameters (args)
  (let ((args-2 (group args 2))
        (count 3))
    (subseq-
     (mapcan (lambda (a-2)
               (dbind (key value) a-2
                 (when (integerp value)
                   (setf value (mkstr value)))
                 (when (and (stringp value)
                            (not (length=0 value)))
                   (list (keyw :pg (incf count)) (search-term-code key)
                         (keyw :s count) (sanitise-string value)
                         (keyw :co count) "AND"))))
             args-2)
     2)))

(defun bibtex-search-uri (args)
  "Create the URI for the BibTeX download link."
  (uri+ "http://www.ams.org/mathscinet/search/publications.html"
        :bdlall "Retrieve First 50"
        :reqargs (subseq (apply #'uri ""
                                :dr "all"           ; time-frame
                                :pg8 "ET" :s8 "All" ; publication type
                                :review_format "html"
                                (searches->req-parameters args))
                         1)
        :batch-title "Selected Matches"
        :fmt "bibtex"))

(defun search-bibtex-entries (&rest search-args)
  "Produce the first 50 results from MathSciNet."
  (let1 (page (uri->html-document (bibtex-search-uri search-args)))
    (bibtex-entries-for-page page)))

(defun search-bibtex-entries/fallbacks (&key author title)
  (or (and author
           (or (search-bibtex-entries :author author :title title)
               (search-bibtex-entries :author author)))
      (search-bibtex-entries :title title)))

;; (length (search-bibtex-entries :author "Masser"))

;;; compute a nicer bibtex identifier (as emacs does)

(defpar autokey-names-count 2
        autokey-titles-count 3
        title-terminators "[.!?:;]|--"
        title-fill-words '("A" "An" "[au]nd" "On" "The" "Eine?" "Der" "Die" "Das" "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*"))

(defun autokey-names (names-string)
  (when names-string
    (awhen (mapcar (lambda (x) (cdar (bibtex-name-last x)))
                   (parse-bibtex-name-list names-string))
      (format nil "~{~(~A~)~^-~}" (take autokey-names-count it)))))

(defun split1 (regex target-string)
  (aif (ppcre:scan regex target-string)
       (subseq target-string 0 it)
       target-string))

(defun match-completely-p (regex string)
  "Test if the `regex' matches the entire `string'."
  (mvbind (start end) (ppcre:scan regex string)
    (and start (= 0 start) (= (length string) end))))

(defun word-match-regex (words)
  `(:sequence :start-anchor
              (:alternation ,@(mapcar #`(:regex ,a1) words))
              :end-anchor))

(defun autokey-title (title-string)
  (let* ((title-string (split1 title-terminators title-string))
         (words (ppcre:all-matches-as-strings "\\b\\w+" title-string))
         (meaning-words (remove-if (clambda (ppcre:scan (word-match-regex title-fill-words) x!w)) words)))
    (format nil "~{~(~A~)~^-~}" (take autokey-titles-count meaning-words))))

(defun generate-autokey (entry)
  ;; stolen from [[file:/usr/share/emacs/24.4/lisp/textmodes/bibtex.el.gz::(defun%20bibtex-generate-autokey%20()][file:/usr/share/emacs/24.4/lisp/textmodes/bibtex.el.gz::(defun bibtex-generate-autokey ()]]
  (let ((author (bib-entry-author entry))
        (editor (bib-entry-editor entry))
        (year (bib-entry-year entry))
        (title (bib-entry-title entry)))
    (format nil "~A-~A-~A"
            (or (autokey-names author) (autokey-names editor))
            year
            (autokey-title title))))
