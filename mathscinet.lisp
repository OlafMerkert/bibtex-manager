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
   #:bibtex-search-uri))

(in-package :bibtex-manager/mathscinet)

;;; retrieve bibtex information by mathscinet ID
(defun bibtex-for-publication-id (publication-id)
  (let1 (doc (uri->html-document (format nil "http://www.ams.org/mathscinet/search/publications.html?fmt=bibtex&pg1=MR&s1=~A" publication-id)))
    (text-content (first (query "div.doc pre" doc)))))

(defun string->bib-entry (string)
  (let ((*bib-database* (make-hash-table :test 'equal)))
   (with-input-from-string (stream string)
     (read-bib-database stream))
   (first (table-values *bib-database*))))

(defun bibtex-entries-for-page (page)
  (mapcar (lambda (pre)
            (le1 (bibtex (text-content pre))
              (cons (string->bib-entry bibtex)
                    bibtex)))
          (query "div.doc pre" page)))

(defmacros! define-bib-entry-accessor (field-name)
  `(defun ,(symb 'bib-entry- field-name) (,g!entry)
     (bib-entry-ref ,(mkstr field-name) ,g!entry)))

(define-bib-entry-accessor author title year)

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
      ("MR Number" . "MR")
      ("Reviewer" . "RVCN")
      ("Anywhere" . "ALLF")
      ("References" . "REFF")))

(defun search-term-code (term)
  (assoc1 (mkstr term) search-term-codes nil :test 'string-equal))

(defun searches->req-parameters (args)
  (let ((args-2 (group args 2))
        (count 3))
    (subseq-
     (mapcan (lambda (a-2)
               (list (keyw :pg (incf count)) (search-term-code (first a-2))
                     (keyw :s count) (second a-2)
                     (keyw :co count) "AND"))
             args-2)
     2)))

(defun bibtex-search-uri (&rest args)
  "Create the URI for the BibTeX download link."
  (uri+ "http://www.ams.org/mathscinet/search/publications.html"
        :bdlall "Retrieve First 50"
        :reqargs (subseq (apply #'uri ""
                                :dr "all" ; time-frame
                                :pg8 "ET" :s8 "All" ; publication type
                                :review_format "html"
                                (searches->req-parameters args))
                         1)
        :batch-title "Selected Matches"
        :fmt "bibtex"))

(defun search-bibtex-entries (&rest search-args)
  "Produce the first 50 results from MathSciNet."
  (let1 (page (uri->html-document (apply #'bibtex-search-uri search-args)))
    (bibtex-entries-for-page page)))

;; (length (search-bibtex-entries :author "Masser"))

