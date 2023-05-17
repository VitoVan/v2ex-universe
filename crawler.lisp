
(ql:quickload '(:drakma :lquery :str :local-time :yason))

(defparameter *v2ex-root* "https://www.v2ex.com")
(defparameter *v2ex-api-root* (str:concat *v2ex-root* "/api/v2/"))
(defparameter *v2ex-default-avatar* (str:concat *v2ex-root* "/static/img/node_default_xlarge.png?v=e18935027ba50bf52abdf1bafdbd2769"))
(defparameter *use-cache* t)
;; (defparameter *dir-name* (pathname (str:concat (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day)) "/")))
(defparameter *dir-name* #P"2023-5-16/")
(ensure-directories-exist *dir-name*)

;; set proxy on my local machine
(when (str:contains? ".local" (uiop:hostname))
  (setf drakma:*default-http-proxy* '("127.0.0.1" 1087)))

;; tell Drakma that HTTP response content labeled application/json is plain text
;; https://courses.cs.northwestern.edu/325/admin/json-client.php
(push (cons "application" "json") drakma:*text-content-types*)

;; (setf (uiop:getenv "V2EX_TOKEN") "000z000z-0000-0z0z-z0z0-0z00z0z00000")

;; go gentle, sleep 7 seconds for each request
(defparameter *delay* 7)
(defparameter *last-request* 0)

(defun url-to-file (url file &key (api))
  (when (< (- (get-universal-time) *last-request*) *delay*)
    (format t "go gentle for ~A seconds~%" *delay*)
    (sleep *delay*))
  (setf *last-request* (get-universal-time))
  (format t "saving: ~A --> ~A~%" url file)
  (str:to-file file
               (drakma:http-request url
                                    :user-agent "V2EX Universe Crawler (WIP)" ;; make sure Livid could block this easily
                                    :redirect nil
                                    :additional-headers (when api (list (cons "Authorization" (str:concat "Bearer " (uiop:getenv "V2EX_TOKEN"))))))))

(defun object-to-file (object file)
  (str:to-file file (write-to-string object)))

(defun str-to-file (str file)
  (str:to-file file str))

;; download V2EX nodes list
(defparameter *planes-file* (merge-pathnames *dir-name* #p"planes.html"))

(defun get-color-by-continent (continent-name)
  (cond
    ((string= continent-name "Limbo") "#8BD3FF")
    ((string= continent-name "Mechanus") "#D9A769")
    ((string= continent-name "Gray Waste") "#CBCBCB")
    ((string= continent-name "Elysium") "#FFECA6")
    ((string= continent-name "Sigil") "#7DE594")
    (t "red")))

(if (or
     (not *use-cache*) ;; don't use cache, or
     (not (probe-file *planes-file*))) ;; no cache
    (url-to-file "https://www.v2ex.com/planes" *planes-file*)
    (format t "Using cache file: ~A~%" *planes-file*))

(defparameter *planes-string* (str:from-file *planes-file*))

;; get all the node and it's parent continent
(defparameter *planes-dom* (lquery:$ (lquery:initialize *planes-string*)))
(defparameter *planes-boxes-dom* (lquery:$ *planes-dom* "#Main > div.box"))

(defstruct node
  id
  name
  title
  topics
  color
  created
  url
  header
  avatar
  modified
  continent
  parent-node
  child-nodes
  related-nodes
  )

(defmethod yason:encode ((node node) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (node-name node))
      (yason:encode-object-element "index" (node-id node))
      (yason:encode-object-element "title" (node-title node))
      ;; Node object accessor function or attribute for name (shown in label).
      (yason:encode-object-element "name"
                                   (str:concat "<div class='label'>" (node-title node) "</div>"))
      ;; Node object accessor function, attribute or a numeric constant for the node numeric value (affects circle area).
      (yason:encode-object-element "val"
                                   (if (equal (node-continent node) (node-name node))
                                       1 ;; it's a continent
                                       ;; (min (max (/ (node-topics node) 1000) 0.01) 1)
                                       (/ (log (max (node-topics node) 1.01)) 10) ;; (log 1) => 0
                                       ))
      ;; Node object accessor function or attribute for node color (affects circle color).
      (yason:encode-object-element "color"
                                   (let ((color (node-color node)))
                                     ;; if node has color and it's not the default dark, then use it's color
                                     (when (and color (not (string= color "#001D25")))
                                       color)))
      (yason:encode-object-element "created" (node-created node))
      (yason:encode-object-element "url" (str:concat *v2ex-root* (node-url node)))
      (yason:encode-object-element "header" (node-header node))
      (yason:encode-object-element "avatar"
                                   (let ((avatar (node-avatar node)))
                                     (if (str:containsp "node_default_large.png" avatar) *v2ex-default-avatar* avatar)))
      (yason:encode-object-element "modified" (node-modified node))
      (yason:encode-object-element "continent" (node-continent node)))
    ))

(defstruct continent
  cn-name
  en-name
  nodes
  )

(defparameter *continents* nil)

(defun get-continents ()
  ;; collect all continents and their nodes
  (loop
    for box-dom across *planes-boxes-dom*
    ;; skip the one without header, it's the title "xxxx nodes now and growing"
    when (> (length (lquery:$ box-dom "div.header")) 0)
      collect
      (let* (
             ;; assemble continent information
             (header-all (str:trim (elt (lquery:$ box-dom "div.header" (text)) 0)))
             (header-en (str:trim (elt (lquery:$ box-dom "div.header > span.fr" (text)) 0)))
             (en-name (str:trim (elt (str:split "•" header-en) 0)))
             (cn-name (str:replace-all header-en "" header-all))
             (continent (make-continent :cn-name cn-name :en-name en-name :nodes nil))
             ;; assembly nodes information, for this continent
             (nodes-dom (lquery:$ box-dom "div.inner > a.item_node"))
             (nodes
               (loop
                 for node-dom across nodes-dom
                 for title = (elt (lquery:$ node-dom (text)) 0)
                 for url =  (elt (lquery:$ node-dom (attr "href")) 0)
                 collect (make-node :title title :url url :name (str:replace-all "/go/" "" url) :continent en-name)))
             )
        (format t "EN: ~A,CN: ~A, NODES: ~%~A~%" en-name cn-name nodes)
        (setf (continent-nodes continent) nodes)
        continent)))

(setf *continents* (get-continents))

;; now let's loop all the nodes inside all the continents, to
;; 1. get HTML file, for their parent node and related nodes (via HTML, no API available)
;;     https://www.v2ex.com/go/<node-name>
;; 2. get JSON file, for their time of creation, count of topics, avatar, etc... (via API)
;;     https://www.v2ex.com/api/v2/nodes/<node-name>

(loop
  for continent in *continents*
  for nodes = (continent-nodes continent)
  for x from 0
  do
     (format t "~%~%Fetching HTML page for: ~A - ~A~%~%" (continent-cn-name continent) (continent-en-name continent))
     (loop
       for node in nodes
       for i from 1
       for l = (length nodes)
       for url = (node-url node)
       for title = (node-title node)
       for name = (node-name node)
       for html-file = (merge-pathnames *dir-name* (str:concat name ".html"))
       for json-file = (merge-pathnames *dir-name* (str:concat name ".json"))
       for html-url = (str:concat *v2ex-root* url)
       for json-url = (str:concat *v2ex-api-root* "nodes/" name)
       do
          (format t "[~A/~A] Processing: ~A~%" i l  title)
          (if (or
               (not *use-cache*) ;; don't use cache, or
               (not (probe-file html-file))
               (not (probe-file json-file)))  ;; no cache
              (progn
                (url-to-file html-url html-file)
                (url-to-file json-url json-file :api t))
              (format t "Using cache: ~A, ~A~%" html-file json-file)
              )
       ))

;; all files downloaded, now let's parse

(defun parse-downloaded-files ()
  (loop
    for continent in *continents*
    for nodes = (continent-nodes continent)
    for x from 0
    do
       (format t "~%~%Parsing Continent: ~A - ~A~%~%" (continent-cn-name continent) (continent-en-name continent))
       (setf
        (continent-nodes continent)
        (loop
          for node in nodes
          for i from 1
          for l = (length nodes)
          for url = (node-url node)
          for title = (node-title node)
          for name = (node-name node)
          for html-file = (merge-pathnames *dir-name* (str:concat name ".html"))
          for json-file = (merge-pathnames *dir-name* (str:concat name ".json"))
          when (and (probe-file html-file) (probe-file json-file))
            collect
            (labels
                ((get-node-names-from-sidebar-nodes-dom (nodes-dom)
                   (loop
                     for node-dom across (lquery:$ nodes-dom "node-sidebar")
                     collect (elt (lquery:$ node-dom (attr "nodeName")) 0))))
              (format t "~%[~A/~A] Parsing Node: ~A~%HTML: ~A~%JSON: ~A~%" i l title html-file json-file)
              (let* ((html-str (str:from-file html-file))
                     (color
                       (multiple-value-bind (s colors)
                           (cl-ppcre:scan-to-strings "background-color: (#.*);"  html-str)
                         (when s (elt colors 0))))
                     (node-dom (lquery:$ (lquery:initialize html-str)))
                     (parent-node-dom (lquery:$ node-dom "div.inner > node-sidebar"))

                     (related-or-children-label-texts (coerce (lquery:$ node-dom "div.cell-top > span" (text)) 'list))
                     (related-or-children-dom (lquery:$ node-dom "div.nodes-sidebar-container"))
                     (has-related-nodes (member "相关节点" related-or-children-label-texts :test #'str:containsp))
                     (has-child-nodes (member "子节点" related-or-children-label-texts :test #'str:containsp))
                     (related-nodes-dom
                       (if (= (length related-or-children-dom) 2)
                           ;; if there are two, then the first one is realted
                           (elt related-or-children-dom 0)
                           ;; otherwise let's check the label
                           (when has-related-nodes
                             (elt related-or-children-dom 0))))
                     (child-nodes-dom
                       (if (= (length related-or-children-dom) 2)
                           ;; if there are two, then the second one is children
                           (elt related-or-children-dom 1)
                           ;; otherwise let's check the label
                           (when has-child-nodes
                             (elt related-or-children-dom 0))))

                     (parent-node (when (> (length parent-node-dom) 0) (elt (lquery:$ parent-node-dom (attr "nodeName")) 0)))
                     (related-nodes (get-node-names-from-sidebar-nodes-dom related-nodes-dom))
                     (child-nodes (get-node-names-from-sidebar-nodes-dom child-nodes-dom))

                     (node-json (yason:parse (str:from-file json-file)))
                     (node-json-success (gethash "success" node-json))
                     (node-json-result (if node-json-success (gethash "result" node-json) (make-hash-table))))

                (setf
                 (node-id node) (gethash "id" node-json-result)
                 (node-topics node) (gethash "topics" node-json-result)
                 (node-color node) color
                 (node-created node) (gethash "created" node-json-result)
                 (node-header node) (gethash "header" node-json-result)
                 (node-avatar node) (gethash "avatar" node-json-result)
                 (node-modified node) (gethash "last_modified" node-json-result)
                 (node-parent-node node) parent-node
                 (node-related-nodes node) related-nodes
                 (node-child-nodes node) child-nodes)
                (format t "~A~%" node)
                node
                ))
          ))))

(parse-downloaded-files)

(defparameter *continents-file* (merge-pathnames *dir-name* #p"continents.sexp"))
(object-to-file *continents* *continents-file*)

;; now let's assemble the JSON data
;; example: https://github.com/vasturiano/force-graph#input-json-syntax

;; here we swap the definition of `id' and  `name', since we only stored `name' in the relationships
;; check the yason:encode for the implementation
;; `id' -> `index'
;; `name' -> `id'

(defparameter *graph-hash-table* (make-hash-table :test 'equalp))

(setf (gethash "nodes" *graph-hash-table*)
      (loop
        for continent in *continents*
        for continent-name = (continent-en-name continent)
        nconcing
        (cons
         ;; continent as a parent node
         (make-node
          :title (str:concat (continent-en-name continent) " - " (continent-cn-name continent))
          :name continent-name
          :continent (continent-en-name continent)
          :topics 999999
          :avatar
          (cond
            ((string= continent-name "Limbo") "https://cdn.v2ex.com/savatar/c4ca/4238/1_large.png?m=1539167720")
            ((string= continent-name "Mechanus") "https://cdn.v2ex.com/savatar/c81e/728d/2_large.png?m=1638151556")
            ((string= continent-name "Gray Waste") "https://cdn.v2ex.com/savatar/eccb/c87e/3_large.png?m=1553026594")
            ((string= continent-name "Elysium") "https://cdn.v2ex.com/savatar/a87f/f679/4_large.png?m=1539167777")
            ((string= continent-name "Sigil") "https://cdn.v2ex.com/savatar/e4da/3b7f/5_large.png?m=1539167821")
            (t "red"))
          :color (get-color-by-continent continent-name))
         (loop
           for node in (continent-nodes continent)
           collect node))))

(setf (gethash "links" *graph-hash-table* )
      (loop
        for all-node-names = (mapcar #'node-name (gethash "nodes" *graph-hash-table*))
        for continent in *continents*
        nconcing
        (loop
          for node in (continent-nodes continent)
          nconcing
          (remove-if #'null
                     (cons
                      ;; continent as a parent node
                      ;; only link doesn't has a parent, and not a continent: i.e. has `created' attribute
                      (when (and (not (node-parent-node node)) (node-created node))
                        (let ((link (make-hash-table)))
                          (setf
                           (gethash "color" link) (get-color-by-continent (node-continent node))
                           (gethash "source" link) (node-continent node)
                           (gethash "target" link) (node-name node))
                          link))
                      (loop
                        for link = (make-hash-table)
                        for source = (node-name node)
                        for target in (node-child-nodes node)
                        ;; for target in (append (node-related-nodes node) (node-child-nodes node))
                        do
                           (setf
                            (gethash "color" link) (get-color-by-continent (node-continent node))
                            (gethash "source" link) source
                            (gethash "target" link) target)
                        when (and (member target all-node-names :test #'equal) (member source all-node-names :test #'equal))
                          collect link))))
        ))

(defun write-json-to-file ()
  (str-to-file
   (with-output-to-string (s)
     (yason:encode *graph-hash-table* s))
   "v2ex.json")

  (format t "~%JSON generated ~%"))

(write-json-to-file)
