#lang racket/base
(require racket/match racket/string racket/list racket/file racket/date racket/format racket/port)

;; ============================================================================
;; kb — A text-first kanban tool
;; Data format: s-expressions (native to Racket, trivially parseable)
;; ============================================================================

(define kb-file (make-parameter "kb.dat"))

;; --- Data structures ---
(struct item (id type title status tags desc deps assignee priority created modified history) #:transparent)
(struct board (name lanes) #:transparent)

(define default-board (board 'default '(backlog doing review done)))

;; --- Utilities ---
(define (today) (date->string (current-date) #t))

(define (gen-id items)
  (define nums (filter-map (λ (i)
    (define m (regexp-match #rx"KAN-([0-9]+)" (symbol->string (item-id i))))
    (and m (string->number (cadr m)))) items))
  (string->symbol (format "KAN-~a" (add1 (if (null? nums) 0 (apply max nums))))))

;; --- Parser (s-expressions) ---
(define (parse-file path)
  (if (file-exists? path)
      (call-with-input-file path
        (λ (in)
          (let loop ([boards '()] [items '()])
            (define expr (read in))
            (if (eof-object? expr)
                (values (if (null? boards) (list default-board) boards) items)
                (match expr
                  [`(board ,name ,lanes) (loop (cons (board name lanes) boards) items)]
                  [`(item ,id . ,fields) (loop boards (cons (parse-item id fields) items))]
                  [_ (loop boards items)])))))
      (values (list default-board) '())))

(define (parse-item id fields)
  (define (get key [default #f])
    (define pair (assoc key fields))
    (if pair (cadr pair) default))
  (item id
        (get 'type 'task)
        (get 'title "Untitled")
        (get 'status 'backlog)
        (get 'tags '())
        (get 'desc "")
        (get 'deps '())
        (get 'assignee #f)
        (get 'priority #f)
        (get 'created (today))
        (get 'modified (today))
        (get 'history '())))

;; --- Serializer ---
(define (serialize-file path boards items)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (for ([b boards])
        (fprintf out "(board ~a ~s)\n\n" (board-name b) (board-lanes b)))
      (for ([i (reverse items)])
        (serialize-item out i)
        (newline out)))))

(define (serialize-item out i)
  (fprintf out "(item ~a\n" (item-id i))
  (fprintf out "  (type ~a)\n" (item-type i))
  (fprintf out "  (title ~s)\n" (item-title i))
  (fprintf out "  (status ~a)\n" (item-status i))
  (unless (null? (item-tags i))
    (fprintf out "  (tags ~s)\n" (item-tags i)))
  (unless (string=? "" (item-desc i))
    (fprintf out "  (desc ~s)\n" (item-desc i)))
  (unless (null? (item-deps i))
    (fprintf out "  (deps ~s)\n" (item-deps i)))
  (when (item-assignee i)
    (fprintf out "  (assignee ~a)\n" (item-assignee i)))
  (when (item-priority i)
    (fprintf out "  (priority ~a)\n" (item-priority i)))
  (fprintf out "  (created ~s)\n" (item-created i))
  (fprintf out "  (modified ~s)\n" (item-modified i))
  (unless (null? (item-history i))
    (fprintf out "  (history ~s)\n" (item-history i)))
  (fprintf out ")\n"))

;; --- Core operations ---
(define (load-kb) (parse-file (kb-file)))
(define (save-kb boards items) (serialize-file (kb-file) boards items))

(define (find-item items id)
  (findf (λ (i) (equal? (item-id i) id)) items))

(define (update-item items id updater)
  (map (λ (i) (if (equal? (item-id i) id) (updater i) i)) items))

(define (add-history i event)
  (struct-copy item i
    [history (cons (list (today) event) (item-history i))]
    [modified (today)]))

;; --- Blocked status (extension) ---
(define (item-blocked? i items)
  (define deps (item-deps i))
  (and (not (null? deps))
       (ormap (λ (dep-id)
                (define dep (find-item items dep-id))
                (and dep (not (eq? (item-status dep) 'done))))
              deps)))

(define (blocking-deps i items)
  (filter (λ (dep-id)
            (define dep (find-item items dep-id))
            (and dep (not (eq? (item-status dep) 'done))))
          (item-deps i)))

;; --- CLI commands ---
(define (cmd-add args)
  (define-values (boards items) (load-kb))
  (match args
    [(list type title rest ...)
     (define new-id (gen-id items))
     (define opts (parse-opts rest))
     (define new-item
       (item new-id
             (string->symbol type)
             title
             (string->symbol (hash-ref opts 'lane "backlog"))
             (filter-map (λ (s) (and (not (string=? s "")) (string->symbol s)))
                         (string-split (hash-ref opts 'tag "") ","))
             ""
             '()
             #f
             (let ([p (hash-ref opts 'priority #f)]) (and p (string->symbol p)))
             (today)
             (today)
             (list (list (today) "created"))))
     (save-kb boards (cons new-item items))
     (printf "Created ~a: ~a\n" new-id title)]
    [_ (displayln "Usage: kb add <type> <title> [--tag=X] [--priority=X] [--lane=X]")]))

(define (parse-opts args)
  (for/hash ([a args] #:when (string-prefix? a "--"))
    (match (regexp-match #rx"--([^=]+)=(.+)" a)
      [(list _ k v) (values (string->symbol k) v)]
      [_ (values 'unknown "")])))

(define (cmd-move args)
  (match args
    [(list id-str lane-str)
     (define id (string->symbol id-str))
     (define lane (string->symbol lane-str))
     (define-values (boards items) (load-kb))
     (define i (find-item items id))
     (if i
         (let* ([old-status (item-status i)]
                [updated (struct-copy item (add-history i (format "~a -> ~a" old-status lane))
                           [status lane])]
                [new-items (update-item items id (λ (_) updated))])
           (save-kb boards new-items)
           (printf "Moved ~a: ~a -> ~a\n" id old-status lane))
         (printf "Item ~a not found\n" id))]
    [_ (displayln "Usage: kb move <id> <lane>")]))

(define (cmd-show args)
  (match args
    [(list id-str)
     (define id (string->symbol id-str))
     (define-values (boards items) (load-kb))
     (define i (find-item items id))
     (if i
         (begin
           (printf "~a [~a] ~a\n" (item-id i) (item-type i) (item-title i))
           (printf "  status: ~a~a\n" (item-status i)
                   (if (item-blocked? i items) " (BLOCKED)" ""))
           (unless (null? (item-tags i))
             (printf "  tags: ~a\n" (string-join (map symbol->string (item-tags i)) ", ")))
           (when (item-priority i)
             (printf "  priority: ~a\n" (item-priority i)))
           (when (item-assignee i)
             (printf "  assignee: ~a\n" (item-assignee i)))
           (unless (null? (item-deps i))
             (printf "  deps: ~a\n" (string-join (map symbol->string (item-deps i)) ", "))
             (define blocking (blocking-deps i items))
             (unless (null? blocking)
               (printf "  blocked by: ~a\n" (string-join (map symbol->string blocking) ", "))))
           (unless (string=? "" (item-desc i))
             (printf "  desc: ~a\n" (item-desc i)))
           (printf "  created: ~a\n" (item-created i))
           (printf "  modified: ~a\n" (item-modified i))
           (unless (null? (item-history i))
             (printf "  history:\n")
             (for ([h (item-history i)])
               (printf "    ~a: ~a\n" (car h) (cadr h)))))
         (printf "Item ~a not found\n" id))]
    [_ (displayln "Usage: kb show <id>")]))

(define (cmd-ls args)
  (define-values (boards items) (load-kb))
  (define opts (parse-opts args))
  (define filtered
    (filter (λ (i)
              (and (or (not (hash-has-key? opts 'lane))
                       (eq? (item-status i) (string->symbol (hash-ref opts 'lane))))
                   (or (not (hash-has-key? opts 'type))
                       (eq? (item-type i) (string->symbol (hash-ref opts 'type))))
                   (or (not (hash-has-key? opts 'tag))
                       (member (string->symbol (hash-ref opts 'tag)) (item-tags i)))
                   (or (not (hash-has-key? opts 'assignee))
                       (equal? (item-assignee i) (string->symbol (hash-ref opts 'assignee))))))
            items))
  (for ([i filtered])
    (printf "~a [~a] ~a (~a)~a\n"
            (item-id i)
            (item-type i)
            (item-title i)
            (item-status i)
            (if (item-blocked? i items) " BLOCKED" ""))))

(define (cmd-board args)
  (define-values (boards items) (load-kb))
  (define b (car boards))
  (define lanes (board-lanes b))
  (define lane-items
    (for/hash ([lane lanes])
      (values lane (filter (λ (i) (eq? (item-status i) lane)) items))))
  (define max-width 24)
  ;; Header
  (for ([lane lanes])
    (printf "~a" (string-append (string-upcase (symbol->string lane))
                                (make-string (- max-width (string-length (symbol->string lane))) #\space))))
  (newline)
  (displayln (make-string (* max-width (length lanes)) #\-))
  ;; Items
  (define max-items (apply max 1 (map length (hash-values lane-items))))
  (for ([row (in-range max-items)])
    (for ([lane lanes])
      (define lane-list (hash-ref lane-items lane))
      (if (< row (length lane-list))
          (let* ([i (list-ref lane-list row)]
                 [label (format "~a~a" (item-id i) (if (item-blocked? i items) "*" ""))]
                 [padded (string-append label (make-string (max 0 (- max-width (string-length label))) #\space))])
            (display padded))
          (display (make-string max-width #\space))))
    (newline)))

(define (cmd-blocked args)
  (define-values (boards items) (load-kb))
  (define blocked-items (filter (λ (i) (item-blocked? i items)) items))
  (if (null? blocked-items)
      (displayln "No blocked items.")
      (for ([i blocked-items])
        (define blocking (blocking-deps i items))
        (printf "~a [~a] ~a\n  blocked by: ~a\n"
                (item-id i)
                (item-status i)
                (item-title i)
                (string-join (map symbol->string blocking) ", ")))))

(define (cmd-edit args)
  (match args
    [(list id-str rest ...)
     (define id (string->symbol id-str))
     (define-values (boards items) (load-kb))
     (define i (find-item items id))
     (if i
         (let* ([opts (parse-opts rest)]
                [updated
                 (struct-copy item (add-history i "edited")
                   [title (hash-ref opts 'title (item-title i))]
                   [desc (hash-ref opts 'desc (item-desc i))]
                   [assignee (let ([a (hash-ref opts 'assignee #f)])
                               (if a (string->symbol a) (item-assignee i)))]
                   [priority (let ([p (hash-ref opts 'priority #f)])
                               (if p (string->symbol p) (item-priority i)))]
                   [deps (let ([d (hash-ref opts 'deps #f)])
                           (if d (map string->symbol (string-split d ",")) (item-deps i)))])])
           (save-kb boards (update-item items id (λ (_) updated)))
           (printf "Updated ~a\n" id))
         (printf "Item ~a not found\n" id))]
    [_ (displayln "Usage: kb edit <id> [--title=X] [--desc=X] [--assignee=X] [--priority=X] [--deps=X,Y]")]))

(define (cmd-log args)
  (match args
    [(list id-str)
     (define id (string->symbol id-str))
     (define-values (boards items) (load-kb))
     (define i (find-item items id))
     (if i
         (if (null? (item-history i))
             (displayln "No history.")
             (for ([h (item-history i)])
               (printf "~a: ~a\n" (car h) (cadr h))))
         (printf "Item ~a not found\n" id))]
    [_ (displayln "Usage: kb log <id>")]))

(define (cmd-archive args)
  (match args
    [(list id-str)
     (define id (string->symbol id-str))
     (define-values (boards items) (load-kb))
     (define i (find-item items id))
     (if i
         (let ([updated (struct-copy item (add-history i "archived") [status 'archived])])
           (save-kb boards (update-item items id (λ (_) updated)))
           (printf "Archived ~a\n" id))
         (printf "Item ~a not found\n" id))]
    [_ (displayln "Usage: kb archive <id>")]))

(define (cmd-help)
  (displayln "kb — text-first kanban tool")
  (displayln "")
  (displayln "Commands:")
  (displayln "  add <type> <title> [--tag=X] [--priority=X] [--lane=X]")
  (displayln "  move <id> <lane>")
  (displayln "  show <id>")
  (displayln "  edit <id> [--title=X] [--desc=X] [--assignee=X] [--priority=X] [--deps=X,Y]")
  (displayln "  ls [--lane=X] [--type=X] [--tag=X] [--assignee=X]")
  (displayln "  board")
  (displayln "  log <id>")
  (displayln "  archive <id>")
  (displayln "  blocked")
  (displayln "  help"))

;; --- Main ---
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (match args
    [(list "add" rest ...) (cmd-add rest)]
    [(list "move" rest ...) (cmd-move rest)]
    [(list "show" rest ...) (cmd-show rest)]
    [(list "edit" rest ...) (cmd-edit rest)]
    [(list "ls" rest ...) (cmd-ls rest)]
    [(list "board" rest ...) (cmd-board rest)]
    [(list "log" rest ...) (cmd-log rest)]
    [(list "archive" rest ...) (cmd-archive rest)]
    [(list "blocked" rest ...) (cmd-blocked rest)]
    [(list "help") (cmd-help)]
    [(list) (cmd-help)]
    [_ (printf "Unknown command. Try: kb help\n")]))
