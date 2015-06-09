(defvar ziria-mode-hook nil)

(defvar ziria-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Ziria major mode")


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zir\\'" . ziria-mode))
(add-to-list 'auto-mode-alist '("\\.wpl\\'" . ziria-mode))
(add-to-list 'auto-mode-alist '("\\.blk\\'" . ziria-mode))


;;;;;;;;;;;;;;;;;;;; Some face definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup ziria-faces nil
  "Special faces for the Ziria mode."
  :group 'ziria)

(defface ziria-font-lock-governing-face
  '((((background light)) (:foreground "blue" :bold t))
    (t (:foreground "orange" :bold t)))
  "Face description for governing/leading keywords."
  :group 'ziria-faces)
(defvar ziria-font-lock-governing-face
  'ziria-font-lock-governing-face)

(defface ziria-font-lock-preprocessor-face
  '((((background light)) (:foreground "red" :italic t))
    (t (:foreground "red" :italic t)))
  "Face description for governing/leading keywords."
  :group 'ziria-faces)
(defvar ziria-font-lock-preprocessor-face
  'ziria-font-lock-preprocessor-face)

(defface ziria-font-lock-type-face
  '((((background light)) (:foreground "blue" t))
    (t (:foreground "yellow" t)))
  "Face description for types."
  :group 'ziria-faces)
(defvar ziria-font-lock-type-face
  'ziria-font-lock-type-face)

(defface ziria-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'ziria-faces)
(defvar ziria-font-lock-operator-face
  'ziria-font-lock-operator-face)



(defconst ziria-font-lock-keywords
  (list
   '("\\<\\(ST\\|arr\\|b\\(?:it\\|ool\\)\\|complex\\(?:16\\|32\\|64\\|8\\)?\\|double\\|int\\(?:16\\|32\\|64\\|8\\)?\\|struct\\|[CT]\\)\\>" . ziria-font-lock-type-face)
   '("\\<\\(\\(?:fals\\|tru\\)e\\)\\>" . font-lock-constant-face)

   '("\\<\\(begin\\|end\\)\\>" . ziria-font-lock-governing-face)

   '("\\<\\(autoinline\\|forceinline\\|no\\(?:inline\\|unroll\\)\\|unroll\\)\\>" . ziria-font-lock-preprocessor-face)

   '("\\(!=\\|&&\\|\\*\\*\\|:=\\|<[<=-]\\|==\\|>\\(?:>>\\|[=>]\\)\\||\\(?:\\(?:>>>\\)?|\\)\\|[!%&*+/<=>|~^-]\\)" . ziria-font-lock-operator-face)


   '("\\<\\(comp\\|do\\(?:ne\\)?\\|e\\(?:lse\\|mits?\\|rror\\|xternal\\)\\|f\\(?:ilter\\|or\\|un\\)\\|i[fn]\\|le\\(?:ngth\\|t\\)\\|map\\|not\\|print\\(?:ln\\)?\\|re\\(?:ad\\|peat\\|turn\\)\\|s\\(?:eq\\|tandalone\\)\\|t\\(?:akes?\\|hen\\|imes\\)\\|until\\|var\\|w\\(?:\\(?:hil\\|rit\\)e\\)\\)\\>" . font-lock-keyword-face))

  "Ziria highlighting definitions")


;;;;;;;;;;;;;;;;;;; Indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ziria-tab-width 2)
(defconst default-tab-width 2)

(defun ziria-indent-line ()
  "Indent current line as Ziriacode"
  (interactive)
  (beginning-of-line)

  (if (bobp) (indent-line-to 0)
      ;; else 
      (let ((not-indented t) cur-indent)
           ;; in
           (if (looking-at "^[ \t]*end") ; Check for rule 2
            (progn
              (save-excursion
                (forward-line -1)
                (setq cur-indent (- (current-indentation) ziria-tab-width)))

                (if (< cur-indent 0)
                  (setq cur-indent 0))
            )

            (save-excursion 
              (while not-indented (forward-line -1)
                (if (looking-at "^[ \t]*\\(done\\|end\\)") ; Check for rule 3
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                    ; ... else Check for rule 4 (do/begin
                    (if (looking-at "^[ \t]*\\(begin\\|do\\)")
                        (progn
                          (setq cur-indent (+ (current-indentation) ziria-tab-width))
                          (setq not-indented nil))
                        ; ... else 
                        (if (bobp) ; Check for rule 5
                            (setq not-indented nil)))))))

      (if cur-indent (indent-line-to cur-indent)
                     (indent-line-to 0))
     )
  )                    
)


;; make tab key always call a indent command.
;; (setq-default tab-always-indent t)

;; make tab key call indent command or insert tab character, 
;; depending on cursor position
(setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
;; (setq-default tab-always-indent 'complete)


;;;;;;;;;;;;;;;;;;; Syntax table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ziria-mode-syntax-table
  (let ((st (make-syntax-table)))
       (modify-syntax-entry ?_ "w" st)
       (modify-syntax-entry ?- ". 124b" st)
       (modify-syntax-entry ?\n "> b" st)
       st)
  "Syntax table for Ziria")

;;;;;;;;;;;;;;;;;;; Entry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ziria-mode ()
  "Ziria major mode"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ziria-mode-syntax-table)
  (use-local-map ziria-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(ziria-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'ziria-indent-line) 

  (setq major-mode 'ziria-mode)
  (setq mode-name "Ziria")
  (run-hooks 'ziria-mode-hook))



(provide 'ziria-mode)


;; (regexp-opt '( 
;;   "+"  
;;   "-"  
;;   "*"  
;;   "/"  
;;   "%"  
;;   "**" 
;;   "<<"  
;;   ">>"  

;;   "==" 
;;   "!=" 
;;   "<"  
;;   ">"  
;;   "<=" 
;;   ">=" 

;;   "~" 
;;   "&" 
;;   "|" 
;;   "^" 

;;   "&&" 
;;   "||" 

;;   "="     
;;   ":="    
;;   "<-"    
;;   ">>>"   
;;   "|>>>|" 

;;   "!"  

;; ) t)


;;;;;;;;;;;;;;;;;; Regexps used to generate keywords ;;;;;;;;;;;;;;;;;;



;; (regexp-opt '( 
;;    "C"           
;;    "ST"          
;;    "T"           
;;    "arr"         
;;    "bit"         
;;    "bool"        
;;    "complex"     
;;    "complex8"    
;;    "complex16"   
;;    "complex32"   
;;    "complex64"   
;;    "double"      
;;    "int"         
;;    "int8"        
;;    "int16"       
;;    "int32"       
;;    "int64"       
;;    "struct"        
;; ) t)


;; (regexp-opt '( 
;;    "false" "true"
;; ) t)


;; (regexp-opt '(
;;   "autoinline" 
;;   "forceinline"
;;   "noinline"   
;;   "nounroll" 
;;   "unroll"
;; ) t)


;; (regexp-opt '(
;;  "emit"        
;;  "emits"       
;;  "filter"      
;;  "map"         
;;  "read"        
;;  "repeat"      
;;  "return"      
;;  "seq"         
;;  "standalone"  
;;  "take"        
;;  "takes"       
;;  "write"
;;  "comp"        
;;  "do"          
;;  "done"        
;;  "else"        
;;  "error"       
;;  "external"    
;;  "for"         
;;  "fun"         
;;  "if"          
;;  "in"          
;;  "length"      
;;  "let"         
;;  "not"         
;;  "print"       
;;  "println"     
;;  "then"        
;;  "times"       
;;  "until"       
;;  "var"         
;;  "while"       
;; ) t)


