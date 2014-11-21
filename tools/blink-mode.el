;; Copyright (c) Microsoft Corporation
;; All rights reserved. 

;; Licensed under the Apache License, Version 2.0 (the ""License""); you
;; may not use this file except in compliance with the License. You may
;; obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
;; LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
;; A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

;; See the Apache Version 2.0 License for specific language governing
;; permissions and limitations under the License.

(defvar blink-mode-hook nil)

(defvar blink-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Blink major mode")


(add-to-list 'auto-mode-alist '("\\.zir\\'" . blink-mode))
(add-to-list 'auto-mode-alist '("\\.blk\\'" . blink-mode))
(add-to-list 'auto-mode-alist '("\\.wpl\\'" . blink-mode))


(setq blink-keywords '("let" "in" "fun"
           "if" "then" "else" "external" "comp"
           "lut" "var"))

(setq blink-functions '("repeat" "times" "until" "unroll" "noinline"
                        "inline" "autoinline" "forceinline" "nounroll"
                        "for" "seq" "do" "return"
                        "bperm" "read" "write" "emit" "take" "takes" 
                    "while" "emits" "map" "filter" "print" "println" "error"))

(setq blink-types '("arr" "struct" "enum"
              "complex" "int" "int8" "bit" "bool" "int"
              "int16" "int32" "int64" "complex8" "complex16" "complex32" "complex64"
              "struct"))

(setq blink-consts '("true" "false" "'0" "'1")) 

(setq blink-keywords-regexp  (regexp-opt blink-keywords  'words))
(setq blink-types-regexp     (regexp-opt blink-types     'words))
(setq blink-consts-regexp    (regexp-opt blink-consts    'words))
(setq blink-functions-regexp (regexp-opt blink-functions 'words))



(defconst blink-font-lock-keywords
  `(
     (,blink-types-regexp . font-lock-type-face)
     (,blink-consts-regexp . font-lock-constant-face)
     (,blink-keywords-regexp . font-lock-keyword-face)
     (,blink-functions-regexp . font-lock-function-name-face)
     (,"\\('\\w*'\\)" . font-lock-variable-name-face)
))



(defvar blink-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)

    (modify-syntax-entry ?\{ ". 1" st)
    (modify-syntax-entry ?- ". 123b" st)
    (modify-syntax-entry ?\} ". 4" st)
    (modify-syntax-entry ?\n "> b" st)
 
    st)
   "Syntax table for Blink mode")


;; This code is not very satisfactory, instead I will rely on Haskell mode for now.

;; (defun blink-indent-line ()
;;   "Indent current line as Blink code."
;;   (interactive)
;;   (beginning-of-line)
;;   (if (bobp) 
;;        (indent-line-to 0)

;;        (let ((not-indented t) cur-indent)
;;             (if (looking-at "^[ \t]*\\(in\\|}\\)") 
;;                 (save-excursion 
;;                     (forward-line -1)
;;                     (if (looking-at "^[ \t]*;")
;;                         (progn (setq cur-indent (current-indentation))
;;                                (setq not-indented nil))
;;                         (progn (setq cur-indent (max 0 (- (current-indentation) blink-tab-width)))
;;                                (setq not-indented nil))
;;                     ))
;;                 ;; if we are not looking at ... } or ... in
;;                 (progn 
;;                    (save-excursion
;;                       (while (and (not (bobp)) not-indented)
;;                          (forward-line -1)
;;                          (if (looking-at "^[ \t]*;") 
;;                              (progn (setq cur-indent (current-indentation)) (setq not-indented nil))
;;                              (if (looking-at "[^{]*?{")
;;                                  (progn (setq cur-indent (+ (current-indentation) blink-tab-width)) (setq not-indented nil))
;;                                  (if (looking-at "\\.*?let")
;;                                      (progn (setq cur-indent (+ (current-indentation) blink-tab-width)) (setq not-indented nil))
;;                                      (progn (setq cur-indent (current-indentation)) (setq not-indented nil))
;;                                  )
;;                              )
;;                          )
;;                       )
;;                    )
;;                    (if cur-indent
;;                          (indent-line-to cur-indent)
;;                          (indent-line-to 0)
;;                    )
;;                  )
;;             )
;;        )
;;    )
;; )



;; (defun blink-mode ()
(define-derived-mode blink-mode haskell-mode  
  "Major mode for Blink"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table blink-mode-syntax-table)
  (use-local-map blink-mode-map)

  (set (make-local-variable 'font-lock-defaults) '(blink-font-lock-keywords))

;; Commented out: we rely on the Haskell mode indentation for now
;; (set (make-local-variable 'indent-line-function) 'blink-indent-line)  

  (set (make-local-variable 'default-indent-tabs-mode) nil)
  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'blink-tab-width) 4)

  (setq major-mode 'blink-mode)
  (setq mode-name "BLINK")
  (run-hooks 'blink-mode-hook))





(provide 'blink-mode)

