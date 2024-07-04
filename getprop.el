(require 'eieio)

(defun getprop ($x $key)
  (cond
   ((eieio-object-p $x)
    (if (stringp $key) (setf $key (intern (concat ":" $key))))
    (slot-value $x $key))
   ((hash-table-p $x)
    (gethash $key $x))
   (t (elt $x $key))))

(defun putprop ($x $key $store)
  (cond
   ((eieio-object-p $x)
    (if (stringp $key) (setf $key (intern (concat ":" $key))))
    (set-slot-value $x $key $store))
   ((hash-table-p $x)
    (puthash $key $store $x))
   (t (setf (elt $x $key) $store))))

(gv-define-setter getprop ($store $seq $n)
  `(putprop ,$seq ,$n ,$store))

(defmacro ! ($x &rest $specs)
  (setf $specs (!::specs $specs))
  (dolist ($spec $specs)
    (setf $x (!::replace $spec $x))
    )
  $x
  )

(defun !::replace ($spec $x)
  (cond
   ((null $spec)
    nil)
   ((eq $spec '!)
    $x)
   ((consp $spec)
    (cons (!::replace (car $spec) $x)
          (!::replace (cdr $spec) $x)))
   (t $spec)
   )
  )

(defun !::specs ($specs)
  (let* ( $result )
    (dolist ($spec $specs (nreverse $result))
      (setf $spec (!::spec $spec))
      (push $spec $result)
      )
    )
  )

(defun !::spec ($spec)
  (cond
   ((listp $spec)
    $spec)
   ((and (symbolp $spec) (string-match "^\\^" (symbol-name $spec)))
    `(,$spec !))
   (t
    `(getprop ! ,$spec))
   )
  )

(provide 'getprop)
