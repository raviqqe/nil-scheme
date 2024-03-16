(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?
    aa-tree-insert!)

  (import (scheme base) (scheme write))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root size)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (size aa-tree-size aa-tree-set-size!))

    (define-record-type aa-tree-node
      (make-aa-tree-node value level left right)
      aa-tree-node?
      (value aa-tree-node-value aa-tree-node-set-value!)
      (level aa-tree-node-level aa-tree-node-set-level!)
      (left aa-tree-node-left aa-tree-node-set-left!)
      (right aa-tree-node-right aa-tree-node-set-right!))

    (define (aa-tree-empty)
      (make-aa-tree #f 0))

    (define (aa-tree-insert! tree value)
      #f)

    (define (aa-tree-node-skew! node)
      (let ((left (and node (aa-tree-node-left node))))
        (if (and
             left
             (eq? (aa-tree-node-level node) (aa-tree-node-level left)))
          (begin
            (aa-tree-node-set-left! tree (aa-tree-node-right left))
            (aa-tree-node-set-right! left tree)
            left)
          node)))

    (define (aa-tree-node-split! node)
      (let* ((right (and node (aa-tree-node-right node)))
             (right-right (and right (aa-tree-node-right right))))
        (if (and
             right-right
             (eq? (aa-tree-node-level node) (aa-tree-node-level right-right)))
          (begin
            (aa-tree-node-set-right! tree (aa-tree-node-left right))
            (aa-tree-node-set-left! right tree)
            (aa-tree-node-set-level! right (+ (aa-tree-node-level right) 1))
            right)
          node)))))
