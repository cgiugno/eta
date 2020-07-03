(store-obj-schema 'BW-concept-primitive.n '|BW-block|
  ; Block
  '(obj-schema
    :header (?x BW-block.n)
    :types (
      !t0 (?x block.n)
      !t1 (?c color.n)
      !t2 (?l BW-logo.n)
    )
    :rigid-conds (
      !r0 ((dimensions-of.f ?x) = ($ dim .15 .15 .15))
      !r1 ((color-of.f ?x) = ?c)
      !r2 (?x carry26.v ?l)
    )
)) ; END |BW-block|



(store-obj-schema 'BW-concept-primitive.n '|BW-row-of|
  ; Row-of
  '(obj-schema
    :header (?x row-of.n ?P)
    ; a row of items of type ?P (where ?P is a monadic predicate *name*)
    :types (
        !t0 (?x (maximal.a (sequence-of.n ?P 'abut.v)))
    )
    :rigid-conds (
        !r1 (?x (form7.v (straight.a line.n))); "form" is a predicate-taking verb, like "is"
        !r2 (?x horizontal.a)
    )
    :skeletal-prototype (
        row.obj
    )
)) ; END |BW-row-of|



(store-obj-schema 'BW-concept-primitive.n '|BW-stack-of|
  ; Stack-of
  '(obj-schema
    :header (?x stack-of.n ?P)
    ; a stack of items of type ?P (where ?P is a monadic predicate *name*)
    :types (
      !t0 (?x (maximal.a (sequence-of.n ?P 'on.n)))
    )
    :rigid-conds (
      !r1 (?x (form7.v (straight.a line.n)))
      !r2 (?x vertical.a)
    )
    :skeletal-prototype (
      stack.obj
    )
)) ; END |BW-stack-of|



(store-obj-schema 'BW-concept-primitive.n '|BW-row|
  ; Row
  '(obj-schema
    :header (?x BW-row.n)
    :types (
      !t0 (?x row-of.n 'BW-block.n)
    )
    :skeletal-prototype (
      bw-row1.obj
      bw-row2.obj
      bw-row3.obj
    )
)) ; END |BW-row|



(store-obj-schema 'BW-concept-primitive.n '|BW-stack|
  ; Stack
  '(obj-schema
    :header (?x BW-stack.n)
    :types (
      !t0 (?x stack-of.n 'BW-block.n)
      !t1 (?b (1st ?x)) ; assume 1st is a special primitive function on sequences
      !t2 (?c (lst ?x)) ; assume lst is a special primitive function on sequences
    )
    :skeletal-prototype (
      bw-stack1.obj
      bw-stack2.obj
      bw-stack3.obj
    )
)) ; END |BW-stack|



(store-concept-set '(plur BW-concept-primitive.n) '|BW-concept-set2|
  '(|BW-block| |BW-row-of| |BW-stack-of| |BW-row| |BW-stack|))