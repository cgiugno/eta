(store-obj-schema 'BW-concept.n '|BW-arch|
  ; Arch
  '(obj-schema
    :header (?x BW-arch.n)

    :types (
      !t0 (?stack1 BW-stack.n)
      !t1 (?stack2 BW-stack.n)
      !t2 (?top BW-block.n)
      ;!r3 (?table (-er (much.adv big.a)) ?b)
    )

    :rigid-conds (
      !r0 (?top on.p ?stack1)
      !r1 (?top on.p ?stack2)  
      !r2 (?stack1 next-to.p ?stack2)
      !r3 (not (?stack1 touching.p ?stack2))
      !r4 (?top clear.a)
      !r5 ((height-of.f ?stack1) = (height-of.f ?stack2))  ; assume height-of.f is a function that acts on stacks, and return # of items
    )

    :skeletal-prototype (
      bw-arch1.obj
      bw-arch2.obj
    )
)) ; END |BW-arch|


(store-obj-schema 'BW-concept.n '|BW-stack|
  ; Stack
  '(obj-schema
    :header (?x BW-stack.n)

    :types (
      !t0 (?stack1 BW-stack.n)
      !t1 (?stack2 BW-stack.n)
      !t2 (?top BW-block.n)
      ;!r3 (?table (-er (much.adv big.a)) ?b)
    )

    :rigid-conds (
      !r0 (?top on.p ?stack1)
      !r1 (?top on.p ?stack2)  
      !r2 (?stack1 next-to.p ?stack2)
      !r3 (not (?stack1 touching.p ?stack2))
      !r4 (?top clear.a)
      !r5 ((height-of.f ?stack1) = (height-of.f ?stack2))  ; assume height-of.f is a function that acts on stacks, and return # of items
    )

    :skeletal-prototype (
      bw-arch1.obj
      bw-arch2.obj
    )
)) ; END |BW-stack|


(store-concept-set '(plur BW-concept.n) '|BW-concept-set1| '(|BW-arch| |BW-stack|))