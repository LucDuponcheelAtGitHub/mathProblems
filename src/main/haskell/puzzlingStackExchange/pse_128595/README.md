**Question:**

From the 2011 South African Junior Olympiad:

Several people line up in single file.
A solitary latecomer wishes to join the queue.
Prove that it is always possible for them to join the line somewhere
so that the number of men in front of them is equal to the number of women behind them.

**Warning:**

I use *position* as a synonym for *index*, where indices start from `0`.
This implies that position `n` has `n` positions before it.

**Notation:**

- *wa* = woman after
- *was* = the set of women after
- *mb* = man before
- *mbs* = the set of men before
- *ws* = the set of all women
- *ms* = the set of all men 

*#xs* stands for the number of elements of *xs*.

**Proposition:**

The position `p` for the latecomer to join is the one with *#ws* positions before it.

*Proof:*

If *mbs* is empty, then *was* is also empty and *#mbs == #was*.

If *mbs* is not empty, then there exists a function from *was* to *mbs*.

The proof then follows from the lemma below.

**Lemma:**

The function from the proposition above is a *bijection* between *was* and *mbs*.

*Proof(by contradiction):*

Assume that the function is not *injective*.
This implies that there are two *wa*'s mapped on one *mb*.
In that case *#ws* would be too large.

Assume that the injection is not *surjective*.
In that case the position is one with more than *#ws* positions before it. 

*qed*