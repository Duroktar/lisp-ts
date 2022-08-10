import { Frame } from "./frame"

export class Stack {
  private data: Frame[] = []
  private unwind: boolean = false
  private tail: boolean = false
  private value: Frame | undefined

  // Pushes a new +Frame+ or +Body+ onto the +Stack+ and then executes the
  // resulting code until the pushed frame returns a value, which is then
  // returned.
  push(frame: Frame) {
    this.data.push(frame)
    this.clear(this.data.length - 1)
  }

  // Creates and returns a copy of the stack, which represents the current
  // computational state: any unfinished expressions and function bodies are
  // stored in the stack. Pass +false+ to discard the final frame, which will
  // typically be a call to <tt>(call/cc)</tt> when creating a +Continuation+.
  copy(keep_last = true) {
    return this.data.slice(0, (keep_last ? undefined : -1))
  }

  // Fills a hole in the final +Frame+ on the +Stack+ by replacing the given
  // epxression +subexpr+ with the given +value+. If the +value+ is a +Frame+,
  // this frame is pushed onto the stack rather than filling a hole in the
  // previous frame.
  fill(subexpr: any, value: any) {
    if (value instanceof Frame) {
      this.data.push(value)
      return
    }

    if (this.data.length === 0) {
      this.value = value
      return
    }

    (<any>this.data[this.data.length-1])
      .fill(subexpr, value)
  }

  // Causes the stack to evaluate expressions in order to pop them off the
  // stack, until it gets down to the size given by +limit+. The resulting
  // value if returned after all necessary computations have been done, and
  // if an error takes place at any point we empty the stack.
  clear(limit = 0) {
    try {
      while (this.data.length > limit)
        this.process()
      return this.value
    } catch(ex) {
      this.restack()
      throw ex
    }
  }

  // Sets the +value+ on the +Stack+, which is always the value returned by
  // the last completed expression or function body. If the given +value+ is
  // another +Stack+, this new stack replaces the state of the receiver; this
  // takes place when a +Continuation+ is called. If the +value+ is a +Frame+,
  // it is pushed onto the stack and we set a flag to indicate that a tail
  // call is in effect and the replacement target of the call needs to be
  // repointed: the expression that generated the tail call will have been
  // removed from the stack by the time the call returns.
  valueEquals(value: any) {
    this.value  = value
    this.unwind = (this.value instanceof Stack)
    this.tail   = (this.value instanceof Frame)
    if (this.unwind)
      this.restack(value)
  }

  empty() {
    return this.data.length === 0
  }

  last() {
    return this.data[this.data.length-1]
  }

  pop() {
    return this.data.pop()
  }

  // Processes one piece of the final +Frame+ on the +Stack+ and inspects the
  // return value. The value must be inspected to see if a +Continuation+ has
  // been called (indicated by <tt>@unwind</tt>), or a tail call has taken
  // place. Continuation calls replace the state of the stack, and tail calls
  // need modifying so they fill the correct hole when they return.
  private process() {
    this.value = this.last().process()
    if (this.empty() || this.unwind || !this.last()!.complete)
      return
    if (this.tail)
      this.value!.replaces(this.last().target())

    this.fill(this.pop()!.target(), this.value)
  }

  // Replaces the state of the receiver with the state of the argument. We
  // call this when calling a +Continuation+, or when recovering from errors.
  private restack(stack = []) {
    while (this.data.length !== 0)
      this.data.pop()

    stack.forEach((frame, i) => {self[i] = frame })

    if (stack instanceof Stack)
      this.value = stack.value
  }
}
