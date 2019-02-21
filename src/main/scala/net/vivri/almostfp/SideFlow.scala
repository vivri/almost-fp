package net.vivri.almostfp

/**
  * Extends any object to perform a side effect, and then return said object
  *
  * For example:
  *
  * val abc = "abc" |-- save_in_db |-- println
  *
  * Note that the effect isn't guarded, and can throw a Throwable.
  *
  */
object SideFlow {

  implicit class SideFlowExt[T](inner: T) {
    /**
      * Performs a side effect within the context of the caller reference, returning that same reference.
      *
      * Symbolic form (sideways T)
      */
    val |-- : (T => Unit) => T =
      (effect) => {
        effect(inner)
        inner
      }

    /**
      * Unix-friendly name
      */
    val tee = |--

    /**
      * FP-friendly name
      */
    val eff = |--
  }

}
