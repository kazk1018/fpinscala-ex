package chap2

import org.specs2.mutable.Specification

class MyModuleSpecification extends Specification {

  "Part1 excercise tests" >> {
    "fib()" >> {
      MyModule.fib(6) must_=== 8
      MyModule.fib(5) must_=== 5
      MyModule.fib(4) must_=== 3
    }

    "isSorted()" >> {
      MyModule.isSorted(List(1,2,3), (n1: Int, n2: Int) => n1 < n2)
    }
  }
}
