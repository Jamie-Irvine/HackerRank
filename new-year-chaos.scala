object Solution {
  def minimumBribes(q: Array[Int]): Int = {
    // Mutable variables
    var list = q
    var swapped = true
    var steps = 0

    // Keep repeating for loop until list is sorted
    while(swapped) {
      swapped = false

      // Loop through each number in list and swap with the following
      // number if the first is bigger than the next. Uses the bubble
      // sort algorithm.
      //
      // E.g. List(3, 2) would become List(2, 3)
      for (i <- list.indices) {
        val currentNumber = list(i)
        val currentNumberIndex = i
        val nextNumberIndex = i + 1

        if (nextNumberIndex != list.length) {
          if (currentNumber > list(nextNumberIndex)) {
            list = swap(list, currentNumberIndex, nextNumberIndex)
            swapped = true
            steps += 1
          }
        }
      }
    }

    steps
  }

  def swap(list: Array[Int], a: Int, b: Int): Array[Int] = {
    val num1 = list(a)
    val num2 = list(b)

    list(a) = num2
    list(b) = num1

    list
  }

  def printResult(r: Int): Unit = {
    print("Steps:", r)
  }

  printResult(minimumBribes(Array(3, 2, 5, 1, 4)))
  printResult(minimumBribes(Array(1, 2, 3, 4, 5)))
  printResult(minimumBribes(Array(1, 3, 4, 2, 5, 8, 7, 6, 9)))
}
