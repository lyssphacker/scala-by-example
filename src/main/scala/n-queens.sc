// exercise 10.1.1 - not finished

def queens(n: Int): List[List[Int]] = {
  def placeQueens(k: Int): List[List[Int]] =
    if (k == 0) List(List())
    else for {queens <- placeQueens(k - 1)
              column <- List.range(1, n + 1)
              if isSafe(column, queens, 1)} yield column :: queens

  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int], delta: Int): Boolean = {
  !queens.contains(col) && isDiagonallySafe(queens.length + 1, col, queens)
}

def isDiagonallySafe(x: Int, y: Int, queens: List[Int]): Boolean = {
  diagonals(x, y, queens.length).intersect(pairsFromQueens(queens)).isEmpty
}

def diagonals(x: Int, y: Int, n: Int): List[(Int, Int)] = {
  diagonalsxy(x, y, n, (s: Int) => s + 1, (t: Int) => t + 1) :::
  diagonalsxy(x, y, n, (s: Int) => s + 1, (t: Int) => t - 1) :::
  diagonalsxy(x, y, n, (s: Int) => s - 1, (t: Int) => t - 1) :::
  diagonalsxy(x, y, n, (s: Int) => s - 1, (t: Int) => t + 1)
}

def diagonalsxy(x: Int, y: Int, n: Int, xfn: Int => Int, yfn: Int => Int): List[(Int, Int)] = {
  def diagonalsIter(x: Int, y: Int, result: List[(Int, Int)]): List[(Int, Int)] = {
    if (x > n || y > n || x < 1 || y < 1) result
    else diagonalsIter(xfn(x), yfn(y), (x, y) :: result)
  }

  diagonalsIter(xfn(x), yfn(y), Nil)
}

diagonals(3, 3, 5)

def pairsFromQueens(queens: List[Int]): List[(Int, Int)] = {
  queens.zipWithIndex.map(p => (p._2 + 1, p._1))
}

pairsFromQueens(List(3, 2, 5)).intersect(List((1, 3)))

queens(3)


