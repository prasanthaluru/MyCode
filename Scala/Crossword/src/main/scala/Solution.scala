import scala.io.StdIn.{readLine, readInt}

/**
 *
 */
object Solution {

  def main(args: Array[String]) {
    val matrix: _root_.scala.Array[Array[Char]] = (1 to 10).map(_ => readLine()).toArray.map(m => m.toCharArray)
    val words: Array[_root_.java.lang.String] = readLine().split(";")
    solve(matrix, words, 0)
  }

  /**
   * to print the matrix
   * @param matrix
   */
  def printMatrix(matrix: Array[Array[Char]]) = {
    for (i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        print(matrix(i)(j))
      }
      println("")
    }
  }

  /**
   * Validates if the word fits matrix horizontally in the given position
   * @param matrix
   * @param word
   * @param i row position
   * @param j column position
   * @return <code>Boolean</code> value indicating the fit
   */
  def validHorizontalPosition(matrix: Array[Array[Char]], word: String, i: Int, j: Int): Boolean = {
    if (j - 1 >= 0 && !matrix(i)(j - 1).equals('+')) { // Invalid as j is out of array bounds or position not starting of slot
      return false
    } else if (j + word.length() < matrix(i).length && !matrix(i)(j + word.length()).equals('+')) { // Invalid as word length mismatch
      return false
    } else if (j + word.length() > matrix(i).length) {
      return false
    }

    //validate placement for each cell
    for (jj <- 0 until word.length()) {
      if (j + jj > matrix(i).length)
        return false
      if (matrix(i)(j + jj).equals('-') || matrix(i)(j + jj).equals(word(jj))) {
        //continue checking for next characters
      } else return false
    }
    true
  }

  /**
   * Fit the word in the Matrix at given position horizontally
   * @param matrix
   * @param word
   * @param i
   * @param j
   * @return <code>Array[Boolean]</code> indicating the positions updated during fitting
   */
  def applyHorizontalPosition(matrix: Array[Array[Char]], word: String, i: Int, j: Int): Array[Boolean] = {
    val updated: _root_.scala.Array[Boolean] = new Array[Boolean](word.length())

    for (jj <- 0 until word.length()) {
      if (matrix(i)(j + jj).equals('-')) {
        matrix(i)(j + jj) = word(jj)
        updated(jj) = true
      }
    }
    updated
  }

  /**
   * Backtrack the word by checking the parameter array <i>updated</i>
   * @param matrix
   * @param updated
   * @param i
   * @param j
   */
  def backtrackHorizontalPosition(matrix: Array[Array[Char]], updated: Array[Boolean], i: Int, j: Int) {
    for (jj <- updated.indices) {
      if (updated(jj)) {
        matrix(i)(j + jj) = '-'
      }
    }
  }

  /**
   * Validates if the word fits matrix vertically in the given position
   * @param matrix
   * @param word
   * @param i row position
   * @param j column position
   * @return <code>Boolean</code> value indicating the fit
   */
  def validVerticalPosition(matrix: Array[Array[Char]], word: String, i: Int, j: Int): Boolean = {
    if (i - 1 >= 0 && !matrix(i - 1)(j).equals('+')) { // Invalid as i is out of array bounds or position not starting of slot
      return false
    } else if (i + word.length() < matrix.length && !matrix(i + word.length())(j).equals('+')) { // Invalid as word length mismatch
      return false
    } else if (i + word.length() > matrix.length) {
      return false
    }

    //validate placement for each cell
    for (ii <- 0 until word.length()) {
      if (i + ii > matrix.length)
        return false
      if (matrix(i + ii)(j).equals('-') || matrix(i + ii)(j).equals(word(ii))) {
        //continue checking for next characters
      } else {
        return false
      }
    }
    true
  }
  /**
   * Fit the word in the Matrix at given position vertically
   * @param matrix
   * @param word
   * @param i
   * @param j
   * @return <code>Array[Boolean]</code> indicating the positions updated during fitting
   */
  def applyVerticalPosition(matrix: Array[Array[Char]], word: String, i: Int, j: Int): Array[Boolean] = {
    val updated: _root_.scala.Array[Boolean] = new Array[Boolean](word.length())

    for (ii <- 0 until word.length()) {
      if (matrix(i + ii)(j).equals('-')) {
        matrix(i + ii)(j) = word(ii)
        updated(ii) = true
      }
    }
    updated
  }
  /**
   * Backtrack the word by checking the parameter array <i>updated</i>
   * @param matrix
   * @param updated
   * @param i
   * @param j
   */
  def backtrackVerticalPosition(matrix: Array[Array[Char]], updated: Array[Boolean], i: Int, j: Int) {
    for (ii <- updated.indices) {
      if (updated(ii)) {
        matrix(i + ii)(j) = '-'
      }
    }
  }

  /**
   * Solve the crossword iteratively. prints only one solution.
   * @param matrix
   * @param words
   * @param wordIndex
   * @return  1 if solution is found else 0
   */
  def solve(matrix: Array[Array[Char]], words: Array[String], wordIndex: Int): Int = {
    if (wordIndex == words.length) {
      printMatrix(matrix)
      return 1
    }

    val word: _root_.scala.Predef.String = words(wordIndex)
    for (i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        // try fixing words in horizontal slots
        if (matrix(i)(j) == '-' || matrix(i)(j) == word(0)) {
          if (validHorizontalPosition(matrix, word, i, j)) {
            val updated: _root_.scala.Array[Boolean] = applyHorizontalPosition(matrix, word, i, j)
            if (solve(matrix, words, wordIndex + 1) == 1)
              return 1
            backtrackHorizontalPosition(matrix, updated, i, j)
          }
          // try fixing words in vertical slots
          if (validVerticalPosition(matrix, word, i, j)) {
            val updated: _root_.scala.Array[Boolean] = applyVerticalPosition(matrix, word, i, j)
            if (solve(matrix, words, wordIndex + 1) == 1)
              return 1
            backtrackVerticalPosition(matrix, updated, i, j)
          }
        }
      }
    }
    0
  }
}
