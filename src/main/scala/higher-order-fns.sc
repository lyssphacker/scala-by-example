case class Book(title: String, authors: List[String])

val books: List[Book] = List(
  Book("Structure and Interpretation of Computer Programs",
    List("Abelson, Harold", "Sussman, Gerald J.")),
  Book("Principles of Compiler Design",
    List("Aho, Alfred", "Ullman, Jeffrey")),
  Book("Programming in Modula-2",
    List("Wirth, Niklaus")),
  Book("Introduction to Functional Programming",
    List("Bird, Richard")),
  Book("The Java Language Specification",
    List("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")))

for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title

books.filter(b => b.authors.filter(a => a.startsWith("Bird")).nonEmpty).map(b => b.title)