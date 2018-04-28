// exercise 11.2.1

def whileLoop(condition: => Boolean)(command: => Unit) {
  if (condition) {
    command;
    whileLoop(condition)(command)
  } else ()
}

def repeatLoop(command: => Unit)(condition: => Boolean): Unit = {
  whileLoop(!condition)(command)
}

def repeatLoop1(command: => Unit) = {
  class Inner {
    def until(condition: => Boolean) = {
      whileLoop(!condition)(command)
    }
  }
  new Inner
}

var i = 5;
repeatLoop1(println("test")) until {
  i = i - 1; i == 0
}