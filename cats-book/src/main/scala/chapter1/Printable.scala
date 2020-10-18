package chapter1

case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A) : String
}


object PrintableInstances {

  implicit val stringPrintable = new Printable[String] {
    override def format(value: String): String = value.toString
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val catPrintable = new Printable[Cat] {
    override def format(value: Cat): String = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age year-old $color cat."
    }

  }

}

object Printable {

  def format[A](input: A)(implicit p: Printable[A]) : String = p.format(input)

  def print[A](input: A)(implicit p: Printable[A]) : Unit = println(p.format(input))


}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {

    def format(implicit p: Printable[A]) : String = p.format(value)

    def print(implicit p: Printable[A]) : Unit = println(p.format(value))

  }

}
