# Functional programming with LUCA

## DAY1

Algebraic type

Product types

The basic example is tuple 
``` 
type IntString =  (Int, String)
val IntString = (42, "foo")

type x[A,B] = (A,B)

val x String

Int either String

 sealed trait Contact
  case class Email(value: String) extends Contact
  case class PhoneNumber(value: String) extends Contact


```

case class is the fancy representation of product type.
