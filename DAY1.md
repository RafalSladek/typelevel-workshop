# Functional programming with LUCA

## DAY1

Algebraic types

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



```
  
  Either[A, Nothing] === A  // Either is addition of types
  A + 0 = A
  
  (A, Unit) === A // tuple is like multiplication of types
  A * 1 = A
  
  (A, Nothing) === Nothing
  A * 0 = 0
  
```
