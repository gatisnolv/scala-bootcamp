package com.evolutiongaming.bootcamp.basics

object ClassesAndTraits {
  // You can follow your progress using the tests in `ClassesAndTraitsSpec`:
  //   sbt "testOnly com.evolutiongaming.bootcamp.basics.ClassesAndTraitsSpec"

  // Classes in Scala are blueprints for creating object instances. They can contain methods, values,
  // variables, types, objects, traits, and classes which are collectively called members.

  class MutablePoint(var x: Double, var y: Double) {
    def move(dx: Double, dy: Double): Unit = {
      x = x + dx
      y = y + dy
    }

    override def toString: String = s"($x, $y)"
  }

  val point1 = new MutablePoint(3, 4)
  println(point1.x) // 3.0
  println(point1) // (3.0, 4.0)

  // Question. Is MutablePoint a good design? Why or why not?

  // Traits define a common interface that classes conform to. They are similar to Java's interfaces.

  // A trait can be thought of as a contract that defines the capabilities and behaviour of a component.

  // Subtyping
  // Where a given trait is required, a subtype of the trait can be used instead.

  // Classes and singleton objects can extend traits.
  //
  // This allows "programming to the interface" approach where you depend on traits instead of their
  // specific implementations (classes or objects).
  //
  // This makes code more reusable and testable.

  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  sealed trait Shape3D extends Located3D with Movable3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def area: Double = 0
    override def move(dx: Double, dy: Double) = Point(x + dx, y + dy)
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = 4 / 3 * Math.PI * Math.pow(radius, 3)
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
  }

  final case class Cube(minX: Double, minY: Double, minZ: Double, edgeLength: Double) extends Shape3D {
    val cuboid = Cuboid(x, y, z, edgeLength, edgeLength) // check if this works - I think shouldnt, should use min.. instead
    override def surfaceArea: Double = cuboid.surfaceArea
    override def volume: Double = cuboid.volume
    override def x: Double = cuboid.x
    override def y: Double = cuboid.y
    override def z: Double = cuboid.z
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, edgeLength)
  }

  final case class Cuboid(minX: Double, minY: Double, minZ: Double, squareEdgeLength: Double, height: Double) extends Shape3D {
    override def surfaceArea: Double = 2 * Math.pow(squareEdgeLength, 2) + 4 * squareEdgeLength * height
    override def volume: Double = Math.pow(squareEdgeLength, 2) * height
    override def x: Double = minX
    override def y: Double = minY
    override def z: Double = minZ
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, squareEdgeLength, height)
  }

  final case class Tetrahedron(point1: Point3D, point2: Point3D, point3: Point3D, point4: Point3D) extends Shape3D {
    // surfaceArea and volume methods left unimplemented due to lengthy math formulas
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def x: Double = point1.x
    override def y: Double = point1.y
    override def z: Double = point1.z
    override def move(dx: Double, dy: Double, dz: Double): Tetrahedron = Tetrahedron(
      Point3D(point1.x + dx, point1.y + dy, point1.z + dz),
      Point3D(point2.x + dx, point2.y + dy, point2.z + dz),
      Point3D(point3.x + dx, point3.y + dy, point3.z + dz),
      Point3D(point4.x + dx, point4.y + dy, point4.z + dz)
    )
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def area: Double = Math.PI * Math.pow(radius, 2)
    override def move(dx: Double, dy: Double) = Circle(x + dx, y + dy, radius)
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
  }

  // Case Classes
  //
  // Case classes are like regular classes, but with extra features which make them good for modelling
  // immutable data. They have all the functionality of regular classes, but the compiler generates additional
  // code, such as:
  // - Case class constructor parameters are public `val` fields, publicly accessible
  // - `apply` method is created in the companion object, so you don't need to use `new` to create a new
  //   instance of the class
  // - `unapply` method which allows you to use case classes in `match` expressions (pattern matching)
  // - a `copy` method is generated
  // - `equals` and `hashCode` methods are generated, which let you compare objects & use them in collections
  // - `toString` method is created for easier debugging purposes

  val point2 = Point(1, 2)
  println(point2.x)

  val shape: Shape = point2
  val point2Description = shape match {
    case Point(x, y) => s"x = $x, y = $y"
    case _           => "other shape"
  }

  val point3 = point2.copy(x = 3)
  println(point3.toString) // Point(3, 2)

  // Exercise. Implement an algorithm for finding the minimum bounding rectangle
  // (https://en.wikipedia.org/wiki/Minimum_bounding_rectangle) for a set of `Bounded` objects.
  //
  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

      // if needed, fix the code to be correct
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape): String = x match {
    case Point(x, y)                             => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius)        => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
    case Rectangle(minX, minY, lengthX, lengthY) => s"Rectangle(minX= $minX, minY= $minY, lengthX= $lengthX, lengthY= $lengthY)"
    case Square(minX, minY, borderLength)        => s"Square(minX= $minX, minY= $minY, borderLength= $borderLength)"
    case Triangle(point1, point2, point3)        => s"Triangle(point1 x= $point1.x, y=$point1.y, point2 x= $point2.x, y=$point2.y, point3 x= $point3.x, y=$point3.y)"
  }

  // Singleton objects are defined using `object`.
  // It is a class that has exactly one instance.
  // They can be thought of as "static classes" in Java.
  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  def main(args: Array[String]) = {
    val a = Circle(1, 3, 4)
    println(a.radius)
    val movedA = a.move(-1, -1)
    println(movedA.radius)
  }

  // An `object` defined with the same name as an existing trait or class is called
  // a "companion object".

  // Use it to contain methods and values related to this trait or class, but that aren't
  // specific to instances of this trait or class.

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = ???
  }

  // Exercise. Add another Shape class called Rectangle and check that the compiler catches that we are
  // missing code to handle it in `describe`.

  // Let us come back to our `Shape`-s and add a `Movable` trait
  // which will have a method:
  //
  //   def move(dx: Double, dy: Double)
  //
  // How should we implement `move` for various types?
  //
  // What should be the return type of the `move` method in `Shape`? In `Point` and other sub-types?
  final case class Rectangle(minX: Double, minY: Double, lengthX: Double, lengthY: Double) extends Shape {
    // assumes positive lengths, otherwise could be abused. Same problem would be with e.g. minX and maxX - the user could specify minX>maxX
    // really a problem for other shapes as well e.g negative radius
    override def maxX: Double = minX + lengthX
    override def maxY: Double = minY + lengthY
    override def area: Double = lengthX * lengthY
    override def move(dx: Double, dy: Double) = Rectangle(minX + dx, minY + dy, lengthX, lengthY)
    override def x: Double = minX
    override def y: Double = minY
  }

  final case class Square(minX: Double, minY: Double, borderLength: Double) extends Shape {
    val rectangle = Rectangle(minX, minY, borderLength, borderLength)
    override def area: Double = rectangle.area
    override def move(dx: Double, dy: Double): Square = Square(minX + dx, minY + dy, borderLength)
    override def x: Double = rectangle.x
    override def y: Double = rectangle.y
    override def maxX: Double = rectangle.maxX
    override def maxY: Double = rectangle.maxY
  }

  final case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape {
    val points = List(point1, point2, point3)
    // area method left unimplemented due to lengthy math
    override def area: Double = ???
    // With this design, depending on the angle of the triangle this 'anchor point' may be outside of the triangle
    override def x: Double = minX
    override def y: Double = minY
    override def minX: Double = points.map(_.x).min
    override def maxX: Double = points.map(_.x).max
    override def minY: Double = points.map(_.y).min
    override def maxY: Double = points.map(_.y).max
    override def move(dx: Double, dy: Double): Triangle = Triangle(Point(point1.x + dx, point1.y + dy), Point(point2.x + dx, point2.y + dy), Point(point3.x + dx, point3.y + dy))
  }

  // Exercise. Change the implementation of `minimumBoundingRectangle` to return a `Rectangle` instance.
  // What are the pros & cons of each implementation?

  // Exercise. The tests for `minimumBoundingRectangle` in `ClassesAndTraitsSpec` are insufficient.
  // Improve the tests.

  // Generic classes and type parameters

  // In a similar way as we saw with polymorphic methods, classes and traits can also take type parameters.
  // For example, you can define a `Stack[A]` which works with any type of element `A`.

  // Question. Do you agree with how the stack is modelled here? What would you do differently?
  final case class Stack[A](elements: List[A] = Nil) {
    def push(x: A): Stack[A] = Stack(x :: elements)
    def peek: Option[A] = elements.headOption
    def pop: Option[(A, Stack[A])] = peek match {
      case None    => None
      case Some(x) => Some((x, Stack(elements.tail)))
    }
    def pop2: Option[(A, Stack[A])] = peek.map(x => (x, Stack(elements.tail)))
    def pop3: Option[(A, Stack[A])] = elements match {
      case Nil     => None
      case x :: xs => Some((x, Stack(xs)))
    }
  }

  val stack = Stack("a" :: "b" :: Nil)
  val a1 = stack.peek
  val retrieved1 = stack.pop
  val retrieved2 = stack.pop2
  val retrieved3 = stack.pop3

  println(a1)
  println(stack)
  println(retrieved1)

  val (_, poppedStack1) = retrieved1.get
  println(poppedStack1.pop)
  val (_, poppedStack2) = retrieved2.get
  println(poppedStack2.pop)
  val (_, poppedStack3) = retrieved3.get
  println(poppedStack3.pop)

  val (_, emptyStack) = poppedStack3.pop.get
  println(emptyStack.pop)

  // Let us come back to our `Shape`-s and add a `Movable` trait
  // which will have a method:
  //
  //   def move(dx: Double, dy: Double)
  //
  // What should be the return type of the `move` method?
  //
  // What if we want `Point#move` to return `Point` and
  // `Circle#move` to return `Circle`?
  //
  // What if we want to ensure that all `move` methods only return
  // other `Movable`-s and not something unrelated like `String`-s?

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.
}
