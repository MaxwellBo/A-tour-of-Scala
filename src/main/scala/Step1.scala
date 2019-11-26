import pprint.log

object Step1 {

  object Types {
    sealed trait Colour
    case object Red extends Colour
    case object Blue extends Colour
    case object Green extends Colour
    case class CustomColour(r: Byte, g: Byte, b: Byte)

    sealed trait Country
    case object Australia extends Country
    case object UnitedStates extends Country
    case object NewZealand extends Country
    case object Netherlands extends Country
    case object Brazil extends Country
    case object Ukraine extends Country
    case object Poland extends Country

    sealed trait State
    case object NSW extends State
    case object QLD extends State
    case object VIC extends State
    case object ADL extends State
    case object WA extends State
    case object TAS extends State

    final case class Address(
      apartment: Option[Int],
      streetNumber: Int,
      streetName: String,
      city: String,
      state: State,
      country: Country
    )
  }

  final case class Person(name: String)

  // We can create a person easy enough
  val person = Person("Alice")
  log(s"Person : ${person}")

  //but because the person is an immutable object, modifying the value requires copying
  val personPrime = person.copy(name = "Alfred")
  log(s"Person: $personPrime")

  // That's already annoying, but it gets worse with nested structures
  final case class State(name: String, phonePrefix: String, regionalManager: Person)
  final case class Address(apartment: Option[Int], streetNumber: Int, streetName: String, city: String, state: State)
  final case class Location(address: Address, buildingManager: Person)
  final case class BusinessUnit(manager: Person, purpose: String, location: Location)

  val regionalManager = Person("Jefferson \"Boss\" Hogg")
  val nsw = State("New South Wales", "02", regionalManager)

  val threeFourOne = Location(Address(None, 341, "George St", "Sydney", nsw), Person("Bill Ding"))

  log(s"341, looked after by Bill Ding: $threeFourOne")

  // If I want to update the regional manager for 341, I have to unwrap and re-wrap the whole object tree
  val threeFourOnePrime = threeFourOne.copy(
    address = threeFourOne.address.copy(
      state = threeFourOne.address.state.copy(  // Such repetition! Disgusting!
        regionalManager = Person("Newell Guy")
      )
    )
  )

  log(s"341 with Newell Guy running things: $threeFourOnePrime")

  // This can result in a lot of bespoke update functions along the lines of...
  def setRegionalManager(location: Location, regionalManager: Person): Location = {
    location.copy(
      address = location.address.copy(
        state = location.address.state.copy(
          regionalManager = regionalManager
        )
      )
    )
  }

  // This seems fine in the short term
  val threeFourOneButWithBeans = setRegionalManager(threeFourOne, Person("Beans Supreme")) // it works!

  log(s"341 but with Beans: $threeFourOneButWithBeans")

  // Until you want to compose things...
  val innovationOps = BusinessUnit(Person("Manny Jürr"), "Building cool stuff then killing it", threeFourOne)

  def promotePerson(businessUnit: BusinessUnit) = businessUnit.copy(
    location = setRegionalManager(businessUnit.location, businessUnit.manager)
  )

  // This is simple, but it's going to result in a lot of hand written code as our data types grow. This scales poorly.
  // We're still having to manually pull out and replace things, it's just spread across multiple functions.
  // That's a bit of a cost to pay for immutable data.

  log(s"\n\nSomeone now has way too many responsibilities: ${promotePerson(innovationOps)}")





  val threeFourThree = Location(Address(None, 343, "George St", "Sydney", nsw), Person("Rocky Roll"))
  val threeSixThree = Location(Address(None, 363, "George St", "Sydney", nsw), Person("Petey Graffiti"))

  val sydneyOffices = List(threeFourOne, threeFourThree, threeSixThree)
}
