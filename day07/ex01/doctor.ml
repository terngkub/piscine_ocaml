class doctor =
object

  val name:string = "Doctor"
  val mutable age:int = 60
  val sidekick:People.people = new People.people
  val mutable hp:int = 100

  method to_string =
    name
    ^ " ("
    ^ "age: " ^ (string_of_int age)
    ^ ", sidekick: <" ^ (sidekick#to_string) ^ ">"
    ^ ", hp: " ^ (string_of_int hp)
    ^ ")"

  method talk =
    print_endline "Hi! I'm the doctor"

  initializer
    print_endline "Doctor is initialized."

  method travel_in_time (start:int) (arrival:int) =
    age <- (age + (arrival - start));
    print_endline "draw TARDIS"

  method use_sonic_screwdriver =
    print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

  method private regenerate =
    hp <- 100

end