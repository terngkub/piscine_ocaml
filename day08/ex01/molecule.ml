class virtual molecule =
object (this)

  method virtual name : string
  method virtual formula : string

  method to_string =
    this#name ^ " (" ^ this#formula ^ ")"
    
  method equals (that:molecule) =
    (this#name = that#name) && (this#formula = that#formula)

end


class water =
object
  inherit molecule
  method name = "Water"
  method formula = "H2O"
end


class carbon_dioxide =
object
  inherit molecule
  method name = "Carbon dioxide"
  method formula = "CO2"
end


class carbon_monoxide =
object
  inherit molecule
  method name = "Carbon Monoxide"
  method formula = "CO"
end


class glucose =
object
  inherit molecule
  method name = "Glucose"
  method formula = "C6H12O6"
end


class alcohol =
object
  inherit molecule
  method name = "Alcolhol"
  method formula = "C2H6O"
end
