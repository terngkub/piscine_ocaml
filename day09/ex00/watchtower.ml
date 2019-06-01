type hour = int


let zero:hour = 0


let add (a:hour) (b:hour) :hour = (a + b) mod 12


let sub (a:hour) (b:hour) :hour = (12 + a - b) mod 12
