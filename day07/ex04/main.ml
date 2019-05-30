let do_war () =

  let people_army = new Army.army in
  let rec add_people n =
    if n > 0 then begin
      people_army#add (new People.people);
      add_people (n - 1)
    end
  in

  let doctor_army = new Army.army in
  let rec add_doctor n =
    if n > 0 then begin
      doctor_army#add (new Doctor.doctor);
      add_doctor (n - 1)
    end
  in

  let dalek_army = new Army.army in
  let rec add_dalek n =
    if n > 0 then begin
      dalek_army#add (new Dalek.dalek);
      add_dalek (n - 1)
    end
  in

  Random.self_init ();
  add_people ((Random.int 40) + 1);
  add_doctor ((Random.int 3) + 1);
  add_dalek ((Random.int 10) + 1);

  let galifrey = new Galifrey.galifrey (people_army#get_member) (doctor_army#get_member) (dalek_army#get_member) in
  galifrey#do_time_war


let () = do_war ()
