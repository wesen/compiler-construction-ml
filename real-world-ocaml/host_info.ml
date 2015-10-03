open Core_extended.Std

type host_info = {
  hostname: string;
  os_name: string;
  cpu_arch: string;
  timestamp: Time.t;
}

let my_host =
  let sh = Shell.sh_one_exn in
  { hostname = sh "hostname";
    os_name = sh "uname -s";
    cpu_arch = sh "uname -p";
    timestamp = Time.now();
  }

type 'a timestamped = { item: 'a; time: Time.t }

type foo = {
  item: string;
  time: Time.t;
  a: int
}

let first_timestamped_ (list : 'a timestamped list) : 'a timestamped option =
  List.reduce list ~f:(fun a b -> if a.time < b.time then a else b)

let first_timestamped (list : foo list) : foo option =
  List.reduce list ~f:(fun a b -> if a.time < b.time then a else b)
