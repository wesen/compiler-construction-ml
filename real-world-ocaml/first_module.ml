module First_module =
struct
  let addone arg = arg + 1

  module InFirst =
  struct
    type suite = Heart | Spade | Club | Diamond
    type card = One of suite | Two of suite | Three of suite
  end
end
