structure Weak = struct
  type 'a t = 'a SMLofNJ.Weak.weak
  val new = SMLofNJ.Weak.weak
  val get = SMLofNJ.Weak.strong
end
