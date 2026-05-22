module type MAGIC = sig 
  val magic : 'a 
  module Magic(M : sig module type S end) : M.S
end
