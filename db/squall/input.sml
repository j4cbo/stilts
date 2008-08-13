structure SquallInput = struct

  datatype vartype = String
                   | Int

  datatype reptype = Rlist | Rarray | Rvector | Rsingle | Roption

  datatype inbinding = IBtuple of vartype list
                     | IBrecord of (string * vartype) list
                     | IBunit

  datatype outbinding = OBtuple of reptype * vartype list
                      | OBrecord of reptype * (string * vartype) list
                      | OBunit

  type sqlfunc = { name: string,
                   inb: inbinding,
                   outb: outbinding,
                   sql: string }

end
