structure SquallInput = struct

  datatype vartype = String | Int

  datatype varspec = Vrequired of vartype | Voption of vartype

  datatype reptype = Rlist | Rarray | Rvector | Rsingle | Roption

  datatype inbinding = IBtuple of varspec list
                     | IBrecord of (string * varspec) list
                     | IBunit

  datatype outbinding = OBtuple of reptype * varspec list
                      | OBrecord of reptype * (string * varspec) list
                      | OBunit

  type sqlfunc = { name: string,
                   inb: inbinding,
                   outb: outbinding,
                   sql: string }

end
