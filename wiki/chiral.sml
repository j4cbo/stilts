structure T = Thread (structure T = ThreadBase
                      structure RC = SelectReactorCore
                      structure C = ConfigPrintEverything)

structure CV = CondVar(T)
structure CS = ChiralSocketFn(T)
structure SU = ChiralSockUtil(CS)
structure LR = LineReader(CS.Socket)

structure HTTPServer = HTTPServerFn(structure CS = CS structure T = T)

