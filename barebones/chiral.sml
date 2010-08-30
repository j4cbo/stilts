structure TH = Thread (structure T = ThreadBase
                      structure RC = SelectReactorCore
                      structure C = ConfigPrintEverything)

structure CV = CondVar(TH)
structure CS = ChiralSocketFn(TH)
structure SU = ChiralSockUtil(CS)
structure LR = LineReader(CS.Socket)

structure CHTTPServer = HTTPServerFn(structure CS = CS structure T = TH)

