class Simulation {

  private var currentTime = 0

  type Action = () => Unit

  case class WorkItem(time: Int, action: Action)

  private type Agenda = List[WorkItem]
  private var agenda: Agenda = List()

  private def insert(ag: Agenda, item: WorkItem): Agenda =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)

  def afterDelay(delay: Int)(block: => Unit) {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(1) {
        output setSignal !inputSig
      }
    }

    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(1) {
        output setSignal (a1Sig & a2Sig)
      }
    }

    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(1) {
        output setSignal (a1Sig | a2Sig)
      }
    }

    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate1(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      val andOutput = new Wire
      andGate(a1, a2, andOutput)
      inverter(andOutput, output)
    }

    a1 addAction orAction
    a2 addAction orAction
  }

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        actions.foreach(action => action())
      }

    def addAction(a: Action) {
      actions = a :: actions;
      a()
    }
  }

  private def next() {
    agenda match {
      case WorkItem(time, action) :: rest =>
        agenda = rest; currentTime = time; action()
      case List() =>
    }
  }

  def probe(name: String, wire: Wire) {
    wire addAction { () =>
      println(name + " " + currentTime + " new_value = " + wire.getSignal)
    }
  }

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started ***")
    }
    while (!agenda.isEmpty) next()
  }

  def init(): Unit = {
    val input1, input2, sum, carry = new Wire
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)

    input1 setSignal true
    input2 setSignal true
  }
}

val sim = new Simulation()
sim.init()
sim.run()

