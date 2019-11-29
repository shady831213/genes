package genes.organs.tester.memory

import chisel3.iotesters.TesterOptionsManager

class TesterOptionsManagerWithMemory(mem: MemoryInterface) extends TesterOptionsManager {
  interpreterOptions = interpreterOptions.copy(
    blackBoxFactories = interpreterOptions.blackBoxFactories :+ new MemBevFactory(mem))
  treadleOptions = treadleOptions.copy(
    blackBoxFactories = treadleOptions.blackBoxFactories :+ new TreadleMemBevFactory(mem))
}
