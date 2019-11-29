package genes.example.design.GenesExample.spec

object Causes {
  val SZ_CAUSE = 4
  val misaligned_fetch = 0x0
  val fetch_access = 0x1
  val illegal_instruction = 0x2
  val breakpoint = 0x3
  val misaligned_load = 0x4
  val load_access = 0x5
  val misaligned_store = 0x6
  val store_access = 0x7
  val user_ecall = 0x8
  val supervisor_ecall = 0x9
  val hypervisor_ecall = 0xa
  val machine_ecall = 0xb
  val fetch_page_fault = 0xc
  val load_page_fault = 0xd
  val store_page_fault = 0xf
  val all = {
    val res = collection.mutable.ArrayBuffer[Int]()
    res += misaligned_fetch
    res += fetch_access
    res += illegal_instruction
    res += breakpoint
    res += misaligned_load
    res += load_access
    res += misaligned_store
    res += store_access
    res += user_ecall
    res += supervisor_ecall
    res += hypervisor_ecall
    res += machine_ecall
    res += fetch_page_fault
    res += load_page_fault
    res += store_page_fault
    res.toArray
  }

  object BreakType extends Enumeration {
    val AddressBP, EevBP, LSAAdressBP = Value
  }

  def ExceptionPriority(cause: Int, breakType: BreakType.Value = BreakType.AddressBP): Int = {
    cause match {
      case `load_access` => 0
      case `store_access` => 1
      case `load_page_fault` => 2
      case `store_page_fault` => 3
      case `misaligned_load` => 4
      case `misaligned_store` => 5
      case `breakpoint` => breakType match {
        case BreakType.AddressBP => 12
        case _ => 6
      }
      case `machine_ecall` => 7
      case `supervisor_ecall` => 7
      case `user_ecall` => 7
      case `misaligned_fetch` => 8
      case `illegal_instruction` => 9
      case `fetch_access` => 10
      case `fetch_page_fault` => 11
    }
  }
}


