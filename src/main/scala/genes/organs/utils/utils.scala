package genes.organs

import chisel3.UInt
import chisel3.util.BitPat

package object utils {

  implicit class UInt2BitPat(uint: UInt) {
    def BP: BitPat = {
      BitPat(uint)
    }
  }

  implicit class BitPat2UInt(bitPat: BitPat) {
    def U: UInt = {
      BitPat.bitPatToUInt(bitPat)
    }
  }

}
