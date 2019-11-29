package genes.backbone

import chisel3._

case object A extends Stageable(UInt(32.W))

case object B extends Stageable(UInt(32.W))

case object C extends Stageable(UInt(32.W))

case object D extends Stageable(UInt(32.W))

case object A_PLUS_B extends Stageable(UInt(32.W))

case object A_MUL_B extends Stageable(UInt(32.W))

case object A_MUL_B_PLUS_A extends Stageable(UInt(32.W))
