/**
 * Iterative string hardware.  (c) duane a. bailey
 * This hardware performs string length and string comparison operations,
 * 8 bytes (64 bits) at a time.  These operations are meant to be used as
 * part of an iteration that scans through the string a longword at a time.
 *
 * Each str8len returns the length of the string found in rs1.  The result is
 * a value 0 through 8, indicating how many of the 8 bytes appear before the
 * end of the string.  If the result is 8, of course, a full implementation of
 * strlen would consider the next longword chunk, until the end-of-string is
 * actually encountered.  Test with str8len.c.
 *
 * Each str8cmp acts like strncmp(8,rs1,rs2): it returns the difference between
 * the first different bytes of the string, or 0 if they are the same.  A result
 * of 256 indicates the strings were the same, so far, but comparison must be
 * continued.  Test with str8cmp.c.
 */
package williams

import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
/**
 * Strnlen, where n is 8.
 * Returns the length of the string contained within the (little-endian) long.
 */
class Str8len(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new Str8lenImp(this)
}

class WithStr8len extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8l = LazyModule.apply(new Str8len(OpcodeSet.custom0)(p))
     s8l
  })
})

/**
 * This accelerator computes the length of the string passed to it; result
 * is between 0 and 8 (if no null byte).  Interprets the long as a little-endian
 * string.
 */
class Str8lenImp(outer: Str8len)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // State is busy precisely while the instruction is executing; else ready.
   val s_ready :: s_busy :: Nil = Enum(Bits(), 2)
   val state = Reg(init = s_ready)   // the starting state
   val req_rd = Reg(io.resp.bits.rd) // the destination register
   // grab the bytes of the input long word
   val b0 = Reg(init = 0.U(8.W)) // lower byte
   val b1 = Reg(init = 0.U(8.W)) // higher byte
   val b2 = Reg(init = 0.U(8.W))
   val b3 = Reg(init = 0.U(8.W))
   val b4 = Reg(init = 0.U(8.W))
   val b5 = Reg(init = 0.U(8.W))
   val b6 = Reg(init = 0.U(8.W))
   val b7 = Reg(init = 0.U(8.W))

   // we're ready for a new command when state is ready.
   io.cmd.ready := (state === s_ready)
   // unit is busy when we're not in the ready state
   io.busy := (state =/= s_ready)

   // when command fires, grab the destination register and source values
   when (io.cmd.fire()) {
      req_rd := io.cmd.bits.inst.rd    // destination register number
      b0 := io.cmd.bits.rs1(7,0)  // source low byte
      b1 := io.cmd.bits.rs1(15,8)// source high byte
      b2 := io.cmd.bits.rs1(23,16)// source high byte
      b3 := io.cmd.bits.rs1(31,24)// source high byte
      b4 := io.cmd.bits.rs1(39,32)// source high byte
      b5 := io.cmd.bits.rs1(47,40)// source high byte
      b6 := io.cmd.bits.rs1(55,48)// source high byte
      b7 := io.cmd.bits.rs1(63,56)// source high byte
      state := s_busy                  // indicate we're busy.
   }
   val nul = 0.U(8.W)
   val end0 = b0 === nul
   val end1 = (b1 === nul) & ~end0
   val end2 = (b2 === nul) & ~end1
   val end3 = (b3 === nul) & ~end2
   val end4 = (b4 === nul) & ~end3
   val end5 = (b5 === nul) & ~end4
   val end6 = (b6 === nul) & ~end5
   val end7 = (b7 === nul) & ~end6
   val len = Mux(end0,0.U,Mux(end1,1.U,Mux(end2,2.U,Mux(end3,3.U,Mux(end4,4.U,
             Mux(end5,5.U,Mux(end6,6.U,Mux(end7,7.U,8.U))))))))
   
    // develop response: set reg # and value
   io.resp.bits.data := len
   io.resp.bits.rd := req_rd                           // to be written here
   io.resp.valid := (state === s_busy)                 // we're done

   // when we respond, become idle again
   when (io.resp.fire()) {
     state := s_ready
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}


/**
 * Strncmp, where n is 8.
 * Compares two strings contained within the (little-endian) longs.
 */
class Str8cmp(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new Str8cmpImp(this)
}

class WithStr8cmp extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8c = LazyModule.apply(new Str8cmp(OpcodeSet.custom0)(p))
     s8c
  })
})

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  If the strings are equal and the
 * end-of-string was not detected, the value is 256.
 * Interprets the long as a little-endian string.
 */
class Str8cmpImp(outer: Str8cmp)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // State is busy precisely while the instruction is executing; else ready.
   val s_ready :: s_busy :: Nil = Enum(Bits(), 2)
   val state = Reg(init = s_ready)   // the starting state
   val req_rd = Reg(io.resp.bits.rd) // the destination register
   // grab the bytes of the input long word
   val a0 = Reg(init = 0.U(8.W)) // lower byte
   val a1 = Reg(init = 0.U(8.W)) // higher byte
   val a2 = Reg(init = 0.U(8.W))
   val a3 = Reg(init = 0.U(8.W))
   val a4 = Reg(init = 0.U(8.W))
   val a5 = Reg(init = 0.U(8.W))
   val a6 = Reg(init = 0.U(8.W))
   val a7 = Reg(init = 0.U(8.W))
   val b0 = Reg(init = 0.U(8.W)) // lower byte
   val b1 = Reg(init = 0.U(8.W)) // higher byte
   val b2 = Reg(init = 0.U(8.W))
   val b3 = Reg(init = 0.U(8.W))
   val b4 = Reg(init = 0.U(8.W))
   val b5 = Reg(init = 0.U(8.W))
   val b6 = Reg(init = 0.U(8.W))
   val b7 = Reg(init = 0.U(8.W))

   // we're ready for a new command when state is ready.
   io.cmd.ready := (state === s_ready)
   // unit is busy when we're not in the ready state
   io.busy := (state =/= s_ready)

   // when command fires, grab the destination register and source values
   when (io.cmd.fire()) {
      req_rd := io.cmd.bits.inst.rd    // destination register number
      a0 := io.cmd.bits.rs1(7,0)  // source low byte
      a1 := io.cmd.bits.rs1(15,8)// source high byte
      a2 := io.cmd.bits.rs1(23,16)// source high byte
      a3 := io.cmd.bits.rs1(31,24)// source high byte
      a4 := io.cmd.bits.rs1(39,32)// source high byte
      a5 := io.cmd.bits.rs1(47,40)// source high byte
      a6 := io.cmd.bits.rs1(55,48)// source high byte
      a7 := io.cmd.bits.rs1(63,56)// source high byte
      b0 := io.cmd.bits.rs2(7,0)  // source low byte
      b1 := io.cmd.bits.rs2(15,8)// source high byte
      b2 := io.cmd.bits.rs2(23,16)// source high byte
      b3 := io.cmd.bits.rs2(31,24)// source high byte
      b4 := io.cmd.bits.rs2(39,32)// source high byte
      b5 := io.cmd.bits.rs2(47,40)// source high byte
      b6 := io.cmd.bits.rs2(55,48)// source high byte
      b7 := io.cmd.bits.rs2(63,56)// source high byte
      state := s_busy                  // indicate we're busy.
   }
   val nul = 0.U(8.W)

   val end0 = a0 === nul
   val end1 = (a1 === nul) & ~end0
   val end2 = (a2 === nul) & ~end1
   val end3 = (a3 === nul) & ~end2
   val end4 = (a4 === nul) & ~end3
   val end5 = (a5 === nul) & ~end4
   val end6 = (a6 === nul) & ~end5
   val end7 = (a7 === nul) & ~end6
   val d0 = a0-b0
   val d1 = a1-b1
   val d2 = a2-b2
   val d3 = a3-b3
   val d4 = a4-b4
   val d5 = a5-b5
   val d6 = a6-b6
   val d7 = a7-b7
   val diff = Mux(((d0 =/= 0.U) || end0), d0,
              Mux(((d1 =/= 0.U) || end1), d1,
              Mux(((d2 =/= 0.U) || end2), d2,
              Mux(((d3 =/= 0.U) || end3), d3,
              Mux(((d4 =/= 0.U) || end4), d4,
              Mux(((d5 =/= 0.U) || end5), d5,
              Mux(((d6 =/= 0.U) || end6), d6,
              Mux(((d7 =/= 0.U) || end7), d7, 256.U))))))))
   // the following only useful for sign extending the difference
   // val sdiff = Cat(Mux(diff(7)===1.U,"hffff_ffff_ffff_ff".U(56.W),0.U(56.W)),diff)
   // develop response: set reg # and value
   io.resp.bits.data := diff
   io.resp.bits.rd := req_rd                           // to be written here
   io.resp.valid := (state === s_busy)                 // we're done

   // when we respond, become idle again
   when (io.resp.fire()) {
     state := s_ready
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}
