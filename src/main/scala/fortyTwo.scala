// Very simple accelerator, based on the Translator accelerator in
//   rocket-chip/src/main/scala/tile/LazyRoCC.scala
package fortyTwo

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}

/**
 * Compare and exchange two bytes.
 * inputs: io.a and io.b
 * outputs: io.small and io.large
 */
class CoEx8 extends Module {
  val io = IO(new Bundle{
     val a = Input(UInt(8.W))
     val b = Input(UInt(8.W))
     val small = Output(UInt(8.W))
     val large = Output(UInt(8.W))
  })
  val inOrder = io.a <= io.b
  io.small := Mux(inOrder,io.a,io.b)
  io.large := Mux(inOrder,io.b,io.a)
}

/**
 * Factory method for compare-exchange 8-bit values.
 */
object CoEx8 {
   def apply() = {
      Module(new CoEx8())
   }
}

/**
 * Compare and exchange two shorts.
 * inputs: io.a and io.b
 * outputs: io.small and io.large
 */
class CoEx16 extends Module {
  val io = IO(new Bundle{
     val a = Input(UInt(16.W))
     val b = Input(UInt(16.W))
     val small = Output(UInt(16.W))
     val large = Output(UInt(16.W))
  })
  val inOrder = io.a <= io.b
  io.small := Mux(inOrder,io.a,io.b)
  io.large := Mux(inOrder,io.b,io.a)
}

/**
 * Factory method for compare-exchange 16-bit values.
 */
object CoEx16 {
   def apply() = {
      Module(new CoEx16())
   }
}

/**
 * Compare and exchange two words.
 * inputs: io.a and io.b
 * outputs: io.small and io.large
 */
class CoEx32 extends Module {
  val io = IO(new Bundle{
     val a = Input(UInt(32.W))
     val b = Input(UInt(32.W))
     val small = Output(UInt(32.W))
     val large = Output(UInt(32.W))
  })
  val inOrder = io.a <= io.b
  io.small := Mux(inOrder,io.a,io.b)
  io.large := Mux(inOrder,io.b,io.a)
}

/**
 * Factory method for compare-exchange 32-bit values.
 */
object CoEx32 {
   def apply() = {
      Module(new CoEx32())
   }
}

/**
 * Trivial RoCC module to move 42 into the destination register.
 */
class FortyTwo(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new FortyTwoImp(this)
}

class FortyTwoImp(outer: FortyTwo)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
{
  val s_idle :: s_resp :: Nil = Enum(Bits(), 2) // 4?
  val state = Reg(init = s_idle)  // start state
  val req_rd = Reg(io.resp.bits.rd) // destination reg number

  // this unit ready when in the s_idle state
  io.cmd.ready := (state === s_idle)

  // this unit's response to cmd execution
  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd    
    state := s_resp
  }

  // when we respond, become idle again
  when (io.resp.fire()) {
    state := s_idle
  }

  // response packet:
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := 42.U
  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
}

class WithFortyTwo extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val fortyTwo = LazyModule.apply(new FortyTwo(OpcodeSet.custom0)(p))
     fortyTwo
  })
})


/**
 * Accelerator for sorting the 8 bytes of a long word.
 * 
 */
class WByteSort(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new WByteSortImp(this)
}

class WithWByteSort extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val bs = LazyModule.apply(new WByteSort(OpcodeSet.custom0)(p))
     bs
  })
})

/**
 * This accelerator sorts the 8 bytes of a long word.
 * This is a stateless accelerator that does not reference memory.
 */
class WByteSortImp(outer: WByteSort)(implicit p: Parameters)
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

   // stage 0
   val s00 = CoEx8()
   s00.io.a := b0
   s00.io.b := b1
   val s01 = CoEx8()
   s01.io.a := b2
   s01.io.b := b3
   val s02 = CoEx8()
   s02.io.a := b4
   s02.io.b := b5
   val s03 = CoEx8()
   s03.io.a := b6
   s03.io.b := b7

   val s10 = CoEx8()
   s10.io.a := s00.io.small
   s10.io.b := s01.io.large
   val s11 = CoEx8()
   s11.io.a := s00.io.large
   s11.io.b := s01.io.small
   val s12 = CoEx8()
   s12.io.a := s02.io.small
   s12.io.b := s03.io.large
   val s13 = CoEx8()
   s13.io.a := s02.io.large
   s13.io.b := s03.io.small

   val s20 = CoEx8()
   s20.io.a := s10.io.small
   s20.io.b := s11.io.small
   val s21 = CoEx8()
   s21.io.a := s10.io.large
   s21.io.b := s11.io.large
   val s22 = CoEx8()
   s22.io.a := s12.io.large
   s22.io.b := s13.io.large
   val s23 = CoEx8()
   s23.io.a := s12.io.small
   s23.io.b := s13.io.small

   val s30 = CoEx8()
   s30.io.a := s20.io.small
   s30.io.b := s22.io.large
   val s31 = CoEx8()
   s31.io.a := s20.io.large
   s31.io.b := s22.io.small
   val s32 = CoEx8()
   s32.io.a := s21.io.small
   s32.io.b := s23.io.large
   val s33 = CoEx8()
   s33.io.a := s21.io.large
   s33.io.b := s23.io.small

   val s40 = CoEx8()
   s40.io.a := s30.io.small
   s40.io.b := s32.io.small
   val s41 = CoEx8()
   s41.io.a := s31.io.small
   s41.io.b := s33.io.small
   val s42 = CoEx8()
   s42.io.a := s30.io.large
   s42.io.b := s32.io.large
   val s43 = CoEx8()
   s43.io.a := s31.io.large
   s43.io.b := s33.io.large

   val s50 = CoEx8()
   s50.io.a := s40.io.small
   s50.io.b := s41.io.small
   val s51 = CoEx8()
   s51.io.a := s40.io.large
   s51.io.b := s41.io.large
   val s52 = CoEx8()
   s52.io.a := s42.io.small
   s52.io.b := s43.io.small
   val s53 = CoEx8()
   s53.io.a := s42.io.large
   s53.io.b := s43.io.large

    // develop response: set reg # and value
   io.resp.bits.data := Cat(s53.io.large,s53.io.small,
                            s52.io.large,s52.io.small,
                            s51.io.large,s51.io.small,
                            s50.io.large,s50.io.small)
   io.resp.bits.rd := req_rd                           // to be written here
   io.resp.valid := (state === s_busy)                 // we're done

   // when we respond, become idle again
   when (io.resp.fire()) {
   // printf("result=%x in register %d\n",Cat(s53.io.large,s53.io.small,
   //                                         s52.io.large,s52.io.small,
   //                                         s51.io.large,s51.io.small,
   //                                         s50.io.large,s50.io.small),req_rd)
     state := s_ready
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}

/**
 * Strnlen, where n is 8.
 * Returns the length of the string contained within the (little-endian) long.
 */
class WStr8len(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new WStr8lenImp(this)
}

class WithWStr8len extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8l = LazyModule.apply(new WStr8len(OpcodeSet.custom0)(p))
     s8l
  })
})

/**
 * This accelerator computes the length of the string passed to it; result
 * is between 0 and 8 (if no null byte).  Interprets the long as a little-endian
 * string.
 */
class WStr8lenImp(outer: WStr8len)(implicit p: Parameters)
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
//     printf("result=%x in register %d\n",len,req_rd)
     state := s_ready
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}

/**
 * Strncmp, where n is 8.
 * Compares two strings contained within the (little-endian) longs.
 */
class WStr8cmp(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new WStr8cmpImp(this)
}

class WithWStr8cmp extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8c = LazyModule.apply(new WStr8cmp(OpcodeSet.custom0)(p))
     s8c
  })
})

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  If the strings are equal and the
 * end-of-string was not detected, the value is 256.
 * Interprets the long as a little-endian string.
 */
class WStr8cmpImp(outer: WStr8cmp)(implicit p: Parameters)
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
//     printf("result=%x in register %d\n",len,req_rd)
     printf("a0=0x%x 1=%x 2=%x 3=%x 4=%x 5=%x 6=%x 7=%x\n",a0,a1,a2,a3,a4,a5,a6,a7)
     printf("b0=0x%x 1=%x 2=%x 3=%x 4=%x 5=%x 6=%x 7=%x\n",b0,b1,b2,b3,b4,b5,b6,b7)
     printf("e0=0x%x 1=%x 2=%x 3=%x 4=%x 5=%x 6=%x 7=%x\n",end0,end1,end2,end3,end4,end5,end6,end7)
     printf("d0=0x%x 1=%x 2=%x 3=%x 4=%x 5=%x 6=%x 7=%x\n",d0,d1,d2,d3,d4,d5,d6,d7)
     printf("diff=0x%x\n",diff)
     state := s_ready
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}

class WCompExchange(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new WCompExchangeImp(this)
}

class WithWCompExchange extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val cex = LazyModule.apply(new WCompExchange(OpcodeSet.custom0)(p))
     cex
  })
})

/**
 * This accelerator compares the two low bytes of the first source
 * register and exchanges them if they are out-of-order.
 * After this instruction, the least significant byte (bits 7..0) is
 * no greater than the next least significant byte (bits 15..8).
 * This is a stateless accelerator that does not reference memory.
 */
class WCompExchangeImp(outer: WCompExchange)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // State is busy precisely while the instruction is executing; else ready.
   val s_ready :: s_busy :: Nil = Enum(Bits(), 2)
   val state = Reg(init = s_ready)   // the starting state
   val req_rd = Reg(io.resp.bits.rd) // the destination register
   val lowByte = Reg(init = 0.U(8.W)) // lower byte
   val highByte = Reg(init = 0.U(8.W)) // higher byte

   // we're ready for a new command when state is ready.
   io.cmd.ready := (state === s_ready)
   // unit is busy when we're not in the ready state
   io.busy := (state =/= s_ready)

   // when command fires, grab the destination register and source values
   when (io.cmd.fire()) {
      req_rd := io.cmd.bits.inst.rd    // destination register number
      lowByte := io.cmd.bits.rs1(7,0)  // source low byte
      highByte := io.cmd.bits.rs1(15,8)// source high byte
      state := s_busy                  // indicate we're busy.
   }

   // combinational ordering of the two bytes
   val cex = Module(new CoEx8())
   cex.io.a := lowByte
   cex.io.b := highByte
   val smaller = cex.io.small 
   val bigger = cex.io.large
   //val leq = lowByte <= highByte
   //val bigger = Mux(leq,highByte,lowByte)
   //val smaller = Mux(leq,lowByte,highByte)

   // develop response: set reg # and value
   io.resp.bits.data := Cat(0.U(48.W),bigger,smaller)  // 64-bit response
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
 * Add the following to the example RocketConfigs.scala file:
class FortyTwoRocketConfig extends Config(
  new WithTop ++
  new WithBootROM ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new fortyTwo.WithFortyTwo ++                                // add "42" rocc accelerator
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new freechips.rocketchip.system.BaseConfig
)
*/

/**
 * Bresenam's Cirle calculation.
 * - call with function code 0: use rs1 as the radius.
 * - call with function code 1: return current x in xd and iterate; or return 0.
 */

/**
 * Trivial RoCC module to move 42 into the destination register.
 */
class WCircle(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new WCircleImp(this)
}

class WithWCircle extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8c = LazyModule.apply(new WCircle(OpcodeSet.custom0)(p))
     s8c
  })
})

/**
 * This accelerator computes the X coordinates of an octant of a circle with
 * radius R.
 * It makes use of the function codes passed with the instruction:
 *  - function code 0: set R to the rs1
 *  - function code 1: set rd's value to current X, or 0 if done.
 */
class WCircleImp(outer: WCircle)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // State is busy precisely while the instruction is executing; else ready.
   val s_ready :: s_initialized :: s_computing :: Nil = Enum(Bits(), 3)
   val state = Reg(init = s_ready)   // the starting state
   val req_rd = Reg(io.resp.bits.rd) // the destination register
   val funct = io.cmd.bits.inst.funct
   val do_setR = funct === 0.U
   val do_iter = funct === 1.U
   val x = Reg(init=0.U(64.W))
   val y = Reg(init=0.U(64.W))
   val e = Reg(init=0.U(64.W))
   val r = Reg(init=0.U(64.W))
   val cooked = (x < y)
   val e2 = (e+y+y+1.U)
   val epos = ~e2(63)
   val e3 = (e2+1.U-x-x)

   // we're ready for a new command when state is ready.
   io.cmd.ready := (state =/= s_computing)
   // unit is busy when we're computing
   io.busy := (state === s_computing)

   // when command fires, grab the destination register and source values
   when (io.cmd.fire() && do_setR) {
      r := io.cmd.bits.rs1
      y := 0.U
      x := io.cmd.bits.rs1
      e := (~io.cmd.bits.rs1)+1.U
      printf("Setting r=%d\n",io.cmd.bits.rs1)
      state := s_computing                  // indicate we're busy.
   }
   when (io.cmd.fire() && do_iter) {
      req_rd := io.cmd.bits.inst.rd
      printf("Attempting return of x=%d\n",Mux(cooked,0.U,x))
      x := Mux(epos,x-1.U,x)
      y := y+1.U
      e := Mux(epos,e3,e2)
      state := s_computing
   }
   io.resp.valid := (state === s_computing)        // we're done
   io.resp.bits.data := Mux(cooked,0.U,x) 
   io.resp.bits.rd := req_rd              // to be written here

   // when we respond, become idle again
   when (io.resp.fire()) {
     state := Mux(!cooked,s_initialized,s_ready)
   }

   io.interrupt := Bool(false)     // instructions never interrupt
   io.mem.req.valid := Bool(false)  // we don't use memory
}
