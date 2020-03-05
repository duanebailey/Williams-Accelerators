/**
 * Two modules for integer root calculation. (c) duane a. bailey
 * Root64 - Compute the integer square root of a 64 bit value (single instr.)
 * Root32 - Multiple pass integer square root of a 32 bit value.
 */
package williams

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}


/**
 * Lazy object for 32 bit integer root calculation 
 */
class Root32(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new Root32Imp(this)
}

class WithRoot32 extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val root = LazyModule.apply(new Root32(OpcodeSet.custom0)(p))
     root
  })
})

/**
 * Generic stage for computing another bit of the root.
 * Generates and returns state (n, remainder, root) after MINOR states of
 * the root-finding pipeline.  In testing code, invoke the instruction
 * 16/MINOR times, feeding result as next input.
 */
class RootBit extends Module {
  val io = IO(new Bundle {
                val nIn = Input(UInt(64.W))
                val remainderIn = Input(UInt(32.W))
                val rootIn = Input(UInt(32.W))
                val nOut = Output(UInt(64.W))
                val remainderOut = Output(UInt(32.W))
                val rootOut = Output(UInt(32.W))
              })

  val v = Cat(io.remainderIn(29,0),io.nIn(63,62))
  val remainderDraft = v-Cat(io.rootIn(29,0),1.U(2.W))
  val neg = remainderDraft(31)

  io.nOut := Cat(io.nIn(61,0),0.U(2.W))
  io.remainderOut := Mux(neg === 1.U,v,remainderDraft)
  io.rootOut := Cat(io.rootIn(30,0),~neg)
}

/**
 * This hardware computes the 32-bit integer square root of a 64 bit number.
 * Input and output are encoded as follows:
 *    n         - bits 63..32
 *    remainder - bits 31..16
 *    root      - bits 15..0
 * Output is result after computing MINOR additional bits of root and reducing
 * n by 2*MINOR bits.  Remainder is the additional amount necessary to bring
 * root*root up to (the initial) n.
 * This instruction must be called 16/MINOR times, feeding output of one to
 * the input of the next.
 */
class Root32Imp(outer: Root32)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // States of instruction execution:
   val readyState :: computeState :: doneState :: Nil = Enum(Bits(),3)
   val state = Reg(init = readyState)
   val destReg = Reg(io.resp.bits.rd)
   val n = Reg(UInt(64.W))        // working value
   val result = Reg(init = 0.U(64.W))
   val MINOR = 4
  

   io.cmd.ready := state === readyState
   io.busy := false.B; // state =/= readyState // experiment: try setting to false.B

   // initialize command
   switch (state) {
     is(readyState) {
       when (io.cmd.fire()) {
         destReg := io.cmd.bits.inst.rd
         n := io.cmd.bits.rs1
         state := computeState
       }
     }
     is(computeState) {
        val stages = Vec(Seq.fill(MINOR){ Module(new RootBit()).io })
        stages(0).nIn := Cat(n(63,32),0.U(32.W))
        stages(0).remainderIn := n(31,16) 
        stages(0).rootIn := n(15,0)
        for (i <- 0 until (MINOR-1)) {
           stages(i+1).nIn := stages(i).nOut
           stages(i+1).remainderIn := stages(i).remainderOut
           stages(i+1).rootIn := stages(i).rootOut
        }
        result := Cat(stages(MINOR-1).nOut(63,32),stages(MINOR-1).remainderOut(15,0),stages(MINOR-1).rootOut(15,0))
        state := doneState
     }
     is(doneState) {
       when (io.resp.fire()) {
         state := readyState
       }
     }
   }
   // response bundle:
   io.resp.bits.rd := destReg
   io.resp.bits.data := result
   io.resp.valid := state === doneState

   // other odds and ends:
   io.interrupt := false.B     // no errors will be generated
   io.mem.req.valid := false.B // ignore memory
}

/**
 * Lazy object for 64-bit integer root calculation 
 */
class Root64(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new Root64Imp(this)
}

class WithRoot64 extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val root = LazyModule.apply(new Root64(OpcodeSet.custom0)(p))
     root
  })
})


/**
 * This hardware computes the 64-bit integer square root of a 64 bit number.
 * Operand 1 is a 64-bit unsigned value.  (32-bit) root is stored in
 * destination.
 */
class Root64Imp(outer: Root64)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // States of instruction execution:
   val readyState :: computeState :: doneState :: Nil = Enum(Bits(),3)
   val state = Reg(init = readyState)
   val destReg = Reg(io.resp.bits.rd)
   val n = Reg(UInt(64.W))        // working value
   val result = Reg(init = 0.U(64.W))
  

   io.cmd.ready := state === readyState
   io.busy := false.B; // state =/= readyState // experiment: try setting to false.B


   // initialize command
   switch (state) {
     is(readyState) {
       when (io.cmd.fire()) {
         destReg := io.cmd.bits.inst.rd
         n := io.cmd.bits.rs1
         state := computeState
       }
     }
     is(computeState) {
        val stages = Vec(Seq.fill(32){ Module(new RootBit()).io })
        stages(0).nIn := n //Cat(n(63,32),0.U(32.W))
        stages(0).remainderIn := 0.U //n(31,16) 
        stages(0).rootIn := 0.U // n(15,0)
        for (i <- 0 until 31) {
           stages(i+1).nIn := stages(i).nOut
           stages(i+1).remainderIn := stages(i).remainderOut
           stages(i+1).rootIn := stages(i).rootOut
        }
        result := stages(31).rootOut
        state := doneState
     }
     is(doneState) {
       when (io.resp.fire()) {
         state := readyState
       }
     }
   }
   // response bundle:
   io.resp.bits.rd := destReg
   io.resp.bits.data := result
   io.resp.valid := state === doneState

   // other odds and ends:
   io.interrupt := false.B     // no errors will be generated
   io.mem.req.valid := false.B // ignore memory
}
