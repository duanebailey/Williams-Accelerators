/**
 * Bresenam's Cirle calculation. (c) duane bailey
 * This hardware computes the x coordinates for the pixels that lie on a
 * circle of radius r in the lower half of the first quadrant.  All other
 * quadrants are various reflections of these points.
 */
package williams

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}


/**
 * Bresenam's Cirle calculation.
 * - call with function code 0: use rs1 as the radius.
 * - call with function code 1: iterate and return next x, or return 0 if x==y
 */
class Circle(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new CircleImp(this)
}

class WithCircle extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8c = LazyModule.apply(new Circle(OpcodeSet.custom0)(p))
     s8c
  })
})

/**
 * This accelerator computes the X coordinates of an octant of a circle with
 * radius R.
 * It makes use of the function codes passed with the instruction:
 *  - function code 0: set R to the rs1
 *  - function code 1: iterate and set rd's value to current X, or 0 if done.
 */
class CircleImp(outer: Circle)(implicit p: Parameters)
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
      state := s_computing                  // indicate we're busy.
   }
   when (io.cmd.fire() && do_iter) {
      req_rd := io.cmd.bits.inst.rd
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
