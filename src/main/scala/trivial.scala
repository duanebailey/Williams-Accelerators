/**
 * Trivial hardware.  (c) duane a. bailey
 * This accelerator simply places a 42 in the destination register.
 * In some sense, it is "trivial".
 */
package williams

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}


/**
 * Lazy module to stand in for the 42 accelerator.
 */
class FortyTwo(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new FortyTwoImp(this)
}

/**
 * The trivial accelerator.  Moves a known value to destination register.
 * Test with trivial.c.
 */
class FortyTwoImp(outer: FortyTwo)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
{
  val s_idle :: s_resp :: Nil = Enum(Bits(), 2)
  val state = Reg(init = s_idle)  // start state
  val req_rd = Reg(io.resp.bits.rd) // destination reg number

  // this unit ready when in the s_idle state
  io.cmd.ready := (state === s_idle)

  // this unit's response to cmd execution:
  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    state := s_resp
  }

  // after result is accepted, become idle again:
  when (io.resp.fire()) {
    state := s_idle
  }

  // response packet:
  io.resp.valid := (state === s_resp)  // we're responding
  io.resp.bits.rd := req_rd            // destination register
  io.resp.bits.data := 42.U            // result
  io.busy := (state =/= s_idle)        // potential to access mem?
  io.interrupt := false.B              // never report an error
  io.mem.req.valid := false.B          // never access memory
}

class WithFortyTwo extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val fortyTwo = LazyModule.apply(new FortyTwo(OpcodeSet.custom0)(p))
     fortyTwo
  })
})

/**
 * Add the following to the example generator's RocketConfigs file:

class FullStrcmpRocketConfig extends Config(
    new WithTop ++
    new WithBootROM ++
    new freechips.rocketchip.subsystem.WithInclusiveCache ++
    new williams.WithFortyTwo ++
    new freechips.rocketchip.subsystem.WithNBigCores(1) ++
    new freechips.rocketchip.system.BaseConfig)

 */
