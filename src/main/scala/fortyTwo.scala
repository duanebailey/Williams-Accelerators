// Very simple accelerator, based on the Translator accelerator in
//   rocket-chip/src/main/scala/tile/LazyRoCC.scala
package fortyTwo

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}

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

