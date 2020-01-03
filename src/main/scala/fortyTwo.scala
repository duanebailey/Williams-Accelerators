package fortyTwo

import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}

class FortyTwo(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new FortyTwoImp(this)
}

class FortyTwoImp(outer: FortyTwo)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
     with HasCoreParameters {
  val s_idle :: s_resp :: Nil = Enum(Bits(), 4) // 4?
  val state = Reg(init = s_idle)  // start state
  val req_rd = Reg(io.resp.bits.rd) // destination reg number

  // this unit ready when in the s_idle state
  io.cmd.ready := (state === s_idle)

  // this unit's response to cmd execution
  when (io.cmd.fire()) {
    state := s_resp
  }

  // when we respond, become idle again
  when (io.resp.fire()) {
    state := s_idle
  }

  // response packet:
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := 42.U()
  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
}

