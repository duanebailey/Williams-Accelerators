package fortyTwo

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
/**
 * Strncmp, where n is 8.
 * Compares two strings contained within the (little-endian) longs.
 */
class WFullStr8cmp(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new WFullStr8cmpImp(this)
}

class WithWFullStr8cmp extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val s8c = LazyModule.apply(new WFullStr8cmp(OpcodeSet.custom0)(p))
     s8c
  })
})

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  This implementation assumes that strings
 * are resident in virtual memory.  Calls to this instruction may require mapping
 * and/or locking pages to physical memory.
 */
class WFullStr8cmpImp(outer: WFullStr8cmp)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // state of the accelerator
   val debugMe = false.B
   val s_idle :: s_a_req :: s_a_resp :: s_b_req :: s_b_resp :: s_resp :: Nil = Enum(Bits(), 6)
   val state = Reg(init = s_idle) // accelerator state
   val req_rd = Reg(io.cmd.bits.inst.rd) // destination register
   val aPtr = Reg(init = 0.U(64.W))  // pointer to a string
   val bPtr = Reg(init = 0.U(64.W))  // pointer to b string
   val aVal = Reg(init = 0.U(64.W))  // character of a
   val bVal = Reg(init = 0.U(64.W))  // character of b
   val diff = aVal-bVal
   val nul = 0.U
   val memv = Reg(init = false.B) // is memory request valid?
   // communication with core and (to) memory depend on ready/valid protocol
   // the accelerator is ready for a new command precisely when it is idle
   io.cmd.ready := (state === s_idle) // ready/valid for receiving cmd

   // the meaning of the accelerator 'busy' signal indicates that memory might
   // be accessed; the following is certainly sufficient
   io.busy := (state =/= s_idle) // held high as long as memory might be ref'd

   io.mem.req.valid := memv
   when (memv) {
      when (debugMe) { printf("Last memory request is valid.\n") }
   } 

   // absorb command
   when (io.cmd.fire()) { // fire happens when io.cmd.ready and io.cmd.valid (set by core)
     when (debugMe) { printf("Got strcmp request.\n") }
     aPtr := io.cmd.bits.rs1  // these pointers are passed as the two source arguments
     bPtr := io.cmd.bits.rs2  // second pointer
     req_rd := io.cmd.bits.inst.rd  // (RoCCInstruction) register number of desitination
     memv := true.B
     io.mem.req.bits.addr := aPtr
     ///
     io.mem.req.bits.tag := 0.U
     ///
     state := s_a_req
   }

   // request a byte from memory.
   // io.mem is a HellaCacheIO
   io.mem.req.bits.cmd := 0.U // M_XRD
   io.mem.req.bits.size := 0.U // log of number of bytes (here 1 byte -> 0)
   io.mem.req.bits.signed := false.B // unsigned values
   io.mem.req.bits.data := Bits(0)  // no outbound data; not writing
   io.mem.req.bits.phys := false.B // addresses are virtual   

   // when a request is made to the memory sub-system, it must be held high
   // for two cycles
   when (state === s_a_req) {
      when (debugMe) { printf("A request valid.\n") }
      memv := true.B
      io.mem.req.bits.addr := aPtr
      io.mem.req.bits.tag := 0.U
   }
   when (state === s_b_req) {
      when (debugMe) { printf("B request valid.\n") }
      memv := true.B
      io.mem.req.bits.addr := bPtr
      io.mem.req.bits.tag := 1.U
   }

   // for resetting the request line   
   val didFire = RegNext(io.mem.req.fire())  // ie. io.mem.req .ready&&.valid
   when (didFire) {
      memv := false.B
   }
   when (io.mem.req.fire()) {
      when (state === s_a_req) {
         when (debugMe) { printf("Memory reading A from %x.\n",aPtr) }
         aPtr := aPtr+1.U
         state := s_a_resp
      }
      when (state === s_b_req) {
         when (debugMe) { printf("Memory reading B from %x.\n",bPtr) }
         bPtr := bPtr+1.U
         state := s_b_resp
      }
   }

   // when byte comes back from memory, capture it
   // values returned from memory do not make use of ready/valid protocol; we must
   // be willing to accept them whenever they appear valid
   when (io.mem.resp.valid && state === s_a_resp) {
     aVal := io.mem.resp.bits.data
     when (debugMe) { printf("A value read is %x\n",io.mem.resp.bits.data) }
     when (debugMe) { printf("A request is not valid.\n") }
     memv := true.B
     io.mem.req.bits.addr := bPtr
     io.mem.req.bits.tag := 1.U
     state := s_b_req
   }

   when (io.mem.resp.valid && state === s_b_resp) {
     val dataRead = io.mem.resp.bits.data
     val continue = (aVal === dataRead) && (aVal =/= nul) && (dataRead =/= nul)
     bVal := dataRead
     when (debugMe) { printf("B value read is %x\n",bVal) }
     when (debugMe) { printf("B request is not valid.\n") }
     memv := continue
     io.mem.req.bits.addr := aPtr
     io.mem.req.bits.tag := 0.U
     state := Mux(continue,s_a_req, s_resp)
   }

   // when finished, return result
   io.resp.valid := (state === s_resp)
   io.resp.bits.rd := req_rd
   io.resp.bits.data := diff

   // when processor accepts result, become idle
   when (io.resp.fire()) {
      when (debugMe) { printf("result=%x\n",diff); }
      state := s_idle
   }
   io.interrupt := false.B

   // print memory interface information
   when (io.busy) {
     printf("State: %d, io.mem.req.valid=%d, .req.ready=%d, .req.tag=%d, .resp.valid=%d, .resp.tag=%d\n",
          state, io.mem.req.valid, io.mem.req.ready, io.mem.req.bits.tag, io.mem.resp.valid, io.mem.resp.bits.tag)
   }
}

