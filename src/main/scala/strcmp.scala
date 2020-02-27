package williams

// Not sure how many of these *really* need to be imported
import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}

/**
 * Compares two strings.
 * This version makes the full comparison in a single instruction.
 */
class FullStrcmp(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
   override lazy val module = new FullStrcmpImp(this)
}

class WithFullStrcmp extends Config ((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
     val sc = LazyModule.apply(new FullStrcmp(OpcodeSet.custom0)(p))
     sc
  })
})

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  This implementation assumes that strings
 * are resident in virtual memory.  Calls to this instruction may require mapping
 * and/or locking pages to physical memory.
 */
class FullStrcmpImp(outer: FullStrcmp)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer)
{
   // states of the state machine
   val idleState :: requestState :: responseState :: doneState :: Nil = Enum(Bits(),4)
   val state = Reg(init = idleState)
   var aPtr = RegInit(0.U(64.W))  // pointer into the string
   var bPtr = RegInit(0.U(64.W))  // pointer into the string
   var aVal = Reg(UInt(8.W))      // the character read
   var bVal = Reg(UInt(8.W))      // the character read
   val diff = aVal - io.mem.resp.bits.data
   val done = (aVal === 0.U) || (io.mem.resp.bits.data === 0.U) || (diff =/= 0.U)
   var source = Reg(init=0.U(1.W))  // which (A=0 or B=1) string is the source
   var destReg = Reg(init=io.cmd.bits.inst.rd)  // register to write to
   io.cmd.ready := (state === idleState)   // when is this accelerator ready?
   io.busy := (state =/= idleState)   // when might this accelerator use mem?

   // the memory request profile
   io.mem.req.valid := false.B   // otherwise set true in FSM
   // static portions:
   io.mem.req.bits.cmd := 0.U   // read
   io.mem.req.bits.size := 0.U  // log2(n); n is one byte
   io.mem.req.bits.signed := false.B  // value is unsigned
   io.mem.req.bits.data := Bits(0)  // data written (never used)
   io.mem.req.bits.phys := false.B  // pointers are virtual addresses
   io.mem.req.bits.addr := Mux(source === 0.U, aPtr, bPtr)  //
   io.mem.req.bits.tag := source   // identify the source string (A=0, B=1)
   val reqFired = RegNext(io.mem.req.fire())
   // the state machine.
   // This machine starts in idle.  When a command is received, the request
   // to memory is made and is held valid for the ready cycle and one more.
   // Response must be collected when valid.
   switch (state) {
     is (idleState) {
        when (io.cmd.fire()) {
           destReg := io.cmd.bits.inst.rd  // get destination register
           aPtr := io.cmd.bits.rs1 // first String
           bPtr := io.cmd.bits.rs2 // second string
           state := requestState                // move to first stage request
        }
     }
     is (requestState) {
       io.mem.req.valid := true.B          // request remains valid
       when (io.mem.req.fire()) {
         state := responseState
       }
     }
     is (responseState) {
       // request is now *not* valid; await response is valid
       io.mem.req.valid := reqFired        // keep request high for an extra beat
       when (io.mem.resp.valid) {          // memory as response data
          when (source === 0.U) {
             aVal := io.mem.resp.bits.data
             aPtr := aPtr+1.U
             source := ~source
             state := requestState
          } .otherwise {
             bVal := io.mem.resp.bits.data
             bPtr := bPtr+1.U
             source := ~source
             state := Mux(done,doneState,requestState)
          }
       }       
     }
   }
   io.resp.bits.rd := destReg
   io.resp.bits.data := diff
   io.resp.valid := state === doneState
   io.interrupt := false.B
   when (io.resp.fire()) {
      state := idleState
   }
   when (io.busy) {
     printf("State: %d, io.mem.req.valid=%d, .req.ready=%d, .req.addr=%d, .req.tag=%d, .resp.valid=%d, .resp.tag=%d, .resp.data=%d aPtr=%d aVal=%d bPtr=%d bVal=%d done=%d\n",
          state, io.mem.req.valid, io.mem.req.ready, io.mem.req.bits.addr, io.mem.req.bits.tag, io.mem.resp.valid, io.mem.resp.bits.tag, io.mem.resp.bits.data, aPtr, aVal, bPtr, bVal, done)
   }
   
}

/* BEGINNING OF COMMENT
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  This implementation assumes that strings
 * are resident in virtual memory.  Calls to this instruction may require mapping
 * and/or locking pages to physical memory.
class FullStrcmpImp(outer: FullStrcmp)(implicit p: Parameters)
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

END OF COMMENT */
