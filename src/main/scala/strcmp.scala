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
   val idleState :: compareState :: doneState :: Nil = Enum(Bits(),3)
   val state = Reg(init = idleState)
   val aPtr = RegInit(0.U(64.W))  // pointer into the string
   val bPtr = RegInit(0.U(64.W))  // pointer into the string
   val aVal = Reg(UInt(8.W))      // the character read
   val bVal = Reg(UInt(8.W))      // the character read
   val diff = aVal - io.mem.resp.bits.data
   val done = ((aVal === 0.U) || (io.mem.resp.bits.data === 0.U) || (diff =/= 0.U))
   val source = Reg(init=0.U(1.W))  // which (A=0 or B=1) string is the source
   val destReg = Reg(init=io.cmd.bits.inst.rd)  // register to write to
   io.cmd.ready := (state === idleState)   // when is this accelerator ready?
   io.busy := (state =/= idleState)   // when might this accelerator use mem?

   val request = Reg(init=false.B)

   // the memory request profile
   io.mem.req.valid := request

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
           request := true.B
           state := compareState                // move to first stage request
        }
     }
     is (compareState) {
       // request is now *not* valid; await response is valid
       when (RegNext(io.mem.req.fire())) {
         request := false.B
       }
       when (io.mem.resp.valid && !RegNext(io.mem.resp.valid)) {          // memory as response data
          request := true.B
          when (source === 0.U) {
             aVal := io.mem.resp.bits.data
             aPtr := aPtr+1.U
          } .otherwise {
             bVal := io.mem.resp.bits.data
             bPtr := bPtr+1.U
             when (done) {
                request := false.B;
                state := doneState
             }
          }
          source := ~source
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
