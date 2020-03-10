//
// A naive implementation of SHA2.
// (c) 2019 duane a. bailey
//
package utils
import chisel3._
import chisel3.util.Cat

class SHAChunklet extends Bundle {
   val index    = UInt(6.W)
   val lastWord = UInt(1.W)
   val data     = UInt(32.W)
}

/*
 * SHA256("")
 * 0x e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
 */

class SHA2 extends Module {
  val io = IO(new Bundle {
     //val digest = Output(UInt(512.W))
     val disp = Output(SegControl())
  })
  // sample message
  val msg = VecInit(Array("x80000000".U(32.W),0.U(32.W),0.U(32.W),0.U(32.W),
                        0.U(32.W),0.U(32.W),0.U(32.W),0.U(32.W),
                        0.U(32.W),0.U(32.W),0.U(32.W),0.U(32.W),
                        0.U(32.W),0.U(32.W),0.U(32.W),0.U(32.W)))

  // Round of constants (fractions of cube roots of the first 64 primes):
  val k = VecInit(Array(
                  "x428a2f98".U(32.W),"x71374491".U(32.W),"xb5c0fbcf".U(32.W),"xe9b5dba5".U(32.W),
                  "x3956c25b".U(32.W),"x59f111f1".U(32.W),"x923f82a4".U(32.W),"xab1c5ed5".U(32.W),
                  "xd807aa98".U(32.W),"x12835b01".U(32.W),"x243185be".U(32.W),"x550c7dc3".U(32.W),
                  "x72be5d74".U(32.W),"x80deb1fe".U(32.W),"x9bdc06a7".U(32.W),"xc19bf174".U(32.W),
                  "xe49b69c1".U(32.W),"xefbe4786".U(32.W),"x0fc19dc6".U(32.W),"x240ca1cc".U(32.W),
                  "x2de92c6f".U(32.W),"x4a7484aa".U(32.W),"x5cb0a9dc".U(32.W),"x76f988da".U(32.W),
                  "x983e5152".U(32.W),"xa831c66d".U(32.W),"xb00327c8".U(32.W),"xbf597fc7".U(32.W),
                  "xc6e00bf3".U(32.W),"xd5a79147".U(32.W),"x06ca6351".U(32.W),"x14292967".U(32.W),
                  "x27b70a85".U(32.W),"x2e1b2138".U(32.W),"x4d2c6dfc".U(32.W),"x53380d13".U(32.W),
                  "x650a7354".U(32.W),"x766a0abb".U(32.W),"x81c2c92e".U(32.W),"x92722c85".U(32.W),
                  "xa2bfe8a1".U(32.W),"xa81a664b".U(32.W),"xc24b8b70".U(32.W),"xc76c51a3".U(32.W),
                  "xd192e819".U(32.W),"xd6990624".U(32.W),"xf40e3585".U(32.W),"x106aa070".U(32.W),
                  "x19a4c116".U(32.W),"x1e376c08".U(32.W),"x2748774c".U(32.W),"x34b0bcb5".U(32.W),
                  "x391c0cb3".U(32.W),"x4ed8aa4a".U(32.W),"x5b9cca4f".U(32.W),"x682e6ff3".U(32.W),
                  "x748f82ee".U(32.W),"x78a5636f".U(32.W),"x84c87814".U(32.W),"x8cc70208".U(32.W),
                  "x90befffa".U(32.W),"xa4506ceb".U(32.W),"xbef9a3f7".U(32.W),"xc67178f2".U(32.W)))
  // Initialize hash values (fractions from roots of first 8 primes):
  val h0 = RegInit("x6a09e667".U(32.W))
  val h1 = RegInit("xbb67ae85".U(32.W))
  val h2 = RegInit("x3c6ef372".U(32.W))
  val h3 = RegInit("xa54ff53a".U(32.W))
  val h4 = RegInit("x510e527f".U(32.W))
  val h5 = RegInit("x9b05688c".U(32.W))
  val h6 = RegInit("x1f83d9ab".U(32.W))
  val h7 = RegInit("x5be0cd19".U(32.W))

  def rightRotate(src: UInt, amt: Int) = {
      val width = 32
      Cat(src(amt-1,0),src(width-1,amt))
  }

  /*
   * This is not quite right.
   * We should widen this pipeline to include a variety of extra bits that indicate EOD, etc.
   */
  val counter = RegInit(0.U(10.W))
  counter := counter + 1.U

  /*
   * TODO:
   * Get io.input with a Decoupled.  As queue becomes empty, set lastWord to true.
   * This causes the hash codes to be released.
   */
  val zero = Wire(new SHAChunklet)
  zero.lastWord := 0.U
  zero.index := 0.U
  zero.data := 0.U
  val w15 = RegInit(zero)
  val w14 = RegInit(zero)
  w14 := w15
  val w13 = RegInit(zero)
  w13 := w14
  val w12 = RegInit(zero)
  w12 := w13
  val w11 = RegInit(zero)
  w11 := w12
  val w10 = RegInit(zero)
  w10 := w11
  val w09 = RegInit(zero)
  w09 := w10
  val w08 = RegInit(zero)
  w08 := w09
  val w07 = RegInit(zero)
  w07 := w08
  val w06 = RegInit(zero)
  w06 := w07
  val w05 = RegInit(zero)
  w05 := w06
  val w04 = RegInit(zero)
  w04 := w05
  val w03 = RegInit(zero)
  w03 := w04
  val w02 = RegInit(zero)
  w02 := w03
  val w01 = RegInit(zero)
  w01 := w02
  val w00 = RegInit(zero)
  w00 := w01
  val scramble = w00.data + w09.data +
          ((w14.data >> 10.U) ^rightRotate(w14.data, 17) ^ rightRotate(w14.data, 19)) +
	  (rightRotate(w01.data, 7) ^ rightRotate(w01.data, 18) ^ (w01.data >> 3.U))

  val enter = Mux(counter(5,4) === 0.U,msg(counter(3,0)), scramble)
  w15.index := counter(5,0)
  w15.data := enter
  w15.lastWord := counter(3,0) === 15.U

  // These variables hold the hash-state mixins
  val a = RegInit(0.U(32.W))
  val b = RegInit(0.U(32.W))
  val c = RegInit(0.U(32.W))
  val d = RegInit(0.U(32.W))
  val e = RegInit(0.U(32.W))
  val f = RegInit(0.U(32.W))
  val g = RegInit(0.U(32.W))
  val h = RegInit(0.U(32.W))

  val init = w00.index === 0.U  
  val A = Mux(init,h0,a)
  val B = Mux(init,h1,b)
  val C = Mux(init,h2,c)
  val D = Mux(init,h3,d)
  val E = Mux(init,h4,e)
  val F = Mux(init,h5,f)
  val G = Mux(init,h6,g)
  val H = Mux(init,h7,h)
  val S0 = rightRotate(A,2)^rightRotate(A,13)^rightRotate(A,22)
  val maj = (A & B) ^ (A & C) ^ (B & C)
  val S1 = rightRotate(E,6) ^ rightRotate(E,11) ^ rightRotate(E,25)
  val ch = (E & F) ^ ((~E) & G)
  val temp1 = H + S1 + ch + k(w00.index) + w00.data
  val temp2 = S0 + maj

  h := G
  g := F
  f := E
  e := D + temp1
  d := C
  c := B
  b := A
  a := temp1 + temp2
  when (w00.index === 63.U) {
    h0 := h0 + temp1 + temp2
    h1 := h1 + A
    h2 := h2 + B
    h3 := h3 + C
    h4 := h4 + D + temp1
    h5 := h5 + E
    h6 := h6 + F
    h7 := h7 + G
  }

  // Latch last value
  val flush = RegInit(0.U(1.W))
  when (w00.index === 63.U) {
     flush := 1.U(1.W)
  }

  val capture = RegInit(0.U(512.W))
  // produce the final hash value
  when (flush === 1.U) {
    capture := Cat(h0,h1,h2,h3,h4,h5,h6,h7)
  }
  //io.digest := capture
  io.disp := SegDisplay(capture(31,0))
}
  
object SHA2 extends App {
  chisel3.Driver.execute(args, () => new SHA2)
}
