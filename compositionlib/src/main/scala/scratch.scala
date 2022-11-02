import dev.ludovic.netlib.blas.{BLAS, NativeBLAS}

object scratch extends App {

  //System.setProperty("-Dcom.github.fommil.netlib.NativeSystemBLAS.natives", "C:\\Program Files\\AMD\\AOCL-Windows\\amd-libflame\\lib\\LP64\\AOCL-LibBlis-Win-MT-dll.dll")
  //System.setProperty("-Dcom.github.fommil.netlib.NativeSystemLAPACK.natives", "C:\\Program Files\\AMD\\AOCL-Windows\\amd-libflame\\lib\\LP64\\AOCL-LibFLAME-Win-MT-dll.dll")

  //System.setProperty("-Dcom.github.fommil.netlib.BLAS", "C:\\Program Files\\AMD\\AOCL-Windows\\amd-libflame\\lib\\LP64\\AOCL-LibBlis-Win-MT-dll.dll")
  //System.setProperty("-Dcom.github.fommil.netlib.LAPACK", "C:\\Program Files\\AMD\\AOCL-Windows\\amd-libflame\\lib\\LP64\\AOCL-LibFLAME-Win-MT-dll.dll")

  System.setProperty("-Ddev.ludovic.netlib.blas.nativeLibPath", "C:\\Program Files\\OpenBLAS\\bin\\libopenblas.dll")

//  System.setProperty("-Ddev.ludovic.netlib.blas.nativeLibPath", "C:\\Program Files\\AMD\\AOCL-Windows\\amd-libflame\\lib\\LP64\\AOCL-LibBlis-Win-MT-dll.dll")

//  resources/native/Windows 10-amd64/libnetlibblasjni.so
  NativeBLAS.getInstance()

}
