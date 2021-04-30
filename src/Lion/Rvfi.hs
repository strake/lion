{-|
Module      : Lion.Rvfi
Description : Lion RISC-V Formal Verification Interface
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

As the pipeline processes instructions, it populates the fields of the Rvfi data. When the instruction reaches the end of the pipeline, the core retires the instruction and writes the Rvfi data to output. This output is inspected and verified by the riscv-formal framework. See [riscv-formal](https://github.com/standardsemiconductor/riscv-formal) for more information about the interface. To verify the Lion core, see [lion-formal](https://github.com/standardsemiconductor/lion/tree/main/lion-formal).
-}

module Lion.Rvfi where

import Clash.Prelude
import Control.Lens
import Data.Maybe
import Data.Monoid

-- | RISC-V Formal Csr Interface
data RvfiCsr n = RvfiCsr
  { _wdataCsr :: "wdata" ::: BitVector n
  , _rdataCsr :: "rdata" ::: BitVector n
  , _wmaskCsr :: "wmask" ::: BitVector n
  , _rmaskCsr :: "rmask" ::: BitVector n
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''RvfiCsr

-- | RISC-V Formal Interface
data Rvfi xl = Rvfi
  { _rvfiValid       :: "valid"        ::: Bool
  , _rvfiOrder       :: "order"        ::: BitVector 64
  , _rvfiInsn        :: "insn"         ::: BitVector 32
  , _rvfiTrap        :: "trap"         ::: Bool
  , _rvfiHalt        :: "halt"         ::: Bool
  , _rvfiIntr        :: "intr"         ::: Bool
  , _rvfiMode        :: "mode"         ::: BitVector 2
  , _rvfiIxl         :: "ixl"          ::: BitVector 2
  , _rvfiRs1Addr     :: "rs1_addr"     ::: Unsigned 5
  , _rvfiRs2Addr     :: "rs2_addr"     ::: Unsigned 5
  , _rvfiRs1Data     :: "rs1_rdata"    ::: BitVector xl
  , _rvfiRs2Data     :: "rs2_rdata"    ::: BitVector xl
  , _rvfiRdAddr      :: "rd_addr"      ::: Unsigned 5
  , _rvfiRdWData     :: "rd_wdata"     ::: BitVector xl
  , _rvfiPcRData     :: "pc_rdata"     ::: BitVector xl
  , _rvfiPcWData     :: "pc_wdata"     ::: BitVector xl
  , _rvfiMemAddr     :: "mem_addr"     ::: BitVector xl
  , _rvfiMemRMask    :: "mem_rmask"    ::: BitVector (Div xl 8)
  , _rvfiMemWMask    :: "mem_wmask"    ::: BitVector (Div xl 8)
  , _rvfiMemRData    :: "mem_rdata"    ::: BitVector xl
  , _rvfiMemWData    :: "mem_wdata"    ::: BitVector xl
  , _rvfiCsrMinstret :: "csr_minstret" ::: RvfiCsr 64
  , _rvfiCsrMcycle   :: "csr_mcycle"   ::: RvfiCsr 64
  , _rvfiCsrMscratch :: "csr_mscratch" ::: RvfiCsr xl
  , _rvfiCsrMstatus  :: "csr_mstatus"  ::: RvfiCsr xl
  , _rvfiCsrMisa     :: "csr_misa"     ::: RvfiCsr xl
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
makeLenses ''Rvfi

-- | Unwrap Rvfi from First monoid
fromRvfi :: KnownNat xl => First (Rvfi xl) -> Rvfi xl
fromRvfi = fromMaybe mkRvfi . getFirst

-- | Construct the RISC-V Formal Interface
mkRvfi :: KnownNat xl => Rvfi xl
mkRvfi = Rvfi
  { _rvfiValid       = False
  , _rvfiOrder       = 0    
  , _rvfiInsn        = 0    
  , _rvfiTrap        = False
  , _rvfiHalt        = False
  , _rvfiIntr        = False
  , _rvfiMode        = 3    
  , _rvfiIxl         = 1    
  , _rvfiRs1Addr     = 0    
  , _rvfiRs2Addr     = 0    
  , _rvfiRs1Data     = 0    
  , _rvfiRs2Data     = 0    
  , _rvfiRdAddr      = 0    
  , _rvfiRdWData     = 0    
  , _rvfiPcRData     = 0    
  , _rvfiPcWData     = 0    
  , _rvfiMemAddr     = 0    
  , _rvfiMemRMask    = 0    
  , _rvfiMemWMask    = 0    
  , _rvfiMemRData    = 0    
  , _rvfiMemWData    = 0    
  , _rvfiCsrMinstret = RvfiCsr 0 0 0 0
  , _rvfiCsrMcycle   = RvfiCsr 0 0 0 0
  , _rvfiCsrMscratch = RvfiCsr 0 0 0 0 
  , _rvfiCsrMstatus  = RvfiCsr 0 0 0 0
  , _rvfiCsrMisa     = RvfiCsr 0 0 0 0
  }