module Testing.Benchmark where 

import qualified Examples.RepMin.RepMinAG as RepMinAG
import qualified Examples.RepMin.RepMinMemo as RepMinMemo
import qualified Examples.RepMin.RepMinZtrategic as RepMinZtrategic

import qualified Testing.RepMinMemo as RepMinDep 

import qualified Examples.RepMin.Benchmark as B


-- versions that compute repmin using the classical, inefficient approach 
repminAG          size = print $ B.sumTree   $ RepMinAG.repmin              $ B.testTree size
repminMemo        size = print $ B.sumTree   $ RepMinMemo.repmin            $ B.testTree size
repminZtrategic   size = print $ B.sumTree_m $ RepMinZtrategic.repmin       $ B.testTree size

-- versions using the new dependency system
repminDep         size = print $ B.sumTree   $ RepMinDep.repmin             $ B.testTree size
repminDepZtr      size = print $ RepMinDep.sumTree_m   $ RepMinDep.repminDepStrategy  $ B.testTree size


-- 2000, in ghci
{-
repminAG - 8.69s
repminMemo - 0.16s
repminZtrategic - 0.19s
repminDep - 0.27s
repminDepZtr - ???
repminDepZtr for size 500 - 13.90s

-}