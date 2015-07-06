/*
((((read[int8] >>>
    var ab{_r134} : arr[8] bit in
    fun auto_map__31015{_pf31015}(x{_r136} : int8) =
          ab{_r134}
    in
    map auto_map__31015{_pf31015}) >>>
   var len{_r126} : int32 in
   let len{_r122} : int32 =
       len{_r126}
   in
   ((((var crc_state{_r20} : arr[4] bit :=
           {'0, '0, '0, '0} in
       seq { _unused_221{_pf221} <- (for _tmp_count{_r21} in [0, len{_r122}]
                                       seq { DD1_free_1306{DD1_free_1306} <- take[arr[8] bit];
                                             emit DD1_free_1306{DD1_free_1306}
                                           } .>>>.
                                     8-mitigate(MA)[bit]-4);
             _unused_220{_pf220} <- emit crc_state{_r20};
             _unused_219{_pf219} <- emit {'0, '0, '0, '0};
             _unused_218{_pf218} <- let len_so_far{_r30} : int32 =
                                        len{_r122}+1*8
                                    in
                                    let len_mod{_r36} : int32 =
                                        if len_so_far{_r30}%24>0 {
                                          1
                                        } else {
                                          0
                                        }
                                    in
                                    let final_len{_r40} : int32 =
                                        len_so_far{_r30}/24+len_mod{_r36}*24
                                    in
                                    (for _tmp_count{_r42} in [0, final_len{_r40}-len_so_far{_r30}]
                                       emit {'0, '0, '0, '0, '0, '0, '0, '0} .>>>.
                                     8-mitigate(MAoMAoMA)[bit]-4);
             return crc_state{_r20}
           } >>>
       var s{_r99} : arr[6] bit :=
           {'0, '0, '0, '0, '0, '0} in
       (repeat seq { UD2_free_9065{UD2_free_9065} <- take[arr[4] bit];
                     var ya_free_9066{ya_free_9066} : arr[8] bit in
                     let _unused_30682{_pf30682} : () =
                         ya_free_9066{ya_free_9066}[0:+2] := -- (earrwrite) 
                           var r{_r103} : arr[2] bit in
                           r{_r103}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[0]^s{_r99}[1]^s{_r99}[2]^s{_r99}[4]^s{_r99}[5];
                           r{_r103}[1] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[0]^s{_r99}[0]^s{_r99}[1]^s{_r99}[2]^s{_r99}[5];
                           s{_r99}[1:+5] := -- (earrwrite) 
                             s{_r99}[0:+5];
                           s{_r99}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[0];
                           r{_r103};
                         ya_free_9066{ya_free_9066}[2:+2] := -- (earrwrite) 
                           var r{_r103} : arr[2] bit in
                           r{_r103}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[1]^s{_r99}[1]^s{_r99}[2]^s{_r99}[4]^s{_r99}[5];
                           r{_r103}[1] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[1]^s{_r99}[0]^s{_r99}[1]^s{_r99}[2]^s{_r99}[5];
                           s{_r99}[1:+5] := -- (earrwrite) 
                             s{_r99}[0:+5];
                           s{_r99}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[1];
                           r{_r103};
                         ya_free_9066{ya_free_9066}[4:+2] := -- (earrwrite) 
                           var r{_r103} : arr[2] bit in
                           r{_r103}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[2]^s{_r99}[1]^s{_r99}[2]^s{_r99}[4]^s{_r99}[5];
                           r{_r103}[1] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[2]^s{_r99}[0]^s{_r99}[1]^s{_r99}[2]^s{_r99}[5];
                           s{_r99}[1:+5] := -- (earrwrite) 
                             s{_r99}[0:+5];
                           s{_r99}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[2];
                           r{_r103};
                         ya_free_9066{ya_free_9066}[6:+2] := -- (earrwrite) 
                           var r{_r103} : arr[2] bit in
                           r{_r103}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[3]^s{_r99}[1]^s{_r99}[2]^s{_r99}[4]^s{_r99}[5];
                           r{_r103}[1] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[3]^s{_r99}[0]^s{_r99}[1]^s{_r99}[2]^s{_r99}[5];
                           s{_r99}[1:+5] := -- (earrwrite) 
                             s{_r99}[0:+5];
                           s{_r99}[0] := -- (earrwrite) 
                             UD2_free_9065{UD2_free_9065}[3];
                           r{_r103}
                     in
                     for idx_free_9068{idx_free_9068} in [0, 8]
                       emit ya_free_9066{ya_free_9066}[idx_free_9068{idx_free_9068}]
                   } .>>>.
        1-mitigate(MF)[bit]-48)) >>>
      fun auto_map__31016{_pf31016}(UD1_free_11438{UD1_free_11438} : arr[48] bit) =
            {complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}, complex16 {re = -1, im = 0}}
      in
      map auto_map__31016{_pf31016}) >>>
     fun auto_map__31017{_pf31017}(DD1_free_20529{DD1_free_20529} : arr[48] complex16) =
           var ya_free_20549{ya_free_20549} : arr[64] complex16 in
           ya_free_20549{ya_free_20549}[0] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[1] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[2] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[3] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[4] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[5] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[6:+5] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[0:+5];
           ya_free_20549{ya_free_20549}[11] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[12:+13] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[5:+13];
           ya_free_20549{ya_free_20549}[25] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[26:+6] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[18:+6];
           ya_free_20549{ya_free_20549}[32] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[33:+6] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[24:+6];
           ya_free_20549{ya_free_20549}[39] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[40:+13] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[30:+13];
           ya_free_20549{ya_free_20549}[53] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[54:+5] := -- (earrwrite) 
             DD1_free_20529{DD1_free_20529}[43:+5];
           ya_free_20549{ya_free_20549}[59] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[60] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[61] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[62] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}[63] := -- (earrwrite) 
             complex16 {re = 0, im = 0};
           ya_free_20549{ya_free_20549}
     in
     map auto_map__31017{_pf31017}) >>>
    fun auto_map__31018{_pf31018}(DD1_free_21638{DD1_free_21638} : arr[64] complex16) =
          {complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}, complex16 {re = 0, im = 0}}
    in
    map auto_map__31018{_pf31018})) >>>
  fun auto_map__31019{_pf31019}(UD1_free_25717{UD1_free_25717} : arr[160] complex16) =
        var ya_free_25719{ya_free_25719} : arr[160] complex16 in
        for idx_free_25720{idx_free_25720} in [0, 160] {
          ya_free_25719{ya_free_25719}[idx_free_25720{idx_free_25720}] := -- (earrwrite) 
            UD1_free_25717{UD1_free_25717}[idx_free_25720{idx_free_25720}]
        };
        ya_free_25719{ya_free_25719}
  in
  map auto_map__31019{_pf31019}) >>>
 write[arr[160] complex16])

type:
ST (C arr[4] bit) (EXTBUF[base=int8]) (EXTBUF[base=arr[160] complex16])
*/
#include "common.h"

#define SKIP 0
#define YIELD 1
#define DONE 3
#define IMMEDIATE 1
#define CONSUME 0
#define FALSE 0
#define TRUE 1
#define UNIT 0
extern BufContextBlock* pbuf_ctx;
extern HeapContextBlock* pheap_ctx;
extern BlinkParams* params;
extern int stop_program;
int auto_map__31015_ln125_12(BitArrPtr _ret15, int __len_unused_17, int8 x16,
                             BitArrPtr ab13, int __len_unused_14);
int auto_map__31015_ln125_12(BitArrPtr _ret15, int __len_unused_17, int8 x16,
                             BitArrPtr ab13, int __len_unused_14)
{
    *(uint8*) (BitArrPtr) (_ret15 + 0) = *(uint8*) (BitArrPtr) (&ab13[0] + 0);
    return UNIT;
}
int auto_map__31016_ln3_143(complex16* _ret144, int __len_unused_146,
                            BitArrPtr UD1_free_11438145, int __len_unused_147);
int auto_map__31016_ln3_143(complex16* _ret144, int __len_unused_146,
                            BitArrPtr UD1_free_11438145, int __len_unused_147)
{
    calign complex16 __exp_arr148[48] = {{0, 0}};
    calign complex16 __struct149 = {0, 0};
    calign complex16 __struct150 = {0, 0};
    calign complex16 __struct151 = {0, 0};
    calign complex16 __struct152 = {0, 0};
    calign complex16 __struct153 = {0, 0};
    calign complex16 __struct154 = {0, 0};
    calign complex16 __struct155 = {0, 0};
    calign complex16 __struct156 = {0, 0};
    calign complex16 __struct157 = {0, 0};
    calign complex16 __struct158 = {0, 0};
    calign complex16 __struct159 = {0, 0};
    calign complex16 __struct160 = {0, 0};
    calign complex16 __struct161 = {0, 0};
    calign complex16 __struct162 = {0, 0};
    calign complex16 __struct163 = {0, 0};
    calign complex16 __struct164 = {0, 0};
    calign complex16 __struct165 = {0, 0};
    calign complex16 __struct166 = {0, 0};
    calign complex16 __struct167 = {0, 0};
    calign complex16 __struct168 = {0, 0};
    calign complex16 __struct169 = {0, 0};
    calign complex16 __struct170 = {0, 0};
    calign complex16 __struct171 = {0, 0};
    calign complex16 __struct172 = {0, 0};
    calign complex16 __struct173 = {0, 0};
    calign complex16 __struct174 = {0, 0};
    calign complex16 __struct175 = {0, 0};
    calign complex16 __struct176 = {0, 0};
    calign complex16 __struct177 = {0, 0};
    calign complex16 __struct178 = {0, 0};
    calign complex16 __struct179 = {0, 0};
    calign complex16 __struct180 = {0, 0};
    calign complex16 __struct181 = {0, 0};
    calign complex16 __struct182 = {0, 0};
    calign complex16 __struct183 = {0, 0};
    calign complex16 __struct184 = {0, 0};
    calign complex16 __struct185 = {0, 0};
    calign complex16 __struct186 = {0, 0};
    calign complex16 __struct187 = {0, 0};
    calign complex16 __struct188 = {0, 0};
    calign complex16 __struct189 = {0, 0};
    calign complex16 __struct190 = {0, 0};
    calign complex16 __struct191 = {0, 0};
    calign complex16 __struct192 = {0, 0};
    calign complex16 __struct193 = {0, 0};
    calign complex16 __struct194 = {0, 0};
    calign complex16 __struct195 = {0, 0};
    calign complex16 __struct196 = {0, 0};
    
    __struct149.re = -1;
    __struct149.im = 0;
    bounds_check(48, 0 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[0].re = __struct149.re;
    __exp_arr148[0].im = __struct149.im;
    __struct150.re = -1;
    __struct150.im = 0;
    bounds_check(48, 1 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[1].re = __struct150.re;
    __exp_arr148[1].im = __struct150.im;
    __struct151.re = -1;
    __struct151.im = 0;
    bounds_check(48, 2 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[2].re = __struct151.re;
    __exp_arr148[2].im = __struct151.im;
    __struct152.re = -1;
    __struct152.im = 0;
    bounds_check(48, 3 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[3].re = __struct152.re;
    __exp_arr148[3].im = __struct152.im;
    __struct153.re = -1;
    __struct153.im = 0;
    bounds_check(48, 4 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[4].re = __struct153.re;
    __exp_arr148[4].im = __struct153.im;
    __struct154.re = -1;
    __struct154.im = 0;
    bounds_check(48, 5 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[5].re = __struct154.re;
    __exp_arr148[5].im = __struct154.im;
    __struct155.re = -1;
    __struct155.im = 0;
    bounds_check(48, 6 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[6].re = __struct155.re;
    __exp_arr148[6].im = __struct155.im;
    __struct156.re = -1;
    __struct156.im = 0;
    bounds_check(48, 7 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[7].re = __struct156.re;
    __exp_arr148[7].im = __struct156.im;
    __struct157.re = -1;
    __struct157.im = 0;
    bounds_check(48, 8 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[8].re = __struct157.re;
    __exp_arr148[8].im = __struct157.im;
    __struct158.re = -1;
    __struct158.im = 0;
    bounds_check(48, 9 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[9].re = __struct158.re;
    __exp_arr148[9].im = __struct158.im;
    __struct159.re = -1;
    __struct159.im = 0;
    bounds_check(48, 10 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[10].re = __struct159.re;
    __exp_arr148[10].im = __struct159.im;
    __struct160.re = -1;
    __struct160.im = 0;
    bounds_check(48, 11 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[11].re = __struct160.re;
    __exp_arr148[11].im = __struct160.im;
    __struct161.re = -1;
    __struct161.im = 0;
    bounds_check(48, 12 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[12].re = __struct161.re;
    __exp_arr148[12].im = __struct161.im;
    __struct162.re = -1;
    __struct162.im = 0;
    bounds_check(48, 13 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[13].re = __struct162.re;
    __exp_arr148[13].im = __struct162.im;
    __struct163.re = -1;
    __struct163.im = 0;
    bounds_check(48, 14 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[14].re = __struct163.re;
    __exp_arr148[14].im = __struct163.im;
    __struct164.re = -1;
    __struct164.im = 0;
    bounds_check(48, 15 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[15].re = __struct164.re;
    __exp_arr148[15].im = __struct164.im;
    __struct165.re = -1;
    __struct165.im = 0;
    bounds_check(48, 16 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[16].re = __struct165.re;
    __exp_arr148[16].im = __struct165.im;
    __struct166.re = -1;
    __struct166.im = 0;
    bounds_check(48, 17 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[17].re = __struct166.re;
    __exp_arr148[17].im = __struct166.im;
    __struct167.re = -1;
    __struct167.im = 0;
    bounds_check(48, 18 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[18].re = __struct167.re;
    __exp_arr148[18].im = __struct167.im;
    __struct168.re = -1;
    __struct168.im = 0;
    bounds_check(48, 19 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[19].re = __struct168.re;
    __exp_arr148[19].im = __struct168.im;
    __struct169.re = -1;
    __struct169.im = 0;
    bounds_check(48, 20 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[20].re = __struct169.re;
    __exp_arr148[20].im = __struct169.im;
    __struct170.re = -1;
    __struct170.im = 0;
    bounds_check(48, 21 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[21].re = __struct170.re;
    __exp_arr148[21].im = __struct170.im;
    __struct171.re = -1;
    __struct171.im = 0;
    bounds_check(48, 22 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[22].re = __struct171.re;
    __exp_arr148[22].im = __struct171.im;
    __struct172.re = -1;
    __struct172.im = 0;
    bounds_check(48, 23 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[23].re = __struct172.re;
    __exp_arr148[23].im = __struct172.im;
    __struct173.re = -1;
    __struct173.im = 0;
    bounds_check(48, 24 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[24].re = __struct173.re;
    __exp_arr148[24].im = __struct173.im;
    __struct174.re = -1;
    __struct174.im = 0;
    bounds_check(48, 25 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[25].re = __struct174.re;
    __exp_arr148[25].im = __struct174.im;
    __struct175.re = -1;
    __struct175.im = 0;
    bounds_check(48, 26 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[26].re = __struct175.re;
    __exp_arr148[26].im = __struct175.im;
    __struct176.re = -1;
    __struct176.im = 0;
    bounds_check(48, 27 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[27].re = __struct176.re;
    __exp_arr148[27].im = __struct176.im;
    __struct177.re = -1;
    __struct177.im = 0;
    bounds_check(48, 28 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[28].re = __struct177.re;
    __exp_arr148[28].im = __struct177.im;
    __struct178.re = -1;
    __struct178.im = 0;
    bounds_check(48, 29 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[29].re = __struct178.re;
    __exp_arr148[29].im = __struct178.im;
    __struct179.re = -1;
    __struct179.im = 0;
    bounds_check(48, 30 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[30].re = __struct179.re;
    __exp_arr148[30].im = __struct179.im;
    __struct180.re = -1;
    __struct180.im = 0;
    bounds_check(48, 31 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[31].re = __struct180.re;
    __exp_arr148[31].im = __struct180.im;
    __struct181.re = -1;
    __struct181.im = 0;
    bounds_check(48, 32 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[32].re = __struct181.re;
    __exp_arr148[32].im = __struct181.im;
    __struct182.re = -1;
    __struct182.im = 0;
    bounds_check(48, 33 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[33].re = __struct182.re;
    __exp_arr148[33].im = __struct182.im;
    __struct183.re = -1;
    __struct183.im = 0;
    bounds_check(48, 34 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[34].re = __struct183.re;
    __exp_arr148[34].im = __struct183.im;
    __struct184.re = -1;
    __struct184.im = 0;
    bounds_check(48, 35 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[35].re = __struct184.re;
    __exp_arr148[35].im = __struct184.im;
    __struct185.re = -1;
    __struct185.im = 0;
    bounds_check(48, 36 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[36].re = __struct185.re;
    __exp_arr148[36].im = __struct185.im;
    __struct186.re = -1;
    __struct186.im = 0;
    bounds_check(48, 37 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[37].re = __struct186.re;
    __exp_arr148[37].im = __struct186.im;
    __struct187.re = -1;
    __struct187.im = 0;
    bounds_check(48, 38 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[38].re = __struct187.re;
    __exp_arr148[38].im = __struct187.im;
    __struct188.re = -1;
    __struct188.im = 0;
    bounds_check(48, 39 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[39].re = __struct188.re;
    __exp_arr148[39].im = __struct188.im;
    __struct189.re = -1;
    __struct189.im = 0;
    bounds_check(48, 40 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[40].re = __struct189.re;
    __exp_arr148[40].im = __struct189.im;
    __struct190.re = -1;
    __struct190.im = 0;
    bounds_check(48, 41 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[41].re = __struct190.re;
    __exp_arr148[41].im = __struct190.im;
    __struct191.re = -1;
    __struct191.im = 0;
    bounds_check(48, 42 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[42].re = __struct191.re;
    __exp_arr148[42].im = __struct191.im;
    __struct192.re = -1;
    __struct192.im = 0;
    bounds_check(48, 43 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[43].re = __struct192.re;
    __exp_arr148[43].im = __struct192.im;
    __struct193.re = -1;
    __struct193.im = 0;
    bounds_check(48, 44 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[44].re = __struct193.re;
    __exp_arr148[44].im = __struct193.im;
    __struct194.re = -1;
    __struct194.im = 0;
    bounds_check(48, 45 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[45].re = __struct194.re;
    __exp_arr148[45].im = __struct194.im;
    __struct195.re = -1;
    __struct195.im = 0;
    bounds_check(48, 46 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[46].re = __struct195.re;
    __exp_arr148[46].im = __struct195.im;
    __struct196.re = -1;
    __struct196.im = 0;
    bounds_check(48, 47 + 0, "test_wifimockup.blk:3:3-4:24");
    __exp_arr148[47].re = __struct196.re;
    __exp_arr148[47].im = __struct196.im;
    blink_copy(_ret144, __exp_arr148, 48 * sizeof(complex16));
    return UNIT;
}
int auto_map__31017_ln42_199(complex16* ya_free_20549, int __len_unused_201,
                             complex16* DD1_free_20529200,
                             int __len_unused_202);
int auto_map__31017_ln42_199(complex16* ya_free_20549, int __len_unused_201,
                             complex16* DD1_free_20529200, int __len_unused_202)
{
    calign complex16 __struct203 = {0, 0};
    calign complex16 __struct204 = {0, 0};
    calign complex16 __struct205 = {0, 0};
    calign complex16 __struct206 = {0, 0};
    calign complex16 __struct207 = {0, 0};
    calign complex16 __struct208 = {0, 0};
    calign complex16 __struct209 = {0, 0};
    calign complex16 __struct210 = {0, 0};
    calign complex16 __struct211 = {0, 0};
    calign complex16 __struct212 = {0, 0};
    calign complex16 __struct213 = {0, 0};
    calign complex16 __struct214 = {0, 0};
    calign complex16 __struct215 = {0, 0};
    calign complex16 __struct216 = {0, 0};
    calign complex16 __struct217 = {0, 0};
    calign complex16 __struct218 = {0, 0};
    
    __struct203.re = 0;
    __struct203.im = 0;
    bounds_check(64, 0 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[0].re = __struct203.re;
    ya_free_20549[0].im = __struct203.im;
    __struct204.re = 0;
    __struct204.im = 0;
    bounds_check(64, 1 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[1].re = __struct204.re;
    ya_free_20549[1].im = __struct204.im;
    __struct205.re = 0;
    __struct205.im = 0;
    bounds_check(64, 2 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[2].re = __struct205.re;
    ya_free_20549[2].im = __struct205.im;
    __struct206.re = 0;
    __struct206.im = 0;
    bounds_check(64, 3 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[3].re = __struct206.re;
    ya_free_20549[3].im = __struct206.im;
    __struct207.re = 0;
    __struct207.im = 0;
    bounds_check(64, 4 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[4].re = __struct207.re;
    ya_free_20549[4].im = __struct207.im;
    __struct208.re = 0;
    __struct208.im = 0;
    bounds_check(64, 5 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[5].re = __struct208.re;
    ya_free_20549[5].im = __struct208.im;
    bounds_check(48, 0 + 4, "test_wifimockup.blk:54:5-17");
    bounds_check(64, 6 + 4, "test_wifimockup.blk:54:5-17");
    blink_copy(&ya_free_20549[6], &DD1_free_20529200[0], 5 * sizeof(complex16));
    __struct209.re = 0;
    __struct209.im = 0;
    bounds_check(64, 11 + 0, "test_wifimockup.blk:55:5-18");
    ya_free_20549[11].re = __struct209.re;
    ya_free_20549[11].im = __struct209.im;
    bounds_check(48, 5 + 12, "test_wifimockup.blk:56:5-18");
    bounds_check(64, 12 + 12, "test_wifimockup.blk:56:5-18");
    blink_copy(&ya_free_20549[12], &DD1_free_20529200[5], 13 *
               sizeof(complex16));
    __struct210.re = 0;
    __struct210.im = 0;
    bounds_check(64, 25 + 0, "test_wifimockup.blk:57:5-18");
    ya_free_20549[25].re = __struct210.re;
    ya_free_20549[25].im = __struct210.im;
    bounds_check(48, 18 + 5, "test_wifimockup.blk:58:5-19");
    bounds_check(64, 26 + 5, "test_wifimockup.blk:58:5-19");
    blink_copy(&ya_free_20549[26], &DD1_free_20529200[18], 6 *
               sizeof(complex16));
    __struct211.re = 0;
    __struct211.im = 0;
    bounds_check(64, 32 + 0, "test_wifimockup.blk:60:5-31");
    ya_free_20549[32].re = __struct211.re;
    ya_free_20549[32].im = __struct211.im;
    bounds_check(48, 24 + 5, "test_wifimockup.blk:62:5-19");
    bounds_check(64, 33 + 5, "test_wifimockup.blk:62:5-19");
    blink_copy(&ya_free_20549[33], &DD1_free_20529200[24], 6 *
               sizeof(complex16));
    __struct212.re = 0;
    __struct212.im = 0;
    bounds_check(64, 39 + 0, "test_wifimockup.blk:63:5-18");
    ya_free_20549[39].re = __struct212.re;
    ya_free_20549[39].im = __struct212.im;
    bounds_check(48, 30 + 12, "test_wifimockup.blk:64:5-19");
    bounds_check(64, 40 + 12, "test_wifimockup.blk:64:5-19");
    blink_copy(&ya_free_20549[40], &DD1_free_20529200[30], 13 *
               sizeof(complex16));
    __struct213.re = 0;
    __struct213.im = 0;
    bounds_check(64, 53 + 0, "test_wifimockup.blk:65:5-18");
    ya_free_20549[53].re = __struct213.re;
    ya_free_20549[53].im = __struct213.im;
    bounds_check(48, 43 + 4, "test_wifimockup.blk:66:5-19");
    bounds_check(64, 54 + 4, "test_wifimockup.blk:66:5-19");
    blink_copy(&ya_free_20549[54], &DD1_free_20529200[43], 5 *
               sizeof(complex16));
    __struct214.re = 0;
    __struct214.im = 0;
    bounds_check(64, 59 + 0, "test_wifimockup.blk:68:5-31");
    ya_free_20549[59].re = __struct214.re;
    ya_free_20549[59].im = __struct214.im;
    __struct215.re = 0;
    __struct215.im = 0;
    bounds_check(64, 60 + 0, "test_wifimockup.blk:69:5-31");
    ya_free_20549[60].re = __struct215.re;
    ya_free_20549[60].im = __struct215.im;
    __struct216.re = 0;
    __struct216.im = 0;
    bounds_check(64, 61 + 0, "test_wifimockup.blk:70:5-31");
    ya_free_20549[61].re = __struct216.re;
    ya_free_20549[61].im = __struct216.im;
    __struct217.re = 0;
    __struct217.im = 0;
    bounds_check(64, 62 + 0, "test_wifimockup.blk:71:5-31");
    ya_free_20549[62].re = __struct217.re;
    ya_free_20549[62].im = __struct217.im;
    __struct218.re = 0;
    __struct218.im = 0;
    bounds_check(64, 63 + 0, "test_wifimockup.blk:72:5-31");
    ya_free_20549[63].re = __struct218.re;
    ya_free_20549[63].im = __struct218.im;
    return 0;
}
int auto_map__31018_ln14_221(complex16* _ret222, int __len_unused_224,
                             complex16* DD1_free_21638223,
                             int __len_unused_225);
int auto_map__31018_ln14_221(complex16* _ret222, int __len_unused_224,
                             complex16* DD1_free_21638223, int __len_unused_225)
{
    calign complex16 __exp_arr226[160] = {{0, 0}};
    calign complex16 __struct227 = {0, 0};
    calign complex16 __struct228 = {0, 0};
    calign complex16 __struct229 = {0, 0};
    calign complex16 __struct230 = {0, 0};
    calign complex16 __struct231 = {0, 0};
    calign complex16 __struct232 = {0, 0};
    calign complex16 __struct233 = {0, 0};
    calign complex16 __struct234 = {0, 0};
    calign complex16 __struct235 = {0, 0};
    calign complex16 __struct236 = {0, 0};
    calign complex16 __struct237 = {0, 0};
    calign complex16 __struct238 = {0, 0};
    calign complex16 __struct239 = {0, 0};
    calign complex16 __struct240 = {0, 0};
    calign complex16 __struct241 = {0, 0};
    calign complex16 __struct242 = {0, 0};
    calign complex16 __struct243 = {0, 0};
    calign complex16 __struct244 = {0, 0};
    calign complex16 __struct245 = {0, 0};
    calign complex16 __struct246 = {0, 0};
    calign complex16 __struct247 = {0, 0};
    calign complex16 __struct248 = {0, 0};
    calign complex16 __struct249 = {0, 0};
    calign complex16 __struct250 = {0, 0};
    calign complex16 __struct251 = {0, 0};
    calign complex16 __struct252 = {0, 0};
    calign complex16 __struct253 = {0, 0};
    calign complex16 __struct254 = {0, 0};
    calign complex16 __struct255 = {0, 0};
    calign complex16 __struct256 = {0, 0};
    calign complex16 __struct257 = {0, 0};
    calign complex16 __struct258 = {0, 0};
    calign complex16 __struct259 = {0, 0};
    calign complex16 __struct260 = {0, 0};
    calign complex16 __struct261 = {0, 0};
    calign complex16 __struct262 = {0, 0};
    calign complex16 __struct263 = {0, 0};
    calign complex16 __struct264 = {0, 0};
    calign complex16 __struct265 = {0, 0};
    calign complex16 __struct266 = {0, 0};
    calign complex16 __struct267 = {0, 0};
    calign complex16 __struct268 = {0, 0};
    calign complex16 __struct269 = {0, 0};
    calign complex16 __struct270 = {0, 0};
    calign complex16 __struct271 = {0, 0};
    calign complex16 __struct272 = {0, 0};
    calign complex16 __struct273 = {0, 0};
    calign complex16 __struct274 = {0, 0};
    calign complex16 __struct275 = {0, 0};
    calign complex16 __struct276 = {0, 0};
    calign complex16 __struct277 = {0, 0};
    calign complex16 __struct278 = {0, 0};
    calign complex16 __struct279 = {0, 0};
    calign complex16 __struct280 = {0, 0};
    calign complex16 __struct281 = {0, 0};
    calign complex16 __struct282 = {0, 0};
    calign complex16 __struct283 = {0, 0};
    calign complex16 __struct284 = {0, 0};
    calign complex16 __struct285 = {0, 0};
    calign complex16 __struct286 = {0, 0};
    calign complex16 __struct287 = {0, 0};
    calign complex16 __struct288 = {0, 0};
    calign complex16 __struct289 = {0, 0};
    calign complex16 __struct290 = {0, 0};
    calign complex16 __struct291 = {0, 0};
    calign complex16 __struct292 = {0, 0};
    calign complex16 __struct293 = {0, 0};
    calign complex16 __struct294 = {0, 0};
    calign complex16 __struct295 = {0, 0};
    calign complex16 __struct296 = {0, 0};
    calign complex16 __struct297 = {0, 0};
    calign complex16 __struct298 = {0, 0};
    calign complex16 __struct299 = {0, 0};
    calign complex16 __struct300 = {0, 0};
    calign complex16 __struct301 = {0, 0};
    calign complex16 __struct302 = {0, 0};
    calign complex16 __struct303 = {0, 0};
    calign complex16 __struct304 = {0, 0};
    calign complex16 __struct305 = {0, 0};
    calign complex16 __struct306 = {0, 0};
    calign complex16 __struct307 = {0, 0};
    calign complex16 __struct308 = {0, 0};
    calign complex16 __struct309 = {0, 0};
    calign complex16 __struct310 = {0, 0};
    calign complex16 __struct311 = {0, 0};
    calign complex16 __struct312 = {0, 0};
    calign complex16 __struct313 = {0, 0};
    calign complex16 __struct314 = {0, 0};
    calign complex16 __struct315 = {0, 0};
    calign complex16 __struct316 = {0, 0};
    calign complex16 __struct317 = {0, 0};
    calign complex16 __struct318 = {0, 0};
    calign complex16 __struct319 = {0, 0};
    calign complex16 __struct320 = {0, 0};
    calign complex16 __struct321 = {0, 0};
    calign complex16 __struct322 = {0, 0};
    calign complex16 __struct323 = {0, 0};
    calign complex16 __struct324 = {0, 0};
    calign complex16 __struct325 = {0, 0};
    calign complex16 __struct326 = {0, 0};
    calign complex16 __struct327 = {0, 0};
    calign complex16 __struct328 = {0, 0};
    calign complex16 __struct329 = {0, 0};
    calign complex16 __struct330 = {0, 0};
    calign complex16 __struct331 = {0, 0};
    calign complex16 __struct332 = {0, 0};
    calign complex16 __struct333 = {0, 0};
    calign complex16 __struct334 = {0, 0};
    calign complex16 __struct335 = {0, 0};
    calign complex16 __struct336 = {0, 0};
    calign complex16 __struct337 = {0, 0};
    calign complex16 __struct338 = {0, 0};
    calign complex16 __struct339 = {0, 0};
    calign complex16 __struct340 = {0, 0};
    calign complex16 __struct341 = {0, 0};
    calign complex16 __struct342 = {0, 0};
    calign complex16 __struct343 = {0, 0};
    calign complex16 __struct344 = {0, 0};
    calign complex16 __struct345 = {0, 0};
    calign complex16 __struct346 = {0, 0};
    calign complex16 __struct347 = {0, 0};
    calign complex16 __struct348 = {0, 0};
    calign complex16 __struct349 = {0, 0};
    calign complex16 __struct350 = {0, 0};
    calign complex16 __struct351 = {0, 0};
    calign complex16 __struct352 = {0, 0};
    calign complex16 __struct353 = {0, 0};
    calign complex16 __struct354 = {0, 0};
    calign complex16 __struct355 = {0, 0};
    calign complex16 __struct356 = {0, 0};
    calign complex16 __struct357 = {0, 0};
    calign complex16 __struct358 = {0, 0};
    calign complex16 __struct359 = {0, 0};
    calign complex16 __struct360 = {0, 0};
    calign complex16 __struct361 = {0, 0};
    calign complex16 __struct362 = {0, 0};
    calign complex16 __struct363 = {0, 0};
    calign complex16 __struct364 = {0, 0};
    calign complex16 __struct365 = {0, 0};
    calign complex16 __struct366 = {0, 0};
    calign complex16 __struct367 = {0, 0};
    calign complex16 __struct368 = {0, 0};
    calign complex16 __struct369 = {0, 0};
    calign complex16 __struct370 = {0, 0};
    calign complex16 __struct371 = {0, 0};
    calign complex16 __struct372 = {0, 0};
    calign complex16 __struct373 = {0, 0};
    calign complex16 __struct374 = {0, 0};
    calign complex16 __struct375 = {0, 0};
    calign complex16 __struct376 = {0, 0};
    calign complex16 __struct377 = {0, 0};
    calign complex16 __struct378 = {0, 0};
    calign complex16 __struct379 = {0, 0};
    calign complex16 __struct380 = {0, 0};
    calign complex16 __struct381 = {0, 0};
    calign complex16 __struct382 = {0, 0};
    calign complex16 __struct383 = {0, 0};
    calign complex16 __struct384 = {0, 0};
    calign complex16 __struct385 = {0, 0};
    calign complex16 __struct386 = {0, 0};
    
    __struct227.re = 0;
    __struct227.im = 0;
    bounds_check(160, 0 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[0].re = __struct227.re;
    __exp_arr226[0].im = __struct227.im;
    __struct228.re = 0;
    __struct228.im = 0;
    bounds_check(160, 1 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[1].re = __struct228.re;
    __exp_arr226[1].im = __struct228.im;
    __struct229.re = 0;
    __struct229.im = 0;
    bounds_check(160, 2 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[2].re = __struct229.re;
    __exp_arr226[2].im = __struct229.im;
    __struct230.re = 0;
    __struct230.im = 0;
    bounds_check(160, 3 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[3].re = __struct230.re;
    __exp_arr226[3].im = __struct230.im;
    __struct231.re = 0;
    __struct231.im = 0;
    bounds_check(160, 4 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[4].re = __struct231.re;
    __exp_arr226[4].im = __struct231.im;
    __struct232.re = 0;
    __struct232.im = 0;
    bounds_check(160, 5 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[5].re = __struct232.re;
    __exp_arr226[5].im = __struct232.im;
    __struct233.re = 0;
    __struct233.im = 0;
    bounds_check(160, 6 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[6].re = __struct233.re;
    __exp_arr226[6].im = __struct233.im;
    __struct234.re = 0;
    __struct234.im = 0;
    bounds_check(160, 7 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[7].re = __struct234.re;
    __exp_arr226[7].im = __struct234.im;
    __struct235.re = 0;
    __struct235.im = 0;
    bounds_check(160, 8 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[8].re = __struct235.re;
    __exp_arr226[8].im = __struct235.im;
    __struct236.re = 0;
    __struct236.im = 0;
    bounds_check(160, 9 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[9].re = __struct236.re;
    __exp_arr226[9].im = __struct236.im;
    __struct237.re = 0;
    __struct237.im = 0;
    bounds_check(160, 10 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[10].re = __struct237.re;
    __exp_arr226[10].im = __struct237.im;
    __struct238.re = 0;
    __struct238.im = 0;
    bounds_check(160, 11 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[11].re = __struct238.re;
    __exp_arr226[11].im = __struct238.im;
    __struct239.re = 0;
    __struct239.im = 0;
    bounds_check(160, 12 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[12].re = __struct239.re;
    __exp_arr226[12].im = __struct239.im;
    __struct240.re = 0;
    __struct240.im = 0;
    bounds_check(160, 13 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[13].re = __struct240.re;
    __exp_arr226[13].im = __struct240.im;
    __struct241.re = 0;
    __struct241.im = 0;
    bounds_check(160, 14 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[14].re = __struct241.re;
    __exp_arr226[14].im = __struct241.im;
    __struct242.re = 0;
    __struct242.im = 0;
    bounds_check(160, 15 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[15].re = __struct242.re;
    __exp_arr226[15].im = __struct242.im;
    __struct243.re = 0;
    __struct243.im = 0;
    bounds_check(160, 16 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[16].re = __struct243.re;
    __exp_arr226[16].im = __struct243.im;
    __struct244.re = 0;
    __struct244.im = 0;
    bounds_check(160, 17 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[17].re = __struct244.re;
    __exp_arr226[17].im = __struct244.im;
    __struct245.re = 0;
    __struct245.im = 0;
    bounds_check(160, 18 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[18].re = __struct245.re;
    __exp_arr226[18].im = __struct245.im;
    __struct246.re = 0;
    __struct246.im = 0;
    bounds_check(160, 19 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[19].re = __struct246.re;
    __exp_arr226[19].im = __struct246.im;
    __struct247.re = 0;
    __struct247.im = 0;
    bounds_check(160, 20 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[20].re = __struct247.re;
    __exp_arr226[20].im = __struct247.im;
    __struct248.re = 0;
    __struct248.im = 0;
    bounds_check(160, 21 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[21].re = __struct248.re;
    __exp_arr226[21].im = __struct248.im;
    __struct249.re = 0;
    __struct249.im = 0;
    bounds_check(160, 22 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[22].re = __struct249.re;
    __exp_arr226[22].im = __struct249.im;
    __struct250.re = 0;
    __struct250.im = 0;
    bounds_check(160, 23 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[23].re = __struct250.re;
    __exp_arr226[23].im = __struct250.im;
    __struct251.re = 0;
    __struct251.im = 0;
    bounds_check(160, 24 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[24].re = __struct251.re;
    __exp_arr226[24].im = __struct251.im;
    __struct252.re = 0;
    __struct252.im = 0;
    bounds_check(160, 25 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[25].re = __struct252.re;
    __exp_arr226[25].im = __struct252.im;
    __struct253.re = 0;
    __struct253.im = 0;
    bounds_check(160, 26 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[26].re = __struct253.re;
    __exp_arr226[26].im = __struct253.im;
    __struct254.re = 0;
    __struct254.im = 0;
    bounds_check(160, 27 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[27].re = __struct254.re;
    __exp_arr226[27].im = __struct254.im;
    __struct255.re = 0;
    __struct255.im = 0;
    bounds_check(160, 28 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[28].re = __struct255.re;
    __exp_arr226[28].im = __struct255.im;
    __struct256.re = 0;
    __struct256.im = 0;
    bounds_check(160, 29 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[29].re = __struct256.re;
    __exp_arr226[29].im = __struct256.im;
    __struct257.re = 0;
    __struct257.im = 0;
    bounds_check(160, 30 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[30].re = __struct257.re;
    __exp_arr226[30].im = __struct257.im;
    __struct258.re = 0;
    __struct258.im = 0;
    bounds_check(160, 31 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[31].re = __struct258.re;
    __exp_arr226[31].im = __struct258.im;
    __struct259.re = 0;
    __struct259.im = 0;
    bounds_check(160, 32 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[32].re = __struct259.re;
    __exp_arr226[32].im = __struct259.im;
    __struct260.re = 0;
    __struct260.im = 0;
    bounds_check(160, 33 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[33].re = __struct260.re;
    __exp_arr226[33].im = __struct260.im;
    __struct261.re = 0;
    __struct261.im = 0;
    bounds_check(160, 34 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[34].re = __struct261.re;
    __exp_arr226[34].im = __struct261.im;
    __struct262.re = 0;
    __struct262.im = 0;
    bounds_check(160, 35 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[35].re = __struct262.re;
    __exp_arr226[35].im = __struct262.im;
    __struct263.re = 0;
    __struct263.im = 0;
    bounds_check(160, 36 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[36].re = __struct263.re;
    __exp_arr226[36].im = __struct263.im;
    __struct264.re = 0;
    __struct264.im = 0;
    bounds_check(160, 37 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[37].re = __struct264.re;
    __exp_arr226[37].im = __struct264.im;
    __struct265.re = 0;
    __struct265.im = 0;
    bounds_check(160, 38 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[38].re = __struct265.re;
    __exp_arr226[38].im = __struct265.im;
    __struct266.re = 0;
    __struct266.im = 0;
    bounds_check(160, 39 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[39].re = __struct266.re;
    __exp_arr226[39].im = __struct266.im;
    __struct267.re = 0;
    __struct267.im = 0;
    bounds_check(160, 40 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[40].re = __struct267.re;
    __exp_arr226[40].im = __struct267.im;
    __struct268.re = 0;
    __struct268.im = 0;
    bounds_check(160, 41 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[41].re = __struct268.re;
    __exp_arr226[41].im = __struct268.im;
    __struct269.re = 0;
    __struct269.im = 0;
    bounds_check(160, 42 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[42].re = __struct269.re;
    __exp_arr226[42].im = __struct269.im;
    __struct270.re = 0;
    __struct270.im = 0;
    bounds_check(160, 43 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[43].re = __struct270.re;
    __exp_arr226[43].im = __struct270.im;
    __struct271.re = 0;
    __struct271.im = 0;
    bounds_check(160, 44 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[44].re = __struct271.re;
    __exp_arr226[44].im = __struct271.im;
    __struct272.re = 0;
    __struct272.im = 0;
    bounds_check(160, 45 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[45].re = __struct272.re;
    __exp_arr226[45].im = __struct272.im;
    __struct273.re = 0;
    __struct273.im = 0;
    bounds_check(160, 46 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[46].re = __struct273.re;
    __exp_arr226[46].im = __struct273.im;
    __struct274.re = 0;
    __struct274.im = 0;
    bounds_check(160, 47 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[47].re = __struct274.re;
    __exp_arr226[47].im = __struct274.im;
    __struct275.re = 0;
    __struct275.im = 0;
    bounds_check(160, 48 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[48].re = __struct275.re;
    __exp_arr226[48].im = __struct275.im;
    __struct276.re = 0;
    __struct276.im = 0;
    bounds_check(160, 49 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[49].re = __struct276.re;
    __exp_arr226[49].im = __struct276.im;
    __struct277.re = 0;
    __struct277.im = 0;
    bounds_check(160, 50 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[50].re = __struct277.re;
    __exp_arr226[50].im = __struct277.im;
    __struct278.re = 0;
    __struct278.im = 0;
    bounds_check(160, 51 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[51].re = __struct278.re;
    __exp_arr226[51].im = __struct278.im;
    __struct279.re = 0;
    __struct279.im = 0;
    bounds_check(160, 52 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[52].re = __struct279.re;
    __exp_arr226[52].im = __struct279.im;
    __struct280.re = 0;
    __struct280.im = 0;
    bounds_check(160, 53 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[53].re = __struct280.re;
    __exp_arr226[53].im = __struct280.im;
    __struct281.re = 0;
    __struct281.im = 0;
    bounds_check(160, 54 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[54].re = __struct281.re;
    __exp_arr226[54].im = __struct281.im;
    __struct282.re = 0;
    __struct282.im = 0;
    bounds_check(160, 55 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[55].re = __struct282.re;
    __exp_arr226[55].im = __struct282.im;
    __struct283.re = 0;
    __struct283.im = 0;
    bounds_check(160, 56 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[56].re = __struct283.re;
    __exp_arr226[56].im = __struct283.im;
    __struct284.re = 0;
    __struct284.im = 0;
    bounds_check(160, 57 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[57].re = __struct284.re;
    __exp_arr226[57].im = __struct284.im;
    __struct285.re = 0;
    __struct285.im = 0;
    bounds_check(160, 58 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[58].re = __struct285.re;
    __exp_arr226[58].im = __struct285.im;
    __struct286.re = 0;
    __struct286.im = 0;
    bounds_check(160, 59 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[59].re = __struct286.re;
    __exp_arr226[59].im = __struct286.im;
    __struct287.re = 0;
    __struct287.im = 0;
    bounds_check(160, 60 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[60].re = __struct287.re;
    __exp_arr226[60].im = __struct287.im;
    __struct288.re = 0;
    __struct288.im = 0;
    bounds_check(160, 61 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[61].re = __struct288.re;
    __exp_arr226[61].im = __struct288.im;
    __struct289.re = 0;
    __struct289.im = 0;
    bounds_check(160, 62 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[62].re = __struct289.re;
    __exp_arr226[62].im = __struct289.im;
    __struct290.re = 0;
    __struct290.im = 0;
    bounds_check(160, 63 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[63].re = __struct290.re;
    __exp_arr226[63].im = __struct290.im;
    __struct291.re = 0;
    __struct291.im = 0;
    bounds_check(160, 64 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[64].re = __struct291.re;
    __exp_arr226[64].im = __struct291.im;
    __struct292.re = 0;
    __struct292.im = 0;
    bounds_check(160, 65 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[65].re = __struct292.re;
    __exp_arr226[65].im = __struct292.im;
    __struct293.re = 0;
    __struct293.im = 0;
    bounds_check(160, 66 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[66].re = __struct293.re;
    __exp_arr226[66].im = __struct293.im;
    __struct294.re = 0;
    __struct294.im = 0;
    bounds_check(160, 67 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[67].re = __struct294.re;
    __exp_arr226[67].im = __struct294.im;
    __struct295.re = 0;
    __struct295.im = 0;
    bounds_check(160, 68 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[68].re = __struct295.re;
    __exp_arr226[68].im = __struct295.im;
    __struct296.re = 0;
    __struct296.im = 0;
    bounds_check(160, 69 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[69].re = __struct296.re;
    __exp_arr226[69].im = __struct296.im;
    __struct297.re = 0;
    __struct297.im = 0;
    bounds_check(160, 70 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[70].re = __struct297.re;
    __exp_arr226[70].im = __struct297.im;
    __struct298.re = 0;
    __struct298.im = 0;
    bounds_check(160, 71 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[71].re = __struct298.re;
    __exp_arr226[71].im = __struct298.im;
    __struct299.re = 0;
    __struct299.im = 0;
    bounds_check(160, 72 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[72].re = __struct299.re;
    __exp_arr226[72].im = __struct299.im;
    __struct300.re = 0;
    __struct300.im = 0;
    bounds_check(160, 73 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[73].re = __struct300.re;
    __exp_arr226[73].im = __struct300.im;
    __struct301.re = 0;
    __struct301.im = 0;
    bounds_check(160, 74 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[74].re = __struct301.re;
    __exp_arr226[74].im = __struct301.im;
    __struct302.re = 0;
    __struct302.im = 0;
    bounds_check(160, 75 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[75].re = __struct302.re;
    __exp_arr226[75].im = __struct302.im;
    __struct303.re = 0;
    __struct303.im = 0;
    bounds_check(160, 76 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[76].re = __struct303.re;
    __exp_arr226[76].im = __struct303.im;
    __struct304.re = 0;
    __struct304.im = 0;
    bounds_check(160, 77 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[77].re = __struct304.re;
    __exp_arr226[77].im = __struct304.im;
    __struct305.re = 0;
    __struct305.im = 0;
    bounds_check(160, 78 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[78].re = __struct305.re;
    __exp_arr226[78].im = __struct305.im;
    __struct306.re = 0;
    __struct306.im = 0;
    bounds_check(160, 79 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[79].re = __struct306.re;
    __exp_arr226[79].im = __struct306.im;
    __struct307.re = 0;
    __struct307.im = 0;
    bounds_check(160, 80 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[80].re = __struct307.re;
    __exp_arr226[80].im = __struct307.im;
    __struct308.re = 0;
    __struct308.im = 0;
    bounds_check(160, 81 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[81].re = __struct308.re;
    __exp_arr226[81].im = __struct308.im;
    __struct309.re = 0;
    __struct309.im = 0;
    bounds_check(160, 82 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[82].re = __struct309.re;
    __exp_arr226[82].im = __struct309.im;
    __struct310.re = 0;
    __struct310.im = 0;
    bounds_check(160, 83 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[83].re = __struct310.re;
    __exp_arr226[83].im = __struct310.im;
    __struct311.re = 0;
    __struct311.im = 0;
    bounds_check(160, 84 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[84].re = __struct311.re;
    __exp_arr226[84].im = __struct311.im;
    __struct312.re = 0;
    __struct312.im = 0;
    bounds_check(160, 85 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[85].re = __struct312.re;
    __exp_arr226[85].im = __struct312.im;
    __struct313.re = 0;
    __struct313.im = 0;
    bounds_check(160, 86 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[86].re = __struct313.re;
    __exp_arr226[86].im = __struct313.im;
    __struct314.re = 0;
    __struct314.im = 0;
    bounds_check(160, 87 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[87].re = __struct314.re;
    __exp_arr226[87].im = __struct314.im;
    __struct315.re = 0;
    __struct315.im = 0;
    bounds_check(160, 88 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[88].re = __struct315.re;
    __exp_arr226[88].im = __struct315.im;
    __struct316.re = 0;
    __struct316.im = 0;
    bounds_check(160, 89 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[89].re = __struct316.re;
    __exp_arr226[89].im = __struct316.im;
    __struct317.re = 0;
    __struct317.im = 0;
    bounds_check(160, 90 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[90].re = __struct317.re;
    __exp_arr226[90].im = __struct317.im;
    __struct318.re = 0;
    __struct318.im = 0;
    bounds_check(160, 91 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[91].re = __struct318.re;
    __exp_arr226[91].im = __struct318.im;
    __struct319.re = 0;
    __struct319.im = 0;
    bounds_check(160, 92 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[92].re = __struct319.re;
    __exp_arr226[92].im = __struct319.im;
    __struct320.re = 0;
    __struct320.im = 0;
    bounds_check(160, 93 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[93].re = __struct320.re;
    __exp_arr226[93].im = __struct320.im;
    __struct321.re = 0;
    __struct321.im = 0;
    bounds_check(160, 94 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[94].re = __struct321.re;
    __exp_arr226[94].im = __struct321.im;
    __struct322.re = 0;
    __struct322.im = 0;
    bounds_check(160, 95 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[95].re = __struct322.re;
    __exp_arr226[95].im = __struct322.im;
    __struct323.re = 0;
    __struct323.im = 0;
    bounds_check(160, 96 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[96].re = __struct323.re;
    __exp_arr226[96].im = __struct323.im;
    __struct324.re = 0;
    __struct324.im = 0;
    bounds_check(160, 97 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[97].re = __struct324.re;
    __exp_arr226[97].im = __struct324.im;
    __struct325.re = 0;
    __struct325.im = 0;
    bounds_check(160, 98 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[98].re = __struct325.re;
    __exp_arr226[98].im = __struct325.im;
    __struct326.re = 0;
    __struct326.im = 0;
    bounds_check(160, 99 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[99].re = __struct326.re;
    __exp_arr226[99].im = __struct326.im;
    __struct327.re = 0;
    __struct327.im = 0;
    bounds_check(160, 100 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[100].re = __struct327.re;
    __exp_arr226[100].im = __struct327.im;
    __struct328.re = 0;
    __struct328.im = 0;
    bounds_check(160, 101 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[101].re = __struct328.re;
    __exp_arr226[101].im = __struct328.im;
    __struct329.re = 0;
    __struct329.im = 0;
    bounds_check(160, 102 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[102].re = __struct329.re;
    __exp_arr226[102].im = __struct329.im;
    __struct330.re = 0;
    __struct330.im = 0;
    bounds_check(160, 103 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[103].re = __struct330.re;
    __exp_arr226[103].im = __struct330.im;
    __struct331.re = 0;
    __struct331.im = 0;
    bounds_check(160, 104 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[104].re = __struct331.re;
    __exp_arr226[104].im = __struct331.im;
    __struct332.re = 0;
    __struct332.im = 0;
    bounds_check(160, 105 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[105].re = __struct332.re;
    __exp_arr226[105].im = __struct332.im;
    __struct333.re = 0;
    __struct333.im = 0;
    bounds_check(160, 106 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[106].re = __struct333.re;
    __exp_arr226[106].im = __struct333.im;
    __struct334.re = 0;
    __struct334.im = 0;
    bounds_check(160, 107 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[107].re = __struct334.re;
    __exp_arr226[107].im = __struct334.im;
    __struct335.re = 0;
    __struct335.im = 0;
    bounds_check(160, 108 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[108].re = __struct335.re;
    __exp_arr226[108].im = __struct335.im;
    __struct336.re = 0;
    __struct336.im = 0;
    bounds_check(160, 109 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[109].re = __struct336.re;
    __exp_arr226[109].im = __struct336.im;
    __struct337.re = 0;
    __struct337.im = 0;
    bounds_check(160, 110 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[110].re = __struct337.re;
    __exp_arr226[110].im = __struct337.im;
    __struct338.re = 0;
    __struct338.im = 0;
    bounds_check(160, 111 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[111].re = __struct338.re;
    __exp_arr226[111].im = __struct338.im;
    __struct339.re = 0;
    __struct339.im = 0;
    bounds_check(160, 112 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[112].re = __struct339.re;
    __exp_arr226[112].im = __struct339.im;
    __struct340.re = 0;
    __struct340.im = 0;
    bounds_check(160, 113 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[113].re = __struct340.re;
    __exp_arr226[113].im = __struct340.im;
    __struct341.re = 0;
    __struct341.im = 0;
    bounds_check(160, 114 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[114].re = __struct341.re;
    __exp_arr226[114].im = __struct341.im;
    __struct342.re = 0;
    __struct342.im = 0;
    bounds_check(160, 115 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[115].re = __struct342.re;
    __exp_arr226[115].im = __struct342.im;
    __struct343.re = 0;
    __struct343.im = 0;
    bounds_check(160, 116 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[116].re = __struct343.re;
    __exp_arr226[116].im = __struct343.im;
    __struct344.re = 0;
    __struct344.im = 0;
    bounds_check(160, 117 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[117].re = __struct344.re;
    __exp_arr226[117].im = __struct344.im;
    __struct345.re = 0;
    __struct345.im = 0;
    bounds_check(160, 118 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[118].re = __struct345.re;
    __exp_arr226[118].im = __struct345.im;
    __struct346.re = 0;
    __struct346.im = 0;
    bounds_check(160, 119 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[119].re = __struct346.re;
    __exp_arr226[119].im = __struct346.im;
    __struct347.re = 0;
    __struct347.im = 0;
    bounds_check(160, 120 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[120].re = __struct347.re;
    __exp_arr226[120].im = __struct347.im;
    __struct348.re = 0;
    __struct348.im = 0;
    bounds_check(160, 121 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[121].re = __struct348.re;
    __exp_arr226[121].im = __struct348.im;
    __struct349.re = 0;
    __struct349.im = 0;
    bounds_check(160, 122 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[122].re = __struct349.re;
    __exp_arr226[122].im = __struct349.im;
    __struct350.re = 0;
    __struct350.im = 0;
    bounds_check(160, 123 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[123].re = __struct350.re;
    __exp_arr226[123].im = __struct350.im;
    __struct351.re = 0;
    __struct351.im = 0;
    bounds_check(160, 124 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[124].re = __struct351.re;
    __exp_arr226[124].im = __struct351.im;
    __struct352.re = 0;
    __struct352.im = 0;
    bounds_check(160, 125 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[125].re = __struct352.re;
    __exp_arr226[125].im = __struct352.im;
    __struct353.re = 0;
    __struct353.im = 0;
    bounds_check(160, 126 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[126].re = __struct353.re;
    __exp_arr226[126].im = __struct353.im;
    __struct354.re = 0;
    __struct354.im = 0;
    bounds_check(160, 127 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[127].re = __struct354.re;
    __exp_arr226[127].im = __struct354.im;
    __struct355.re = 0;
    __struct355.im = 0;
    bounds_check(160, 128 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[128].re = __struct355.re;
    __exp_arr226[128].im = __struct355.im;
    __struct356.re = 0;
    __struct356.im = 0;
    bounds_check(160, 129 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[129].re = __struct356.re;
    __exp_arr226[129].im = __struct356.im;
    __struct357.re = 0;
    __struct357.im = 0;
    bounds_check(160, 130 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[130].re = __struct357.re;
    __exp_arr226[130].im = __struct357.im;
    __struct358.re = 0;
    __struct358.im = 0;
    bounds_check(160, 131 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[131].re = __struct358.re;
    __exp_arr226[131].im = __struct358.im;
    __struct359.re = 0;
    __struct359.im = 0;
    bounds_check(160, 132 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[132].re = __struct359.re;
    __exp_arr226[132].im = __struct359.im;
    __struct360.re = 0;
    __struct360.im = 0;
    bounds_check(160, 133 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[133].re = __struct360.re;
    __exp_arr226[133].im = __struct360.im;
    __struct361.re = 0;
    __struct361.im = 0;
    bounds_check(160, 134 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[134].re = __struct361.re;
    __exp_arr226[134].im = __struct361.im;
    __struct362.re = 0;
    __struct362.im = 0;
    bounds_check(160, 135 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[135].re = __struct362.re;
    __exp_arr226[135].im = __struct362.im;
    __struct363.re = 0;
    __struct363.im = 0;
    bounds_check(160, 136 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[136].re = __struct363.re;
    __exp_arr226[136].im = __struct363.im;
    __struct364.re = 0;
    __struct364.im = 0;
    bounds_check(160, 137 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[137].re = __struct364.re;
    __exp_arr226[137].im = __struct364.im;
    __struct365.re = 0;
    __struct365.im = 0;
    bounds_check(160, 138 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[138].re = __struct365.re;
    __exp_arr226[138].im = __struct365.im;
    __struct366.re = 0;
    __struct366.im = 0;
    bounds_check(160, 139 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[139].re = __struct366.re;
    __exp_arr226[139].im = __struct366.im;
    __struct367.re = 0;
    __struct367.im = 0;
    bounds_check(160, 140 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[140].re = __struct367.re;
    __exp_arr226[140].im = __struct367.im;
    __struct368.re = 0;
    __struct368.im = 0;
    bounds_check(160, 141 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[141].re = __struct368.re;
    __exp_arr226[141].im = __struct368.im;
    __struct369.re = 0;
    __struct369.im = 0;
    bounds_check(160, 142 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[142].re = __struct369.re;
    __exp_arr226[142].im = __struct369.im;
    __struct370.re = 0;
    __struct370.im = 0;
    bounds_check(160, 143 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[143].re = __struct370.re;
    __exp_arr226[143].im = __struct370.im;
    __struct371.re = 0;
    __struct371.im = 0;
    bounds_check(160, 144 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[144].re = __struct371.re;
    __exp_arr226[144].im = __struct371.im;
    __struct372.re = 0;
    __struct372.im = 0;
    bounds_check(160, 145 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[145].re = __struct372.re;
    __exp_arr226[145].im = __struct372.im;
    __struct373.re = 0;
    __struct373.im = 0;
    bounds_check(160, 146 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[146].re = __struct373.re;
    __exp_arr226[146].im = __struct373.im;
    __struct374.re = 0;
    __struct374.im = 0;
    bounds_check(160, 147 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[147].re = __struct374.re;
    __exp_arr226[147].im = __struct374.im;
    __struct375.re = 0;
    __struct375.im = 0;
    bounds_check(160, 148 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[148].re = __struct375.re;
    __exp_arr226[148].im = __struct375.im;
    __struct376.re = 0;
    __struct376.im = 0;
    bounds_check(160, 149 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[149].re = __struct376.re;
    __exp_arr226[149].im = __struct376.im;
    __struct377.re = 0;
    __struct377.im = 0;
    bounds_check(160, 150 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[150].re = __struct377.re;
    __exp_arr226[150].im = __struct377.im;
    __struct378.re = 0;
    __struct378.im = 0;
    bounds_check(160, 151 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[151].re = __struct378.re;
    __exp_arr226[151].im = __struct378.im;
    __struct379.re = 0;
    __struct379.im = 0;
    bounds_check(160, 152 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[152].re = __struct379.re;
    __exp_arr226[152].im = __struct379.im;
    __struct380.re = 0;
    __struct380.im = 0;
    bounds_check(160, 153 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[153].re = __struct380.re;
    __exp_arr226[153].im = __struct380.im;
    __struct381.re = 0;
    __struct381.im = 0;
    bounds_check(160, 154 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[154].re = __struct381.re;
    __exp_arr226[154].im = __struct381.im;
    __struct382.re = 0;
    __struct382.im = 0;
    bounds_check(160, 155 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[155].re = __struct382.re;
    __exp_arr226[155].im = __struct382.im;
    __struct383.re = 0;
    __struct383.im = 0;
    bounds_check(160, 156 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[156].re = __struct383.re;
    __exp_arr226[156].im = __struct383.im;
    __struct384.re = 0;
    __struct384.im = 0;
    bounds_check(160, 157 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[157].re = __struct384.re;
    __exp_arr226[157].im = __struct384.im;
    __struct385.re = 0;
    __struct385.im = 0;
    bounds_check(160, 158 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[158].re = __struct385.re;
    __exp_arr226[158].im = __struct385.im;
    __struct386.re = 0;
    __struct386.im = 0;
    bounds_check(160, 159 + 0, "test_wifimockup.blk:15:5-38");
    __exp_arr226[159].re = __struct386.re;
    __exp_arr226[159].im = __struct386.im;
    blink_copy(_ret222, __exp_arr226, 160 * sizeof(complex16));
    return UNIT;
}
int auto_map__31019_ln117_389(complex16* ya_free_25719, int __len_unused_391,
                              complex16* UD1_free_25717390,
                              int __len_unused_392);
int auto_map__31019_ln117_389(complex16* ya_free_25719, int __len_unused_391,
                              complex16* UD1_free_25717390,
                              int __len_unused_392)
{
    for (int32 idx_free_25720393 = 0; idx_free_25720393 < 0 + 160;
         idx_free_25720393++) {
        bounds_check(160, idx_free_25720393 + 0,
                     "test_wifimockup.blk:117:3-118:25");
        bounds_check(160, idx_free_25720393 + 0,
                     "test_wifimockup.blk:117:3-118:25");
        ya_free_25719[idx_free_25720393] = UD1_free_25717390[idx_free_25720393];
    }
    return 0;
}
int wpl_go_aux(int initialized)
{
    unsigned int loop_counter = 0;
    calign complex16* __yv_tmp_ln131_3_buf = NULL;
    calign complex16* __yv_tmp_ln131_5_buf = NULL;
    calign BitArrPtr __yv_tmp_ln131_7_buf = NULL;
    calign int8 __yv_tmp_ln131_9_buf = 0;
    calign unsigned char ab_ln124_11[1] = {0};
    calign unsigned char ret18[1] = {0};
    unsigned int mem_idx19;
    calign int32 len_ln108_20 = 0;
    calign int32 len_ln109_21 = 0;
    calign complex16* __yv_tmp_ln109_23_buf = NULL;
    calign complex16* __yv_tmp_ln109_25_buf = NULL;
    calign BitArrPtr __yv_tmp_ln100_27_buf = NULL;
    calign BitArrPtr __yv_tmp_ln100_29_buf = NULL;
    calign unsigned char __bit_arr30[1] = {0};
    calign unsigned char crc_state_ln23_31[1] = {0};
    volatile int32 __branch_var_ln37_33 = 0;
    calign int __dv_tmp__unused_221_ln37_34_doneVal = 0;
    calign int __dv_tmp__unused_220_ln37_36_doneVal = 0;
    calign int __dv_tmp__unused_219_ln37_38_doneVal = 0;
    calign int __dv_tmp__unused_218_ln37_40_doneVal = 0;
    calign int32 len_so_far_ln37_42 = 0;
    int __c43;
    calign int32 len_mod_ln37_44 = 0;
    calign int32 final_len_ln37_45 = 0;
    calign BitArrPtr __yv_tmp_ln37_47_buf = NULL;
    calign int32 _tmp_count_ln35_48 = 0;
    volatile int32 __branch_var_ln35_52 = 0;
    calign int __dv_tmp___seq_unused_ln35_49_ln35_53_doneVal = 0;
    volatile int32 __bnd_fst_ln35_50_state = 0;
    volatile int32 __branch_var_ln35_57 = 0;
    volatile int32 __par_c2_ln37_46_mit_state = 2;
    volatile int32 __bnd_rest_ln37_37_state = 0;
    volatile int32 __bnd_rest_ln37_35_state = 0;
    calign BitArrPtr __yv_tmp_ln_61_buf = NULL;
    calign int32 _tmp_count_ln25_62 = 0;
    volatile int32 __branch_var_ln25_66 = 0;
    calign unsigned char __dv_tmp_DD1_free_1306_ln25_67_doneVal[1] = {0};
    calign int __dv_tmp___seq_unused_ln25_63_ln25_69_doneVal = 0;
    volatile int32 __bnd_rest_ln25_68_state = 0;
    volatile int32 __branch_var_ln25_72 = 0;
    volatile int32 __par_c2_ln_60_mit_state = 2;
    calign unsigned char __bit_arr74[1] = {0};
    calign unsigned char s_ln78_75[1] = {0};
    calign Bit __yv_tmp_ln_77_buf = 0;
    calign int __dv_tmp_ln90_78_doneVal = 0;
    volatile int32 __branch_var_ln91_80 = 0;
    calign unsigned char __dv_tmp_UD2_free_9065_ln91_81_doneVal[1] = {0};
    calign unsigned char ya_free_9066_ln91_83[1] = {0};
    calign unsigned char rln81_84[1] = {0};
    calign Bit bitres85 = 0;
    calign Bit bitres86 = 0;
    calign Bit bitres87 = 0;
    calign Bit bitres88 = 0;
    calign Bit bitres89 = 0;
    calign Bit bitres90 = 0;
    calign Bit bitres91 = 0;
    calign Bit bitres92 = 0;
    calign Bit bitres93 = 0;
    calign Bit bitres94 = 0;
    calign Bit bitres95 = 0;
    calign unsigned char rln81_96[1] = {0};
    calign Bit bitres97 = 0;
    calign Bit bitres98 = 0;
    calign Bit bitres99 = 0;
    calign Bit bitres100 = 0;
    calign Bit bitres101 = 0;
    calign Bit bitres102 = 0;
    calign Bit bitres103 = 0;
    calign Bit bitres104 = 0;
    calign Bit bitres105 = 0;
    calign Bit bitres106 = 0;
    calign Bit bitres107 = 0;
    calign unsigned char rln81_108[1] = {0};
    calign Bit bitres109 = 0;
    calign Bit bitres110 = 0;
    calign Bit bitres111 = 0;
    calign Bit bitres112 = 0;
    calign Bit bitres113 = 0;
    calign Bit bitres114 = 0;
    calign Bit bitres115 = 0;
    calign Bit bitres116 = 0;
    calign Bit bitres117 = 0;
    calign Bit bitres118 = 0;
    calign Bit bitres119 = 0;
    calign unsigned char rln81_120[1] = {0};
    calign Bit bitres121 = 0;
    calign Bit bitres122 = 0;
    calign Bit bitres123 = 0;
    calign Bit bitres124 = 0;
    calign Bit bitres125 = 0;
    calign Bit bitres126 = 0;
    calign Bit bitres127 = 0;
    calign Bit bitres128 = 0;
    calign Bit bitres129 = 0;
    calign Bit bitres130 = 0;
    calign Bit bitres131 = 0;
    calign int _unused_30682_ln91_132 = 0;
    calign int32 idx_free_9068_ln91_133 = 0;
    volatile int32 __branch_var_ln91_137 = 0;
    calign int __dv_tmp___seq_unused_ln91_134_ln91_138_doneVal = 0;
    volatile int32 __bnd_fst_ln91_135_state = 0;
    volatile int32 __branch_var_ln91_142 = 0;
    volatile int32 __par_c2_ln_76_mit_state = 0;
    calign unsigned char __par_c2_ln_76_mit_buff[6] = {0};
    calign complex16 ret197[48] = {{0, 0}};
    unsigned int mem_idx198;
    calign complex16 ret219[64] = {{0, 0}};
    unsigned int mem_idx220;
    calign complex16 ret387[160] = {{0, 0}};
    unsigned int mem_idx388;
    calign complex16 ret394[160] = {{0, 0}};
    unsigned int mem_idx395;
    calign unsigned char __global__doneVal[1] = {0};
    char __globalWhatIs;
    
    if (!initialized) {
        len_ln109_21 = len_ln108_20;
        *(uint8*) (BitArrPtr) (crc_state_ln23_31 + 0) =
            *(uint8*) (BitArrPtr) (&__bit_arr30[0] + 0);
        __branch_var_ln37_33 = 0;
        _tmp_count_ln25_62 = 0;
        if (_tmp_count_ln25_62 < len_ln109_21 + 0) {
            __branch_var_ln25_72 = 0;
            __branch_var_ln25_66 = 0;
        } else {
            __branch_var_ln25_72 = 1;
        }
        *(uint8*) (BitArrPtr) (s_ln78_75 + 0) =
            *(uint8*) (BitArrPtr) (&__bit_arr74[0] + 0);
        __branch_var_ln91_80 = 0;
    }
    
  l_DEFAULT_LBL: 
    {
        
      l_LOOP: 
        {
            goto __par_c2_ln100_28_tick;
            
          l_IMMEDIATE: 
            switch (__globalWhatIs) {
                
                
              case SKIP:
                goto l_LOOP;
                
                
              case YIELD:
                printf("BUG in code generation: YIELD!");
                exit(-1);
                
                
              case DONE:
                return 0;
            }
            return 2;
            // error
            
          l_CONSUME: 
            printf("BUG in code generation: CONSUME!");
            exit(-1);
        }
        ORIGIN("test_wifimockup.blk:131:17-41");
        
      __par_c1_ln131_1_tick: 
        {
            if (buf_getint8(params, pbuf_ctx, &__yv_tmp_ln131_9_buf) == GS_EOF)
                return 2;
            
            goto __par_c2_ln131_8_process;
        }
        
      __par_c1_ln131_1_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_wifimockup.blk:125:3-126:21");
        
      __par_c2_ln131_8_tick: 
        {
            goto __par_c1_ln131_1_tick;
        }
        
      __par_c2_ln131_8_process: 
        {
            mem_idx19 = wpl_get_free_idx(pheap_ctx);
            auto_map__31015_ln125_12(ret18, 8, __yv_tmp_ln131_9_buf,
                                     ab_ln124_11, 8);
            wpl_restore_free_idx(pheap_ctx, mem_idx19);
            __yv_tmp_ln131_7_buf = ret18;
            goto __par_c2_ln131_6_process;
        }
        
      __bnd_rest_ln37_41_tick: 
        {
            *(uint8*) (BitArrPtr) (__global__doneVal + 0) =
                *(uint8*) (BitArrPtr) (&crc_state_ln23_31[0] + 0);
            __globalWhatIs = DONE;
            __globalWhatIs = DONE;
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln37_41_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln35_54_tick: 
        {
            _tmp_count_ln35_48 = _tmp_count_ln35_48 + 1;
            __dv_tmp__unused_218_ln37_40_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (_tmp_count_ln35_48 < final_len_ln37_45 - len_so_far_ln37_42 +
                0) {
                __branch_var_ln35_57 = 0;
                __branch_var_ln35_52 = 0;
                __bnd_fst_ln35_50_state = FALSE;
            } else {
                __branch_var_ln35_57 = 1;
            }
            goto __bnd_rest_ln37_39_tick;
        }
        
      __bnd_rest_ln35_54_process: 
        {
            goto l_IMMEDIATE;
        }
        /* test_wifimockup.blk:35:34-64 */
        ;
        
      __bnd_fst_ln35_50_tick: 
        {
            if (!__bnd_fst_ln35_50_state) {
                calign unsigned char __bit_arr55[1] = {0};
                
                __yv_tmp_ln37_47_buf = __bit_arr55;
                __bnd_fst_ln35_50_state = TRUE;
                goto __par_c2_ln37_46_process;
            } else {
                __dv_tmp___seq_unused_ln35_49_ln35_53_doneVal = UNIT;
                __branch_var_ln35_52 = __branch_var_ln35_52 + 1;
                goto __bnd_rest_ln35_54_tick;
            }
        }
        
      __bnd_fst_ln35_50_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_wifimockup.blk:35:10-32");
        
      __bind_ln35_51_tick: 
        {
            switch (__branch_var_ln35_52) {
                
                
              case 0:
                goto __bnd_fst_ln35_50_tick;
                
                
              case 1:
                goto __bnd_rest_ln35_54_tick;
            }
        }
        
      __bind_ln35_51_process: 
        {
            switch (__branch_var_ln35_52) {
                
                
              case 0:
                goto __bnd_fst_ln35_50_process;
                
                
              case 1:
                goto __bnd_rest_ln35_54_process;
            }
        }
        
      __ret_ln35_56_tick: 
        {
            __dv_tmp__unused_218_ln37_40_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln37_33 = __branch_var_ln37_33 + 1;
            goto __bnd_rest_ln37_41_tick;
        }
        
      __ret_ln35_56_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln37_39_tick: 
        {
            if (__branch_var_ln35_57 == 0) {
                goto __bind_ln35_51_tick;
            } else {
                goto __ret_ln35_56_tick;
            }
        }
        
      __bnd_rest_ln37_39_process: 
        {
            if (__branch_var_ln35_57 == 0) {
                goto __bind_ln35_51_process;
            } else {
                goto __ret_ln35_56_process;
            }
        }
        ORIGIN("test_wifimockup.blk:37:10-19");
        
      __par_c2_ln37_46_tick: 
        {
            if (__par_c2_ln37_46_mit_state >= 2) {
                goto __bnd_rest_ln37_39_tick;
            } else {
                calign unsigned char bitarrres58[1] = {0};
                
                bitArrRead(__yv_tmp_ln37_47_buf, 4 * __par_c2_ln37_46_mit_state,
                           4, bitarrres58);
                __yv_tmp_ln100_29_buf = bitarrres58;
                __par_c2_ln37_46_mit_state++;
                goto __par_c2_ln100_28_process;
            }
        }
        
      __par_c2_ln37_46_process: 
        {
            __yv_tmp_ln100_29_buf = &__yv_tmp_ln37_47_buf[0];
            __par_c2_ln37_46_mit_state = 1;
            goto __par_c2_ln100_28_process;
        }
        /* test_wifimockup.blk:30:3-21 */
        ;
        
      __bnd_rest_ln37_37_tick: 
        {
            if (!__bnd_rest_ln37_37_state) {
                calign unsigned char __bit_arr59[1] = {0};
                
                __yv_tmp_ln100_29_buf = __bit_arr59;
                __bnd_rest_ln37_37_state = TRUE;
                goto __par_c2_ln100_28_process;
            } else {
                __dv_tmp__unused_219_ln37_38_doneVal = UNIT;
                __branch_var_ln37_33 = __branch_var_ln37_33 + 1;
                len_so_far_ln37_42 = (len_ln109_21 + 1) * 8;
                __c43 = len_so_far_ln37_42 % 24 > 0;
                len_mod_ln37_44 = __c43 ? 1 : 0;
                final_len_ln37_45 = (len_so_far_ln37_42 / 24 +
                                     len_mod_ln37_44) * 24;
                _tmp_count_ln35_48 = 0;
                if (_tmp_count_ln35_48 < final_len_ln37_45 -
                    len_so_far_ln37_42 + 0) {
                    __branch_var_ln35_57 = 0;
                    __branch_var_ln35_52 = 0;
                    __bnd_fst_ln35_50_state = FALSE;
                } else {
                    __branch_var_ln35_57 = 1;
                }
                goto __par_c2_ln37_46_tick;
            }
        }
        
      __bnd_rest_ln37_37_process: 
        {
            return IMMEDIATE;
        }
        /* test_wifimockup.blk:29:3-18 */
        ;
        
      __bnd_rest_ln37_35_tick: 
        {
            if (!__bnd_rest_ln37_35_state) {
                __yv_tmp_ln100_29_buf = crc_state_ln23_31;
                __bnd_rest_ln37_35_state = TRUE;
                goto __par_c2_ln100_28_process;
            } else {
                __dv_tmp__unused_220_ln37_36_doneVal = UNIT;
                __branch_var_ln37_33 = __branch_var_ln37_33 + 1;
                __bnd_rest_ln37_37_state = FALSE;
                goto __bnd_rest_ln37_37_tick;
            }
        }
        
      __bnd_rest_ln37_35_process: 
        {
            return IMMEDIATE;
        }
        
      __bnd_rest_ln25_70_tick: 
        {
            _tmp_count_ln25_62 = _tmp_count_ln25_62 + 1;
            __dv_tmp__unused_221_ln37_34_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (_tmp_count_ln25_62 < len_ln109_21 + 0) {
                __branch_var_ln25_72 = 0;
                __branch_var_ln25_66 = 0;
            } else {
                __branch_var_ln25_72 = 1;
            }
            goto __bnd_fst_ln37_32_tick;
        }
        
      __bnd_rest_ln25_70_process: 
        {
            goto l_IMMEDIATE;
        }
        /* test_wifimockup.blk:26:5-30 */
        ;
        
      __bnd_rest_ln25_68_tick: 
        {
            if (!__bnd_rest_ln25_68_state) {
                __yv_tmp_ln_61_buf = __dv_tmp_DD1_free_1306_ln25_67_doneVal;
                __bnd_rest_ln25_68_state = TRUE;
                goto __par_c2_ln_60_process;
            } else {
                __dv_tmp___seq_unused_ln25_63_ln25_69_doneVal = UNIT;
                __branch_var_ln25_66 = __branch_var_ln25_66 + 1;
                goto __bnd_rest_ln25_70_tick;
            }
        }
        
      __bnd_rest_ln25_68_process: 
        {
            return IMMEDIATE;
        }
        
      __bnd_fst_ln25_64_tick: 
        {
            goto __par_c1_ln131_1_tick;
        }
        
      __bnd_fst_ln25_64_process: 
        {
            *(uint8*) (BitArrPtr) (__dv_tmp_DD1_free_1306_ln25_67_doneVal + 0) =
                *(uint8*) (BitArrPtr) (&__yv_tmp_ln131_7_buf[0] + 0);
            __globalWhatIs = DONE;
            __branch_var_ln25_66 = __branch_var_ln25_66 + 1;
            __bnd_rest_ln25_68_state = FALSE;
            goto __bnd_rest_ln25_68_tick;
        }
        ORIGIN("test_wifimockup.blk:25:9-12");
        
      __bind_ln25_65_tick: 
        {
            switch (__branch_var_ln25_66) {
                
                
              case 0:
                goto __bnd_fst_ln25_64_tick;
                
                
              case 1:
                goto __bnd_rest_ln25_68_tick;
                
                
              case 2:
                goto __bnd_rest_ln25_70_tick;
            }
        }
        
      __bind_ln25_65_process: 
        {
            switch (__branch_var_ln25_66) {
                
                
              case 0:
                goto __bnd_fst_ln25_64_process;
                
                
              case 1:
                goto __bnd_rest_ln25_68_process;
                
                
              case 2:
                goto __bnd_rest_ln25_70_process;
            }
        }
        
      __ret_ln25_71_tick: 
        {
            __dv_tmp__unused_221_ln37_34_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln37_33 = __branch_var_ln37_33 + 1;
            __bnd_rest_ln37_35_state = FALSE;
            goto __bnd_rest_ln37_35_tick;
        }
        
      __ret_ln25_71_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_fst_ln37_32_tick: 
        {
            if (__branch_var_ln25_72 == 0) {
                goto __bind_ln25_65_tick;
            } else {
                goto __ret_ln25_71_tick;
            }
        }
        
      __bnd_fst_ln37_32_process: 
        {
            if (__branch_var_ln25_72 == 0) {
                goto __bind_ln25_65_process;
            } else {
                goto __ret_ln25_71_process;
            }
        }
        ORIGIN("<no location>");
        
      __par_c2_ln_60_tick: 
        {
            if (__par_c2_ln_60_mit_state >= 2) {
                goto __bnd_fst_ln37_32_tick;
            } else {
                calign unsigned char bitarrres73[1] = {0};
                
                bitArrRead(__yv_tmp_ln_61_buf, 4 * __par_c2_ln_60_mit_state, 4,
                           bitarrres73);
                __yv_tmp_ln100_29_buf = bitarrres73;
                __par_c2_ln_60_mit_state++;
                goto __par_c2_ln100_28_process;
            }
        }
        
      __par_c2_ln_60_process: 
        {
            __yv_tmp_ln100_29_buf = &__yv_tmp_ln_61_buf[0];
            __par_c2_ln_60_mit_state = 1;
            goto __par_c2_ln100_28_process;
        }
        ORIGIN("test_wifimockup.blk:37:10-19");
        
      __par_c2_ln131_6_tick: 
        {
            switch (__branch_var_ln37_33) {
                
                
              case 0:
                goto __par_c2_ln_60_tick;
                
                
              case 1:
                goto __bnd_rest_ln37_35_tick;
                
                
              case 2:
                goto __bnd_rest_ln37_37_tick;
                
                
              case 3:
                goto __par_c2_ln37_46_tick;
                
                
              case 4:
                goto __bnd_rest_ln37_41_tick;
            }
        }
        
      __par_c2_ln131_6_process: 
        {
            switch (__branch_var_ln37_33) {
                
                
              case 0:
                goto __bnd_fst_ln37_32_process;
                
                
              case 1:
                goto __bnd_rest_ln37_35_process;
                
                
              case 2:
                goto __bnd_rest_ln37_37_process;
                
                
              case 3:
                goto __bnd_rest_ln37_39_process;
                
                
              case 4:
                goto __bnd_rest_ln37_41_process;
            }
        }
        
      __bnd_rest_ln91_139_tick: 
        {
            idx_free_9068_ln91_133 = idx_free_9068_ln91_133 + 1;
            __dv_tmp_ln90_78_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (idx_free_9068_ln91_133 < 8 + 0) {
                __branch_var_ln91_142 = 0;
                __branch_var_ln91_137 = 0;
                __bnd_fst_ln91_135_state = FALSE;
            } else {
                __branch_var_ln91_142 = 1;
            }
            goto __bnd_rest_ln91_82_tick;
        }
        
      __bnd_rest_ln91_139_process: 
        {
            goto l_IMMEDIATE;
        }
        /* test_wifimockup.blk:91:5-17 */
        ;
        
      __bnd_fst_ln91_135_tick: 
        {
            if (!__bnd_fst_ln91_135_state) {
                calign Bit bitres140 = 0;
                
                bounds_check(8, idx_free_9068_ln91_133 + 0,
                             "test_wifimockup.blk:91:5-17");
                bitRead(ya_free_9066_ln91_83, idx_free_9068_ln91_133,
                        &bitres140);
                __yv_tmp_ln_77_buf = bitres140;
                __bnd_fst_ln91_135_state = TRUE;
                goto __par_c2_ln_76_process;
            } else {
                __dv_tmp___seq_unused_ln91_134_ln91_138_doneVal = UNIT;
                __branch_var_ln91_137 = __branch_var_ln91_137 + 1;
                goto __bnd_rest_ln91_139_tick;
            }
        }
        
      __bnd_fst_ln91_135_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_wifimockup.blk:91:5-17");
        
      __bind_ln91_136_tick: 
        {
            switch (__branch_var_ln91_137) {
                
                
              case 0:
                goto __bnd_fst_ln91_135_tick;
                
                
              case 1:
                goto __bnd_rest_ln91_139_tick;
            }
        }
        
      __bind_ln91_136_process: 
        {
            switch (__branch_var_ln91_137) {
                
                
              case 0:
                goto __bnd_fst_ln91_135_process;
                
                
              case 1:
                goto __bnd_rest_ln91_139_process;
            }
        }
        
      __ret_ln91_141_tick: 
        {
            __dv_tmp_ln90_78_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln91_80 = 0;
            goto __par_c2_ln100_28_tick;
        }
        
      __ret_ln91_141_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln91_82_tick: 
        {
            if (__branch_var_ln91_142 == 0) {
                goto __bind_ln91_136_tick;
            } else {
                goto __ret_ln91_141_tick;
            }
        }
        
      __bnd_rest_ln91_82_process: 
        {
            if (__branch_var_ln91_142 == 0) {
                goto __bind_ln91_136_process;
            } else {
                goto __ret_ln91_141_process;
            }
        }
        
      __bnd_fst_ln91_79_tick: 
        {
            goto __par_c2_ln131_6_tick;
        }
        
      __bnd_fst_ln91_79_process: 
        {
            *(uint8*) (BitArrPtr) (__dv_tmp_UD2_free_9065_ln91_81_doneVal + 0) =
                *(uint8*) (BitArrPtr) (&__yv_tmp_ln100_29_buf[0] + 0);
            __globalWhatIs = DONE;
            __branch_var_ln91_80 = __branch_var_ln91_80 + 1;
            bounds_check(4, 0 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 0, &bitres85);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:82:19-20");
            bitRead(s_ln78_75, 1, &bitres86);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:82:26-27");
            bitRead(s_ln78_75, 2, &bitres87);
            bounds_check(6, 4 + 0, "test_wifimockup.blk:82:33-34");
            bitRead(s_ln78_75, 4, &bitres88);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:82:40-41");
            bitRead(s_ln78_75, 5, &bitres89);
            bounds_check(2, 0 + 0, "test_wifimockup.blk:82:5-41");
            bitWrite(rln81_84, 0, bitres85 ^ bitres86 ^ bitres87 ^ bitres88 ^
                     bitres89);
            bounds_check(4, 0 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 0, &bitres90);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:83:19-20");
            bitRead(s_ln78_75, 0, &bitres91);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:83:26-27");
            bitRead(s_ln78_75, 1, &bitres92);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:83:33-34");
            bitRead(s_ln78_75, 2, &bitres93);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:83:40-41");
            bitRead(s_ln78_75, 5, &bitres94);
            bounds_check(2, 1 + 0, "test_wifimockup.blk:83:5-41");
            bitWrite(rln81_84, 1, bitres90 ^ bitres91 ^ bitres92 ^ bitres93 ^
                     bitres94);
            bounds_check(6, 0 + 4, "test_wifimockup.blk:85:5-21");
            bounds_check(6, 1 + 4, "test_wifimockup.blk:85:5-21");
            bitArrWrite(&s_ln78_75[0], 1, 5, s_ln78_75);
            bounds_check(4, 0 + 0, "test_wifimockup.blk:86:5-14");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 0, &bitres95);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:86:5-14");
            bitWrite(s_ln78_75, 0, bitres95);
            bounds_check(8, 0 + 1, "test_wifimockup.blk:91:5-17");
            bitArrWrite(rln81_84, 0, 2, ya_free_9066_ln91_83);
            bounds_check(4, 1 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 1, &bitres97);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:82:19-20");
            bitRead(s_ln78_75, 1, &bitres98);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:82:26-27");
            bitRead(s_ln78_75, 2, &bitres99);
            bounds_check(6, 4 + 0, "test_wifimockup.blk:82:33-34");
            bitRead(s_ln78_75, 4, &bitres100);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:82:40-41");
            bitRead(s_ln78_75, 5, &bitres101);
            bounds_check(2, 0 + 0, "test_wifimockup.blk:82:5-41");
            bitWrite(rln81_96, 0, bitres97 ^ bitres98 ^ bitres99 ^ bitres100 ^
                     bitres101);
            bounds_check(4, 1 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 1, &bitres102);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:83:19-20");
            bitRead(s_ln78_75, 0, &bitres103);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:83:26-27");
            bitRead(s_ln78_75, 1, &bitres104);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:83:33-34");
            bitRead(s_ln78_75, 2, &bitres105);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:83:40-41");
            bitRead(s_ln78_75, 5, &bitres106);
            bounds_check(2, 1 + 0, "test_wifimockup.blk:83:5-41");
            bitWrite(rln81_96, 1, bitres102 ^ bitres103 ^ bitres104 ^
                     bitres105 ^ bitres106);
            bounds_check(6, 0 + 4, "test_wifimockup.blk:85:5-21");
            bounds_check(6, 1 + 4, "test_wifimockup.blk:85:5-21");
            bitArrWrite(&s_ln78_75[0], 1, 5, s_ln78_75);
            bounds_check(4, 1 + 0, "test_wifimockup.blk:86:5-14");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 1, &bitres107);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:86:5-14");
            bitWrite(s_ln78_75, 0, bitres107);
            bounds_check(8, 2 + 1, "test_wifimockup.blk:91:5-17");
            bitArrWrite(rln81_96, 2, 2, ya_free_9066_ln91_83);
            bounds_check(4, 2 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 2, &bitres109);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:82:19-20");
            bitRead(s_ln78_75, 1, &bitres110);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:82:26-27");
            bitRead(s_ln78_75, 2, &bitres111);
            bounds_check(6, 4 + 0, "test_wifimockup.blk:82:33-34");
            bitRead(s_ln78_75, 4, &bitres112);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:82:40-41");
            bitRead(s_ln78_75, 5, &bitres113);
            bounds_check(2, 0 + 0, "test_wifimockup.blk:82:5-41");
            bitWrite(rln81_108, 0, bitres109 ^ bitres110 ^ bitres111 ^
                     bitres112 ^ bitres113);
            bounds_check(4, 2 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 2, &bitres114);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:83:19-20");
            bitRead(s_ln78_75, 0, &bitres115);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:83:26-27");
            bitRead(s_ln78_75, 1, &bitres116);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:83:33-34");
            bitRead(s_ln78_75, 2, &bitres117);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:83:40-41");
            bitRead(s_ln78_75, 5, &bitres118);
            bounds_check(2, 1 + 0, "test_wifimockup.blk:83:5-41");
            bitWrite(rln81_108, 1, bitres114 ^ bitres115 ^ bitres116 ^
                     bitres117 ^ bitres118);
            bounds_check(6, 0 + 4, "test_wifimockup.blk:85:5-21");
            bounds_check(6, 1 + 4, "test_wifimockup.blk:85:5-21");
            bitArrWrite(&s_ln78_75[0], 1, 5, s_ln78_75);
            bounds_check(4, 2 + 0, "test_wifimockup.blk:86:5-14");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 2, &bitres119);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:86:5-14");
            bitWrite(s_ln78_75, 0, bitres119);
            bounds_check(8, 4 + 1, "test_wifimockup.blk:91:5-17");
            bitArrWrite(rln81_108, 4, 2, ya_free_9066_ln91_83);
            bounds_check(4, 3 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 3, &bitres121);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:82:19-20");
            bitRead(s_ln78_75, 1, &bitres122);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:82:26-27");
            bitRead(s_ln78_75, 2, &bitres123);
            bounds_check(6, 4 + 0, "test_wifimockup.blk:82:33-34");
            bitRead(s_ln78_75, 4, &bitres124);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:82:40-41");
            bitRead(s_ln78_75, 5, &bitres125);
            bounds_check(2, 0 + 0, "test_wifimockup.blk:82:5-41");
            bitWrite(rln81_120, 0, bitres121 ^ bitres122 ^ bitres123 ^
                     bitres124 ^ bitres125);
            bounds_check(4, 3 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 3, &bitres126);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:83:19-20");
            bitRead(s_ln78_75, 0, &bitres127);
            bounds_check(6, 1 + 0, "test_wifimockup.blk:83:26-27");
            bitRead(s_ln78_75, 1, &bitres128);
            bounds_check(6, 2 + 0, "test_wifimockup.blk:83:33-34");
            bitRead(s_ln78_75, 2, &bitres129);
            bounds_check(6, 5 + 0, "test_wifimockup.blk:83:40-41");
            bitRead(s_ln78_75, 5, &bitres130);
            bounds_check(2, 1 + 0, "test_wifimockup.blk:83:5-41");
            bitWrite(rln81_120, 1, bitres126 ^ bitres127 ^ bitres128 ^
                     bitres129 ^ bitres130);
            bounds_check(6, 0 + 4, "test_wifimockup.blk:85:5-21");
            bounds_check(6, 1 + 4, "test_wifimockup.blk:85:5-21");
            bitArrWrite(&s_ln78_75[0], 1, 5, s_ln78_75);
            bounds_check(4, 3 + 0, "test_wifimockup.blk:86:5-14");
            bitRead(__dv_tmp_UD2_free_9065_ln91_81_doneVal, 3, &bitres131);
            bounds_check(6, 0 + 0, "test_wifimockup.blk:86:5-14");
            bitWrite(s_ln78_75, 0, bitres131);
            bounds_check(8, 6 + 1, "test_wifimockup.blk:91:5-17");
            bitArrWrite(rln81_120, 6, 2, ya_free_9066_ln91_83);
            _unused_30682_ln91_132 = UNIT;
            idx_free_9068_ln91_133 = 0;
            if (idx_free_9068_ln91_133 < 8 + 0) {
                __branch_var_ln91_142 = 0;
                __branch_var_ln91_137 = 0;
                __bnd_fst_ln91_135_state = FALSE;
            } else {
                __branch_var_ln91_142 = 1;
            }
            goto __bnd_rest_ln91_82_tick;
        }
        ORIGIN("test_wifimockup.blk:91:5-17");
        
      __par_c2_ln100_28_tick: 
        {
            switch (__branch_var_ln91_80) {
                
                
              case 0:
                goto __bnd_fst_ln91_79_tick;
                
                
              case 1:
                goto __bnd_rest_ln91_82_tick;
            }
        }
        
      __par_c2_ln100_28_process: 
        {
            switch (__branch_var_ln91_80) {
                
                
              case 0:
                goto __bnd_fst_ln91_79_process;
                
                
              case 1:
                goto __bnd_rest_ln91_82_process;
            }
        }
        ORIGIN("<no location>");
        
      __par_c2_ln_76_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln_76_process: 
        {
            bitWrite(__par_c2_ln_76_mit_buff, 1 * __par_c2_ln_76_mit_state,
                     __yv_tmp_ln_77_buf);
            __par_c2_ln_76_mit_state++;
            if (__par_c2_ln_76_mit_state >= 48) {
                __yv_tmp_ln100_27_buf = __par_c2_ln_76_mit_buff;
                __par_c2_ln_76_mit_state = 0;
                goto __par_c2_ln100_26_process;
            } else {
                goto __par_c2_ln100_28_tick;
            }
        }
        ORIGIN("test_wifimockup.blk:3:3-4:24");
        
      __par_c2_ln100_26_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln100_26_process: 
        {
            mem_idx198 = wpl_get_free_idx(pheap_ctx);
            auto_map__31016_ln3_143(ret197, 48, __yv_tmp_ln100_27_buf, 48);
            wpl_restore_free_idx(pheap_ctx, mem_idx198);
            __yv_tmp_ln109_25_buf = ret197;
            goto __par_c2_ln109_24_process;
        }
        ORIGIN("test_wifimockup.blk:42:2-43:18");
        
      __par_c2_ln109_24_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln109_24_process: 
        {
            mem_idx220 = wpl_get_free_idx(pheap_ctx);
            auto_map__31017_ln42_199(ret219, 64, __yv_tmp_ln109_25_buf, 48);
            wpl_restore_free_idx(pheap_ctx, mem_idx220);
            __yv_tmp_ln109_23_buf = ret219;
            goto __par_c2_ln109_22_process;
        }
        ORIGIN("test_wifimockup.blk:14:3-15:38");
        
      __par_c2_ln109_22_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln109_22_process: 
        {
            mem_idx388 = wpl_get_free_idx(pheap_ctx);
            auto_map__31018_ln14_221(ret387, 160, __yv_tmp_ln109_23_buf, 64);
            wpl_restore_free_idx(pheap_ctx, mem_idx388);
            __yv_tmp_ln131_5_buf = ret387;
            goto __par_c2_ln131_4_process;
        }
        ORIGIN("test_wifimockup.blk:117:3-118:25");
        
      __par_c2_ln131_4_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln131_4_process: 
        {
            mem_idx395 = wpl_get_free_idx(pheap_ctx);
            auto_map__31019_ln117_389(ret394, 160, __yv_tmp_ln131_5_buf, 160);
            wpl_restore_free_idx(pheap_ctx, mem_idx395);
            __yv_tmp_ln131_3_buf = ret394;
            goto __par_c2_ln131_2_process;
        }
        ORIGIN("test_wifimockup.blk:131:17-83");
        
      __par_c2_ln131_2_tick: 
        {
            goto __par_c2_ln100_28_tick;
        }
        
      __par_c2_ln131_2_process: 
        {
            buf_putarrcomplex16(params, pbuf_ctx, __yv_tmp_ln131_3_buf, 160);
            __globalWhatIs = SKIP;
            goto l_IMMEDIATE;
        }
        return 2;
    }
}
int wpl_go()

{
    return wpl_go_aux(0);
}
extern int SetUpThreads(PSORA_UTHREAD_PROC* User_Routines);
int wpl_set_up_threads(PSORA_UTHREAD_PROC* User_Routines)
{
    return SetUpThreads(User_Routines);
}
void wpl_input_initialize()

{
    init_getint8(params, pbuf_ctx, pheap_ctx, 1);
    init_putcomplex16(params, pbuf_ctx, pheap_ctx, sizeof(complex16));
}
void wpl_output_finalize()

{
    flush_putcomplex16(params, pbuf_ctx);
}
void wpl_output_reset()

{
    reset_putcomplex16(params, pbuf_ctx);
}
void wpl_global_init(memsize_int max_heap_siz)
{
    wpl_init_heap(pheap_ctx, max_heap_siz);
}
