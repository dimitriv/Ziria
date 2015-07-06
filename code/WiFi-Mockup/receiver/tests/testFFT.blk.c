/*
fun arctan2(y{_r3} : int16, x{_r4} : int16, th{_r5} : arr[1] int16) : ()
in
fun ucosw(y{_r8} : int16, out{_r9} : arr[1] int16) : ()
in
fun usinw(y{_r12} : int16, out{_r13} : arr[1] int16) : ()
in
fun sin_int16(x{_r16} : int16) : int16
in
fun cos_int16(x{_r19} : int16) : int16
in
fun atan2_int16(y{_r22} : int16, x{_r23} : int16) : int16
in
fun atan2_int32(y{_r26} : int32, x{_r27} : int32) : int32
in
fun sine_double_int16(x{_r33} : int16, prec{_r34} : int16) : int16
in
fun sine_double_int32(x{_r37} : int32, prec{_r38} : int32) : int32
in
fun cosine_double_int16(x{_r41} : int16, prec{_r42} : int16) : int16
in
fun cosine_double_int32(x{_r45} : int32, prec{_r46} : int32) : int32
in
fun imin_int16(x{_r49} : int16, y{_r50} : int16) : int16
in
fun imin_int32(x{_r53} : int32, y{_r54} : int32) : int32
in
fun imax_int16(x{_r57} : int16, y{_r58} : int16) : int16
in
fun imax_int32(x{_r61} : int32, y{_r62} : int32) : int32
in
fun ceil_int16(d{_r65} : double) : int16
in
fun ceil_int32(d{_r68} : double) : int32
in
fun round_int16(d{_r71} : double) : int16
in
fun round_int32(d{_r74} : double) : int32
in
fun log2(d{_r77} : double) : double
in
fun log2_int16(d{_r80} : int16) : int16
in
fun log2_int32(d{_r83} : int32) : int32
in
fun sqrt(d{_r86} : double) : double
in
fun sqrt_int16(d{_r89} : int16) : int16
in
fun sqrt_int32(d{_r92} : int32) : int32
in
fun v_downsample_complex16(input{_r95} : arr["n96"] complex16) : arr["n96"] complex16
in
fun permutate_low1032w(x{_r99} : arr[4] complex16, y{_r100} : arr[4] complex16) : ()
in
fun permutate_high1032w(x{_r103} : arr[4] complex16, y{_r104} : arr[4] complex16) : ()
in
fun pairwise_muladdw(x{_r107} : arr[4] complex16, y{_r108} : arr[4] complex16, z{_r109} : arr[4] int32) : ()
in
fun conjrew(x{_r112} : arr[4] complex16, y{_r113} : arr[4] complex16) : ()
in
fun sum_conj_mulw32(x{_r116} : arr[4] complex16, y{_r117} : arr[4] complex16) : complex32
in
fun conj0w(ret{_r120} : arr[4] complex16, x{_r121} : arr[4] complex16) : ()
in
fun muladdw(ret{_r124} : arr[4] int32, a{_r125} : arr[4] complex16, b{_r126} : arr[4] complex16) : ()
in
fun conj_mulw(ret{_r129} : arr[4] complex16, x{_r130} : arr[4] complex16, y{_r131} : arr[4] complex16) : ()
in
fun sumc16(x{_r134} : arr[4] complex16) : complex16
in
fun sumc32(x{_r137} : arr[4] complex32) : complex32
in
fun sumi16(x{_r140} : arr[4] int16) : int16
in
fun sumi32(x{_r143} : arr[4] int32) : int32
in
fun v_sum_complex16(x{_r146} : arr["n147"] complex16) : complex16
in
fun v_sum_int32(x{_r150} : arr["n151"] int32) : int32
in
fun v_hadd_complex16(z{_r154} : arr[4] complex16, x{_r155} : arr[4] complex16) : ()
in
fun v_hadd_int32(z{_r158} : arr[4] int32, x{_r159} : arr[4] int32) : ()
in
fun v_add_complex16(c{_r162} : arr["n163"] complex16, a{_r164} : arr["n163"] complex16, b{_r165} : arr["n163"] complex16) : ()
in
fun v_add_complex32(c{_r168} : arr["n169"] complex32, a{_r170} : arr["n169"] complex32, b{_r171} : arr["n169"] complex32) : ()
in
fun v_add_int16(c{_r174} : arr["n175"] int16, a{_r176} : arr["n175"] int16, b{_r177} : arr["n175"] int16) : ()
in
fun v_add_int32(c{_r180} : arr["n181"] int32, a{_r182} : arr["n181"] int32, b{_r183} : arr["n181"] int32) : ()
in
fun v_sub_complex16(c{_r186} : arr["n187"] complex16, a{_r188} : arr["n187"] complex16, b{_r189} : arr["n187"] complex16) : ()
in
fun v_sub_complex32(c{_r192} : arr["n193"] complex32, a{_r194} : arr["n193"] complex32, b{_r195} : arr["n193"] complex32) : ()
in
fun v_sub_int16(c{_r198} : arr["n199"] int16, a{_r200} : arr["n199"] int16, b{_r201} : arr["n199"] int16) : ()
in
fun v_sub_int32(c{_r204} : arr["n205"] int32, a{_r206} : arr["n205"] int32, b{_r207} : arr["n205"] int32) : ()
in
fun v_mul_complex16(c{_r210} : arr["n211"] complex16, a{_r212} : arr["n211"] complex16, b{_r213} : arr["n211"] complex16) : ()
in
fun v_shift_right_complex32(z{_r216} : arr["n217"] complex32, x{_r218} : arr["n217"] complex32, shift{_r219} : int32) : ()
in
fun v_shift_right_complex16(z{_r222} : arr["n223"] complex16, x{_r224} : arr["n223"] complex16, shift{_r225} : int32) : ()
in
fun v_shift_right_int32(z{_r228} : arr["n229"] int32, x{_r230} : arr["n229"] int32, shift{_r231} : int32) : ()
in
fun v_shift_right_int16(z{_r234} : arr["n235"] int16, x{_r236} : arr["n235"] int16, shift{_r237} : int32) : ()
in
fun v_shift_left_complex32(z{_r240} : arr["n241"] complex32, x{_r242} : arr["n241"] complex32, shift{_r243} : int32) : ()
in
fun v_shift_left_complex16(z{_r246} : arr["n247"] complex16, x{_r248} : arr["n247"] complex16, shift{_r249} : int32) : ()
in
fun v_shift_left_int32(z{_r252} : arr["n253"] int32, x{_r254} : arr["n253"] int32, shift{_r255} : int32) : ()
in
fun v_shift_left_int16(z{_r258} : arr["n259"] int16, x{_r260} : arr["n259"] int16, shift{_r261} : int32) : ()
in
fun zero_bit(x{_r264} : arr["n265"] bit) : ()
in
fun zero_complex8(x{_r268} : arr["n269"] complex8) : ()
in
fun zero_complex16(x{_r272} : arr["n273"] complex16) : ()
in
fun zero_complex32(x{_r276} : arr["n277"] complex32) : ()
in
fun zero_int8(x{_r280} : arr["n281"] int8) : ()
in
fun zero_int16(x{_r284} : arr["n285"] int16) : ()
in
fun zero_int32(x{_r288} : arr["n289"] int32) : ()
in
fun copy_complex8(dst{_r292} : arr["n293"] complex8, src{_r294} : arr["n295"] complex8, len{_r296} : int32) : ()
in
fun copy_complex16(dst{_r299} : arr["n300"] complex16, src{_r301} : arr["n302"] complex16, len{_r303} : int32) : ()
in
fun copy_complex32(dst{_r306} : arr["n307"] complex32, src{_r308} : arr["n309"] complex32, len{_r310} : int32) : ()
in
fun copy_int8(dst{_r313} : arr["n314"] int8, src{_r315} : arr["n316"] int8, len{_r317} : int32) : ()
in
fun copy_int16(dst{_r320} : arr["n321"] int16, src{_r322} : arr["n323"] int16, len{_r324} : int32) : ()
in
fun copy_int32(dst{_r327} : arr["n328"] int32, src{_r329} : arr["n330"] int32, len{_r331} : int32) : ()
in
fun bits_to_int8(dst{_r334} : arr["n335"] int8, src{_r336} : arr["n337"] bit) : ()
in
fun int8_to_bits(dst{_r340} : arr["n341"] bit, src{_r342} : arr["n343"] int8) : ()
in
fun hexprint_int8(a{_r346} : arr["n347"] int8, length{_r348} : int32) : ()
in
fun v_pack_int32_complex16(z{_r351} : arr["n352"] complex16, x{_r353} : arr["n352"] int32, y{_r354} : arr["n352"] int32) : ()
in
fun v_pack_complex16_complex8(input{_r357} : arr["n358"] complex16) : arr["n358"] complex8
in
fun v_negate_complex8(input{_r361} : arr["n362"] complex8) : arr["n362"] complex8
in
fun v_sign_int8(input1{_r365} : arr["n366"] int8, input2{_r367} : arr["n366"] int8) : arr["n366"] int8
in
fun v_cast_complex8_int8(output{_r370} : arr["n371"] int8, input{_r372} : arr["n373"] complex8) : ()
in
fun v_or(input1{_r376} : arr["n377"] bit, input2{_r378} : arr["n377"] bit) : arr["n377"] bit
in
fun v_and(input1{_r381} : arr["n382"] bit, input2{_r383} : arr["n382"] bit) : arr["n382"] bit
in
fun v_andnot(input1{_r386} : arr["n387"] bit, input2{_r388} : arr["n387"] bit) : arr["n387"] bit
in
fun v_xor(input1{_r391} : arr["n392"] bit, input2{_r393} : arr["n392"] bit) : arr["n392"] bit
in
fun sora_ifft(inp{_r396} : arr["n397"] complex16) : arr["n397"] complex16
in
fun sora_fft(inp{_r400} : arr["n401"] complex16) : arr["n401"] complex16
in
fun sora_ifft_dynamic(nFFTSize{_r404} : int16, inp{_r405} : arr["n406"] complex16) : arr["n406"] complex16
in
fun sora_fft_dynamic(nFFTSize{_r409} : int16, inp{_r410} : arr["n411"] complex16) : arr["n411"] complex16
in
fun viterbi_init(frame_length{_r414} : int32) : ()
in
fun viterbi_decode(svalue{_r417} : arr[48] int16, code_rate{_r418} : int32, bitValue{_r419} : arr[96000] bit, const1{_r420} : int16, const2{_r421} : int16) : int16
in
fun viterbi_decode_old(intInput{_r424} : arr[48] int16, code_rate{_r425} : int16, out{_r426} : arr[12000] int16) : int16
in
fun qpsk_demap(input_sample{_r429} : complex16, softBits{_r430} : arr[6] int16) : ()
in
fun bpsk_demap(input_sample{_r433} : complex16) : int16
in
fun qam16_demap(input_sample{_r436} : complex16, softBits{_r437} : arr[6] int16) : ()
in
fun qam64_demap(input_sample{_r440} : complex16, softBits{_r441} : arr[6] int16) : ()
in
fun viterbi_brick_init(frame_length{_r444} : int32, code_rate{_r445} : int16) : ()
in
fun viterbiSig11a_brick_init(frame_length{_r448} : int32, code_rate{_r449} : int16) : ()
in
fun viterbi_brick_decode(svalue{_r452} : arr[48] int8, bitValue{_r453} : arr[96000] bit) : int16
in
fun viterbi_brick_init_fast(frame_length{_r456} : int32, code_rate{_r457} : int16, depth{_r458} : int16) : ()
in
fun viterbi_brick_decode_fast(svalue{_r461} : arr[48] int8, bitValue{_r462} : arr["n463"] bit) : int16
in
fun viterbiSig11a_brick_decode_fast(svalue{_r466} : arr[48] int8, bitValue{_r467} : arr["n468"] bit) : int16
in
fun dbgplot_real_line(data{_r471} : arr["n472"] int16) : ()
in
fun dbgplot_real_line32(data{_r475} : arr["n476"] int32) : ()
in
fun dbgplot_complex_line(data{_r479} : arr["n480"] complex16, type{_r481} : int16) : ()
in
fun dbgplot_spectrum(item{_r484} : arr["n485"] complex16) : ()
in
fun dbgplot_dots(data{_r488} : arr["n489"] complex16) : ()
in
fun dbgplot_dot(data{_r492} : complex16) : ()
in
fun print_time() : ()
in
fun record_time_start() : ()
in
fun record_time_stop() : ()
in
fun populate_rand_array(arr{_r501} : arr["n502"] bit) : ()
in
((read[arr[64] complex16] >>>
  fun auto_map__1769{_pf1769}(DD1_free_1498{DD1_free_1498} : arr[64] complex16) =
        sora_fft{_r398}(DD1_free_1498{DD1_free_1498})
  in
  map auto_map__1769{_pf1769}) >>>
 write[arr[64] complex16])

type:
ST T (EXTBUF[base=arr[64] complex16]) (EXTBUF[base=arr[64] complex16])
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
int __ext_arctan2(int16 y, int16 x, int16* th, int __len_unused_1);
int __ext_ucosw(int16 y, int16* out, int __len_unused_2);
int __ext_usinw(int16 y, int16* out, int __len_unused_3);
int16 __ext_sin_int16(int16 x);
int16 __ext_cos_int16(int16 x);
int16 __ext_atan2_int16(int16 y, int16 x);
int32 __ext_atan2_int32(int32 y, int32 x);
int16 __ext_sine_double_int16(int16 x, int16 prec);
int32 __ext_sine_double_int32(int32 x, int32 prec);
int16 __ext_cosine_double_int16(int16 x, int16 prec);
int32 __ext_cosine_double_int32(int32 x, int32 prec);
int16 __ext_imin_int16(int16 x, int16 y);
int32 __ext_imin_int32(int32 x, int32 y);
int16 __ext_imax_int16(int16 x, int16 y);
int32 __ext_imax_int32(int32 x, int32 y);
int16 __ext_ceil_int16(double d);
int32 __ext_ceil_int32(double d);
int16 __ext_round_int16(double d);
int32 __ext_round_int32(double d);
double __ext_log2(double d);
int16 __ext_log2_int16(int16 d);
int32 __ext_log2_int32(int32 d);
double __ext_sqrt(double d);
int16 __ext_sqrt_int16(int16 d);
int32 __ext_sqrt_int32(int32 d);
void __ext_v_downsample_complex16(complex16* __retf_v_downsample_complex16,
                                  int n96, complex16* input, int __len_unused4);
int __ext_permutate_low1032w(complex16* x, int __len_unused_5, complex16* y,
                             int __len_unused_6);
int __ext_permutate_high1032w(complex16* x, int __len_unused_7, complex16* y,
                              int __len_unused_8);
int __ext_pairwise_muladdw(complex16* x, int __len_unused_9, complex16* y,
                           int __len_unused_10, int32* z, int __len_unused_11);
int __ext_conjrew(complex16* x, int __len_unused_12, complex16* y,
                  int __len_unused_13);
complex32 __ext_sum_conj_mulw32(complex16* x, int __len_unused_14, complex16* y,
                                int __len_unused_15);
int __ext_conj0w(complex16* ret, int __len_unused_16, complex16* x,
                 int __len_unused_17);
int __ext_muladdw(int32* ret, int __len_unused_18, complex16* a,
                  int __len_unused_19, complex16* b, int __len_unused_20);
int __ext_conj_mulw(complex16* ret, int __len_unused_21, complex16* x,
                    int __len_unused_22, complex16* y, int __len_unused_23);
complex16 __ext_sumc16(complex16* x, int __len_unused_24);
complex32 __ext_sumc32(complex32* x, int __len_unused_25);
int16 __ext_sumi16(int16* x, int __len_unused_26);
int32 __ext_sumi32(int32* x, int __len_unused_27);
complex16 __ext_v_sum_complex16(complex16* x, int n147);
int32 __ext_v_sum_int32(int32* x, int n151);
int __ext_v_hadd_complex16(complex16* z, int __len_unused_28, complex16* x,
                           int __len_unused_29);
int __ext_v_hadd_int32(int32* z, int __len_unused_30, int32* x,
                       int __len_unused_31);
int __ext_v_add_complex16(complex16* c, int n163, complex16* a,
                          int __len_unused32, complex16* b, int __len_unused33);
int __ext_v_add_complex32(complex32* c, int n169, complex32* a,
                          int __len_unused34, complex32* b, int __len_unused35);
int __ext_v_add_int16(int16* c, int n175, int16* a, int __len_unused36,
                      int16* b, int __len_unused37);
int __ext_v_add_int32(int32* c, int n181, int32* a, int __len_unused38,
                      int32* b, int __len_unused39);
int __ext_v_sub_complex16(complex16* c, int n187, complex16* a,
                          int __len_unused40, complex16* b, int __len_unused41);
int __ext_v_sub_complex32(complex32* c, int n193, complex32* a,
                          int __len_unused42, complex32* b, int __len_unused43);
int __ext_v_sub_int16(int16* c, int n199, int16* a, int __len_unused44,
                      int16* b, int __len_unused45);
int __ext_v_sub_int32(int32* c, int n205, int32* a, int __len_unused46,
                      int32* b, int __len_unused47);
int __ext_v_mul_complex16(complex16* c, int n211, complex16* a,
                          int __len_unused48, complex16* b, int __len_unused49);
int __ext_v_shift_right_complex32(complex32* z, int n217, complex32* x,
                                  int __len_unused50, int32 shift);
int __ext_v_shift_right_complex16(complex16* z, int n223, complex16* x,
                                  int __len_unused51, int32 shift);
int __ext_v_shift_right_int32(int32* z, int n229, int32* x, int __len_unused52,
                              int32 shift);
int __ext_v_shift_right_int16(int16* z, int n235, int16* x, int __len_unused53,
                              int32 shift);
int __ext_v_shift_left_complex32(complex32* z, int n241, complex32* x,
                                 int __len_unused54, int32 shift);
int __ext_v_shift_left_complex16(complex16* z, int n247, complex16* x,
                                 int __len_unused55, int32 shift);
int __ext_v_shift_left_int32(int32* z, int n253, int32* x, int __len_unused56,
                             int32 shift);
int __ext_v_shift_left_int16(int16* z, int n259, int16* x, int __len_unused57,
                             int32 shift);
int __ext_zero_bit(BitArrPtr x, int n265);
int __ext_zero_complex8(complex8* x, int n269);
int __ext_zero_complex16(complex16* x, int n273);
int __ext_zero_complex32(complex32* x, int n277);
int __ext_zero_int8(int8* x, int n281);
int __ext_zero_int16(int16* x, int n285);
int __ext_zero_int32(int32* x, int n289);
int __ext_copy_complex8(complex8* dst, int n293, complex8* src, int n295,
                        int32 len);
int __ext_copy_complex16(complex16* dst, int n300, complex16* src, int n302,
                         int32 len);
int __ext_copy_complex32(complex32* dst, int n307, complex32* src, int n309,
                         int32 len);
int __ext_copy_int8(int8* dst, int n314, int8* src, int n316, int32 len);
int __ext_copy_int16(int16* dst, int n321, int16* src, int n323, int32 len);
int __ext_copy_int32(int32* dst, int n328, int32* src, int n330, int32 len);
int __ext_bits_to_int8(int8* dst, int n335, BitArrPtr src, int n337);
int __ext_int8_to_bits(BitArrPtr dst, int n341, int8* src, int n343);
int __ext_hexprint_int8(int8* a, int n347, int32 length);
int __ext_v_pack_int32_complex16(complex16* z, int n352, int32* x,
                                 int __len_unused58, int32* y,
                                 int __len_unused59);
void __ext_v_pack_complex16_complex8(complex8* __retf_v_pack_complex16_complex8,
                                     int n358, complex16* input,
                                     int __len_unused60);
void __ext_v_negate_complex8(complex8* __retf_v_negate_complex8, int n362,
                             complex8* input, int __len_unused61);
void __ext_v_sign_int8(int8* __retf_v_sign_int8, int n366, int8* input1,
                       int __len_unused62, int8* input2, int __len_unused63);
int __ext_v_cast_complex8_int8(int8* output, int n371, complex8* input,
                               int n373);
void __ext_v_or(BitArrPtr __retf_v_or, int n377, BitArrPtr input1,
                int __len_unused64, BitArrPtr input2, int __len_unused65);
void __ext_v_and(BitArrPtr __retf_v_and, int n382, BitArrPtr input1,
                 int __len_unused66, BitArrPtr input2, int __len_unused67);
void __ext_v_andnot(BitArrPtr __retf_v_andnot, int n387, BitArrPtr input1,
                    int __len_unused68, BitArrPtr input2, int __len_unused69);
void __ext_v_xor(BitArrPtr __retf_v_xor, int n392, BitArrPtr input1,
                 int __len_unused70, BitArrPtr input2, int __len_unused71);
void __ext_sora_ifft(complex16* __retf_sora_ifft, int n397, complex16* inp,
                     int __len_unused72);
void __ext_sora_fft(complex16* __retf_sora_fft, int n401, complex16* inp,
                    int __len_unused73);
void __ext_sora_ifft_dynamic(complex16* __retf_sora_ifft_dynamic, int n406,
                             int16 nFFTSize, complex16* inp,
                             int __len_unused74);
void __ext_sora_fft_dynamic(complex16* __retf_sora_fft_dynamic, int n411,
                            int16 nFFTSize, complex16* inp, int __len_unused75);
int __ext_viterbi_init(int32 frame_length);
int16 __ext_viterbi_decode(int16* svalue, int __len_unused_76, int32 code_rate,
                           BitArrPtr bitValue, int __len_unused_77,
                           int16 const1, int16 const2);
int16 __ext_viterbi_decode_old(int16* intInput, int __len_unused_78,
                               int16 code_rate, int16* out,
                               int __len_unused_79);
int __ext_qpsk_demap(complex16 input_sample, int16* softBits,
                     int __len_unused_80);
int16 __ext_bpsk_demap(complex16 input_sample);
int __ext_qam16_demap(complex16 input_sample, int16* softBits,
                      int __len_unused_81);
int __ext_qam64_demap(complex16 input_sample, int16* softBits,
                      int __len_unused_82);
int __ext_viterbi_brick_init(int32 frame_length, int16 code_rate);
int __ext_viterbiSig11a_brick_init(int32 frame_length, int16 code_rate);
int16 __ext_viterbi_brick_decode(int8* svalue, int __len_unused_83,
                                 BitArrPtr bitValue, int __len_unused_84);
int __ext_viterbi_brick_init_fast(int32 frame_length, int16 code_rate,
                                  int16 depth);
int16 __ext_viterbi_brick_decode_fast(int8* svalue, int __len_unused_85,
                                      BitArrPtr bitValue, int n463);
int16 __ext_viterbiSig11a_brick_decode_fast(int8* svalue, int __len_unused_86,
                                            BitArrPtr bitValue, int n468);
int __ext_dbgplot_real_line(int16* data, int n472);
int __ext_dbgplot_real_line32(int32* data, int n476);
int __ext_dbgplot_complex_line(complex16* data, int n480, int16 type);
int __ext_dbgplot_spectrum(complex16* item, int n485);
int __ext_dbgplot_dots(complex16* data, int n489);
int __ext_dbgplot_dot(complex16 data);
int __ext_print_time();
int __ext_record_time_start();
int __ext_record_time_stop();
int __ext_populate_rand_array(BitArrPtr arr, int n502);
int auto_map__1769_ln25_93(complex16* _ret94, int __len_unused_96,
                           complex16* DD1_free_149895, int __len_unused_97);
int auto_map__1769_ln25_93(complex16* _ret94, int __len_unused_96,
                           complex16* DD1_free_149895, int __len_unused_97)
{
    __ext_sora_fft(_ret94, 64, DD1_free_149895, 64);
    return UNIT;
}
int wpl_go_aux(int initialized)
{
    unsigned int loop_counter = 0;
    calign complex16* __yv_tmp_ln23_89_buf = NULL;
    calign complex16* __yv_tmp_ln23_91_buf = NULL;
    calign complex16 __yldarr_ln23_92[64] = {{0, 0}};
    calign complex16 ret98[64] = {{0, 0}};
    unsigned int mem_idx99;
    calign int32 __global__doneVal = 0;
    char __globalWhatIs;
    
    if (!initialized) { }
    
  l_DEFAULT_LBL: 
    {
        
      l_LOOP: 
        {
            goto __par_c1_ln23_87_tick;
            
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
        ORIGIN("testFFT.blk:23:17-29");
        
      __par_c1_ln23_87_tick: 
        {
            if (buf_getarrcomplex16(params, pbuf_ctx, __yldarr_ln23_92, 64) ==
                GS_EOF)
                return 2;
            
            __yv_tmp_ln23_91_buf = __yldarr_ln23_92;
            //CONTINUE is a flag checked by the multithread version
            ;
            goto __par_c2_ln23_90_process;
        }
        
      __par_c1_ln23_87_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("../OFDM/FFT.blk:25:4-26:45");
        
      __par_c2_ln23_90_tick: 
        {
            goto __par_c1_ln23_87_tick;
        }
        
      __par_c2_ln23_90_process: 
        {
            mem_idx99 = wpl_get_free_idx(pheap_ctx);
            auto_map__1769_ln25_93(ret98, 64, __yv_tmp_ln23_91_buf, 64);
            wpl_restore_free_idx(pheap_ctx, mem_idx99);
            __yv_tmp_ln23_89_buf = ret98;
            goto __par_c2_ln23_88_process;
        }
        ORIGIN("testFFT.blk:23:17-41");
        
      __par_c2_ln23_88_tick: 
        {
            goto __par_c1_ln23_87_tick;
        }
        
      __par_c2_ln23_88_process: 
        {
            buf_putarrcomplex16(params, pbuf_ctx, __yv_tmp_ln23_89_buf, 64);
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
    init_getcomplex16(params, pbuf_ctx, pheap_ctx, sizeof(complex16));
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
