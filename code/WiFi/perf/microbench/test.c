/*
struct  CCAParams = noSamples: int32, shift: int16, energy: int32, noise: int32, maxCorr: int32
in
struct  LTECoeffs = freqCoeffs: arr[64] complex16, channelCoeffs: arr[64] complex16
in
struct  HeaderInfo = coding: int32, modulation: int32, len: int32, err: bool
in
fun sin_int16(x{_r1074} : int16) : int16
in
fun cos_int16(x{_r1077} : int16) : int16
in
fun atan2_int16(y{_r1080} : int16, x{_r1081} : int16) : int16
in
fun atan2_int32(y{_r1084} : int32, x{_r1085} : int32) : int32
in
fun sine_double_int16(x{_r1091} : int16, prec{_r1092} : int16) : int16
in
fun sine_double_int32(x{_r1095} : int32, prec{_r1096} : int32) : int32
in
fun cosine_double_int16(x{_r1099} : int16, prec{_r1100} : int16) : int16
in
fun cosine_double_int32(x{_r1103} : int32, prec{_r1104} : int32) : int32
in
fun imin_int16(x{_r1107} : int16, y{_r1108} : int16) : int16
in
fun imin_int32(x{_r1111} : int32, y{_r1112} : int32) : int32
in
fun imax_int16(x{_r1115} : int16, y{_r1116} : int16) : int16
in
fun imax_int32(x{_r1119} : int32, y{_r1120} : int32) : int32
in
fun ceil_int16(d{_r1123} : double) : int16
in
fun ceil_int32(d{_r1126} : double) : int32
in
fun round_int16(d{_r1129} : double) : int16
in
fun round_int32(d{_r1132} : double) : int32
in
fun log2(d{_r1135} : double) : double
in
fun log2_int16(d{_r1138} : int16) : int16
in
fun log2_int32(d{_r1141} : int32) : int32
in
fun sqrt(d{_r1144} : double) : double
in
fun sqrt_int16(d{_r1147} : int16) : int16
in
fun sqrt_int32(d{_r1150} : int32) : int32
in
fun sumc16(x{_r1153} : arr[4] complex16) : complex16
in
fun sumc32(x{_r1156} : arr[4] complex32) : complex32
in
fun sumi16(x{_r1159} : arr[4] int16) : int16
in
fun sumi32(x{_r1162} : arr[4] int32) : int32
in
fun v_sum_int32(x{_r1165} : arr["n1166"] int32) : int32
in
fun v_sum_int16(x{_r1169} : arr["n1170"] int16) : int16
in
fun v_sum_complex16(x{_r1173} : arr["n1174"] complex16) : complex16
in
fun v_sum_complex32(x{_r1177} : arr["n1178"] complex32) : complex32
in
fun v_hadd_int32(var z{_r1181} : arr[4] int32, x{_r1182} : arr[4] int32) : ()
in
fun v_hadd_complex16(var z{_r1185} : arr[4] complex16, x{_r1186} : arr[4] complex16) : ()
in
fun v_add_complex16(var c{_r1189} : arr["n1190"] complex16, a{_r1191} : arr["n1190"] complex16, b{_r1192} : arr["n1190"] complex16) : ()
in
fun v_add_complex32(var c{_r1195} : arr["n1196"] complex32, a{_r1197} : arr["n1196"] complex32, b{_r1198} : arr["n1196"] complex32) : ()
in
fun v_add_int16(var c{_r1201} : arr["n1202"] int16, a{_r1203} : arr["n1202"] int16, b{_r1204} : arr["n1202"] int16) : ()
in
fun v_add_int32(var c{_r1207} : arr["n1208"] int32, a{_r1209} : arr["n1208"] int32, b{_r1210} : arr["n1208"] int32) : ()
in
fun v_sub_complex16(var c{_r1213} : arr["n1214"] complex16, a{_r1215} : arr["n1214"] complex16, b{_r1216} : arr["n1214"] complex16) : ()
in
fun v_sub_complex32(var c{_r1219} : arr["n1220"] complex32, a{_r1221} : arr["n1220"] complex32, b{_r1222} : arr["n1220"] complex32) : ()
in
fun v_sub_int16(var c{_r1225} : arr["n1226"] int16, a{_r1227} : arr["n1226"] int16, b{_r1228} : arr["n1226"] int16) : ()
in
fun v_sub_int32(var c{_r1231} : arr["n1232"] int32, a{_r1233} : arr["n1232"] int32, b{_r1234} : arr["n1232"] int32) : ()
in
fun v_mul_complex16(var c{_r1237} : arr["n1238"] complex16, a{_r1239} : arr["n1238"] complex16, b{_r1240} : arr["n1238"] complex16, shift{_r1241} : int32) : ()
in
fun v_mul_complex16_int32(var re{_r1244} : arr["n1245"] int32, var im{_r1246} : arr["n1245"] int32, a{_r1247} : arr["n1245"] complex16, b{_r1248} : arr["n1245"] complex16) : ()
in
fun v_conj_mul_complex16(var c{_r1251} : arr["n1252"] complex16, a{_r1253} : arr["n1252"] complex16, b{_r1254} : arr["n1252"] complex16, shift{_r1255} : int32) : ()
in
fun v_conj_mul_complex16_int32(var re{_r1258} : arr["n1259"] int32, var im{_r1260} : arr["n1259"] int32, a{_r1261} : arr["n1259"] complex16, b{_r1262} : arr["n1259"] complex16) : ()
in
fun v_shift_right_complex32(var z{_r1265} : arr["n1266"] complex32, x{_r1267} : arr["n1266"] complex32, shift{_r1268} : int32) : ()
in
fun v_shift_right_complex16(var z{_r1271} : arr["n1272"] complex16, x{_r1273} : arr["n1272"] complex16, shift{_r1274} : int32) : ()
in
fun v_shift_right_int32(var z{_r1277} : arr["n1278"] int32, x{_r1279} : arr["n1278"] int32, shift{_r1280} : int32) : ()
in
fun v_shift_right_int16(var z{_r1283} : arr["n1284"] int16, x{_r1285} : arr["n1284"] int16, shift{_r1286} : int32) : ()
in
fun v_shift_left_complex32(var z{_r1289} : arr["n1290"] complex32, x{_r1291} : arr["n1290"] complex32, shift{_r1292} : int32) : ()
in
fun v_shift_left_complex16(var z{_r1295} : arr["n1296"] complex16, x{_r1297} : arr["n1296"] complex16, shift{_r1298} : int32) : ()
in
fun v_shift_left_int32(var z{_r1301} : arr["n1302"] int32, x{_r1303} : arr["n1302"] int32, shift{_r1304} : int32) : ()
in
fun v_shift_left_int16(var z{_r1307} : arr["n1308"] int16, x{_r1309} : arr["n1308"] int16, shift{_r1310} : int32) : ()
in
fun zero_bit(var x{_r1313} : arr["n1314"] bit) : ()
in
fun zero_complex8(var x{_r1317} : arr["n1318"] complex8) : ()
in
fun zero_complex16(var x{_r1321} : arr["n1322"] complex16) : ()
in
fun zero_complex32(var x{_r1325} : arr["n1326"] complex32) : ()
in
fun zero_int8(var x{_r1329} : arr["n1330"] int8) : ()
in
fun zero_int16(var x{_r1333} : arr["n1334"] int16) : ()
in
fun zero_int32(var x{_r1337} : arr["n1338"] int32) : ()
in
fun copy_complex8(var dst{_r1341} : arr["n1342"] complex8, src{_r1343} : arr["n1344"] complex8, len{_r1345} : int32) : ()
in
fun copy_complex16(var dst{_r1348} : arr["n1349"] complex16, src{_r1350} : arr["n1351"] complex16, len{_r1352} : int32) : ()
in
fun copy_complex32(var dst{_r1355} : arr["n1356"] complex32, src{_r1357} : arr["n1358"] complex32, len{_r1359} : int32) : ()
in
fun copy_int8(var dst{_r1362} : arr["n1363"] int8, src{_r1364} : arr["n1365"] int8, len{_r1366} : int32) : ()
in
fun copy_int16(var dst{_r1369} : arr["n1370"] int16, src{_r1371} : arr["n1372"] int16, len{_r1373} : int32) : ()
in
fun copy_int32(var dst{_r1376} : arr["n1377"] int32, src{_r1378} : arr["n1379"] int32, len{_r1380} : int32) : ()
in
fun bits_to_int8(var dst{_r1383} : arr["n1384"] int8, src{_r1385} : arr["n1386"] bit) : ()
in
fun int8_to_bits(var dst{_r1389} : arr["n1390"] bit, src{_r1391} : arr["n1392"] int8) : ()
in
fun hexprint_int8(a{_r1395} : arr["n1396"] int8, length{_r1397} : int32) : ()
in
fun v_pack_int32_complex16(var z{_r1400} : arr["n1401"] complex16, x{_r1402} : arr["n1401"] int32, y{_r1403} : arr["n1401"] int32) : ()
in
fun v_pack_complex16_complex8(input{_r1406} : arr["n1407"] complex16) : arr["n1407"] complex8
in
fun v_or(input1{_r1410} : arr["n1411"] bit, input2{_r1412} : arr["n1411"] bit) : arr["n1411"] bit
in
fun v_and(input1{_r1415} : arr["n1416"] bit, input2{_r1417} : arr["n1416"] bit) : arr["n1416"] bit
in
fun v_andnot(input1{_r1420} : arr["n1421"] bit, input2{_r1422} : arr["n1421"] bit) : arr["n1421"] bit
in
fun v_xor(input1{_r1425} : arr["n1426"] bit, input2{_r1427} : arr["n1426"] bit) : arr["n1426"] bit
in
fun v_or8(input1{_r1430} : arr[8] bit, input2{_r1431} : arr[8] bit) : arr[8] bit
in
fun v_and8(input1{_r1434} : arr[8] bit, input2{_r1435} : arr[8] bit) : arr[8] bit
in
fun v_andnot8(input1{_r1438} : arr[8] bit, input2{_r1439} : arr[8] bit) : arr[8] bit
in
fun v_xor8(input1{_r1442} : arr[8] bit, input2{_r1443} : arr[8] bit) : arr[8] bit
in
fun v_cast_complex8_int8(var output{_r1446} : arr["n1447"] int8, input{_r1448} : arr["n1449"] complex8) : ()
in
fun v_negate_complex8(input{_r1452} : arr["n1453"] complex8) : arr["n1453"] complex8
in
fun v_sign_int8(input1{_r1456} : arr["n1457"] int8, input2{_r1458} : arr["n1457"] int8) : arr["n1457"] int8
in
fun v_downsample_complex16(input{_r1461} : arr["n1462"] complex16) : arr["n1462"] complex16
in
fun sora_ifft(inp{_r1465} : arr["n1466"] complex16) : arr["n1466"] complex16
in
fun sora_fft(inp{_r1469} : arr["n1470"] complex16) : arr["n1470"] complex16
in
fun sora_ifft_dynamic(nFFTSize{_r1473} : int16, inp{_r1474} : arr["n1475"] complex16) : arr["n1475"] complex16
in
fun sora_fft_dynamic(nFFTSize{_r1478} : int16, inp{_r1479} : arr["n1480"] complex16) : arr["n1480"] complex16
in
fun viterbi_brick_init_fast(frame_length{_r1483} : int32, code_rate{_r1484} : int16, depth{_r1485} : int16) : ()
in
fun viterbi_brick_decode_fast(svalue{_r1488} : arr[48] int8, bitValue{_r1489} : arr["n1490"] bit) : int16
in
fun viterbiSig11a_brick_decode_fast(svalue{_r1493} : arr[48] int8, bitValue{_r1494} : arr["n1495"] bit) : int16
in
fun dbgplot_real_line(data{_r1498} : arr["n1499"] int16) : ()
in
fun dbgplot_real_line32(data{_r1502} : arr["n1503"] int32) : ()
in
fun dbgplot_complex_line(data{_r1506} : arr["n1507"] complex16, type{_r1508} : int16) : ()
in
fun dbgplot_spectrum(item{_r1511} : arr["n1512"] complex16) : ()
in
fun dbgplot_dots(data{_r1515} : arr["n1516"] complex16) : ()
in
fun dbgplot_dot(data{_r1519} : complex16) : ()
in
fun print_time() : ()
in
fun record_time_start() : ()
in
fun record_time_stop() : ()
in
fun populate_rand_array(var a{_r1528} : arr["n1529"] bit) : ()
in
fun do_not_inline_int8(x{_r1532} : int8) : int8
in
fun do_not_inline_int16(x{_r1535} : int16) : int16
in
fun do_not_inline_int32(x{_r1538} : int32) : int32
in
fun do_not_inline_int64(x{_r1541} : int64) : int64
in
fun do_not_inline_complex8(x{_r1544} : complex8) : complex8
in
fun do_not_inline_complex16(x{_r1547} : complex16) : complex16
in
fun do_not_inline_complex32(x{_r1550} : complex32) : complex32
in
fun do_not_inline_complex64(x{_r1553} : complex64) : complex64
in
(((read[arr[64] complex16] >>>
   fun auto_map__5298{_pf5298}(DD1_free_3533{DD1_free_3533} : arr[64] complex16) =
         sora_fft{_r1467}(DD1_free_3533{DD1_free_3533})
   in
   map auto_map__5298{_pf5298}) >>>
  fun auto_map__5299{_pf5299}(DD1_free_4002{DD1_free_4002} : arr[64] complex16) =
        sora_fft{_r1467}(DD1_free_4002{DD1_free_4002})
  in
  map auto_map__5299{_pf5299}) |>>>|
 ((fun auto_map__5300{_pf5300}(DD1_free_4471{DD1_free_4471} : arr[64] complex16) =
         sora_fft{_r1467}(DD1_free_4471{DD1_free_4471})
   in
   map auto_map__5300{_pf5300} >>>
   fun auto_map__5301{_pf5301}(DD1_free_4940{DD1_free_4940} : arr[64] complex16) =
         sora_fft{_r1467}(DD1_free_4940{DD1_free_4940})
   in
   map auto_map__5301{_pf5301}) >>>
  write[arr[64] complex16]))

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
long volatile* barr_hist1 = NULL;
long volatile* barr_hist2 = NULL;
bool atomix = 1;
#ifndef STRUCT_HEADERINFO
#define STRUCT_HEADERINFO
typedef struct {
    int32 coding;
    int32 modulation;
    int32 len;
    unsigned char err;
} HeaderInfo;
#endif
#ifndef STRUCT_LTECOEFFS
#define STRUCT_LTECOEFFS
typedef struct {
    complex16 freqCoeffs[64];
    complex16 channelCoeffs[64];
} LTECoeffs;
#endif
#ifndef STRUCT_CCAPARAMS
#define STRUCT_CCAPARAMS
typedef struct {
    int32 noSamples;
    int16 shift;
    int32 energy;
    int32 noise;
    int32 maxCorr;
} CCAParams;
#endif
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
complex16 __ext_sumc16(complex16* x, int __len_unused_327);
complex32 __ext_sumc32(complex32* x, int __len_unused_328);
int16 __ext_sumi16(int16* x, int __len_unused_329);
int32 __ext_sumi32(int32* x, int __len_unused_330);
int32 __ext_v_sum_int32(int32* x, int n1166);
int16 __ext_v_sum_int16(int16* x, int n1170);
complex16 __ext_v_sum_complex16(complex16* x, int n1174);
complex32 __ext_v_sum_complex32(complex32* x, int n1178);
int __ext_v_hadd_int32(int32* z, int __len_unused_331, int32* x,
                       int __len_unused_332);
int __ext_v_hadd_complex16(complex16* z, int __len_unused_333, complex16* x,
                           int __len_unused_334);
int __ext_v_add_complex16(complex16* c, int n1190, complex16* a,
                          int __len_unused335, complex16* b,
                          int __len_unused336);
int __ext_v_add_complex32(complex32* c, int n1196, complex32* a,
                          int __len_unused337, complex32* b,
                          int __len_unused338);
int __ext_v_add_int16(int16* c, int n1202, int16* a, int __len_unused339,
                      int16* b, int __len_unused340);
int __ext_v_add_int32(int32* c, int n1208, int32* a, int __len_unused341,
                      int32* b, int __len_unused342);
int __ext_v_sub_complex16(complex16* c, int n1214, complex16* a,
                          int __len_unused343, complex16* b,
                          int __len_unused344);
int __ext_v_sub_complex32(complex32* c, int n1220, complex32* a,
                          int __len_unused345, complex32* b,
                          int __len_unused346);
int __ext_v_sub_int16(int16* c, int n1226, int16* a, int __len_unused347,
                      int16* b, int __len_unused348);
int __ext_v_sub_int32(int32* c, int n1232, int32* a, int __len_unused349,
                      int32* b, int __len_unused350);
int __ext_v_mul_complex16(complex16* c, int n1238, complex16* a,
                          int __len_unused351, complex16* b,
                          int __len_unused352, int32 shift);
int __ext_v_mul_complex16_int32(int32* re, int n1245, int32* im,
                                int __len_unused353, complex16* a,
                                int __len_unused354, complex16* b,
                                int __len_unused355);
int __ext_v_conj_mul_complex16(complex16* c, int n1252, complex16* a,
                               int __len_unused356, complex16* b,
                               int __len_unused357, int32 shift);
int __ext_v_conj_mul_complex16_int32(int32* re, int n1259, int32* im,
                                     int __len_unused358, complex16* a,
                                     int __len_unused359, complex16* b,
                                     int __len_unused360);
int __ext_v_shift_right_complex32(complex32* z, int n1266, complex32* x,
                                  int __len_unused361, int32 shift);
int __ext_v_shift_right_complex16(complex16* z, int n1272, complex16* x,
                                  int __len_unused362, int32 shift);
int __ext_v_shift_right_int32(int32* z, int n1278, int32* x,
                              int __len_unused363, int32 shift);
int __ext_v_shift_right_int16(int16* z, int n1284, int16* x,
                              int __len_unused364, int32 shift);
int __ext_v_shift_left_complex32(complex32* z, int n1290, complex32* x,
                                 int __len_unused365, int32 shift);
int __ext_v_shift_left_complex16(complex16* z, int n1296, complex16* x,
                                 int __len_unused366, int32 shift);
int __ext_v_shift_left_int32(int32* z, int n1302, int32* x, int __len_unused367,
                             int32 shift);
int __ext_v_shift_left_int16(int16* z, int n1308, int16* x, int __len_unused368,
                             int32 shift);
int __ext_zero_bit(BitArrPtr x, int n1314);
int __ext_zero_complex8(complex8* x, int n1318);
int __ext_zero_complex16(complex16* x, int n1322);
int __ext_zero_complex32(complex32* x, int n1326);
int __ext_zero_int8(int8* x, int n1330);
int __ext_zero_int16(int16* x, int n1334);
int __ext_zero_int32(int32* x, int n1338);
int __ext_copy_complex8(complex8* dst, int n1342, complex8* src, int n1344,
                        int32 len);
int __ext_copy_complex16(complex16* dst, int n1349, complex16* src, int n1351,
                         int32 len);
int __ext_copy_complex32(complex32* dst, int n1356, complex32* src, int n1358,
                         int32 len);
int __ext_copy_int8(int8* dst, int n1363, int8* src, int n1365, int32 len);
int __ext_copy_int16(int16* dst, int n1370, int16* src, int n1372, int32 len);
int __ext_copy_int32(int32* dst, int n1377, int32* src, int n1379, int32 len);
int __ext_bits_to_int8(int8* dst, int n1384, BitArrPtr src, int n1386);
int __ext_int8_to_bits(BitArrPtr dst, int n1390, int8* src, int n1392);
int __ext_hexprint_int8(int8* a, int n1396, int32 length);
int __ext_v_pack_int32_complex16(complex16* z, int n1401, int32* x,
                                 int __len_unused369, int32* y,
                                 int __len_unused370);
void __ext_v_pack_complex16_complex8(complex8* __retf_v_pack_complex16_complex8,
                                     int n1407, complex16* input,
                                     int __len_unused371);
void __ext_v_or(BitArrPtr __retf_v_or, int n1411, BitArrPtr input1,
                int __len_unused372, BitArrPtr input2, int __len_unused373);
void __ext_v_and(BitArrPtr __retf_v_and, int n1416, BitArrPtr input1,
                 int __len_unused374, BitArrPtr input2, int __len_unused375);
void __ext_v_andnot(BitArrPtr __retf_v_andnot, int n1421, BitArrPtr input1,
                    int __len_unused376, BitArrPtr input2, int __len_unused377);
void __ext_v_xor(BitArrPtr __retf_v_xor, int n1426, BitArrPtr input1,
                 int __len_unused378, BitArrPtr input2, int __len_unused379);
void __ext_v_or8(BitArrPtr __retf_v_or8, int __len_unused_380, BitArrPtr input1,
                 int __len_unused_381, BitArrPtr input2, int __len_unused_382);
void __ext_v_and8(BitArrPtr __retf_v_and8, int __len_unused_383,
                  BitArrPtr input1, int __len_unused_384, BitArrPtr input2,
                  int __len_unused_385);
void __ext_v_andnot8(BitArrPtr __retf_v_andnot8, int __len_unused_386,
                     BitArrPtr input1, int __len_unused_387, BitArrPtr input2,
                     int __len_unused_388);
void __ext_v_xor8(BitArrPtr __retf_v_xor8, int __len_unused_389,
                  BitArrPtr input1, int __len_unused_390, BitArrPtr input2,
                  int __len_unused_391);
int __ext_v_cast_complex8_int8(int8* output, int n1447, complex8* input,
                               int n1449);
void __ext_v_negate_complex8(complex8* __retf_v_negate_complex8, int n1453,
                             complex8* input, int __len_unused392);
void __ext_v_sign_int8(int8* __retf_v_sign_int8, int n1457, int8* input1,
                       int __len_unused393, int8* input2, int __len_unused394);
void __ext_v_downsample_complex16(complex16* __retf_v_downsample_complex16,
                                  int n1462, complex16* input,
                                  int __len_unused395);
void __ext_sora_ifft(complex16* __retf_sora_ifft, int n1466, complex16* inp,
                     int __len_unused396);
void __ext_sora_fft(complex16* __retf_sora_fft, int n1470, complex16* inp,
                    int __len_unused397);
void __ext_sora_ifft_dynamic(complex16* __retf_sora_ifft_dynamic, int n1475,
                             int16 nFFTSize, complex16* inp,
                             int __len_unused398);
void __ext_sora_fft_dynamic(complex16* __retf_sora_fft_dynamic, int n1480,
                            int16 nFFTSize, complex16* inp,
                            int __len_unused399);
int __ext_viterbi_brick_init_fast(int32 frame_length, int16 code_rate,
                                  int16 depth);
int16 __ext_viterbi_brick_decode_fast(int8* svalue, int __len_unused_400,
                                      BitArrPtr bitValue, int n1490);
int16 __ext_viterbiSig11a_brick_decode_fast(int8* svalue, int __len_unused_401,
                                            BitArrPtr bitValue, int n1495);
int __ext_dbgplot_real_line(int16* data, int n1499);
int __ext_dbgplot_real_line32(int32* data, int n1503);
int __ext_dbgplot_complex_line(complex16* data, int n1507, int16 type);
int __ext_dbgplot_spectrum(complex16* item, int n1512);
int __ext_dbgplot_dots(complex16* data, int n1516);
int __ext_dbgplot_dot(complex16 data);
int __ext_print_time();
int __ext_record_time_start();
int __ext_record_time_stop();
int __ext_populate_rand_array(BitArrPtr a, int n1529);
int8 __ext_do_not_inline_int8(int8 x);
int16 __ext_do_not_inline_int16(int16 x);
int32 __ext_do_not_inline_int32(int32 x);
int64 __ext_do_not_inline_int64(int64 x);
complex8 __ext_do_not_inline_complex8(complex8 x);
complex16 __ext_do_not_inline_complex16(complex16 x);
complex32 __ext_do_not_inline_complex32(complex32 x);
complex64 __ext_do_not_inline_complex64(complex64 x);
int auto_map__5298_ln35_402(complex16* _ret403, int __len_unused_405,
                            complex16* DD1_free_3533404, int __len_unused_406);
int auto_map__5298_ln35_402(complex16* _ret403, int __len_unused_405,
                            complex16* DD1_free_3533404, int __len_unused_406)
{
    __ext_sora_fft(_ret403, 64, DD1_free_3533404, 64);
    return UNIT;
}
int auto_map__5299_ln35_407(complex16* _ret408, int __len_unused_410,
                            complex16* DD1_free_4002409, int __len_unused_411);
int auto_map__5299_ln35_407(complex16* _ret408, int __len_unused_410,
                            complex16* DD1_free_4002409, int __len_unused_411)
{
    __ext_sora_fft(_ret408, 64, DD1_free_4002409, 64);
    return UNIT;
}
int auto_map__5300_ln35_412(complex16* _ret413, int __len_unused_415,
                            complex16* DD1_free_4471414, int __len_unused_416);
int auto_map__5300_ln35_412(complex16* _ret413, int __len_unused_415,
                            complex16* DD1_free_4471414, int __len_unused_416)
{
    __ext_sora_fft(_ret413, 64, DD1_free_4471414, 64);
    return UNIT;
}
int auto_map__5301_ln35_417(complex16* _ret418, int __len_unused_420,
                            complex16* DD1_free_4940419, int __len_unused_421);
int auto_map__5301_ln35_417(complex16* _ret418, int __len_unused_420,
                            complex16* DD1_free_4940419, int __len_unused_421)
{
    __ext_sora_fft(_ret418, 64, DD1_free_4940419, 64);
    return UNIT;
}
calign complex16 auto_map__5298_inln_422[64] = {{0, 0}};
calign complex16 auto_map__5299_inln_423[64] = {{0, 0}};
calign complex16 auto_map__5300_inln_424[64] = {{0, 0}};
calign complex16 auto_map__5301_inln_425[64] = {{0, 0}};
calign complex16 srcln_426[64] = {{0, 0}};
calign complex16 snkln_427[64] = {{0, 0}};
queue* sing_queue_arr;
size_t my_sizes_sing[2];
int my_slots_sing[2];
ts_context* sora_queue_arr;
size_t my_sizes_sora[1];
int my_slots_sora[1];
static ts_context* ptr_q_0;
static queue* ptr_q_1;
static queue* ptr_q_2;
FINL int aexp__mbenchutils_blk_35_5_18_319();
FINL int aexp__mbenchutils_blk_35_5_18_319()

{
    complex16* _qptr_428;
    complex16* _qptr_429;
    
    _qptr_428 = (complex16*) ts_acquire(ptr_q_0);
    _qptr_429 = (complex16*) stq_reserve(ptr_q_2);
    auto_map__5300_ln35_412(_qptr_429, 64, _qptr_428, 64);
    stq_push(ptr_q_2);
    ts_release(ptr_q_0);
    return UNIT;
}
FINL int aexp__mbenchutils_blk_35_5_18_315();
FINL int aexp__mbenchutils_blk_35_5_18_315()

{
    complex16* _qptr_430;
    
    if (buf_getarrcomplex16(params, pbuf_ctx, srcln_426, 64) == GS_EOF)
        return -7;
    
    _qptr_430 = (complex16*) stq_reserve(ptr_q_1);
    auto_map__5298_ln35_402(_qptr_430, 64, srcln_426, 64);
    stq_push(ptr_q_1);
    return UNIT;
}
FINL int aexp__mbenchutils_blk_35_5_18_317();
FINL int aexp__mbenchutils_blk_35_5_18_317()

{
    complex16* _qptr_431;
    complex16* _qptr_432;
    
    _qptr_431 = (complex16*) stq_acquire(ptr_q_1);
    _qptr_432 = (complex16*) ts_reserve(ptr_q_0);
    auto_map__5299_ln35_407(_qptr_432, 64, _qptr_431, 64);
    ts_push(ptr_q_0);
    stq_release(ptr_q_1);
    return UNIT;
}
FINL int aexp__mbenchutils_blk_35_5_18_321();
FINL int aexp__mbenchutils_blk_35_5_18_321()

{
    complex16* _qptr_433;
    
    _qptr_433 = (complex16*) stq_acquire(ptr_q_2);
    auto_map__5301_ln35_417(snkln_427, 64, _qptr_433, 64);
    buf_putarrcomplex16(params, pbuf_ctx, snkln_427, 64);
    stq_release(ptr_q_2);
    return UNIT;
}
long volatile __barr_BLOCK_0[3] = {0, 0, 0};
long volatile __barr2_BLOCK_0[3] = {0, 0, 0};
long volatile __barr_BLOCK_2[3] = {0, 0, 0};
long volatile __barr2_BLOCK_2[3] = {0, 0, 0};
extern bool atomix;
int wpl_gothread0()

{
    // Ask driver.c to look for atomix threads
    atomix = 1;
    
    unsigned int mem_idx434;
    
    goto BLOCK_2;
    
  BLOCK_0: 
    ORIGIN("");
    mem_idx434 = wpl_get_free_idx(pheap_ctx);
    aexp__mbenchutils_blk_35_5_18_319();
    UNIT;
    UNIT;
    aexp__mbenchutils_blk_35_5_18_321();
    wpl_restore_free_idx(pheap_ctx, mem_idx434);
    goto BLOCK_0;
    
  BLOCK_2: 
    ORIGIN("");
    mem_idx434 = wpl_get_free_idx(pheap_ctx);
    if (aexp__mbenchutils_blk_35_5_18_315() == -7)
        return 0;
    
    aexp__mbenchutils_blk_35_5_18_317();
    if (aexp__mbenchutils_blk_35_5_18_315() == -7)
        return 0;
    
    aexp__mbenchutils_blk_35_5_18_317();
    wpl_restore_free_idx(pheap_ctx, mem_idx434);
    goto BLOCK_0;
    exit(2);
}
extern bool atomix;
int wpl_gothread1()

{
    // Ask driver.c to look for atomix threads
    atomix = 1;
    
    unsigned int mem_idx435;
    
    goto BLOCK_2;
    
  BLOCK_0: 
    ORIGIN("");
    mem_idx435 = wpl_get_free_idx(pheap_ctx);
    UNIT;
    if (aexp__mbenchutils_blk_35_5_18_315() == -7)
        return 0;
    
    aexp__mbenchutils_blk_35_5_18_317();
    UNIT;
    wpl_restore_free_idx(pheap_ctx, mem_idx435);
    goto BLOCK_0;
    
  BLOCK_2: 
    ORIGIN("");
    mem_idx435 = wpl_get_free_idx(pheap_ctx);
    UNIT;
    UNIT;
    UNIT;
    UNIT;
    wpl_restore_free_idx(pheap_ctx, mem_idx435);
    goto BLOCK_0;
    exit(2);
}
int wpl_go()

{
    exit(-1);
}
DWORD __gothread1(void* pParam)
{
    thread_info * ti;
    ti = (thread_info*) pParam;
    printf("Standalone thread (%s) ID: %u\n", "1", GetCurrentThreadId());
    wpl_gothread1();
    ti->fRunning = 0;
    return 0;
}
DWORD __gothread0(void* pParam)
{
    thread_info * ti;
    ti = (thread_info*) pParam;
    printf("Standalone thread (%s) ID: %u\n", "0", GetCurrentThreadId());
    wpl_gothread0();
    ti->fRunning = 0;
    return 0;
}
int wpl_set_up_threads(PWIN_UTHREAD_PROC* User_Routines)
{
    User_Routines[1] = __gothread1;
    User_Routines[0] = __gothread0;
    return 2;
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
    my_sizes_sing[0] = 64 * sizeof(complex16);
    my_slots_sing[0] = 1;
    my_sizes_sing[1] = 64 * sizeof(complex16);
    my_slots_sing[1] = 1;
    sing_queue_arr = stq_init(2, my_sizes_sing, my_slots_sing);
    my_sizes_sora[0] = 64 * sizeof(complex16);
    my_slots_sora[0] = 2;
    sora_queue_arr = ts_init(1, my_sizes_sora, my_slots_sora);
    ptr_q_0 = &sora_queue_arr[0];
    ptr_q_1 = &sing_queue_arr[0];
    ptr_q_2 = &sing_queue_arr[1];
}
