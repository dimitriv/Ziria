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
struct  CCAParams = noSamples: int32, shift: int16, energy: int32, noise: int32, maxCorr: int32
in
struct  LTECoeffs = freqCoeffs: arr[64] complex16, channelCoeffs: arr[64] complex16
in
struct  HeaderInfo = coding: int32, modulation: int32, len: int32, err: bool
in
fun parsePLCPHeader{_r1670}(hdata{_r1672} : arr[24] bit) =
      var b{_r1673} : int32 in
      var h{_r1674} : HeaderInfo in
      var p{_r1675} : bit in
      if hdata{_r1672}[0]=='1&&hdata{_r1672}[1]=='1&&hdata{_r1672}[2]=='0&&hdata{_r1672}[3]=='1 {
        h{_r1674}.modulation := -- (easgn)
          0;
        h{_r1674}.coding := -- (easgn)
          0
      } else {
        if hdata{_r1672}[0]=='1&&hdata{_r1672}[1]=='1&&hdata{_r1672}[2]=='1&&hdata{_r1672}[3]=='1 {
          h{_r1674}.modulation := -- (easgn)
            0;
          h{_r1674}.coding := -- (easgn)
            2
        } else {
          if hdata{_r1672}[0]=='0&&hdata{_r1672}[1]=='1&&hdata{_r1672}[2]=='0&&hdata{_r1672}[3]=='1 {
            h{_r1674}.modulation := -- (easgn)
              1;
            h{_r1674}.coding := -- (easgn)
              0
          } else {
            if hdata{_r1672}[0]=='0&&hdata{_r1672}[1]=='1&&hdata{_r1672}[2]=='1&&hdata{_r1672}[3]=='1 {
              h{_r1674}.modulation := -- (easgn)
                1;
              h{_r1674}.coding := -- (easgn)
                2
            } else {
              if hdata{_r1672}[0]=='1&&hdata{_r1672}[1]=='0&&hdata{_r1672}[2]=='0&&hdata{_r1672}[3]=='1 {
                h{_r1674}.modulation := -- (easgn)
                  2;
                h{_r1674}.coding := -- (easgn)
                  0
              } else {
                if hdata{_r1672}[0]=='1&&hdata{_r1672}[1]=='0&&hdata{_r1672}[2]=='1&&hdata{_r1672}[3]=='1 {
                  h{_r1674}.modulation := -- (easgn)
                    2;
                  h{_r1674}.coding := -- (easgn)
                    2
                } else {
                  if hdata{_r1672}[0]=='0&&hdata{_r1672}[1]=='0&&hdata{_r1672}[2]=='0&&hdata{_r1672}[3]=='1 {
                    h{_r1674}.modulation := -- (easgn)
                      3;
                    h{_r1674}.coding := -- (easgn)
                      1
                  } else {
                    if hdata{_r1672}[0]=='0&&hdata{_r1672}[1]=='0&&hdata{_r1672}[2]=='1&&hdata{_r1672}[3]=='1 {
                      h{_r1674}.modulation := -- (easgn)
                        3;
                      h{_r1674}.coding := -- (easgn)
                        2
                    } else {
                      h{_r1674}.modulation := -- (easgn)
                        0;
                      h{_r1674}.coding := -- (easgn)
                        0
                    }
                  }
                }
              }
            }
          }
        }
      };
      b{_r1673} := -- (easgn)
        1;
      h{_r1674}.len := -- (easgn)
        0;
      for j{_r1744} in [5, 12] {
        if hdata{_r1672}[j{_r1744}]=='1 {
          h{_r1674}.len := -- (easgn)
            h{_r1674}.len+b{_r1673}
        } else {
          tt
        };
        b{_r1673} := -- (easgn)
          2*b{_r1673}
      };
      h{_r1674}.err := -- (easgn)
        false;
      if h{_r1674}.len>2048 {
        h{_r1674}.err := -- (easgn)
          true;
        h{_r1674}.len := -- (easgn)
          2048
      } else {
        tt
      };
      p{_r1675} := -- (easgn)
        '0;
      for j{_r1764} in [0, 8] {
        p{_r1675} := -- (easgn)
          p{_r1675}^hdata{_r1672}[0:+8][j{_r1764}]
      };
      for j{_r1764} in [0, 8] {
        p{_r1675} := -- (easgn)
          p{_r1675}^hdata{_r1672}[8:+8][j{_r1764}]
      };
      for j{_r1764} in [0, 8] {
        p{_r1675} := -- (easgn)
          p{_r1675}^hdata{_r1672}[16:+8][j{_r1764}]
      };
      if p{_r1675}!='0 {
        h{_r1674}.err := -- (easgn)
          true
      } else {
        tt
      };
      p{_r1675} := -- (easgn)
        '0;
      for j{_r1775} in [2, 6] {
        p{_r1675} := -- (easgn)
          p{_r1675}|hdata{_r1672}[16:+8][j{_r1775}]
      };
      if p{_r1675}!='0 {
        h{_r1674}.err := -- (easgn)
          true
      } else {
        tt
      };
      if not(h{_r1674}.err) {
        print Header - modulation: ;
        if h{_r1674}.modulation==0 {
          print BPSK
        } else {
          if h{_r1674}.modulation==1 {
            print M_QPSK
          } else {
            if h{_r1674}.modulation==2 {
              print 16QAM
            } else {
              print 64QAM
            }
          }
        };
        print , coding: ;
        if h{_r1674}.coding==0 {
          print 1/2
        } else {
          if h{_r1674}.coding==1 {
            print 2/3
          } else {
            print 3/4
          }
        };
        println , length: , h{_r1674}.len,  B;
        println Header bits: , hdata{_r1672}
      } else {
        tt
      };
      h{_r1674}
in
fun perm_orig{_r1914}(p{_r1916} : arr["n1917"] int32, iarr{_r1918} : arr["n1919"] bit) =
      var oarr{_r1920} : arr["n1917"] bit in
      var oarrtmp{_r1921} : arr["n1917"] bit in
      zero_bit{_r262}(oarr{_r1920});
      unroll for j{_r1924} in [0, length(p{_r1916})/8] {
               let p1{_r1927} : arr[8] int32 =
                 p{_r1916}[j{_r1924}*8:+8]
               in
               let iarr{_r1907} : arr[8] bit =
                 iarr{_r1918}[j{_r1924}*8:+8]
               in
               for i{_r1912} in [0, 8] {
                 oarr{_r1920}[p1{_r1927}[i{_r1912}]] := -- (earrwrite) 
                   iarr{_r1907}[i{_r1912}]
               }
             };
      oarr{_r1920}
in
((read[int32] >>>
  seq { _unused_3381{_pf3381} <- emit var ya_free_4111{ya_free_4111} : arr[24] bit in
                                      ya_free_4111{ya_free_4111}[0] := -- (earrwrite) 
                                        '1;
                                      ya_free_4111{ya_free_4111}[1] := -- (earrwrite) 
                                        '0;
                                      ya_free_4111{ya_free_4111}[2] := -- (earrwrite) 
                                        '1;
                                      ya_free_4111{ya_free_4111}[3] := -- (earrwrite) 
                                        '1;
                                      ya_free_4111{ya_free_4111}[4] := -- (earrwrite) 
                                        '0;
                                      var ai{_r1592} : arr[2] int8 in
                                      ai{_r1592}[0] := -- (earrwrite) 
                                        100;
                                      ai{_r1592}[1] := -- (earrwrite) 
                                        0;
                                      var ab{_r1593} : arr[16] bit in
                                      var p{_r1594} : bit in
                                      int8_to_bits{_r338}(ab{_r1593}, ai{_r1592});
                                      ya_free_4111{ya_free_4111}[5:+12] := -- (earrwrite) 
                                        ab{_r1593}[0:+12];
                                      ya_free_4111{ya_free_4111}[17:+7] := -- (earrwrite) 
                                        {'0, '0, '0, '0, '0, '0, '0};
                                      p{_r1594} := -- (easgn)
                                        '0;
                                      for j{_r1666} in [0, 8] {
                                        p{_r1594} := -- (easgn)
                                          p{_r1594}&ya_free_4111{ya_free_4111}[0:+8][j{_r1666}]
                                      };
                                      for j{_r1666} in [0, 8] {
                                        p{_r1594} := -- (easgn)
                                          p{_r1594}&ya_free_4111{ya_free_4111}[8:+8][j{_r1666}]
                                      };
                                      for j{_r1666} in [0, 8] {
                                        p{_r1594} := -- (easgn)
                                          p{_r1594}&ya_free_4111{ya_free_4111}[16:+8][j{_r1666}]
                                      };
                                      ya_free_4111{ya_free_4111}[17] := -- (earrwrite) 
                                        ya_free_4111{ya_free_4111}[17]^p{_r1594};
                                      tt;
                                      ya_free_4111{ya_free_4111};
        _unused_3380{_pf3380} <- ((var ya_free_4125{ya_free_4125} : arr[24] bit in
                                   let _unused_24048{_pf24048} : () =
                                       ya_free_4125{ya_free_4125}[0] := -- (earrwrite) 
                                         '1;
                                       ya_free_4125{ya_free_4125}[1] := -- (earrwrite) 
                                         '0;
                                       ya_free_4125{ya_free_4125}[2] := -- (earrwrite) 
                                         '1;
                                       ya_free_4125{ya_free_4125}[3] := -- (earrwrite) 
                                         '1;
                                       ya_free_4125{ya_free_4125}[4] := -- (earrwrite) 
                                         '0;
                                       var ai{_r1592} : arr[2] int8 in
                                       ai{_r1592}[0] := -- (earrwrite) 
                                         100;
                                       ai{_r1592}[1] := -- (earrwrite) 
                                         0;
                                       var ab{_r1593} : arr[16] bit in
                                       var p{_r1594} : bit in
                                       int8_to_bits{_r338}(ab{_r1593}, ai{_r1592});
                                       ya_free_4125{ya_free_4125}[5:+12] := -- (earrwrite) 
                                         ab{_r1593}[0:+12];
                                       ya_free_4125{ya_free_4125}[17:+7] := -- (earrwrite) 
                                         {'0, '0, '0, '0, '0, '0, '0};
                                       p{_r1594} := -- (easgn)
                                         '0;
                                       for j{_r1666} in [0, 8] {
                                         p{_r1594} := -- (easgn)
                                           p{_r1594}&ya_free_4125{ya_free_4125}[0:+8][j{_r1666}]
                                       };
                                       for j{_r1666} in [0, 8] {
                                         p{_r1594} := -- (easgn)
                                           p{_r1594}&ya_free_4125{ya_free_4125}[8:+8][j{_r1666}]
                                       };
                                       for j{_r1666} in [0, 8] {
                                         p{_r1594} := -- (easgn)
                                           p{_r1594}&ya_free_4125{ya_free_4125}[16:+8][j{_r1666}]
                                       };
                                       ya_free_4125{ya_free_4125}[17] := -- (earrwrite) 
                                         ya_free_4125{ya_free_4125}[17]^p{_r1594};
                                       tt
                                   in
                                   for idx_free_4126{idx_free_4126} in [0, 4]
                                     emit ya_free_4125{ya_free_4125}[idx_free_4126{idx_free_4126}*6:+6] >>>
                                   var s{_r1876} : arr[6] bit :=
                                       {'0, '0, '0, '0, '0, '0} in
                                   var x{_r1877} : bit in
                                   var oA{_r1878} : bit in
                                   var oB{_r1879} : bit in
                                   repeat seq { UD2_free_11865{UD2_free_11865} <- take[arr[6] bit];
                                                var ya_free_11866{ya_free_11866} : arr[12] bit in
                                                let _unused_24049{_pf24049} : () =
                                                    ya_free_11866{ya_free_11866}[0:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[0]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[0];
                                                      r{_r1883};
                                                    ya_free_11866{ya_free_11866}[2:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[1]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[1]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[1];
                                                      r{_r1883};
                                                    ya_free_11866{ya_free_11866}[4:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[2]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[2]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[2];
                                                      r{_r1883};
                                                    ya_free_11866{ya_free_11866}[6:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[3]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[3]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[3];
                                                      r{_r1883};
                                                    ya_free_11866{ya_free_11866}[8:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[4]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[4]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[4];
                                                      r{_r1883};
                                                    ya_free_11866{ya_free_11866}[10:+2] := -- (earrwrite) 
                                                      var r{_r1883} : arr[2] bit in
                                                      r{_r1883}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[5]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                                                      r{_r1883}[1] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[5]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                                                      s{_r1876}[1:+5] := -- (earrwrite) 
                                                        s{_r1876}[0:+5];
                                                      s{_r1876}[0] := -- (earrwrite) 
                                                        UD2_free_11865{UD2_free_11865}[5];
                                                      r{_r1883}
                                                in
                                                for idx_free_11868{idx_free_11868} in [0, 12]
                                                  emit ya_free_11866{ya_free_11866}[idx_free_11868{idx_free_11868}]
                                              }) .>>>.
                                  1-mitigate(MFoMAoMA)[bit]-24);
        (((var ya_free_13309{ya_free_13309} : arr[24] bit in
           let _unused_24050{_pf24050} : () =
               ya_free_13309{ya_free_13309}[0] := -- (earrwrite) 
                 '1;
               ya_free_13309{ya_free_13309}[1] := -- (earrwrite) 
                 '0;
               ya_free_13309{ya_free_13309}[2] := -- (earrwrite) 
                 '1;
               ya_free_13309{ya_free_13309}[3] := -- (earrwrite) 
                 '1;
               ya_free_13309{ya_free_13309}[4] := -- (earrwrite) 
                 '0;
               var ai{_r1592} : arr[2] int8 in
               ai{_r1592}[0] := -- (earrwrite) 
                 100;
               ai{_r1592}[1] := -- (earrwrite) 
                 0;
               var ab{_r1593} : arr[16] bit in
               var p{_r1594} : bit in
               int8_to_bits{_r338}(ab{_r1593}, ai{_r1592});
               ya_free_13309{ya_free_13309}[5:+12] := -- (earrwrite) 
                 ab{_r1593}[0:+12];
               ya_free_13309{ya_free_13309}[17:+7] := -- (earrwrite) 
                 {'0, '0, '0, '0, '0, '0, '0};
               p{_r1594} := -- (easgn)
                 '0;
               for j{_r1666} in [0, 8] {
                 p{_r1594} := -- (easgn)
                   p{_r1594}&ya_free_13309{ya_free_13309}[0:+8][j{_r1666}]
               };
               for j{_r1666} in [0, 8] {
                 p{_r1594} := -- (easgn)
                   p{_r1594}&ya_free_13309{ya_free_13309}[8:+8][j{_r1666}]
               };
               for j{_r1666} in [0, 8] {
                 p{_r1594} := -- (easgn)
                   p{_r1594}&ya_free_13309{ya_free_13309}[16:+8][j{_r1666}]
               };
               ya_free_13309{ya_free_13309}[17] := -- (earrwrite) 
                 ya_free_13309{ya_free_13309}[17]^p{_r1594};
               tt
           in
           for idx_free_13310{idx_free_13310} in [0, 3]
             emit ya_free_13309{ya_free_13309}[idx_free_13310{idx_free_13310}*8:+8] >>>
           var s{_r1876} : arr[6] bit :=
               {'0, '0, '0, '0, '0, '0} in
           var x{_r1877} : bit in
           var oA{_r1878} : bit in
           var oB{_r1879} : bit in
           fun auto_map__24622{_pf24622}(UD1_free_13407{UD1_free_13407} : arr[8] bit) =
                 var ya_free_13409{ya_free_13409} : arr[16] bit in
                 for idx_free_13410{idx_free_13410} in [0, 8] {
                   ya_free_13409{ya_free_13409}[idx_free_13410{idx_free_13410}*2:+2] := -- (earrwrite) 
                     var r{_r1883} : arr[2] bit in
                     r{_r1883}[0] := -- (earrwrite) 
                       UD1_free_13407{UD1_free_13407}[idx_free_13410{idx_free_13410}]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[4]^s{_r1876}[5];
                     r{_r1883}[1] := -- (earrwrite) 
                       UD1_free_13407{UD1_free_13407}[idx_free_13410{idx_free_13410}]^s{_r1876}[0]^s{_r1876}[1]^s{_r1876}[2]^s{_r1876}[5];
                     s{_r1876}[1:+5] := -- (earrwrite) 
                       s{_r1876}[0:+5];
                     s{_r1876}[0] := -- (earrwrite) 
                       UD1_free_13407{UD1_free_13407}[idx_free_13410{idx_free_13410}];
                     r{_r1883}
                 };
                 ya_free_13409{ya_free_13409}
           in
           map auto_map__24622{_pf24622}) >>>
          var y{_r2009} : arr[48] bit in
          repeat seq { x{_r2020} <- var xa_free_24008{xa_free_24008} : arr[48] bit in
                                    seq { _unused_24052{_pf24052} <- for idx_free_24009{idx_free_24009} in [0, 3]
                                                                       seq { DD1_free_24010{DD1_free_24010} <- take[arr[16] bit];
                                                                             return xa_free_24008{xa_free_24008}[idx_free_24009{idx_free_24009}*16:+16] := -- (earrwrite) 
                                                                                      DD1_free_24010{DD1_free_24010}
                                                                           };
                                          return[ForceInline] xa_free_24008{xa_free_24008}
                                        };
                       emit var ya_free_24045{ya_free_24045} : arr[48] bit in
                            var oarrtmp{_r1952} : arr[48] bit in
                            zero_bit{_r262}(ya_free_24045{ya_free_24045});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={0, 3, 6, 9, 12, 15, 18, 21}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[0:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={24, 27, 30, 33, 36, 39, 42, 45}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[8:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={1, 4, 7, 10, 13, 16, 19, 22}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[16:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={25, 28, 31, 34, 37, 40, 43, 46}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[24:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={2, 5, 8, 11, 14, 17, 20, 23}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[32:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            var oarrtmp{_r1960} : arr[48] bit in
                            for j{_r1937} in [0, 48] {
                              oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                '0;
                              for i{_r1942} in [0, 8] {
                                if j{_r1937}=={26, 29, 32, 35, 38, 41, 44, 47}[i{_r1942}] {
                                  oarrtmp{_r1960}[j{_r1937}] := -- (earrwrite) 
                                    x{_r2020}[40:+8][i{_r1942}]
                                } else {
                                  tt
                                }
                              }
                            };
                            ya_free_24045{ya_free_24045} := -- (easgn)
                              v_or{_r374}(ya_free_24045{ya_free_24045}, oarrtmp{_r1960});
                            tt;
                            ya_free_24045{ya_free_24045}
                     }) .>>>.
         48-mitigate(MA)[bit]-24)
      }) >>>
 write[arr[24] bit])

type:
ST (C ()) (EXTBUF[base=int32]) (EXTBUF[base=arr[24] bit])
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
#ifndef STRUCT_LTECOEFFS
#define STRUCT_LTECOEFFS
typedef struct {
    complex16 freqCoeffs[64];
    complex16 channelCoeffs[64];
} LTECoeffs;
#endif
#ifndef STRUCT_HEADERINFO
#define STRUCT_HEADERINFO
typedef struct {
    int32 coding;
    int32 modulation;
    int32 len;
    unsigned char err;
} HeaderInfo;
#endif
HeaderInfo parsePLCPHeader_ln119_87(BitArrPtr hdata88, int __len_unused_89);
HeaderInfo parsePLCPHeader_ln119_87(BitArrPtr hdata88, int __len_unused_89)
{
    calign int32 bln120_90 = 0;
    calign HeaderInfo hln121_91 = {0, 0, 0, 0};
    calign Bit pln122_92 = 0;
    calign Bit bitres93 = 0;
    calign Bit bitres94 = 0;
    calign Bit bitres95 = 0;
    calign Bit bitres96 = 0;
    int __c97;
    calign Bit bitres98 = 0;
    calign Bit bitres99 = 0;
    calign Bit bitres100 = 0;
    calign Bit bitres101 = 0;
    int __c102;
    calign Bit bitres103 = 0;
    calign Bit bitres104 = 0;
    calign Bit bitres105 = 0;
    calign Bit bitres106 = 0;
    int __c107;
    calign Bit bitres108 = 0;
    calign Bit bitres109 = 0;
    calign Bit bitres110 = 0;
    calign Bit bitres111 = 0;
    int __c112;
    calign Bit bitres113 = 0;
    calign Bit bitres114 = 0;
    calign Bit bitres115 = 0;
    calign Bit bitres116 = 0;
    int __c117;
    calign Bit bitres118 = 0;
    calign Bit bitres119 = 0;
    calign Bit bitres120 = 0;
    calign Bit bitres121 = 0;
    int __c122;
    calign Bit bitres123 = 0;
    calign Bit bitres124 = 0;
    calign Bit bitres125 = 0;
    calign Bit bitres126 = 0;
    int __c127;
    calign Bit bitres128 = 0;
    calign Bit bitres129 = 0;
    calign Bit bitres130 = 0;
    calign Bit bitres131 = 0;
    int __c132;
    calign Bit bitres134 = 0;
    int __c135;
    int __c136;
    calign Bit bitres138 = 0;
    calign Bit bitres140 = 0;
    calign Bit bitres142 = 0;
    int __c143;
    calign Bit bitres145 = 0;
    int __c146;
    int __c147;
    int __c148;
    int __c149;
    int __c150;
    int __c151;
    int __c152;
    
    bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:124:15-16");
    bitRead(hdata88, 0, &bitres93);
    bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:124:33-34");
    bitRead(hdata88, 1, &bitres94);
    bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:124:51-52");
    bitRead(hdata88, 2, &bitres95);
    bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:124:69-70");
    bitRead(hdata88, 3, &bitres96);
    __c97 = bitres93 == 1 && bitres94 == 1 && bitres95 == 0 && bitres96 == 1;
    if (__c97) {
        hln121_91.modulation = 0;
        hln121_91.coding = 0;
    } else {
        bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:127:17-18");
        bitRead(hdata88, 0, &bitres98);
        bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:127:35-36");
        bitRead(hdata88, 1, &bitres99);
        bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:127:53-54");
        bitRead(hdata88, 2, &bitres100);
        bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:127:71-72");
        bitRead(hdata88, 3, &bitres101);
        __c102 = bitres98 == 1 && bitres99 == 1 && bitres100 == 1 &&
            bitres101 == 1;
        if (__c102) {
            hln121_91.modulation = 0;
            hln121_91.coding = 2;
        } else {
            bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:130:20-21");
            bitRead(hdata88, 0, &bitres103);
            bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:130:38-39");
            bitRead(hdata88, 1, &bitres104);
            bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:130:56-57");
            bitRead(hdata88, 2, &bitres105);
            bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:130:74-75");
            bitRead(hdata88, 3, &bitres106);
            __c107 = bitres103 == 0 && bitres104 == 1 && bitres105 == 0 &&
                bitres106 == 1;
            if (__c107) {
                hln121_91.modulation = 1;
                hln121_91.coding = 0;
            } else {
                bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:133:25-26");
                bitRead(hdata88, 0, &bitres108);
                bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:133:43-44");
                bitRead(hdata88, 1, &bitres109);
                bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:133:61-62");
                bitRead(hdata88, 2, &bitres110);
                bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:133:79-80");
                bitRead(hdata88, 3, &bitres111);
                __c112 = bitres108 == 0 && bitres109 == 1 && bitres110 == 1 &&
                    bitres111 == 1;
                if (__c112) {
                    hln121_91.modulation = 1;
                    hln121_91.coding = 2;
                } else {
                    bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:136:28-29");
                    bitRead(hdata88, 0, &bitres113);
                    bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:136:46-47");
                    bitRead(hdata88, 1, &bitres114);
                    bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:136:64-65");
                    bitRead(hdata88, 2, &bitres115);
                    bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:136:82-83");
                    bitRead(hdata88, 3, &bitres116);
                    __c117 = bitres113 == 1 && bitres114 == 0 && bitres115 ==
                        0 && bitres116 == 1;
                    if (__c117) {
                        hln121_91.modulation = 2;
                        hln121_91.coding = 0;
                    } else {
                        bounds_check(24, 0 + 0,
                                     "../parsePLCPHeader.blk:139:31-32");
                        bitRead(hdata88, 0, &bitres118);
                        bounds_check(24, 1 + 0,
                                     "../parsePLCPHeader.blk:139:49-50");
                        bitRead(hdata88, 1, &bitres119);
                        bounds_check(24, 2 + 0,
                                     "../parsePLCPHeader.blk:139:67-68");
                        bitRead(hdata88, 2, &bitres120);
                        bounds_check(24, 3 + 0,
                                     "../parsePLCPHeader.blk:139:85-86");
                        bitRead(hdata88, 3, &bitres121);
                        __c122 = bitres118 == 1 && bitres119 == 0 &&
                            bitres120 == 1 && bitres121 == 1;
                        if (__c122) {
                            hln121_91.modulation = 2;
                            hln121_91.coding = 2;
                        } else {
                            bounds_check(24, 0 + 0,
                                         "../parsePLCPHeader.blk:142:33-34");
                            bitRead(hdata88, 0, &bitres123);
                            bounds_check(24, 1 + 0,
                                         "../parsePLCPHeader.blk:142:51-52");
                            bitRead(hdata88, 1, &bitres124);
                            bounds_check(24, 2 + 0,
                                         "../parsePLCPHeader.blk:142:69-70");
                            bitRead(hdata88, 2, &bitres125);
                            bounds_check(24, 3 + 0,
                                         "../parsePLCPHeader.blk:142:87-88");
                            bitRead(hdata88, 3, &bitres126);
                            __c127 = bitres123 == 0 && bitres124 == 0 &&
                                bitres125 == 0 && bitres126 == 1;
                            if (__c127) {
                                hln121_91.modulation = 3;
                                hln121_91.coding = 1;
                            } else {
                                bounds_check(24, 0 + 0,
                                             "../parsePLCPHeader.blk:145:35-36");
                                bitRead(hdata88, 0, &bitres128);
                                bounds_check(24, 1 + 0,
                                             "../parsePLCPHeader.blk:145:53-54");
                                bitRead(hdata88, 1, &bitres129);
                                bounds_check(24, 2 + 0,
                                             "../parsePLCPHeader.blk:145:71-72");
                                bitRead(hdata88, 2, &bitres130);
                                bounds_check(24, 3 + 0,
                                             "../parsePLCPHeader.blk:145:89-90");
                                bitRead(hdata88, 3, &bitres131);
                                __c132 = bitres128 == 0 && bitres129 == 0 &&
                                    bitres130 == 1 && bitres131 == 1;
                                if (__c132) {
                                    hln121_91.modulation = 3;
                                    hln121_91.coding = 2;
                                } else {
                                    hln121_91.modulation = 0;
                                    hln121_91.coding = 0;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    bln120_90 = 1;
    hln121_91.len = 0;
    for (int32 j133 = 5; j133 < 5 + 12; j133++) {
        bounds_check(24, j133 + 0, "../parsePLCPHeader.blk:163:17-18");
        bitRead(hdata88, j133, &bitres134);
        __c135 = bitres134 == 1;
        if (__c135) {
            hln121_91.len = hln121_91.len + bln120_90;
        } else { }
        bln120_90 = 2 * bln120_90;
    }
    hln121_91.err = 0U;
    __c136 = hln121_91.len > 2048;
    if (__c136) {
        hln121_91.err = 1U;
        hln121_91.len = 2048;
    } else { }
    pln122_92 = 0;
    for (int32 j137 = 0; j137 < 0 + 8; j137++) {
        bounds_check(24, 0 + j137 + 0, "../parsePLCPHeader.blk:182:21-22");
        bitRead(hdata88, 0 + j137, &bitres138);
        pln122_92 = pln122_92 ^ bitres138;
    }
    for (int32 j139 = 0; j139 < 0 + 8; j139++) {
        bounds_check(24, 8 + j139 + 0, "../parsePLCPHeader.blk:182:21-22");
        bitRead(hdata88, 8 + j139, &bitres140);
        pln122_92 = pln122_92 ^ bitres140;
    }
    for (int32 j141 = 0; j141 < 0 + 8; j141++) {
        bounds_check(24, 16 + j141 + 0, "../parsePLCPHeader.blk:182:21-22");
        bitRead(hdata88, 16 + j141, &bitres142);
        pln122_92 = pln122_92 ^ bitres142;
    }
    __c143 = pln122_92 != 0;
    if (__c143) {
        hln121_91.err = 1U;
    } else { }
    pln122_92 = 0;
    for (int32 j144 = 2; j144 < 2 + 6; j144++) {
        bounds_check(24, 16 + j144 + 0, "../parsePLCPHeader.blk:196:21-22");
        bitRead(hdata88, 16 + j144, &bitres145);
        pln122_92 = pln122_92 | bitres145;
    }
    __c146 = pln122_92 != 0;
    if (__c146) {
        hln121_91.err = 1U;
    } else { }
    __c147 = !hln121_91.err;
    if (__c147) {
        printf("%s", "Header - modulation: ");
        __c148 = hln121_91.modulation == 0;
        if (__c148) {
            printf("%s", "BPSK");
        } else {
            __c149 = hln121_91.modulation == 1;
            if (__c149) {
                printf("%s", "M_QPSK");
            } else {
                __c150 = hln121_91.modulation == 2;
                if (__c150) {
                    printf("%s", "16QAM");
                } else {
                    printf("%s", "64QAM");
                }
            }
        }
        printf("%s", ", coding: ");
        __c151 = hln121_91.coding == 0;
        if (__c151) {
            printf("%s", "1/2");
        } else {
            __c152 = hln121_91.coding == 1;
            if (__c152) {
                printf("%s", "2/3");
            } else {
                printf("%s", "3/4");
            }
        }
        printf("%s", ", length: ");
        printf("%ld", hln121_91.len);
        printf("%s", " B");
        printf("\n");
        printf("%s", "Header bits: ");
        printBitArr(hdata88, 24);
        printf("\n");
    } else { }
    return hln121_91;
}
int perm_orig_ln30_153(BitArrPtr oarr, int n1917, int32* p154,
                       int __len_unused156, BitArrPtr iarr155, int n1919);
int perm_orig_ln30_153(BitArrPtr oarr, int n1917, int32* p154,
                       int __len_unused156, BitArrPtr iarr155, int n1919)
{
    calign unsigned char* oarrtmpln32_157 = NULL;
    calign int ret158 = 0;
    calign int32 p1ln36_160[8] = {0};
    calign unsigned char iarrln37_161[1] = {0};
    calign Bit bitres163 = 0;
    
    oarrtmpln32_157 = (unsigned char*) wpl_alloca(pheap_ctx, n1917 + 7 >> 3);
    __ext_zero_bit(oarr, n1917);
    for (int32 j159 = 0; j159 < 0 + n1917 / 8; j159++) {
        bounds_check(n1917, 8 * j159 + 7,
                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:36:17-20");
        blink_copy(p1ln36_160, &p154[8 * j159], 8 * 4);
        bounds_check(n1919, 8 * j159 + 7,
                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:37:20-23");
        *(uint8*) (BitArrPtr) (iarrln37_161 + 0) =
            *(uint8*) (BitArrPtr) (&(&iarr155[1 * j159])[0] + 0);
        for (int32 i162 = 0; i162 < 0 + 8; i162++) {
            bounds_check(8, i162 + 0,
                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:26:12-13");
            bounds_check(8, i162 + 0,
                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:26:5-25");
            bitRead(iarrln37_161, i162, &bitres163);
            bounds_check(n1917, p1ln36_160[i162] + 0,
                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:26:5-25");
            bitWrite(oarr, p1ln36_160[i162], bitres163);
        }
    }
    return 0;
}
int auto_map__24622_ln108_210(BitArrPtr ya_free_13409, int __len_unused_214,
                              BitArrPtr UD1_free_13407213, int __len_unused_215,
                              BitArrPtr s211, int __len_unused_212);
int auto_map__24622_ln108_210(BitArrPtr ya_free_13409, int __len_unused_214,
                              BitArrPtr UD1_free_13407213, int __len_unused_215,
                              BitArrPtr s211, int __len_unused_212)
{
    calign unsigned char rln99_217[1] = {0};
    calign Bit bitres218 = 0;
    calign Bit bitres219 = 0;
    calign Bit bitres220 = 0;
    calign Bit bitres221 = 0;
    calign Bit bitres222 = 0;
    calign Bit bitres223 = 0;
    calign Bit bitres224 = 0;
    calign Bit bitres225 = 0;
    calign Bit bitres226 = 0;
    calign Bit bitres227 = 0;
    calign Bit bitres228 = 0;
    
    for (int32 idx_free_13410216 = 0; idx_free_13410216 < 0 + 8;
         idx_free_13410216++) {
        bounds_check(8, idx_free_13410216 + 0, "<no location>");
        bitRead(UD1_free_13407213, idx_free_13410216, &bitres218);
        bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
        bitRead(s211, 1, &bitres219);
        bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
        bitRead(s211, 2, &bitres220);
        bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
        bitRead(s211, 4, &bitres221);
        bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
        bitRead(s211, 5, &bitres222);
        bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
        bitWrite(rln99_217, 0, bitres218 ^ bitres219 ^ bitres220 ^ bitres221 ^
                 bitres222);
        bounds_check(8, idx_free_13410216 + 0, "<no location>");
        bitRead(UD1_free_13407213, idx_free_13410216, &bitres223);
        bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
        bitRead(s211, 0, &bitres224);
        bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
        bitRead(s211, 1, &bitres225);
        bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
        bitRead(s211, 2, &bitres226);
        bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
        bitRead(s211, 5, &bitres227);
        bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
        bitWrite(rln99_217, 1, bitres223 ^ bitres224 ^ bitres225 ^ bitres226 ^
                 bitres227);
        bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
        bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
        bitArrWrite(&s211[0], 1, 5, s211);
        bounds_check(8, idx_free_13410216 + 0, "../encoding.blk:104:4-13");
        bitRead(UD1_free_13407213, idx_free_13410216, &bitres228);
        bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
        bitWrite(s211, 0, bitres228);
        bounds_check(16, 2 * idx_free_13410216 + 1, "../encoding.blk:109:4-16");
        bitArrWrite(rln99_217, 2 * idx_free_13410216, 2, ya_free_13409);
    }
    return 0;
}
int wpl_go_aux(int initialized)
{
    unsigned int loop_counter = 0;
    calign BitArrPtr __yv_tmp_ln29_166_buf = NULL;
    calign int32 __yv_tmp_ln29_168_buf = 0;
    volatile int32 __branch_var_ln32_171 = 0;
    calign int __dv_tmp__unused_3381_ln32_172_doneVal = 0;
    calign int __dv_tmp__unused_3380_ln32_174_doneVal = 0;
    calign BitArrPtr __yv_tmp_ln32_177_buf = NULL;
    calign BitArrPtr __yv_tmp_ln32_179_buf = NULL;
    calign BitArrPtr __yv_tmp_ln32_181_buf = NULL;
    calign unsigned char ya_free_13309_ln32_182[3] = {0};
    calign int8 ailn51_183[2] = {0};
    calign unsigned char abln52_184[2] = {0};
    calign Bit pln53_185 = 0;
    calign int ret186 = 0;
    calign unsigned char __bit_arr187[1] = {0};
    calign Bit bitres189 = 0;
    calign Bit bitres191 = 0;
    calign Bit bitres193 = 0;
    calign Bit bitres194 = 0;
    calign int _unused_24050_ln32_195 = 0;
    calign int32 idx_free_13310_ln32_196 = 0;
    volatile int32 __branch_var_ln32_200 = 0;
    calign int __dv_tmp___seq_unused_ln32_197_ln32_201_doneVal = 0;
    volatile int32 __bnd_fst_ln32_198_state = 0;
    volatile int32 __branch_var_ln32_204 = 0;
    calign unsigned char __bit_arr205[1] = {0};
    calign unsigned char s_ln93_206[1] = {0};
    calign Bit x_ln94_207 = 0;
    calign Bit oA_ln95_208 = 0;
    calign Bit oB_ln96_209 = 0;
    calign unsigned char ret229[2] = {0};
    unsigned int mem_idx230;
    calign unsigned char y_ln66_231[6] = {0};
    calign int __dv_tmp_ln72_232_doneVal = 0;
    volatile int32 __branch_var_ln73_234 = 0;
    calign unsigned char __dv_tmp_x_ln73_235_doneVal[6] = {0};
    volatile int32 __bnd_rest_ln73_236_state = 0;
    calign unsigned char xa_free_24008_ln73_276[6] = {0};
    volatile int32 __branch_var_ln73_278 = 0;
    calign int __dv_tmp__unused_24052_ln73_279_doneVal = 0;
    calign int32 idx_free_24009_ln73_281 = 0;
    volatile int32 __branch_var_ln73_285 = 0;
    calign unsigned char __dv_tmp_DD1_free_24010_ln73_286_doneVal[2] = {0};
    calign int __dv_tmp___seq_unused_ln73_282_ln73_288_doneVal = 0;
    volatile int32 __branch_var_ln73_291 = 0;
    volatile int32 __par_c2_ln32_176_mit_state = 2;
    calign Bit __yv_tmp_ln32_293_buf = 0;
    calign BitArrPtr __yv_tmp_ln31_295_buf = NULL;
    calign unsigned char ya_free_4125_ln31_296[3] = {0};
    calign int8 ailn51_297[2] = {0};
    calign unsigned char abln52_298[2] = {0};
    calign Bit pln53_299 = 0;
    calign int ret300 = 0;
    calign unsigned char __bit_arr301[1] = {0};
    calign Bit bitres303 = 0;
    calign Bit bitres305 = 0;
    calign Bit bitres307 = 0;
    calign Bit bitres308 = 0;
    calign int _unused_24048_ln31_309 = 0;
    calign int32 idx_free_4126_ln31_310 = 0;
    volatile int32 __branch_var_ln31_314 = 0;
    calign int __dv_tmp___seq_unused_ln31_311_ln31_315_doneVal = 0;
    volatile int32 __bnd_fst_ln31_312_state = 0;
    volatile int32 __branch_var_ln31_319 = 0;
    calign unsigned char __bit_arr320[1] = {0};
    calign unsigned char s_ln93_321[1] = {0};
    calign Bit x_ln94_322 = 0;
    calign Bit oA_ln95_323 = 0;
    calign Bit oB_ln96_324 = 0;
    calign int __dv_tmp_ln108_325_doneVal = 0;
    volatile int32 __branch_var_ln109_327 = 0;
    calign unsigned char __dv_tmp_UD2_free_11865_ln109_328_doneVal[1] = {0};
    calign unsigned char ya_free_11866_ln109_330[2] = {0};
    calign unsigned char rln99_331[1] = {0};
    calign Bit bitres332 = 0;
    calign Bit bitres333 = 0;
    calign Bit bitres334 = 0;
    calign Bit bitres335 = 0;
    calign Bit bitres336 = 0;
    calign Bit bitres337 = 0;
    calign Bit bitres338 = 0;
    calign Bit bitres339 = 0;
    calign Bit bitres340 = 0;
    calign Bit bitres341 = 0;
    calign Bit bitres342 = 0;
    calign unsigned char rln99_343[1] = {0};
    calign Bit bitres344 = 0;
    calign Bit bitres345 = 0;
    calign Bit bitres346 = 0;
    calign Bit bitres347 = 0;
    calign Bit bitres348 = 0;
    calign Bit bitres349 = 0;
    calign Bit bitres350 = 0;
    calign Bit bitres351 = 0;
    calign Bit bitres352 = 0;
    calign Bit bitres353 = 0;
    calign Bit bitres354 = 0;
    calign unsigned char rln99_355[1] = {0};
    calign Bit bitres356 = 0;
    calign Bit bitres357 = 0;
    calign Bit bitres358 = 0;
    calign Bit bitres359 = 0;
    calign Bit bitres360 = 0;
    calign Bit bitres361 = 0;
    calign Bit bitres362 = 0;
    calign Bit bitres363 = 0;
    calign Bit bitres364 = 0;
    calign Bit bitres365 = 0;
    calign Bit bitres366 = 0;
    calign unsigned char rln99_367[1] = {0};
    calign Bit bitres368 = 0;
    calign Bit bitres369 = 0;
    calign Bit bitres370 = 0;
    calign Bit bitres371 = 0;
    calign Bit bitres372 = 0;
    calign Bit bitres373 = 0;
    calign Bit bitres374 = 0;
    calign Bit bitres375 = 0;
    calign Bit bitres376 = 0;
    calign Bit bitres377 = 0;
    calign Bit bitres378 = 0;
    calign unsigned char rln99_379[1] = {0};
    calign Bit bitres380 = 0;
    calign Bit bitres381 = 0;
    calign Bit bitres382 = 0;
    calign Bit bitres383 = 0;
    calign Bit bitres384 = 0;
    calign Bit bitres385 = 0;
    calign Bit bitres386 = 0;
    calign Bit bitres387 = 0;
    calign Bit bitres388 = 0;
    calign Bit bitres389 = 0;
    calign Bit bitres390 = 0;
    calign unsigned char rln99_391[1] = {0};
    calign Bit bitres392 = 0;
    calign Bit bitres393 = 0;
    calign Bit bitres394 = 0;
    calign Bit bitres395 = 0;
    calign Bit bitres396 = 0;
    calign Bit bitres397 = 0;
    calign Bit bitres398 = 0;
    calign Bit bitres399 = 0;
    calign Bit bitres400 = 0;
    calign Bit bitres401 = 0;
    calign Bit bitres402 = 0;
    calign int _unused_24049_ln109_403 = 0;
    calign int32 idx_free_11868_ln109_404 = 0;
    volatile int32 __branch_var_ln109_408 = 0;
    calign int __dv_tmp___seq_unused_ln109_405_ln109_409_doneVal = 0;
    volatile int32 __bnd_fst_ln109_406_state = 0;
    volatile int32 __branch_var_ln109_413 = 0;
    volatile int32 __par_c2_ln32_292_mit_state = 0;
    calign unsigned char __par_c2_ln32_292_mit_buff[3] = {0};
    volatile int32 __bnd_fst_ln32_170_state = 0;
    calign int __global__doneVal = 0;
    char __globalWhatIs;
    
    if (!initialized) {
        __branch_var_ln32_171 = 0;
        __bnd_fst_ln32_170_state = FALSE;
    }
    
  l_DEFAULT_LBL: 
    {
        
      l_LOOP: 
        {
            goto __par_c2_ln29_167_tick;
            
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
        ORIGIN("test_tx_header.blk:29:17-32:67");
        
      __par_c1_ln29_164_tick: 
        {
            if (buf_getint32(params, pbuf_ctx, &__yv_tmp_ln29_168_buf) ==
                GS_EOF)
                return 2;
            
            goto __par_c2_ln29_167_process;
        }
        
      __par_c1_ln29_164_process: 
        {
            return IMMEDIATE;
        }
        
      __bnd_rest_ln32_202_tick: 
        {
            idx_free_13310_ln32_196 = idx_free_13310_ln32_196 + 1;
            __global__doneVal = UNIT;
            __globalWhatIs = DONE;
            if (idx_free_13310_ln32_196 < 3 + 0) {
                __branch_var_ln32_204 = 0;
                __branch_var_ln32_200 = 0;
                __bnd_fst_ln32_198_state = FALSE;
            } else {
                __branch_var_ln32_204 = 1;
            }
            goto __bnd_rest_ln32_175_tick;
        }
        
      __bnd_rest_ln32_202_process: 
        {
            goto l_IMMEDIATE;
        }
        /* test_tx_header.blk:32:6-31 */
        ;
        
      __bnd_fst_ln32_198_tick: 
        {
            if (!__bnd_fst_ln32_198_state) {
                bounds_check(24, 8 * idx_free_13310_ln32_196 + 7,
                             "test_tx_header.blk:32:6-31");
                __yv_tmp_ln32_181_buf = &ya_free_13309_ln32_182[1 *
                                                                idx_free_13310_ln32_196];
                __bnd_fst_ln32_198_state = TRUE;
                goto __par_c2_ln32_180_process;
            } else {
                __dv_tmp___seq_unused_ln32_197_ln32_201_doneVal = UNIT;
                __branch_var_ln32_200 = __branch_var_ln32_200 + 1;
                goto __bnd_rest_ln32_202_tick;
            }
        }
        
      __bnd_fst_ln32_198_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_tx_header.blk:32:6-31");
        
      __bind_ln32_199_tick: 
        {
            switch (__branch_var_ln32_200) {
                
                
              case 0:
                goto __bnd_fst_ln32_198_tick;
                
                
              case 1:
                goto __bnd_rest_ln32_202_tick;
            }
        }
        
      __bind_ln32_199_process: 
        {
            switch (__branch_var_ln32_200) {
                
                
              case 0:
                goto __bnd_fst_ln32_198_process;
                
                
              case 1:
                goto __bnd_rest_ln32_202_process;
            }
        }
        
      __ret_ln32_203_tick: 
        {
            __global__doneVal = 0;
            __globalWhatIs = DONE;
            __globalWhatIs = DONE;
            goto l_IMMEDIATE;
        }
        
      __ret_ln32_203_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln32_175_tick: 
        {
            if (__branch_var_ln32_204 == 0) {
                goto __bind_ln32_199_tick;
            } else {
                goto __ret_ln32_203_tick;
            }
        }
        
      __bnd_rest_ln32_175_process: 
        {
            if (__branch_var_ln32_204 == 0) {
                goto __bind_ln32_199_process;
            } else {
                goto __ret_ln32_203_process;
            }
        }
        ORIGIN("../encoding.blk:108:2-109:16");
        
      __par_c2_ln32_180_tick: 
        {
            goto __bnd_rest_ln32_175_tick;
        }
        
      __par_c2_ln32_180_process: 
        {
            mem_idx230 = wpl_get_free_idx(pheap_ctx);
            auto_map__24622_ln108_210(ret229, 16, __yv_tmp_ln32_181_buf, 8,
                                      s_ln93_206, 6);
            wpl_restore_free_idx(pheap_ctx, mem_idx230);
            __yv_tmp_ln32_179_buf = ret229;
            goto __par_c2_ln32_178_process;
        }
        /* ../interleaving.blk:73:5-34 */
        ;
        
      __bnd_rest_ln73_236_tick: 
        {
            if (!__bnd_rest_ln73_236_state) {
                calign unsigned char ya_free_24045ln73_237[6] = {0};
                calign unsigned char oarrtmpln58_238[6] = {0};
                calign int ret239 = 0;
                calign unsigned char oarrtmpln64_240[6] = {0};
                calign int32 __val_arr243[8] = {0, 3, 6, 9, 12, 15, 18, 21};
                int __c244;
                calign Bit bitres245 = 0;
                calign unsigned char oarrtmpln64_246[6] = {0};
                calign int32 __val_arr249[8] = {24, 27, 30, 33, 36, 39, 42, 45};
                int __c250;
                calign Bit bitres251 = 0;
                calign unsigned char oarrtmpln64_252[6] = {0};
                calign int32 __val_arr255[8] = {1, 4, 7, 10, 13, 16, 19, 22};
                int __c256;
                calign Bit bitres257 = 0;
                calign unsigned char oarrtmpln64_258[6] = {0};
                calign int32 __val_arr261[8] = {25, 28, 31, 34, 37, 40, 43, 46};
                int __c262;
                calign Bit bitres263 = 0;
                calign unsigned char oarrtmpln64_264[6] = {0};
                calign int32 __val_arr267[8] = {2, 5, 8, 11, 14, 17, 20, 23};
                int __c268;
                calign Bit bitres269 = 0;
                calign unsigned char oarrtmpln64_270[6] = {0};
                calign int32 __val_arr273[8] = {26, 29, 32, 35, 38, 41, 44, 47};
                int __c274;
                calign Bit bitres275 = 0;
                
                __ext_zero_bit(ya_free_24045ln73_237, 48);
                for (int32 j241 = 0; j241 < 0 + 48; j241++) {
                    bounds_check(48, j241 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_240, j241, 0);
                    for (int32 i242 = 0; i242 < 0 + 8; i242++) {
                        bounds_check(8, i242 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c244 = j241 == __val_arr243[i242];
                        if (__c244) {
                            bounds_check(48, 0 + i242 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 0 + i242,
                                    &bitres245);
                            bounds_check(48, j241 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_240, j241, bitres245);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_240, 48);
                for (int32 j247 = 0; j247 < 0 + 48; j247++) {
                    bounds_check(48, j247 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_246, j247, 0);
                    for (int32 i248 = 0; i248 < 0 + 8; i248++) {
                        bounds_check(8, i248 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c250 = j247 == __val_arr249[i248];
                        if (__c250) {
                            bounds_check(48, 8 + i248 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 8 + i248,
                                    &bitres251);
                            bounds_check(48, j247 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_246, j247, bitres251);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_246, 48);
                for (int32 j253 = 0; j253 < 0 + 48; j253++) {
                    bounds_check(48, j253 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_252, j253, 0);
                    for (int32 i254 = 0; i254 < 0 + 8; i254++) {
                        bounds_check(8, i254 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c256 = j253 == __val_arr255[i254];
                        if (__c256) {
                            bounds_check(48, 16 + i254 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 16 + i254,
                                    &bitres257);
                            bounds_check(48, j253 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_252, j253, bitres257);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_252, 48);
                for (int32 j259 = 0; j259 < 0 + 48; j259++) {
                    bounds_check(48, j259 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_258, j259, 0);
                    for (int32 i260 = 0; i260 < 0 + 8; i260++) {
                        bounds_check(8, i260 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c262 = j259 == __val_arr261[i260];
                        if (__c262) {
                            bounds_check(48, 24 + i260 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 24 + i260,
                                    &bitres263);
                            bounds_check(48, j259 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_258, j259, bitres263);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_258, 48);
                for (int32 j265 = 0; j265 < 0 + 48; j265++) {
                    bounds_check(48, j265 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_264, j265, 0);
                    for (int32 i266 = 0; i266 < 0 + 8; i266++) {
                        bounds_check(8, i266 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c268 = j265 == __val_arr267[i266];
                        if (__c268) {
                            bounds_check(48, 32 + i266 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 32 + i266,
                                    &bitres269);
                            bounds_check(48, j265 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_264, j265, bitres269);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_264, 48);
                for (int32 j271 = 0; j271 < 0 + 48; j271++) {
                    bounds_check(48, j271 + 0,
                                 "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:48:5-18");
                    bitWrite(oarrtmpln64_270, j271, 0);
                    for (int32 i272 = 0; i272 < 0 + 8; i272++) {
                        bounds_check(8, i272 + 0,
                                     "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:18-19");
                        __c274 = j271 == __val_arr273[i272];
                        if (__c274) {
                            bounds_check(48, 40 + i272 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitRead(__dv_tmp_x_ln73_235_doneVal, 40 + i272,
                                    &bitres275);
                            bounds_check(48, j271 + 0,
                                         "/cygdrive/c/Users/t-stsmo/src/Ziria/lib/permutation.blk:51:29-46");
                            bitWrite(oarrtmpln64_270, j271, bitres275);
                        } else { }
                    }
                }
                __ext_v_or(ya_free_24045ln73_237, 48, ya_free_24045ln73_237, 48,
                           oarrtmpln64_270, 48);
                __yv_tmp_ln32_177_buf = ya_free_24045ln73_237;
                __bnd_rest_ln73_236_state = TRUE;
                goto __par_c2_ln32_176_process;
            } else {
                __dv_tmp_ln72_232_doneVal = UNIT;
                __branch_var_ln73_234 = 0;
                __branch_var_ln73_278 = 0;
                idx_free_24009_ln73_281 = 0;
                if (idx_free_24009_ln73_281 < 3 + 0) {
                    __branch_var_ln73_291 = 0;
                    __branch_var_ln73_285 = 0;
                } else {
                    __branch_var_ln73_291 = 1;
                }
                goto __par_c2_ln32_178_tick;
            }
        }
        
      __bnd_rest_ln73_236_process: 
        {
            return IMMEDIATE;
        }
        
      __bnd_rest_ln73_280_tick: 
        {
            *(uint32*) (BitArrPtr) (__dv_tmp_x_ln73_235_doneVal + 0) =
                *(uint32*) (BitArrPtr) (&xa_free_24008_ln73_276[0] + 0);
            *(uint16*) (BitArrPtr) (__dv_tmp_x_ln73_235_doneVal + 4) =
                *(uint16*) (BitArrPtr) (&xa_free_24008_ln73_276[0] + 4);
            __globalWhatIs = DONE;
            __branch_var_ln73_234 = __branch_var_ln73_234 + 1;
            __bnd_rest_ln73_236_state = FALSE;
            goto __bnd_rest_ln73_236_tick;
        }
        
      __bnd_rest_ln73_280_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln73_289_tick: 
        {
            idx_free_24009_ln73_281 = idx_free_24009_ln73_281 + 1;
            __dv_tmp__unused_24052_ln73_279_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (idx_free_24009_ln73_281 < 3 + 0) {
                __branch_var_ln73_291 = 0;
                __branch_var_ln73_285 = 0;
            } else {
                __branch_var_ln73_291 = 1;
            }
            goto __bnd_fst_ln73_277_tick;
        }
        
      __bnd_rest_ln73_289_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln73_287_tick: 
        {
            bounds_check(48, 16 * idx_free_24009_ln73_281 + 15,
                         "../interleaving.blk:73:26-34");
            *(uint16*) (BitArrPtr) (&xa_free_24008_ln73_276[2 *
                                                            idx_free_24009_ln73_281] +
                                    0) =
                *(uint16*) (BitArrPtr) (&__dv_tmp_DD1_free_24010_ln73_286_doneVal[0] +
                                        0);
            __dv_tmp___seq_unused_ln73_282_ln73_288_doneVal = UNIT;
            __globalWhatIs = DONE;
            __branch_var_ln73_285 = __branch_var_ln73_285 + 1;
            goto __bnd_rest_ln73_289_tick;
        }
        
      __bnd_rest_ln73_287_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_fst_ln73_283_tick: 
        {
            goto __bnd_rest_ln32_175_tick;
        }
        
      __bnd_fst_ln73_283_process: 
        {
            *(uint16*) (BitArrPtr) (__dv_tmp_DD1_free_24010_ln73_286_doneVal +
                                    0) =
                *(uint16*) (BitArrPtr) (&__yv_tmp_ln32_179_buf[0] + 0);
            __globalWhatIs = DONE;
            __branch_var_ln73_285 = __branch_var_ln73_285 + 1;
            goto __bnd_rest_ln73_287_tick;
        }
        ORIGIN("../interleaving.blk:73:26-34");
        
      __bind_ln73_284_tick: 
        {
            switch (__branch_var_ln73_285) {
                
                
              case 0:
                goto __bnd_fst_ln73_283_tick;
                
                
              case 1:
                goto __bnd_rest_ln73_287_tick;
                
                
              case 2:
                goto __bnd_rest_ln73_289_tick;
            }
        }
        
      __bind_ln73_284_process: 
        {
            switch (__branch_var_ln73_285) {
                
                
              case 0:
                goto __bnd_fst_ln73_283_process;
                
                
              case 1:
                goto __bnd_rest_ln73_287_process;
                
                
              case 2:
                goto __bnd_rest_ln73_289_process;
            }
        }
        
      __ret_ln73_290_tick: 
        {
            __dv_tmp__unused_24052_ln73_279_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln73_278 = __branch_var_ln73_278 + 1;
            goto __bnd_rest_ln73_280_tick;
        }
        
      __ret_ln73_290_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_fst_ln73_277_tick: 
        {
            if (__branch_var_ln73_291 == 0) {
                goto __bind_ln73_284_tick;
            } else {
                goto __ret_ln73_290_tick;
            }
        }
        
      __bnd_fst_ln73_277_process: 
        {
            if (__branch_var_ln73_291 == 0) {
                goto __bind_ln73_284_process;
            } else {
                goto __ret_ln73_290_process;
            }
        }
        ORIGIN("../interleaving.blk:73:26-34");
        
      __bnd_fst_ln73_233_tick: 
        {
            switch (__branch_var_ln73_278) {
                
                
              case 0:
                goto __bnd_fst_ln73_277_tick;
                
                
              case 1:
                goto __bnd_rest_ln73_280_tick;
            }
        }
        
      __bnd_fst_ln73_233_process: 
        {
            switch (__branch_var_ln73_278) {
                
                
              case 0:
                goto __bnd_fst_ln73_277_process;
                
                
              case 1:
                goto __bnd_rest_ln73_280_process;
            }
        }
        ORIGIN("../interleaving.blk:73:5-34");
        
      __par_c2_ln32_178_tick: 
        {
            switch (__branch_var_ln73_234) {
                
                
              case 0:
                goto __bnd_fst_ln73_233_tick;
                
                
              case 1:
                goto __bnd_rest_ln73_236_tick;
            }
        }
        
      __par_c2_ln32_178_process: 
        {
            switch (__branch_var_ln73_234) {
                
                
              case 0:
                goto __bnd_fst_ln73_233_process;
                
                
              case 1:
                goto __bnd_rest_ln73_236_process;
            }
        }
        ORIGIN("<no location>");
        
      __par_c2_ln32_176_tick: 
        {
            if (__par_c2_ln32_176_mit_state >= 2) {
                goto __par_c2_ln32_178_tick;
            } else {
                __yv_tmp_ln29_166_buf = &__yv_tmp_ln32_177_buf[3 *
                                                               __par_c2_ln32_176_mit_state];
                __par_c2_ln32_176_mit_state++;
                goto __par_c2_ln29_165_process;
            }
        }
        
      __par_c2_ln32_176_process: 
        {
            __yv_tmp_ln29_166_buf = &__yv_tmp_ln32_177_buf[0];
            __par_c2_ln32_176_mit_state = 1;
            goto __par_c2_ln29_165_process;
        }
        
      __bnd_rest_ln31_316_tick: 
        {
            idx_free_4126_ln31_310 = idx_free_4126_ln31_310 + 1;
            __dv_tmp__unused_3380_ln32_174_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (idx_free_4126_ln31_310 < 4 + 0) {
                __branch_var_ln31_319 = 0;
                __branch_var_ln31_314 = 0;
                __bnd_fst_ln31_312_state = FALSE;
            } else {
                __branch_var_ln31_319 = 1;
            }
            goto __bnd_rest_ln32_173_tick;
        }
        
      __bnd_rest_ln31_316_process: 
        {
            goto l_IMMEDIATE;
        }
        /* test_tx_header.blk:31:6-31 */
        ;
        
      __bnd_fst_ln31_312_tick: 
        {
            if (!__bnd_fst_ln31_312_state) {
                calign unsigned char bitarrres317[1] = {0};
                
                bounds_check(24, 6 * idx_free_4126_ln31_310 + 5,
                             "test_tx_header.blk:31:6-31");
                bitArrRead(ya_free_4125_ln31_296, 6 * idx_free_4126_ln31_310, 6,
                           bitarrres317);
                __yv_tmp_ln31_295_buf = bitarrres317;
                __bnd_fst_ln31_312_state = TRUE;
                goto __par_c2_ln31_294_process;
            } else {
                __dv_tmp___seq_unused_ln31_311_ln31_315_doneVal = UNIT;
                __branch_var_ln31_314 = __branch_var_ln31_314 + 1;
                goto __bnd_rest_ln31_316_tick;
            }
        }
        
      __bnd_fst_ln31_312_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_tx_header.blk:31:6-31");
        
      __bind_ln31_313_tick: 
        {
            switch (__branch_var_ln31_314) {
                
                
              case 0:
                goto __bnd_fst_ln31_312_tick;
                
                
              case 1:
                goto __bnd_rest_ln31_316_tick;
            }
        }
        
      __bind_ln31_313_process: 
        {
            switch (__branch_var_ln31_314) {
                
                
              case 0:
                goto __bnd_fst_ln31_312_process;
                
                
              case 1:
                goto __bnd_rest_ln31_316_process;
            }
        }
        
      __ret_ln31_318_tick: 
        {
            __dv_tmp__unused_3380_ln32_174_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln32_171 = __branch_var_ln32_171 + 1;
            bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:50:1-4");
            bitWrite(ya_free_13309_ln32_182, 0, 1);
            bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:50:1-4");
            bitWrite(ya_free_13309_ln32_182, 1, 0);
            bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:50:1-4");
            bitWrite(ya_free_13309_ln32_182, 2, 1);
            bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:50:1-4");
            bitWrite(ya_free_13309_ln32_182, 3, 1);
            bounds_check(24, 4 + 0, "../parsePLCPHeader.blk:50:1-4");
            bitWrite(ya_free_13309_ln32_182, 4, 0);
            bounds_check(2, 0 + 0, "../parsePLCPHeader.blk:51:1-4");
            ailn51_183[0] = 100;
            bounds_check(2, 1 + 0, "../parsePLCPHeader.blk:51:1-4");
            ailn51_183[1] = 0;
            __ext_int8_to_bits(abln52_184, 16, ailn51_183, 2);
            bounds_check(16, 0 + 11, "../parsePLCPHeader.blk:98:3-23");
            bounds_check(24, 5 + 11, "../parsePLCPHeader.blk:98:3-23");
            bitArrWrite(&abln52_184[0], 5, 12, ya_free_13309_ln32_182);
            bounds_check(24, 17 + 6, "../parsePLCPHeader.blk:101:3-45");
            bitArrWrite(__bit_arr187, 17, 7, ya_free_13309_ln32_182);
            pln53_185 = 0;
            for (int32 j188 = 0; j188 < 0 + 8; j188++) {
                bounds_check(24, 0 + j188 + 0,
                             "../parsePLCPHeader.blk:108:18-19");
                bitRead(ya_free_13309_ln32_182, 0 + j188, &bitres189);
                pln53_185 = pln53_185 & bitres189;
            }
            for (int32 j190 = 0; j190 < 0 + 8; j190++) {
                bounds_check(24, 8 + j190 + 0,
                             "../parsePLCPHeader.blk:108:18-19");
                bitRead(ya_free_13309_ln32_182, 8 + j190, &bitres191);
                pln53_185 = pln53_185 & bitres191;
            }
            for (int32 j192 = 0; j192 < 0 + 8; j192++) {
                bounds_check(24, 16 + j192 + 0,
                             "../parsePLCPHeader.blk:108:18-19");
                bitRead(ya_free_13309_ln32_182, 16 + j192, &bitres193);
                pln53_185 = pln53_185 & bitres193;
            }
            bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:22-24");
            bitRead(ya_free_13309_ln32_182, 17, &bitres194);
            bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:3-29");
            bitWrite(ya_free_13309_ln32_182, 17, bitres194 ^ pln53_185);
            _unused_24050_ln32_195 = 0;
            idx_free_13310_ln32_196 = 0;
            if (idx_free_13310_ln32_196 < 3 + 0) {
                __branch_var_ln32_204 = 0;
                __branch_var_ln32_200 = 0;
                __bnd_fst_ln32_198_state = FALSE;
            } else {
                __branch_var_ln32_204 = 1;
            }
            *(uint8*) (BitArrPtr) (s_ln93_206 + 0) =
                *(uint8*) (BitArrPtr) (&__bit_arr205[0] + 0);
            __branch_var_ln73_234 = 0;
            __branch_var_ln73_278 = 0;
            idx_free_24009_ln73_281 = 0;
            if (idx_free_24009_ln73_281 < 3 + 0) {
                __branch_var_ln73_291 = 0;
                __branch_var_ln73_285 = 0;
            } else {
                __branch_var_ln73_291 = 1;
            }
            goto __par_c2_ln32_176_tick;
        }
        
      __ret_ln31_318_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln32_173_tick: 
        {
            if (__branch_var_ln31_319 == 0) {
                goto __bind_ln31_313_tick;
            } else {
                goto __ret_ln31_318_tick;
            }
        }
        
      __bnd_rest_ln32_173_process: 
        {
            if (__branch_var_ln31_319 == 0) {
                goto __bind_ln31_313_process;
            } else {
                goto __ret_ln31_318_process;
            }
        }
        
      __bnd_rest_ln109_410_tick: 
        {
            idx_free_11868_ln109_404 = idx_free_11868_ln109_404 + 1;
            __dv_tmp_ln108_325_doneVal = UNIT;
            __globalWhatIs = DONE;
            if (idx_free_11868_ln109_404 < 12 + 0) {
                __branch_var_ln109_413 = 0;
                __branch_var_ln109_408 = 0;
                __bnd_fst_ln109_406_state = FALSE;
            } else {
                __branch_var_ln109_413 = 1;
            }
            goto __bnd_rest_ln109_329_tick;
        }
        
      __bnd_rest_ln109_410_process: 
        {
            goto l_IMMEDIATE;
        }
        /* ../encoding.blk:109:4-16 */
        ;
        
      __bnd_fst_ln109_406_tick: 
        {
            if (!__bnd_fst_ln109_406_state) {
                calign Bit bitres411 = 0;
                
                bounds_check(12, idx_free_11868_ln109_404 + 0,
                             "../encoding.blk:109:4-16");
                bitRead(ya_free_11866_ln109_330, idx_free_11868_ln109_404,
                        &bitres411);
                __yv_tmp_ln32_293_buf = bitres411;
                __bnd_fst_ln109_406_state = TRUE;
                goto __par_c2_ln32_292_process;
            } else {
                __dv_tmp___seq_unused_ln109_405_ln109_409_doneVal = UNIT;
                __branch_var_ln109_408 = __branch_var_ln109_408 + 1;
                goto __bnd_rest_ln109_410_tick;
            }
        }
        
      __bnd_fst_ln109_406_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("../encoding.blk:109:4-16");
        
      __bind_ln109_407_tick: 
        {
            switch (__branch_var_ln109_408) {
                
                
              case 0:
                goto __bnd_fst_ln109_406_tick;
                
                
              case 1:
                goto __bnd_rest_ln109_410_tick;
            }
        }
        
      __bind_ln109_407_process: 
        {
            switch (__branch_var_ln109_408) {
                
                
              case 0:
                goto __bnd_fst_ln109_406_process;
                
                
              case 1:
                goto __bnd_rest_ln109_410_process;
            }
        }
        
      __ret_ln109_412_tick: 
        {
            __dv_tmp_ln108_325_doneVal = 0;
            __globalWhatIs = DONE;
            __branch_var_ln109_327 = 0;
            goto __par_c2_ln31_294_tick;
        }
        
      __ret_ln109_412_process: 
        {
            goto l_IMMEDIATE;
        }
        
      __bnd_rest_ln109_329_tick: 
        {
            if (__branch_var_ln109_413 == 0) {
                goto __bind_ln109_407_tick;
            } else {
                goto __ret_ln109_412_tick;
            }
        }
        
      __bnd_rest_ln109_329_process: 
        {
            if (__branch_var_ln109_413 == 0) {
                goto __bind_ln109_407_process;
            } else {
                goto __ret_ln109_412_process;
            }
        }
        
      __bnd_fst_ln109_326_tick: 
        {
            goto __bnd_rest_ln32_173_tick;
        }
        
      __bnd_fst_ln109_326_process: 
        {
            *(uint8*) (BitArrPtr) (__dv_tmp_UD2_free_11865_ln109_328_doneVal +
                                   0) =
                *(uint8*) (BitArrPtr) (&__yv_tmp_ln31_295_buf[0] + 0);
            __globalWhatIs = DONE;
            __branch_var_ln109_327 = __branch_var_ln109_327 + 1;
            bounds_check(6, 0 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 0, &bitres332);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres333);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres334);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres335);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres336);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_331, 0, bitres332 ^ bitres333 ^ bitres334 ^
                     bitres335 ^ bitres336);
            bounds_check(6, 0 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 0, &bitres337);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres338);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres339);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres340);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres341);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_331, 1, bitres337 ^ bitres338 ^ bitres339 ^
                     bitres340 ^ bitres341);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 0, &bitres342);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres342);
            bounds_check(12, 0 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_331, 0, 2, ya_free_11866_ln109_330);
            bounds_check(6, 1 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 1, &bitres344);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres345);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres346);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres347);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres348);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_343, 0, bitres344 ^ bitres345 ^ bitres346 ^
                     bitres347 ^ bitres348);
            bounds_check(6, 1 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 1, &bitres349);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres350);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres351);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres352);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres353);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_343, 1, bitres349 ^ bitres350 ^ bitres351 ^
                     bitres352 ^ bitres353);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 1 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 1, &bitres354);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres354);
            bounds_check(12, 2 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_343, 2, 2, ya_free_11866_ln109_330);
            bounds_check(6, 2 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 2, &bitres356);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres357);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres358);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres359);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres360);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_355, 0, bitres356 ^ bitres357 ^ bitres358 ^
                     bitres359 ^ bitres360);
            bounds_check(6, 2 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 2, &bitres361);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres362);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres363);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres364);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres365);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_355, 1, bitres361 ^ bitres362 ^ bitres363 ^
                     bitres364 ^ bitres365);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 2 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 2, &bitres366);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres366);
            bounds_check(12, 4 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_355, 4, 2, ya_free_11866_ln109_330);
            bounds_check(6, 3 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 3, &bitres368);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres369);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres370);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres371);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres372);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_367, 0, bitres368 ^ bitres369 ^ bitres370 ^
                     bitres371 ^ bitres372);
            bounds_check(6, 3 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 3, &bitres373);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres374);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres375);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres376);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres377);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_367, 1, bitres373 ^ bitres374 ^ bitres375 ^
                     bitres376 ^ bitres377);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 3 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 3, &bitres378);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres378);
            bounds_check(12, 6 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_367, 6, 2, ya_free_11866_ln109_330);
            bounds_check(6, 4 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 4, &bitres380);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres381);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres382);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres383);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres384);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_379, 0, bitres380 ^ bitres381 ^ bitres382 ^
                     bitres383 ^ bitres384);
            bounds_check(6, 4 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 4, &bitres385);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres386);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres387);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres388);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres389);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_379, 1, bitres385 ^ bitres386 ^ bitres387 ^
                     bitres388 ^ bitres389);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 4 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 4, &bitres390);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres390);
            bounds_check(12, 8 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_379, 8, 2, ya_free_11866_ln109_330);
            bounds_check(6, 5 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 5, &bitres392);
            bounds_check(6, 1 + 0, "../encoding.blk:100:18-19");
            bitRead(s_ln93_321, 1, &bitres393);
            bounds_check(6, 2 + 0, "../encoding.blk:100:25-26");
            bitRead(s_ln93_321, 2, &bitres394);
            bounds_check(6, 4 + 0, "../encoding.blk:100:32-33");
            bitRead(s_ln93_321, 4, &bitres395);
            bounds_check(6, 5 + 0, "../encoding.blk:100:39-40");
            bitRead(s_ln93_321, 5, &bitres396);
            bounds_check(2, 0 + 0, "../encoding.blk:100:4-40");
            bitWrite(rln99_391, 0, bitres392 ^ bitres393 ^ bitres394 ^
                     bitres395 ^ bitres396);
            bounds_check(6, 5 + 0, "<no location>");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 5, &bitres397);
            bounds_check(6, 0 + 0, "../encoding.blk:101:18-19");
            bitRead(s_ln93_321, 0, &bitres398);
            bounds_check(6, 1 + 0, "../encoding.blk:101:25-26");
            bitRead(s_ln93_321, 1, &bitres399);
            bounds_check(6, 2 + 0, "../encoding.blk:101:32-33");
            bitRead(s_ln93_321, 2, &bitres400);
            bounds_check(6, 5 + 0, "../encoding.blk:101:39-40");
            bitRead(s_ln93_321, 5, &bitres401);
            bounds_check(2, 1 + 0, "../encoding.blk:101:4-40");
            bitWrite(rln99_391, 1, bitres397 ^ bitres398 ^ bitres399 ^
                     bitres400 ^ bitres401);
            bounds_check(6, 0 + 4, "../encoding.blk:103:4-20");
            bounds_check(6, 1 + 4, "../encoding.blk:103:4-20");
            bitArrWrite(&s_ln93_321[0], 1, 5, s_ln93_321);
            bounds_check(6, 5 + 0, "../encoding.blk:104:4-13");
            bitRead(__dv_tmp_UD2_free_11865_ln109_328_doneVal, 5, &bitres402);
            bounds_check(6, 0 + 0, "../encoding.blk:104:4-13");
            bitWrite(s_ln93_321, 0, bitres402);
            bounds_check(12, 10 + 1, "../encoding.blk:109:4-16");
            bitArrWrite(rln99_391, 10, 2, ya_free_11866_ln109_330);
            _unused_24049_ln109_403 = UNIT;
            idx_free_11868_ln109_404 = 0;
            if (idx_free_11868_ln109_404 < 12 + 0) {
                __branch_var_ln109_413 = 0;
                __branch_var_ln109_408 = 0;
                __bnd_fst_ln109_406_state = FALSE;
            } else {
                __branch_var_ln109_413 = 1;
            }
            goto __bnd_rest_ln109_329_tick;
        }
        ORIGIN("../encoding.blk:109:4-16");
        
      __par_c2_ln31_294_tick: 
        {
            switch (__branch_var_ln109_327) {
                
                
              case 0:
                goto __bnd_fst_ln109_326_tick;
                
                
              case 1:
                goto __bnd_rest_ln109_329_tick;
            }
        }
        
      __par_c2_ln31_294_process: 
        {
            switch (__branch_var_ln109_327) {
                
                
              case 0:
                goto __bnd_fst_ln109_326_process;
                
                
              case 1:
                goto __bnd_rest_ln109_329_process;
            }
        }
        ORIGIN("test_tx_header.blk:32:6-67");
        
      __par_c2_ln32_292_tick: 
        {
            goto __par_c2_ln31_294_tick;
        }
        
      __par_c2_ln32_292_process: 
        {
            bitWrite(__par_c2_ln32_292_mit_buff, 1 *
                     __par_c2_ln32_292_mit_state, __yv_tmp_ln32_293_buf);
            __par_c2_ln32_292_mit_state++;
            if (__par_c2_ln32_292_mit_state >= 24) {
                __yv_tmp_ln29_166_buf = __par_c2_ln32_292_mit_buff;
                __par_c2_ln32_292_mit_state = 0;
                goto __par_c2_ln29_165_process;
            } else {
                goto __par_c2_ln31_294_tick;
            }
        }
        /* test_tx_header.blk:30:5-30 */
        ;
        
      __bnd_fst_ln32_170_tick: 
        {
            if (!__bnd_fst_ln32_170_state) {
                calign unsigned char ya_free_4111ln30_414[3] = {0};
                calign int8 ailn51_415[2] = {0};
                calign unsigned char abln52_416[2] = {0};
                calign Bit pln53_417 = 0;
                calign int ret418 = 0;
                calign unsigned char __bit_arr419[1] = {0};
                calign Bit bitres421 = 0;
                calign Bit bitres423 = 0;
                calign Bit bitres425 = 0;
                calign Bit bitres426 = 0;
                
                bounds_check(24, 0 + 0, "test_tx_header.blk:30:5-30");
                bitWrite(ya_free_4111ln30_414, 0, 1);
                bounds_check(24, 1 + 0, "test_tx_header.blk:30:5-30");
                bitWrite(ya_free_4111ln30_414, 1, 0);
                bounds_check(24, 2 + 0, "test_tx_header.blk:30:5-30");
                bitWrite(ya_free_4111ln30_414, 2, 1);
                bounds_check(24, 3 + 0, "test_tx_header.blk:30:5-30");
                bitWrite(ya_free_4111ln30_414, 3, 1);
                bounds_check(24, 4 + 0, "test_tx_header.blk:30:5-30");
                bitWrite(ya_free_4111ln30_414, 4, 0);
                bounds_check(2, 0 + 0, "../parsePLCPHeader.blk:51:1-4");
                ailn51_415[0] = 100;
                bounds_check(2, 1 + 0, "../parsePLCPHeader.blk:51:1-4");
                ailn51_415[1] = 0;
                __ext_int8_to_bits(abln52_416, 16, ailn51_415, 2);
                bounds_check(16, 0 + 11, "../parsePLCPHeader.blk:98:3-23");
                bounds_check(24, 5 + 11, "../parsePLCPHeader.blk:98:3-23");
                bitArrWrite(&abln52_416[0], 5, 12, ya_free_4111ln30_414);
                bounds_check(24, 17 + 6, "../parsePLCPHeader.blk:101:3-45");
                bitArrWrite(__bit_arr419, 17, 7, ya_free_4111ln30_414);
                pln53_417 = 0;
                for (int32 j420 = 0; j420 < 0 + 8; j420++) {
                    bounds_check(24, 0 + j420 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4111ln30_414, 0 + j420, &bitres421);
                    pln53_417 = pln53_417 & bitres421;
                }
                for (int32 j422 = 0; j422 < 0 + 8; j422++) {
                    bounds_check(24, 8 + j422 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4111ln30_414, 8 + j422, &bitres423);
                    pln53_417 = pln53_417 & bitres423;
                }
                for (int32 j424 = 0; j424 < 0 + 8; j424++) {
                    bounds_check(24, 16 + j424 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4111ln30_414, 16 + j424, &bitres425);
                    pln53_417 = pln53_417 & bitres425;
                }
                bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:22-24");
                bitRead(ya_free_4111ln30_414, 17, &bitres426);
                bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:3-29");
                bitWrite(ya_free_4111ln30_414, 17, bitres426 ^ pln53_417);
                __yv_tmp_ln29_166_buf = ya_free_4111ln30_414;
                __bnd_fst_ln32_170_state = TRUE;
                goto __par_c2_ln29_165_process;
            } else {
                __dv_tmp__unused_3381_ln32_172_doneVal = UNIT;
                __branch_var_ln32_171 = __branch_var_ln32_171 + 1;
                bounds_check(24, 0 + 0, "../parsePLCPHeader.blk:50:1-4");
                bitWrite(ya_free_4125_ln31_296, 0, 1);
                bounds_check(24, 1 + 0, "../parsePLCPHeader.blk:50:1-4");
                bitWrite(ya_free_4125_ln31_296, 1, 0);
                bounds_check(24, 2 + 0, "../parsePLCPHeader.blk:50:1-4");
                bitWrite(ya_free_4125_ln31_296, 2, 1);
                bounds_check(24, 3 + 0, "../parsePLCPHeader.blk:50:1-4");
                bitWrite(ya_free_4125_ln31_296, 3, 1);
                bounds_check(24, 4 + 0, "../parsePLCPHeader.blk:50:1-4");
                bitWrite(ya_free_4125_ln31_296, 4, 0);
                bounds_check(2, 0 + 0, "../parsePLCPHeader.blk:51:1-4");
                ailn51_297[0] = 100;
                bounds_check(2, 1 + 0, "../parsePLCPHeader.blk:51:1-4");
                ailn51_297[1] = 0;
                __ext_int8_to_bits(abln52_298, 16, ailn51_297, 2);
                bounds_check(16, 0 + 11, "../parsePLCPHeader.blk:98:3-23");
                bounds_check(24, 5 + 11, "../parsePLCPHeader.blk:98:3-23");
                bitArrWrite(&abln52_298[0], 5, 12, ya_free_4125_ln31_296);
                bounds_check(24, 17 + 6, "../parsePLCPHeader.blk:101:3-45");
                bitArrWrite(__bit_arr301, 17, 7, ya_free_4125_ln31_296);
                pln53_299 = 0;
                for (int32 j302 = 0; j302 < 0 + 8; j302++) {
                    bounds_check(24, 0 + j302 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4125_ln31_296, 0 + j302, &bitres303);
                    pln53_299 = pln53_299 & bitres303;
                }
                for (int32 j304 = 0; j304 < 0 + 8; j304++) {
                    bounds_check(24, 8 + j304 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4125_ln31_296, 8 + j304, &bitres305);
                    pln53_299 = pln53_299 & bitres305;
                }
                for (int32 j306 = 0; j306 < 0 + 8; j306++) {
                    bounds_check(24, 16 + j306 + 0,
                                 "../parsePLCPHeader.blk:108:18-19");
                    bitRead(ya_free_4125_ln31_296, 16 + j306, &bitres307);
                    pln53_299 = pln53_299 & bitres307;
                }
                bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:22-24");
                bitRead(ya_free_4125_ln31_296, 17, &bitres308);
                bounds_check(24, 17 + 0, "../parsePLCPHeader.blk:111:3-29");
                bitWrite(ya_free_4125_ln31_296, 17, bitres308 ^ pln53_299);
                _unused_24048_ln31_309 = 0;
                idx_free_4126_ln31_310 = 0;
                if (idx_free_4126_ln31_310 < 4 + 0) {
                    __branch_var_ln31_319 = 0;
                    __branch_var_ln31_314 = 0;
                    __bnd_fst_ln31_312_state = FALSE;
                } else {
                    __branch_var_ln31_319 = 1;
                }
                *(uint8*) (BitArrPtr) (s_ln93_321 + 0) =
                    *(uint8*) (BitArrPtr) (&__bit_arr320[0] + 0);
                __branch_var_ln109_327 = 0;
                goto __par_c2_ln31_294_tick;
            }
        }
        
      __bnd_fst_ln32_170_process: 
        {
            return IMMEDIATE;
        }
        ORIGIN("test_tx_header.blk:32:6-67");
        
      __par_c2_ln29_167_tick: 
        {
            switch (__branch_var_ln32_171) {
                
                
              case 0:
                goto __bnd_fst_ln32_170_tick;
                
                
              case 1:
                goto __par_c2_ln31_294_tick;
                
                
              case 2:
                goto __par_c2_ln32_176_tick;
            }
        }
        
      __par_c2_ln29_167_process: 
        {
            switch (__branch_var_ln32_171) {
                
                
              case 0:
                goto __bnd_fst_ln32_170_process;
                
                
              case 1:
                goto __bnd_rest_ln32_173_process;
                
                
              case 2:
                goto __bnd_rest_ln32_175_process;
            }
        }
        ORIGIN("test_tx_header.blk:29:17-33:14");
        
      __par_c2_ln29_165_tick: 
        {
            goto __par_c2_ln29_167_tick;
        }
        
      __par_c2_ln29_165_process: 
        {
            buf_putarrbit(params, pbuf_ctx, __yv_tmp_ln29_166_buf, 24);
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
    init_getint32(params, pbuf_ctx, pheap_ctx, 4);
    init_putbit(params, pbuf_ctx, pheap_ctx, 1);
}
void wpl_output_finalize()

{
    flush_putbit(params, pbuf_ctx);
}
void wpl_output_reset()

{
    reset_putbit(params, pbuf_ctx);
}
void wpl_global_init(memsize_int max_heap_siz)
{
    wpl_init_heap(pheap_ctx, max_heap_siz);
}
